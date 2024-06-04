module Offchain (
    compressG1Point,
    compressG2Point,
    quotRemScalarPoly,
    extendedEuclideanPoly,
) where

import PlutusTx (makeIsDataIndexed, makeLift, unstableMakeIsData)
import PlutusTx.Builtins (
    BuiltinBLS12_381_G1_Element,
    BuiltinBLS12_381_G2_Element,
    BuiltinByteString,
    bls12_381_G1_compress,
    bls12_381_G1_compressed_zero,
    bls12_381_G1_neg,
    bls12_381_G1_uncompress,
    bls12_381_G2_compress,
    bls12_381_G2_compressed_zero,
    bls12_381_G2_neg,
    bls12_381_G2_uncompress,
    byteStringToInteger,
    integerToByteString,
 )
import PlutusTx.List (dropWhile, foldl, last, map, replicate, reverse, zip, zipWith)
import PlutusTx.Numeric (
    AdditiveGroup (..),
    AdditiveMonoid (..),
    AdditiveSemigroup (..),
    Module (..),
    MultiplicativeMonoid (..),
    MultiplicativeSemigroup (..),
    negate,
 )
import PlutusTx.Prelude (
    Bool (..),
    Eq (..),
    Integer,
    Ord ((<), (<=)),
    divide,
    enumFromTo,
    error,
    even,
    foldr,
    length,
    not,
    otherwise,
    ($),
    (&&),
    (*),
    (+),
    (.),
    (<>),
    (>),
    (>=),
    (||),
 )
import qualified Prelude as Haskell

import Data.Bits (setBit, testBit)
import GHC.ByteOrder (ByteOrder (..))
import Plutus.Crypto.BlsUtils (Fp (..), Fp2 (..), MultiplicativeGroup (..), Scalar (..), ScalarPoly (..), negateScalar)

-- Helper functions (this is some legacy code that we need to refactor).
-- These functions are there to help with the offchain, but note that
-- they are inefficient and definitly are not intended to be used onchain.

-- Convert a G1 point from affine coordinates to compressed form.
{-# NOINLINE compressG1Point #-}
compressG1Point :: (Fp, Fp) -> BuiltinBLS12_381_G1_Element
compressG1Point (x, y)
    | x == zero && y == one = bls12_381_G1_uncompress bls12_381_G1_compressed_zero
    | otherwise = go ((bls12_381_G1_uncompress . integerToByteString BigEndian 48 . setCompressedBit . unFp) x) y
  where
    setCompressedBit x = setBit x 383
    go :: BuiltinBLS12_381_G1_Element -> Fp -> BuiltinBLS12_381_G1_Element
    go p y'
        | y' < negate y' = p
        | otherwise = bls12_381_G1_neg p

{-# NOINLINE pow #-}
pow :: Integer -> Integer -> Integer
pow b e
    | e < 0 = zero
    | e == 0 = 1
    | even e = pow (b * b) (e `divide` 2)
    | otherwise = b * pow (b * b) ((e - 1) `divide` 2)

-- Convert a G2 point from affine coordinates to compressed form.
{-# NOINLINE compressG2Point #-}
compressG2Point :: (Fp2, Fp2) -> BuiltinBLS12_381_G2_Element
compressG2Point (x, y)
    | x == zero && y == one = bls12_381_G2_uncompress bls12_381_G2_compressed_zero
    | otherwise = go (bls12_381_G2_uncompress xBsG2) y
  where
    x' = unFp (c0 x) + unFp (c1 x) * pow 2 384
    xBsG2 = integerToByteString BigEndian 96 $ setBit x' 767
    go :: BuiltinBLS12_381_G2_Element -> Fp2 -> BuiltinBLS12_381_G2_Element
    go x y
        | y < negate y = x
        | otherwise = bls12_381_G2_neg x

-- Function to remove trailing zeros from a ScalarPoly
-- Note that since this polynomial works over a field, any operation
-- can result in a highest degree coefficient of 0.
-- This function is useful to remove these trailing zeros after all operations.
{-# NOINLINE removeTrailingZeros #-}
removeTrailingZeros :: ScalarPoly -> ScalarPoly
removeTrailingZeros (ScalarPoly coeffs) = ScalarPoly (reverse (dropWhile (== zero) (reverse coeffs)))

-- Function to get the degree of a ScalarPoly
{-# NOINLINE degree #-}
degree :: ScalarPoly -> Integer
degree (ScalarPoly coeffs) = length coeffs - 1

instance Eq ScalarPoly where
    {-# NOINLINE (==) #-}
    ScalarPoly a == ScalarPoly b = a == b

instance AdditiveSemigroup ScalarPoly where
    {-# NOINLINE (+) #-}
    (ScalarPoly a) + (ScalarPoly b) =
        let lengthA = length a
            lengthB = length b
         in if lengthA >= lengthB
                then removeTrailingZeros . ScalarPoly $ zipWith (+) a (b <> replicate (lengthA - lengthB) zero)
                else removeTrailingZeros . ScalarPoly $ zipWith (+) (a <> replicate (lengthB - lengthA) zero) b

instance AdditiveMonoid ScalarPoly where
    {-# NOINLINE zero #-}
    zero = ScalarPoly [zero]

-- Note that + already performs removal of trailing zeros.
instance AdditiveGroup ScalarPoly where
    {-# NOINLINE (-) #-}
    (-) a (ScalarPoly b) = a + ScalarPoly (map negateScalar b)

instance MultiplicativeSemigroup ScalarPoly where
    {-# NOINLINE (*) #-}
    (*) a@(ScalarPoly ax) b@(ScalarPoly bx) =
        let la = length ax
            lb = length bx
         in if la <= lb
                then foldl (+) zero (map f (zip (enumFromTo 0 la) ax))
                else b * a
      where
        f :: (Integer, Scalar) -> ScalarPoly
        f (i, coef) = ScalarPoly (replicate i zero <> map (coef *) bx)

instance MultiplicativeMonoid ScalarPoly where
    {-# NOINLINE one #-}
    one = ScalarPoly [one]

-- Testing only, do not use onchain, its inefficient. In a real application use
-- something like: https://flintlib.org/doc/fmpq_poly.html?highlight=gcd#c.fmpq_poly_xgcd
-- same for large polynomials, use a library like flint.
{-# NOINLINE quotRemScalarPoly #-}
quotRemScalarPoly :: ScalarPoly -> ScalarPoly -> (ScalarPoly, ScalarPoly)
quotRemScalarPoly dividend@(ScalarPoly ds) divisor@(ScalarPoly dsr)
    | dividend == zero = (zero, zero)
    | divisor == zero = error ()
    | degree dividend < degree divisor = (zero, dividend)
    | otherwise = divide dividend divisor zero
  where
    divide :: ScalarPoly -> ScalarPoly -> ScalarPoly -> (ScalarPoly, ScalarPoly)
    divide r@(ScalarPoly rs) d@(ScalarPoly dsr) q
        | degree r < degree d = (q, if r == ScalarPoly [] then zero else r)
        | otherwise =
            let leadR = last rs
                leadD = last dsr
                degR = degree r
                degD = degree d
                coeff = leadR `div` leadD
                degDiff = degR - degD
                scaledDivisor = ScalarPoly (replicate degDiff zero <> map (* coeff) dsr)
                newR = removeTrailingZeros (r - scaledDivisor)
                newQ = removeTrailingZeros (q + ScalarPoly (replicate degDiff zero <> [coeff]))
             in divide newR d newQ

{-# NOINLINE extendedEuclideanPoly #-}
extendedEuclideanPoly :: ScalarPoly -> ScalarPoly -> (ScalarPoly, ScalarPoly, ScalarPoly)
extendedEuclideanPoly a b =
    if b == zero
        then (a, one, zero)
        else
            let (q, r) = quotRemScalarPoly a b
                (gcd, x1, y1) = extendedEuclideanPoly b r
                x = y1
                y = x1 - q * y1
             in (gcd, x, y)
