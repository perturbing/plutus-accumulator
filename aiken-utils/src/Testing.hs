{-# LANGUAGE OverloadedStrings #-}

module Testing (printMembershipTestingVectors, printNonMembershipTestingVectors) where

import Plutus.Crypto.BlsUtils (
    Scalar (..),
    getFinalPoly,
    getG1Commitment,
    getG2Commitment,
    mkScalar,
 )

import PlutusTx.Builtins qualified as P
import PlutusTx.Prelude qualified as P

import Data.ByteString (ByteString)
import Data.Word ()

import Data.Text (replace, unpack)
import GHC.ByteOrder (ByteOrder (..))
import GHC.IO (unsafePerformIO)
import Hedgehog.Internal.Gen qualified as G
import Hedgehog.Internal.Range qualified as R
import Offchain (
    extendedEuclideanPoly,
    quotRemScalarPoly,
 )
import Text.Hex (encodeHex)
import Text.Numerals (NumToWord (toCardinal), english)
import Prelude hiding (gcd)

listOfSizedByteStrings :: Integer -> Integer -> [ByteString]
listOfSizedByteStrings n l =
    unsafePerformIO
        . G.sample
        $ G.list
            (R.singleton $ fromIntegral n)
            (G.bytes (R.singleton $ fromIntegral l))

printMembershipTestingVectors :: Integer -> IO ()
printMembershipTestingVectors n = do
    let g1 = P.bls12_381_G1_uncompress P.bls12_381_G1_compressed_generator
        g2 = P.bls12_381_G2_uncompress P.bls12_381_G2_compressed_generator
        tau = mkScalar 5
        k = 1024
        -- power of tau
        crsG1 = map (\x -> P.bls12_381_G1_scalarMul (unScalar (P.scale x tau)) g1) [0 .. k + 10]
        crsG2 = map (\x -> P.bls12_381_G2_scalarMul (unScalar (P.scale x tau)) g2) [0 .. k + 10]
        -- A list of k random elements of size 28 bytes (blake2b_224 hash size)
        setBs = map ((mkScalar . P.byteStringToInteger BigEndian) . P.toBuiltin) $ listOfSizedByteStrings k 28
        -- the 96 bytes commitmet of the bls accumulator
        accBs = P.bls12_381_G2_compress . getG2Commitment crsG2 $ getFinalPoly setBs

        -- Hex string conversion for the output
        crsG1Hex = map (showByteArray . P.bls12_381_G1_compress) (P.take (n + 1) crsG1)

        -- Subset
        subset = P.take n setBs

        -- Proof
        setMinusSubset = P.drop n setBs
        proofBs = P.bls12_381_G2_compress . getG2Commitment crsG2 $ getFinalPoly setMinusSubset

    putStrLn ("test check_membership_" ++ unpack (replace "-" "_" (toCardinal english n)) ++ "_elements() {")
    putStrLn "  // CRS G1:"
    putStrLn "  let crs_g1 = ["
    putStr $ " " ++ makeByteArrayListWithType "<Bls12_381, G1>" crsG1Hex
    putStrLn "    ]\n\n"

    putStrLn "  // Accumulator:"
    putStrLn ("  let accumulator = #<Bls12_381, G2>\"" ++ showByteArray accBs ++ "\"\n\n")

    putStrLn "  // Subset:"
    putStrLn "  let subset = ["
    putStr $ "  " ++ makeIntList (map (show . unScalar) subset)
    putStrLn "    ]\n\n"

    putStrLn "  // Proof:"
    putStrLn ("  let proof = #<Bls12_381, G2>\"" ++ showByteArray proofBs ++ "\"\n\n")

    putStrLn "  check_membership("
    putStrLn "    crs_g1,"
    putStrLn "    accumulator,"
    putStrLn "    subset,"
    putStrLn "    proof,"
    putStrLn "  )"
    putStrLn "}"

printNonMembershipTestingVectors :: Integer -> IO ()
printNonMembershipTestingVectors n = do
    -- setup
    let g1 = P.bls12_381_G1_uncompress P.bls12_381_G1_compressed_generator
        g2 = P.bls12_381_G2_uncompress P.bls12_381_G2_compressed_generator
        tau = mkScalar 5
        k = 1024
        -- power of tau
        crsG1 = map (\x -> P.bls12_381_G1_scalarMul (unScalar (P.scale x tau)) g1) [0 .. k + 10]
        crsG2 = map (\x -> P.bls12_381_G2_scalarMul (unScalar (P.scale x tau)) g2) [0 .. k + 10]
        -- A list of k random elements of size 28 bytes (blake2b_224 hash size)
        setBs = map P.toBuiltin $ listOfSizedByteStrings k 28 :: [P.BuiltinByteString]
        -- the 96 bytes commitmet of the bls accumulator
        accBs = P.bls12_381_G2_compress . getG2Commitment crsG2 . getFinalPoly . map (mkScalar . P.byteStringToInteger BigEndian) $ setBs
        -- disjointSet = map P.toBuiltin $ listOfSizedByteStrings n 28 :: [P.BuiltinByteString]

        g = getFinalPoly . map (mkScalar . P.byteStringToInteger BigEndian) $ disjointSet
        f = getFinalPoly . map (mkScalar . P.byteStringToInteger BigEndian) $ setBs
        (gcd, a, b) = extendedEuclideanPoly f g
        (aNorm, bNorm) = (fst (a `quotRemScalarPoly` gcd), fst (b `quotRemScalarPoly` gcd))
        (aCom, bCom) = (getG1Commitment crsG1 aNorm, getG2Commitment crsG2 bNorm)
        proofBs = (P.bls12_381_G1_compress aCom, P.bls12_381_G2_compress bCom)

        -- Hex string conversion for the output
        crsG1Hex = map (showByteArray . P.bls12_381_G1_compress) (P.take (n + 1) crsG1)

        -- Disjoint set
        disjointSetInt = P.replicate n 55
        disjointSet = map (P.integerToByteString BigEndian 28) disjointSetInt :: [P.BuiltinByteString]

    putStrLn ("test check_non_membership_" ++ unpack (replace "-" "_" (toCardinal english n)) ++ "_elements() {")
    putStrLn "  // CRS G1:"
    putStrLn "  let crs_g1 = ["
    putStr $ " " ++ makeByteArrayListWithType "<Bls12_381, G1>" crsG1Hex
    putStrLn "    ]\n\n"

    putStrLn "  // G2:"
    putStrLn ("  let g2 = #<Bls12_381, G2>\"" ++ showByteArray (P.bls12_381_G2_compress g2) ++ "\"\n\n")

    putStrLn "  // Accumulator:"
    putStrLn ("  let accumulator = #<Bls12_381, G2>\"" ++ showByteArray accBs ++ "\"\n\n")

    putStrLn "  // Disjoint set:"
    putStrLn "  let disjoint_set = ["
    putStr $ "  " ++ makeIntList (map show disjointSetInt)
    putStrLn "    ]\n\n"

    putStrLn "  // Proof:"
    putStrLn ("  let proof = (#<Bls12_381, G1>\"" ++ (showByteArray . fst) proofBs ++ "\", #<Bls12_381, G2>\"" ++ (showByteArray . snd) proofBs ++ "\") \n\n")

    putStrLn "  check_nonmembership("
    putStrLn "    crs_g1,"
    putStrLn "    g2,"
    putStrLn "    accumulator,"
    putStrLn "    disjoint_set,"
    putStrLn "    proof,"
    putStrLn "  )"
    putStrLn "}"

showByteArray :: P.BuiltinByteString -> String
showByteArray bs = do
    let bs' = P.fromBuiltin bs :: ByteString
    unpack (encodeHex bs')

makeByteArrayListWithType :: String -> [String] -> String
makeByteArrayListWithType t s = go s ""
  where
    go [] acc = acc
    go [x] acc = acc ++ "#" ++ t ++ "\"" ++ x ++ "\"" ++ "\n"
    go (x : xs) acc = go xs (acc ++ "#" ++ t ++ "\"" ++ x ++ "\"," ++ "\n")

makeIntList :: [String] -> String
makeIntList s = go s ""
  where
    go [] acc = acc
    go [x] acc = acc ++ x ++ "\n"
    go (x : xs) acc = go xs (acc ++ x ++ "," ++ "\n")
