module Main where

import Plutus.Crypto.BlsUtils (
    Scalar (..),
    getFinalPoly,
    getG2Commitment,
    mkScalar,
 )

import PlutusTx.Builtins qualified as P
import PlutusTx.Prelude qualified as P

import Data.ByteString (ByteString)
import Data.Word ()

import Data.Text (unpack)
import GHC.ByteOrder (ByteOrder (..))
import GHC.IO (unsafePerformIO)
import Hedgehog.Internal.Gen qualified as G
import Hedgehog.Internal.Range qualified as R
import Text.Hex (encodeHex)

listOfSizedByteStrings :: Integer -> Integer -> [ByteString]
listOfSizedByteStrings n l =
    unsafePerformIO
        . G.sample
        $ G.list
            (R.singleton $ fromIntegral n)
            (G.bytes (R.singleton $ fromIntegral l))

main :: IO ()
main = do
    let g1 = P.bls12_381_G1_uncompress P.bls12_381_G1_compressed_generator
        g2 = P.bls12_381_G2_uncompress P.bls12_381_G2_compressed_generator
        tau = mkScalar 5
        k = 1024
        n = 10
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

    putStrLn "// CRS G1:"
    putStrLn "let crs_g1 = ["
    putStr $ makeByteArrayList crsG1Hex
    putStrLn "  ]\n\n"

    putStrLn "// Accumulator:"
    putStrLn ("let accumulator = #\"" ++ showByteArray accBs ++ "\"\n\n")

    putStrLn "// Subset:"
    putStrLn "let subset = ["
    putStr $ makeIntList (map (show . unScalar) subset)
    putStrLn "  ]\n\n"

    putStrLn "// Proof:"
    putStrLn ("let proof = #\"" ++ showByteArray proofBs ++ "\"\n\n")

showByteArray :: P.BuiltinByteString -> String
showByteArray bs = do
    let bs' = P.fromBuiltin bs :: ByteString
    unpack (encodeHex bs')

makeByteArrayList :: [String] -> String
makeByteArrayList s = go s ""
  where
    go [] acc = acc
    go [x] acc = acc ++ "#\"" ++ x ++ "\"" ++ "\n"
    go (x : xs) acc = go xs (acc ++ "#\"" ++ x ++ "\"," ++ "\n")

makeIntList :: [String] -> String
makeIntList s = go s ""
  where
    go [] acc = acc
    go [x] acc = acc ++ x ++ "\n"
    go (x : xs) acc = go xs (acc ++ x ++ "," ++ "\n")
