{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module RunVerifier (
    runCheckMembership,
    runCheckNonMembership,
) where

import Offchain (extendedEuclideanPoly, quotRemScalarPoly)
import Plutus.Crypto.Accumulator (checkMembership, checkNonMembership)
import Plutus.Crypto.BlsUtils (Scalar (..), getFinalPoly, getG1Commitment, getG2Commitment, mkScalar)

import Scripts (checkMembershipScript, checkNonMembershipScript, listOfSizedByteStrings)

import PlutusBenchmark.Common (TestSize (..), printHeader, printSizeStatistics)
import qualified PlutusTx.Builtins as P
import qualified PlutusTx.Numeric as Num
import qualified PlutusTx.Prelude as P

import System.IO (Handle)
import Text.Printf (hPrintf)

import Data.Aeson (FromJSON, ToJSON, decode)
import GHC.Generics (Generic)

import Data.ByteString (pack)
import qualified Data.ByteString.Lazy as BL
import Data.Word ()

import GHC.ByteOrder (ByteOrder (..))

printCostsCheckMembership ::
    Handle ->
    Integer ->
    [P.BuiltinBLS12_381_G1_Element] ->
    [P.BuiltinBLS12_381_G2_Element] ->
    P.BuiltinByteString ->
    [P.BuiltinByteString] ->
    IO ()
printCostsCheckMembership h n crsG1 crsG2 accBs setBs =
    let
        subsetBs = P.take n setBs
        setMinusSubset = filter (`notElem` subsetBs) setBs
        proofBs = P.bls12_381_G2_compress . getG2Commitment crsG2 . getFinalPoly . map (mkScalar . P.byteStringToInteger BigEndian) $ setMinusSubset
        crsBs = P.take (n + 1) $ map P.bls12_381_G1_compress crsG1
        script = checkMembershipScript crsBs accBs subsetBs proofBs
     in
        do printSizeStatistics h (TestSize n) script

-- Uncomment below to print if the proof succeeds at all in between each benchmark
--   print $ checkMembership
--     (map P.bls12_381_G1_uncompress crsBs)
--     (P.bls12_381_G2_uncompress accBs)
--     (map (mkScalar . P.byteStringToInteger BigEndian) subsetBs)
--     (P.bls12_381_G2_uncompress proofBs)

runCheckMembership :: Handle -> IO ()
runCheckMembership h = do
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
    hPrintf h "n membership proofs aggregated verification\n\n"
    printHeader h
    mapM_ (\x -> printCostsCheckMembership h x crsG1 crsG2 accBs setBs) ([1, 2, 3, 4] <> [5, 10 .. 45])
    hPrintf h "\n\n"

printCostsCheckNonMembership ::
    Handle ->
    Integer ->
    [P.BuiltinBLS12_381_G1_Element] ->
    [P.BuiltinBLS12_381_G2_Element] ->
    P.BuiltinByteString ->
    [P.BuiltinByteString] ->
    IO ()
printCostsCheckNonMembership h n crsG1 (g2 : xs) accBs setBs =
    let
        -- disjointSet = map P.toBuiltin $ listOfSizedByteStrings n 28 :: [P.BuiltinByteString]
        disjointSet = map (P.integerToByteString BigEndian 28) (P.replicate n 55) :: [P.BuiltinByteString]
        g = getFinalPoly . map (mkScalar . P.byteStringToInteger BigEndian) $ disjointSet
        f = getFinalPoly . map (mkScalar . P.byteStringToInteger BigEndian) $ setBs
        (gcd, a, b) = extendedEuclideanPoly f g
        (aNorm, bNorm) = (fst (a `quotRemScalarPoly` gcd), fst (b `quotRemScalarPoly` gcd))
        (aCom, bCom) = (getG1Commitment crsG1 aNorm, getG2Commitment (g2 : xs) bNorm)
        proofBs = (P.bls12_381_G1_compress aCom, P.bls12_381_G2_compress bCom)
        crsBs = P.take (n + 1) $ map P.bls12_381_G1_compress crsG1
        script = checkNonMembershipScript crsBs (P.bls12_381_G2_compress g2) accBs disjointSet proofBs
     in
        do printSizeStatistics h (TestSize n) script

-- Uncomment below to print if the proof succeeds at all in between each benchmark
--   print $ checkNonMembership
--     (map P.bls12_381_G1_uncompress crsBs)
--     g2
--     (P.bls12_381_G2_uncompress accBs)
--     (map (mkScalar . P.byteStringToInteger BigEndian) disjointSet)
--     (aCom, bCom)

runCheckNonMembership :: Handle -> IO ()
runCheckNonMembership h = do
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
    hPrintf h "n non-membership proofs aggregated verification\n\n"
    printHeader h
    mapM_ (\x -> printCostsCheckNonMembership h x crsG1 crsG2 accBs setBs) ([1, 2, 3, 4] <> [5, 10 .. 45])
    hPrintf h "\n\n"
