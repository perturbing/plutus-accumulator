{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module RunVerifier (
    runCheckMembership,
) where

import Offchain (compressG1Point, compressG2Point)
import Plutus.Crypto.Accumulator (getG2Commitment)
import Plutus.Crypto.BlsUtils (Fp (..), Fp2 (..), Scalar (..), bls12_381_scalar_prime, getFinalPoly, mkFp, mkScalar)

import Scripts (checkMembershipScript, listOfSizedByteStrings)

import PlutusBenchmark.Common (TestSize (..), printHeader, printSizeStatistics)
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
    Handle -> -- h
    Integer -> -- n
    [P.BuiltinBLS12_381_G1_Element] -> -- crsG1
    [P.BuiltinBLS12_381_G2_Element] -> -- crsG2
    P.BuiltinByteString -> -- accBs
    [P.BuiltinByteString] -> -- setBs
    IO ()
printCostsCheckMembership h n crsG1 crsG2 accBs setBs =
    let
        subsetBs = P.take n setBs
        setMinusSubset = filter (`notElem` subsetBs) setBs
        proofBs = P.bls12_381_G2_compress . getG2Commitment crsG2 . getFinalPoly . map (mkScalar . P.byteStringToInteger BigEndian) $ setMinusSubset
        crsBs = P.take n $ map P.bls12_381_G1_compress crsG1
        script = checkMembershipScript crsBs accBs subsetBs proofBs
     in
        printSizeStatistics h (TestSize n) script

runCheckMembership :: Handle -> IO ()
runCheckMembership h = do
    -- setup
    let g1 = P.bls12_381_G1_uncompress P.bls12_381_G1_compressed_generator
        g2 = P.bls12_381_G2_uncompress P.bls12_381_G2_compressed_generator
        tau = mkScalar 5
        k = 400
        -- power of tau
        crsG1 = map (\x -> P.bls12_381_G1_scalarMul (unScalar (P.scale x tau)) g1) [0 .. k]
        crsG2 = map (\x -> P.bls12_381_G2_scalarMul (unScalar (P.scale x tau)) g2) [0 .. k]
        -- A list of k random elements of size 28 bytest (blake2b_224 hash size)
        setBs = map P.toBuiltin $ listOfSizedByteStrings k 28 :: [P.BuiltinByteString]
        -- the 96 bytes commitmet of the bls accumulator
        accBs = P.bls12_381_G2_compress . getG2Commitment crsG2 . getFinalPoly . map (mkScalar . P.byteStringToInteger BigEndian) $ setBs

    hPrintf h "n membership proofs aggregated verification\n\n"
    printHeader h
    mapM_ (\x -> printCostsCheckMembership h x crsG1 crsG2 accBs setBs) [0, 5 .. 45]
    hPrintf h "\n\n"
