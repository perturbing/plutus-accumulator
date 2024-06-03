{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Scripts (
    checkMembershipScript,
    checkNonMembershipScript,
    listOfSizedByteStrings,
) where

import PlutusTx (compile, getPlcNoAnn, liftCodeDef, unsafeApplyCode)
import PlutusTx.Builtins (bls12_381_G1_uncompress, bls12_381_G2_uncompress, byteStringToInteger)
import PlutusTx.Prelude (Bool, BuiltinByteString, Integer, map, ($), (.))

import PlutusCore (DefaultFun, DefaultUni)
import qualified UntypedPlutusCore as UPLC

import Data.ByteString (ByteString)
import GHC.ByteOrder (ByteOrder (..))
import qualified Hedgehog.Internal.Gen as G
import qualified Hedgehog.Internal.Range as R
import Plutus.Crypto.Accumulator (checkMembership, checkNonMembership)
import Plutus.Crypto.BlsUtils (mkScalar)
import System.IO.Unsafe (unsafePerformIO)
import qualified Prelude as Haskell

{-# NOINLINE listOfSizedByteStrings #-}
listOfSizedByteStrings :: Integer -> Integer -> [ByteString]
listOfSizedByteStrings n l =
    unsafePerformIO
        . G.sample
        $ G.list
            (R.singleton $ Haskell.fromIntegral n)
            (G.bytes (R.singleton $ Haskell.fromIntegral l))

{-# INLINEABLE checkMembershipWithBytes #-}
checkMembershipWithBytes ::
    [BuiltinByteString] ->
    BuiltinByteString ->
    [BuiltinByteString] ->
    BuiltinByteString ->
    Bool
checkMembershipWithBytes crsBs accBs subset proofBs =
    checkMembership
        (map bls12_381_G1_uncompress crsBs)
        (bls12_381_G2_uncompress accBs)
        (map (mkScalar . byteStringToInteger BigEndian) subset)
        (bls12_381_G2_uncompress proofBs)

checkMembershipScript ::
    [BuiltinByteString] ->
    BuiltinByteString ->
    [BuiltinByteString] ->
    BuiltinByteString ->
    UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
checkMembershipScript crsBs accBs subset proofBs =
    getPlcNoAnn
        $ $$(compile [||checkMembershipWithBytes||])
        `unsafeApplyCode` liftCodeDef crsBs
        `unsafeApplyCode` liftCodeDef accBs
        `unsafeApplyCode` liftCodeDef subset
        `unsafeApplyCode` liftCodeDef proofBs

{-# INLINEABLE chechNonMembershipWithBytes #-}
chechNonMembershipWithBytes ::
    [BuiltinByteString] ->
    BuiltinByteString ->
    BuiltinByteString ->
    [BuiltinByteString] ->
    (BuiltinByteString, BuiltinByteString) ->
    Bool
chechNonMembershipWithBytes crsBs g2Bs accBs disjointSet (a, b) =
    checkNonMembership
        (map bls12_381_G1_uncompress crsBs)
        (bls12_381_G2_uncompress g2Bs)
        (bls12_381_G2_uncompress accBs)
        (map (mkScalar . byteStringToInteger BigEndian) disjointSet)
        (bls12_381_G1_uncompress a, bls12_381_G2_uncompress b)

checkNonMembershipScript ::
    [BuiltinByteString] ->
    BuiltinByteString ->
    BuiltinByteString ->
    [BuiltinByteString] ->
    (BuiltinByteString, BuiltinByteString) ->
    UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
checkNonMembershipScript crsBs g2Bs accBs disjointSet proofBs =
    getPlcNoAnn
        $ $$(compile [||chechNonMembershipWithBytes||])
        `unsafeApplyCode` liftCodeDef crsBs
        `unsafeApplyCode` liftCodeDef g2Bs
        `unsafeApplyCode` liftCodeDef accBs
        `unsafeApplyCode` liftCodeDef disjointSet
        `unsafeApplyCode` liftCodeDef proofBs
