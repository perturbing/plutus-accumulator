{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}

module Plutus.Crypto.Accumulator (
    checkMembership,
    checkNonMembership,
    getG1Commitment,
    getG2Commitment,
) where

import Plutus.Crypto.BlsUtils (
    MultiplicativeGroup (..),
    Scalar (..),
    ScalarPoly (..),
    bls12_381_scalar_prime,
    getFinalPoly,
    mkScalar,
    negateScalar,
    powerOfTwoExponentiationScalar,
 )
import PlutusTx.Builtins (
    BuiltinBLS12_381_G1_Element,
    BuiltinBLS12_381_G2_Element,
    BuiltinByteString,
    Integer,
    blake2b_224,
    bls12_381_G1_add,
    bls12_381_G1_compress,
    bls12_381_G1_compressed_generator,
    bls12_381_G1_scalarMul,
    bls12_381_G1_uncompress,
    bls12_381_G2_add,
    bls12_381_G2_compress,
    bls12_381_G2_compressed_generator,
    bls12_381_G2_scalarMul,
    bls12_381_G2_uncompress,
    bls12_381_finalVerify,
    bls12_381_millerLoop,
    bls12_381_mulMlResult,
    byteStringToInteger,
    integerToByteString,
 )
import PlutusTx.Eq (Eq (..))
import PlutusTx.List (and, foldl, head, map, tail, zipWith)
import PlutusTx.Numeric (
    AdditiveGroup (..),
    AdditiveMonoid (..),
    AdditiveSemigroup (..),
    Module (..),
    MultiplicativeMonoid (..),
    MultiplicativeSemigroup (..),
 )
import PlutusTx.Prelude (
    Bool (..),
    divide,
    enumFromTo,
    error,
    even,
    mconcat,
    modulo,
    otherwise,
    sum,
    ($),
    (&&),
    (.),
    (<),
    (<>),
    (>),
    (||),
 )

{-# INLINEABLE getG1Commitment #-}
getG1Commitment :: [BuiltinBLS12_381_G1_Element] -> ScalarPoly -> BuiltinBLS12_381_G1_Element
getG1Commitment crsG1 (ScalarPoly poly) = foldl bls12_381_G1_add zero $ zipWith (bls12_381_G1_scalarMul . unScalar) poly crsG1

{-# INLINEABLE getG2Commitment #-}
getG2Commitment :: [BuiltinBLS12_381_G2_Element] -> ScalarPoly -> BuiltinBLS12_381_G2_Element
getG2Commitment crsG2 (ScalarPoly poly) = foldl bls12_381_G2_add zero $ zipWith (bls12_381_G2_scalarMul . unScalar) poly crsG2

{-# INLINEABLE checkMembership #-}
checkMembership :: [BuiltinBLS12_381_G1_Element] -> BuiltinBLS12_381_G2_Element -> [Scalar] -> BuiltinBLS12_381_G2_Element -> Bool
checkMembership (g1 : xs) acc subset proof = bls12_381_finalVerify (bls12_381_millerLoop g1 acc) (bls12_381_millerLoop (getG1Commitment (g1 : xs) (getFinalPoly subset)) proof)

{-# INLINEABLE checkNonMembership #-}
checkNonMembership :: [BuiltinBLS12_381_G1_Element] -> BuiltinBLS12_381_G2_Element -> BuiltinBLS12_381_G2_Element -> [Scalar] -> (BuiltinBLS12_381_G1_Element, BuiltinBLS12_381_G2_Element) -> Bool
checkNonMembership (g1 : xs) g2 acc disjointSet (a, b) = bls12_381_finalVerify (bls12_381_mulMlResult (bls12_381_millerLoop a acc) (bls12_381_millerLoop (getG1Commitment (g1 : xs) (getFinalPoly disjointSet)) b)) (bls12_381_millerLoop g1 g2)
