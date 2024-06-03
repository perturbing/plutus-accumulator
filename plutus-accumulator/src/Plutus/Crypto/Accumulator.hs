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
    Scalar (..),
    ScalarPoly (..),
    getFinalPoly,
 )
import PlutusTx.Builtins (
    BuiltinBLS12_381_G1_Element,
    BuiltinBLS12_381_G2_Element,
    bls12_381_G1_add,
    bls12_381_G1_compress,
    bls12_381_G1_scalarMul,
    bls12_381_G2_add,
    bls12_381_G2_scalarMul,
    bls12_381_finalVerify,
    bls12_381_millerLoop,
    bls12_381_mulMlResult,
 )
import PlutusTx.List (foldl, zipWith)
import PlutusTx.Numeric (
    AdditiveMonoid (..),
 )
import PlutusTx.Prelude (
    Bool (..),
    ($),
    (.),
 )

{-# INLINEABLE getG1Commitment #-}
getG1Commitment :: [BuiltinBLS12_381_G1_Element] -> ScalarPoly -> BuiltinBLS12_381_G1_Element
getG1Commitment crsG1 (ScalarPoly poly) =
    foldl bls12_381_G1_add zero
        $ zipWith (bls12_381_G1_scalarMul . unScalar) poly crsG1

{-# INLINEABLE getG2Commitment #-}
getG2Commitment :: [BuiltinBLS12_381_G2_Element] -> ScalarPoly -> BuiltinBLS12_381_G2_Element
getG2Commitment crsG2 (ScalarPoly poly) =
    foldl bls12_381_G2_add zero
        $ zipWith (bls12_381_G2_scalarMul . unScalar) poly crsG2

{-# INLINEABLE checkMembership #-}
checkMembership ::
    [BuiltinBLS12_381_G1_Element] ->
    BuiltinBLS12_381_G2_Element ->
    [Scalar] ->
    BuiltinBLS12_381_G2_Element ->
    Bool
checkMembership (g1 : xs) acc subset proof =
    bls12_381_finalVerify
        (bls12_381_millerLoop g1 acc)
        (bls12_381_millerLoop (getG1Commitment (g1 : xs) (getFinalPoly subset)) proof)

{-# INLINEABLE checkNonMembership #-}
checkNonMembership ::
    [BuiltinBLS12_381_G1_Element] ->
    BuiltinBLS12_381_G2_Element ->
    BuiltinBLS12_381_G2_Element ->
    [Scalar] ->
    (BuiltinBLS12_381_G1_Element, BuiltinBLS12_381_G2_Element) ->
    Bool
checkNonMembership (g1 : xs) g2 acc disjointSet (a, b) =
    bls12_381_finalVerify
        (bls12_381_mulMlResult (bls12_381_millerLoop a acc) (bls12_381_millerLoop (getG1Commitment (g1 : xs) (getFinalPoly disjointSet)) b))
        (bls12_381_millerLoop g1 g2)

-- TODO: add interface to add and remove elements from the accumulator.
