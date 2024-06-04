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
) where

import Plutus.Crypto.BlsUtils (
    Scalar (..),
    ScalarPoly (..),
    getFinalPoly,
    getG1Commitment,
    getG2Commitment,
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

-- This module contains the basic function to check membership and non-membership in the accumulator.
-- DISCLAIMER: it is bad practice and insecure to combine the two pairing checks in a single
-- bls12_381_finalVerify call. This is because check is not linear independent.

{- | Check if the given elements are a member of the accumulator.
  This function takes in a common reference string (crsG1) for its first argument.
  This is not hard-coded, as you can optimize the transactions by referencing a UTXO
  that contains this setup of G1 elements (the same for the G2 generator).

  Besides the above, the function takes in the commitment to the accumulator (a G2 element)
  and a list of scalars that are proven to be a subset of the commitment. The last argument is
  the proof, that also is an G2 element.

  This proof relies on the unique decomposition of the accumulator given as

  acc(x) = \prod_{i \in S}(x + x_i) = \prod_{i \in subset}(x_i + x) * quotient(x) + remainder(x)

  where the remainder is 0 if the polynomial is divisible by the subset.
  See; https://en.wikipedia.org/wiki/Polynomial_remainder_theorem
-}
{-# INLINEABLE checkMembership #-}
checkMembership ::
    [BuiltinBLS12_381_G1_Element] -> -- crsG1 setup
    BuiltinBLS12_381_G2_Element -> -- accumulator commitment
    [Scalar] -> -- subset of the commitment that are proven
    BuiltinBLS12_381_G2_Element -> -- proof
    Bool
checkMembership (g1 : xs) acc subset proof =
    -- Here we are checking that
    --
    -- e(g1, acc) = e(comm(subset),proof)
    --
    -- which equates to
    --
    -- 1 * acc(\tau) == ( \prod_{i \in subset}(\tau + x_i) ) * proof(\tau)
    bls12_381_finalVerify
        (bls12_381_millerLoop g1 acc)
        (bls12_381_millerLoop (getG1Commitment (g1 : xs) (getFinalPoly subset)) proof)

{- | Check if the given elements are not a member of the accumulator.
  This function takes in a common reference string (crsG1) for its first argument.
  This is not hard-coded, as you can optimize the transactions by referencing a UTXO
  that contains this setup of G1 elements (the same for the G2 generator).

  Besides the above, the function takes in the commitment to the accumulator (a G2 element)
  and a list of scalars that are proven to be a disjoint set of the commitment.
  The last argument is the proof, which is a G1 and G2 element.

  This proof relies on the fact that any commitment to a disjoint set of elements
  will have no overlapping zero points with the accumulator. Meaning that the greatest
  common divisor of the accumulator and the disjoint set is 1. For such two
  polynomials, you can find an a(x) and b(x) such that

  a(x) * acc(x) + b(x) * disjoint(x) = 1

  See; https://en.wikipedia.org/wiki/B%C3%A9zout%27s_identity

  Note there that:

  For univariate polynomials f and g with coefficients in a field,
  there exist polynomials a and b such that af + bg = 1 if and only if
  f and g have no common root in any algebraically closed field.
-}
{-# INLINEABLE checkNonMembership #-}
checkNonMembership ::
    [BuiltinBLS12_381_G1_Element] -> -- crsG1 setup
    BuiltinBLS12_381_G2_Element -> -- g2 generator
    BuiltinBLS12_381_G2_Element -> -- accumulator commitment
    [Scalar] -> -- disjoint set
    (BuiltinBLS12_381_G1_Element, BuiltinBLS12_381_G2_Element) -> -- proof
    Bool
checkNonMembership (g1 : xs) g2 acc disjointSet (a, b) =
    -- Here we are checking that
    --
    -- e(g1, g2) = e(acc,proofA) * e(comm(disjointSet),proofB)
    --
    -- which equates to
    --
    -- 1 == acc(\tau)*proofA(\tau) + ( \prod_{i \in subset}(\tau + x_i) ) * proofB(\tau)
    bls12_381_finalVerify
        (bls12_381_mulMlResult (bls12_381_millerLoop a acc) (bls12_381_millerLoop (getG1Commitment (g1 : xs) (getFinalPoly disjointSet)) b))
        (bls12_381_millerLoop g1 g2)
