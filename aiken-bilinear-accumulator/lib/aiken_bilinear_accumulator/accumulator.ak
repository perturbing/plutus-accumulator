use aiken/builtin.{
  bls12_381_final_verify, bls12_381_g1_add, bls12_381_g1_scalar_mul,
  bls12_381_g1_uncompress, bls12_381_miller_loop,
  bls12_381_mul_miller_loop_result,
}
use aiken/collection/list.{foldl, head, map2}
use aiken/crypto/bls12_381/g1
use aiken/crypto/bls12_381/scalar.{Scalar, to_int}
use aiken_bilinear_accumulator/poly.{
  get_final_poly_int, get_final_poly_int_mod, get_final_poly_scalar,
}

const g1_zero_bs =
  #"c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

pub fn get_g1_commitment_int(
  setup: List<G1Element>,
  subset: List<Int>,
) -> G1Element {
  let g1_zero = bls12_381_g1_uncompress(g1_zero_bs)

  let subset_in_g1 =
    map2(
      get_final_poly_int(subset),
      setup,
      fn(sb, st) { bls12_381_g1_scalar_mul(sb, st) },
    )

  foldl(subset_in_g1, g1_zero, fn(a, b) { bls12_381_g1_add(a, b) })
}

pub fn get_g1_commitment_int_mod(
  setup: List<G1Element>,
  subset: List<Int>,
) -> G1Element {
  let g1_zero = bls12_381_g1_uncompress(g1_zero_bs)

  let subset_in_g1 =
    map2(
      get_final_poly_int_mod(subset),
      setup,
      fn(sb, st) { bls12_381_g1_scalar_mul(sb, st) },
    )

  foldl(subset_in_g1, g1_zero, fn(a, b) { bls12_381_g1_add(a, b) })
}

pub fn get_g1_commitment_scalar(
  setup: List<G1Element>,
  subset: List<Scalar>,
) -> G1Element {
  let g1_zero = bls12_381_g1_uncompress(g1_zero_bs)

  let subset_in_g1 =
    map2(
      get_final_poly_scalar(subset),
      setup,
      fn(sb, st) { bls12_381_g1_scalar_mul(to_int(sb), st) },
    )

  foldl(subset_in_g1, g1_zero, fn(a, b) { bls12_381_g1_add(a, b) })
}

pub fn check_membership_using_commitment(
  acc: G2Element,
  subset: G1Element,
  proof: G2Element,
) -> Bool {
  let lhs = bls12_381_miller_loop(g1.generator, acc)
  let rhs = bls12_381_miller_loop(subset, proof)
  bls12_381_final_verify(lhs, rhs)
}

pub fn check_membership_int(
  setup: List<G1Element>,
  acc: G2Element,
  subset: List<Int>,
  proof: G2Element,
) -> Bool {
  expect Some(g1) = head(setup)

  let lhs = bls12_381_miller_loop(g1, acc)
  let rhs = bls12_381_miller_loop(get_g1_commitment_int(setup, subset), proof)
  bls12_381_final_verify(lhs, rhs)
}

pub fn check_membership_scalar(
  setup: List<G1Element>,
  acc: G2Element,
  subset: List<Scalar>,
  proof: G2Element,
) -> Bool {
  expect Some(g1) = head(setup)

  let lhs = bls12_381_miller_loop(g1, acc)
  let rhs =
    bls12_381_miller_loop(get_g1_commitment_scalar(setup, subset), proof)
  bls12_381_final_verify(lhs, rhs)
}

pub fn check_nonmembership_int(
  setup: List<G1Element>,
  g2: G2Element,
  acc: G2Element,
  disjoint_set: List<Int>,
  proof: (G1Element, G2Element),
) -> Bool {
  expect Some(g1) = head(setup)

  let lhs1 = bls12_381_miller_loop(proof.1st, acc)
  let lhs2 =
    bls12_381_miller_loop(get_g1_commitment_int(setup, disjoint_set), proof.2nd)
  let lhs = bls12_381_mul_miller_loop_result(lhs1, lhs2)
  let rhs = bls12_381_miller_loop(g1, g2)

  bls12_381_final_verify(lhs, rhs)
}
