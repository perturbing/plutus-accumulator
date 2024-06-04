use aiken/builtin.{
  bls12_381_final_verify, bls12_381_g1_add, bls12_381_g1_scalar_mul,
  bls12_381_g1_uncompress, bls12_381_miller_loop,
}
use aiken/list.{foldl, head, map2}

const g1_zero_bs =
  #"c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

fn get_g1_commitment(setup: List<G1Element>, subset: List<Int>) -> G1Element {
  let g1_zero = bls12_381_g1_uncompress(g1_zero_bs)

  let subset_in_g1 =
    map2(subset, setup, fn(sb, st) { bls12_381_g1_scalar_mul(sb, st) })

  foldl(subset_in_g1, g1_zero, fn(a, b) { bls12_381_g1_add(a, b) })
}

pub fn check_membership(
  setup: List<G1Element>,
  acc: G2Element,
  subset: List<Int>,
  proof: G2Element,
) -> Bool {
  expect Some(g1) = head(setup)

  let rhs = bls12_381_miller_loop(g1, acc)
  let lhs = bls12_381_miller_loop(get_g1_commitment(setup, subset), proof)
  bls12_381_final_verify(rhs, lhs)
}
