use aiken/crypto/bls12_381/scalar
use aiken/fuzz
use aiken/primitive/string
use aiken_bilinear_accumulator/accumulator.{
  check_membership_using_commitment, get_g1_commitment, get_g1_commitment_int,
  get_g1_commitment_int_mod, get_g1_commitment_scalar,
}
use aiken_bilinear_accumulator/poly.{
  get_final_poly_int, get_final_poly_int_mod, get_final_poly_scalar,
  make_scalars,
}

fn dummy_false(_: Int) -> Fuzzer<Bool> {
  fuzz.constant(False)
}

// Constant benchmark for the final step of membership check:
// the pairing, CPU ~ 1 * 10^9
bench bench_membership_using_commitment(_false via dummy_false) {
  check_membership_using_commitment(
    // Accumulator
    #<Bls12_381, G2>"b0f15b32629d02514af939e5b660d27a4db9f84cde5eecfef7db87c056163a9f21925653519cf9972f4b6c115e195baf1439203af99d121fce39ec8eed3fa72a0a31dd537642ab7cb1da52dfbacab1a032c5579aa702a59f1991e9aefae1d9c5",
    // Subset
    #<Bls12_381, G1>"92fdd4aa02003203790b8aedcd9f28b9424deb95df8e49480fa8fc828a363c2d95b8383267e4566b6a55ca9a36fc9a03",
    // Proof
    #<Bls12_381, G2>"809875352e4cd02184ecd07429198ca87364ca0c4bf895482fb8364662bce1945d33c599b47b7d2b34724f45fa17fab8141afa380d192a9134d7f1238e17475af8f6c862c1eecf9666bb5e00b17461ad33112ef2f8dd9580c178b36300cb6dd8",
  )
}

fn bls_scalar_fuzzer() -> Fuzzer<Int> {
  fuzz.int_between(
    // 2^254
    28948022309329048855892746252171976963317496166410141009864396001978282409984,
    scalar.field_prime,
  )
}

test prop_scalar_distribution(scalar via bls_scalar_fuzzer()) {
  fuzz.label(scalar |> string.from_int)
  True
}

fn scalars_sampler(size: Int) -> Fuzzer<List<Int>> {
  fuzz.list_between(bls_scalar_fuzzer(), size, size)
}

bench final_poly_int(scalars: List<Int> via scalars_sampler) {
  get_final_poly_int(scalars)
}

bench final_poly_int_mod(scalars: List<Int> via scalars_sampler) {
  get_final_poly_int_mod(scalars)
}

bench final_poly_scalar(scalars: List<Int> via scalars_sampler) {
  get_final_poly_scalar(make_scalars(scalars))
}

// Commitment benchmarks

// bench g1_commitment_int(scalars: List<Int> via scalars_sampler) {
//   get_g1_commitment_int(setup(), scalars)
// }

// bench g1_commitment_int_mod(scalars: List<Int> via scalars_sampler) {
//   get_g1_commitment_int_mod(setup(), scalars)
// }

// bench g1_commitment_scalar(scalars: List<Int> via scalars_sampler) {
//   get_g1_commitment_scalar(setup(), make_scalars(scalars))
// }

fn unbound_final_coeffs_sampler(size: Int) -> Fuzzer<List<Int>> {
  fn(s0) {
    let fuzzer = fuzz.list_between(bls_scalar_fuzzer(), size, size)
    expect Some((s1, scalars)) = fuzzer(s0)
    Some((s1, get_final_poly_int(scalars)))
  }
}

bench g1_commitment_unbound(
  final_coeffs: List<Int> via unbound_final_coeffs_sampler,
) {
  get_g1_commitment(setup(), final_coeffs)
}

fn field_final_coeffs_sampler(size: Int) -> Fuzzer<List<Int>> {
  fn(s0) {
    let fuzzer = fuzz.list_between(bls_scalar_fuzzer(), size, size)
    expect Some((s1, scalars)) = fuzzer(s0)
    Some((s1, get_final_poly_int_mod(scalars)))
  }
}

bench g1_commitment_field(
  final_coeffs: List<Int> via field_final_coeffs_sampler,
) {
  get_g1_commitment(setup(), final_coeffs)
}

fn setup() -> List<G1Element> {
  [
    #<Bls12_381, G1>"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
    #<Bls12_381, G1>"b0e7791fb972fe014159aa33a98622da3cdc98ff707965e536d8636b5fcc5ac7a91a8c46e59a00dca575af0f18fb13dc",
    #<Bls12_381, G1>"acb58c81ae0cae2e9d4d446b730922239923c345744eee58efaadb36e9a0925545b18a987acf0bad469035b291e37269",
    #<Bls12_381, G1>"82681717d96c5d63a931c4ee8447ca0201c5951f516a876e78dcbc1689b9c4cf57a00a61c6fd0d92361a4b723c307e2d",
    #<Bls12_381, G1>"adb357468d28f2c222024e3745e6197336f10de2e53ee2376bc79e2f0f2313e4509e7512b221d6050364d1df338d1f06",
    #<Bls12_381, G1>"a91d6c2d1007eb2def5f8657f831167a98e5969c8f14b628e0ddbab7cfc53601c81df6e969aca7061344d5e8323ad90d",
    #<Bls12_381, G1>"829a601a644878b0ac6d06ed7f000c163200909eedbbd32a956485b3c7ae398877c6a3625de36cb44a7e3b1b9f63234d",
    #<Bls12_381, G1>"8245ceb0cb176dfae3ef880a936cc8afc5772dc79ade0e25d08aef0ea067c1d355732658daf6e72646c459fafc48f567",
    #<Bls12_381, G1>"96903ac25c513f9559a3769678b84169b02ae04706a5f47440995b41c6350dcf18f3f26db06a16184f1b7cba70cb9cca",
    #<Bls12_381, G1>"ae095cae1961131f64ea7fc962c9f2a6c1891d6d26c9e7f6d59d8c93aded2e4acdb6290361d2d99bef5f2de4fc9cabc7",
    #<Bls12_381, G1>"92e4f628c663ac0e56057cf0758d7b588994f068ebb2d6c371a2b1dde93a3cb59f15725c5a8eb2629b56aa8f5a063c5b",
    #<Bls12_381, G1>"8561e4f4b1ae08a470a781debdd1bcf99ff03f05030b74519dd7c38744d20d7304e8444af273bfa34bfaa26a1f878541",
    #<Bls12_381, G1>"8cb1cdd886e892bc4a094f776962f237f6d7b7b2c311f54ab76c6c951632bfb7dc31289c1600957c672c98e9383d07fc",
    #<Bls12_381, G1>"99ddfd8ee22ab516c6aaaf35eda5c19c942316e37e29ee0baa9b2262bb37d98974b45cd461c811d0ae5a401c0b91a484",
    #<Bls12_381, G1>"ab43e5a08c84ce1808c76f46a2ff2d8bcdd8e87f6ebc40854085b10e73745f30e08ced2ed813428ea20818e5a5381138",
    #<Bls12_381, G1>"8a07fdfc5041e47cdb84657c00110e4fed3c695a72c87f290457a8b3fd47052b22d043e64cdbe250cef9d6d37cbe4456",
    #<Bls12_381, G1>"a6c146801a20d0d7250ebec4b555ca8907eb5397bbf0b0871c39e0f4f411bd5f5a9cc4fcfe3c5d8a8c8b64e3a93514f9",
    #<Bls12_381, G1>"b050cba2b8f63114289db8cacb7133b36bba5b21e42528be715f98cf0dd6504741de291b44d42c4368255ffb63ab4bf9",
    #<Bls12_381, G1>"a8664a1c55f4d7aebe62f2c043bdb9dd0c9a20725c1dfe8f67824d54f7a2b61af2a8639335c2345abf6cc3f79ddf3718",
    #<Bls12_381, G1>"af431ea59e116b70109f7adb4d260f1d05d72973fca24081df4093ad3307867c45fa9f312ab464fa5f2f0c59683c7dad",
    #<Bls12_381, G1>"82074b5d969dcc1e852e5100888330f79ae4455eb4520f28bd8321a8869325adf3f527a1e170a2062e459c8b4a81679c",
    #<Bls12_381, G1>"b2d1200ac058abe7e08c0efaec3ef93daaddbff2bc3ceae4c40e6ff26858791e5a050f410d33f208bd74367229216e9f",
    #<Bls12_381, G1>"aac124c5e0bc9701cb4e51bdaaae57b82af6fb53319cb458c55bb8fffa7315306f19d0e3347baef113f0926c9a86b067",
    #<Bls12_381, G1>"846eb53ac2a1ff34c7b4a2e2453e9b6f7f521f4c669dd9536ffef3e74ab04a5215214b51903a514b9789d72537487654",
    #<Bls12_381, G1>"98ca0a62a24f81a87e618769076f9b991a9659b1a2d7d66bb54adbca9317ac15a8ce20a4ccd056257854b0715f549f26",
    #<Bls12_381, G1>"838bc31ab11d34711236106c83e68362c86610e1631a8f8b940ba63fd18ca343a4fa3f9d2f0f9ed8309efd798e536f93",
    #<Bls12_381, G1>"844bf8551e3e4fc0174889355f556bfa5eecc083faebd1262744ad2c224e7f8720134b341c8ea4d534076bb1ef4c5f42",
    #<Bls12_381, G1>"a343e4dc3629e8a58dbd1dd5bf1408efc78f9586b0d2ce0bbc8e6716daf4b9551f780ad958eba75ed3609122b9c8a9d9",
    #<Bls12_381, G1>"8292754b0f7b4c9481e6b84adfec08e10e47d8146cf037e0c08aa290fc0ae5f631b32e5764862af793103337ef46e171",
    #<Bls12_381, G1>"9773cb0a95ed7062f51a300e544d94e6d13430ce97583ddc195192b85beb0929ccc8917fb784e3aafd9da038378e83ca",
    #<Bls12_381, G1>"98b61b0cdfd840df81766f02855f8a3fc31e119514b207557c497416b3fb7061895285ac2b3214ba75e37df7d4ae8ee0",
    #<Bls12_381, G1>"965d47f577fb917dd4ba73ada45c467bf0d3759854b2a042dcfe10805ec83e3ca228577e3732c180f204c72f24da1ddd",
    #<Bls12_381, G1>"a3b02b9d84224715df79bd79790311eed63e602e4463b93e512ad6988ff0212688e09f8d538e1de1a282b8d2c309636b",
    #<Bls12_381, G1>"a9e8ae006488eef524a005e21281fd27098b2c0e37777d075333909aa11d92e53923e321622ce35f9f475d5dd8d10cfe",
    #<Bls12_381, G1>"87b94efe473f152ea7eb80c2da9e261cd48b65dc43e0eb4b0bf881b609dd25da72b0741831aa3848b7cd3b316a9b212e",
    #<Bls12_381, G1>"a448203b1608d867e70495370371742053ca4572e033b9cc2713a000d37fd6389205d2cd11c9bc2dc8e524ea9896c8a6",
    #<Bls12_381, G1>"869ec09853df4d8dae8515fed9a9a444e32c1b4159f15b98b76fa4067ed2dbad6bfff7da576d6140a445629b0042fabc",
    #<Bls12_381, G1>"90db92b89f9db8996a93616d48f7822dda162e50df0ca41a26045f8d4eee5523fea0d84738f7dbb65c0af76a8c0a2c1d",
    #<Bls12_381, G1>"b9520f7a0fe0b96482db4328a3b79d6cb4cb74b1099af35eeb5288d15ef3593aac3f1a5da0d39805473f89f83b149479",
    #<Bls12_381, G1>"9621df24094008cf78facc67f829e834af6a9662ccea6ad87f750e65f95d7d80e0d3244957d4816d78a2b75c16e561e9",
    #<Bls12_381, G1>"963e17c933edf5903bbd6ba9d24e9c64672f62977db71ec14bb64400431e56f25913551bac8ad4ccef5f758626861981",
  ]
}
