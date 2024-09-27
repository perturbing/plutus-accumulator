use aiken/collection/list.{map}
use aiken/crypto/bls12_381/g1.{decompress as g1_decompress}
use aiken/crypto/bls12_381/g2.{decompress as g2_decompress}
use aiken_bilinear_accumulator/accumulator.{
  check_membership, check_nonmembership,
}
use cardano/transaction.{OutputReference, Transaction, placeholder}

pub type Proof {
  Membership { proof: ByteArray }
  NonMembership { proof: (ByteArray, ByteArray) }
}

pub type Datum {
  accumulator: ByteArray,
}

pub type Redeemer {
  setup: List<ByteArray>,
  proof: Proof,
  subset: List<Int>,
}

const g2 =
  #<Bls12_381, G2>"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"

validator bilinear_accumulator {
  spend(
    datum: Option<Datum>,
    redeemer: Redeemer,
    _utxo: OutputReference,
    _self: Transaction,
  ) {
    expect Some(datum) = datum

    when redeemer.proof is {
      Membership(proof) ->
        check_membership(
          map(redeemer.setup, g1_decompress),
          g2_decompress(datum.accumulator),
          redeemer.subset,
          g2_decompress(proof),
        )
      NonMembership((proof_1st, proof_2nd)) ->
        check_nonmembership(
          map(redeemer.setup, g1_decompress),
          g2,
          g2_decompress(datum.accumulator),
          redeemer.subset,
          (g1_decompress(proof_1st), g2_decompress(proof_2nd)),
        )
    }
  }

  else(_) {
    fail
  }
}

test validator_membership_example() {
  let datum =
    Datum {
      accumulator: #"b0f15b32629d02514af939e5b660d27a4db9f84cde5eecfef7db87c056163a9f21925653519cf9972f4b6c115e195baf1439203af99d121fce39ec8eed3fa72a0a31dd537642ab7cb1da52dfbacab1a032c5579aa702a59f1991e9aefae1d9c5",
    }
  let redeemer =
    Redeemer {
      setup: [
        #"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
        #"b0e7791fb972fe014159aa33a98622da3cdc98ff707965e536d8636b5fcc5ac7a91a8c46e59a00dca575af0f18fb13dc",
      ],
      subset: [
        22401154959170154123134540742828377934364533580409315286338474307961,
      ],
      proof: Membership {
        proof: #"809875352e4cd02184ecd07429198ca87364ca0c4bf895482fb8364662bce1945d33c599b47b7d2b34724f45fa17fab8141afa380d192a9134d7f1238e17475af8f6c862c1eecf9666bb5e00b17461ad33112ef2f8dd9580c178b36300cb6dd8",
      },
    }

  let utxo =
    OutputReference {
      transaction_id: #"0000000000000000000000000000000000000000000000000000000000000000",
      output_index: 0,
    }
  bilinear_accumulator.spend(Some(datum), redeemer, utxo, placeholder)
}

test validator_nonmembership_example() {
  let datum =
    Datum {
      accumulator: #"a82b83da0a22649c8228cb7fa581813f6b110ab422e4876d57cb074c952ef95efe5c929a8c6c0a87c33c47d58797f65e06a024473d2271d7205d0b5bd85ce2b6ed152150a1575f5b18d60e3e01762ea1d18374cceac551c622bac144f85f66e4",
    }
  let redeemer =
    Redeemer {
      setup: [
        #"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
        #"b0e7791fb972fe014159aa33a98622da3cdc98ff707965e536d8636b5fcc5ac7a91a8c46e59a00dca575af0f18fb13dc",
      ],
      subset: [55],
      proof: NonMembership {
        proof: (
          #"89f56e5654cfc32a30085d18f4c1a64bdf397b80369ce0b27d98f1431a82a1f625cb2595c737dd0885ddbe8ea0839458",
          #"87fa45b2f91ceb469308014c86941e1ab4112cd3658544b828e89d5254d0e396148543add5966146b8b39dafe321ba0705a311ed9d4209807339759a53aa2a8367820c41ee009a5a94707f7ea30e516aac804fa314d9854af83f1c73f0f64650",
        ),
      },
    }

  let utxo =
    OutputReference {
      transaction_id: #"0000000000000000000000000000000000000000000000000000000000000000",
      output_index: 0,
    }
  bilinear_accumulator.spend(Some(datum), redeemer, utxo, placeholder)
}
