# ⚠️ Disclaimer

**This repository is not audited and should be used only for educational purposes. Use at your own risk.**

# aiken-bilinear-accumulator

This repository contains the aiken implementation of the bilinear accumulator of the repository [plutus-accumulator](https://github.com/perturbing/plutus-accumulator). The main functions are contained in the module `aiken_bilinear_accumulator/accumulator`. The functions `check_membership` and `check_nonmembership` are used to verify the proofs of membership and non-membership, respectively.

## Use

Add this package as a dependency in your `aiken.toml` using the aiken package manager.

```bash
aiken packages add --version 0.1.0  jmagan/aiken-bilinear-accumulator
```

To use the accumulator in your own validator, you can import the module `aiken_bilinear_accumulator/accumulator` and use the functions `check_membership` and `check_nonmembership` to verify the proofs of membership and non-membership, respectively. Refer to the file `validator/accumulator.ak`to see the full implementation.

```gleam
use aiken_bilinear_accumulator/accumulator.{
  check_membership, check_nonmembership,
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
```

## Building

```sh
aiken build
```

## Testing

To run all tests, simply do:

```sh
aiken check
```

## Documentation

If you're writing a library, you might want to generate an HTML documentation for it.

Use:

```sh
aiken docs
```
