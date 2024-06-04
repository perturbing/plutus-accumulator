# ⚠️ Disclaimer

**This repository is not audited and should be used only for educational purposes. Use at your own risk.**

## Overview
An accumulator is a cryptographic primitive that allows a prover to succinctly commit to a set of values while being able to provide proofs of (non-)membership. A batch proof is an accumulator proof that can be used to prove (non-)membership of multiple values simultaneously. An accumulator is said to be dynamic if it allows for the addition and removal of elements.

In this repository, we implement such a dynamic universal accumulator as described in [this](https://dl.acm.org/doi/pdf/10.1145/3548606.3560676) paper, using the BLS12-381 pairing curve in plutus V3. Though that paper also describes how one can make this scheme zero-knowledge, this work does not implement this, as the usage of a vector scheme onchain is generally not hiding anyway. If you want to understand the math behind this work, there is [this](https://hackmd.io/@CjIlIbTxRqWOCpWzxuWmkQ/BybaUlSN0) blog.

The main take-away of this exploratory work is that such an accumulator can be very efficient for onchain application where transaction size and memory usage is restricted. The trade-off here is that these costs are moved to the offchain and onchain CPU budget. Besides that, this work is also important for applications which need to assert (non-)membership of multiple elements in the context of one transaction, as batching does not increase the proof size. The size of the limit on the size is not constrained by the onchain.

In short, this accumulator stores a commitment to a set of elements in one G2 element (96 bytes) and allows for constant size (non)-membership proofs. The membership proofs are also a G2 element (96 bytes) and the non-membership proof requires an extra G1 element (48 bytes).

The core strength of this scheme is that the subtraction of elements from the commitment does not require any more overhead than the required (batched) membership proof. That is because the proof, used for this, is the commitment of an accumulator that represents the old state minus the subset you are proving. Making this scheme ideal for situations where a large state needs to be fanned out in batched stages. The additive case requires a (batched) non-membership proof and a (batched) membership proof for the new accumulator.

## Benchmarks
Some preliminary benchmarks for a batched membership proof of size `n`,

| n   |    CPU units usage      |   Memory units usage   |
|---: | ----------------------: | ---------------------: |
|   1 |   1519939432  (15.2%)   |   79167   (0.6%)       |
|   2 |   1648275236  (16.5%)   |  130061   (0.9%)       |
|   3 |   1779993011  (17.8%)   |  190073   (1.4%)       |
|   4 |   1915092757  (19.2%)   |  259203   (1.9%)       |
|   5 |   2053574474  (20.5%)   |  337451   (2.4%)       |
|  10 |   2796717024  (28.0%)   |  865466   (6.2%)       |
|  15 |   3624408849  (36.2%)   | 1621431  (11.6%)       |
|  20 |   4536648189  (45.4%)   | 2605344  (18.6%)       |
|  25 |   5533437684  (55.3%)   | 3817208  (27.3%)       |
|  30 |   6614777334  (66.1%)   | 5257023  (37.6%)       |
|  35 |   7780666259  (77.8%)   | 6924788  (49.5%)       |
|  40 |   9031104459  (90.3%)   | 8820503 (63.0%)        |
|  45 |  10366091934 (103.7%)   | 10944168  (78.2%)      |

Some preliminary benchmarks for a batched non-membership proof of size `n`,

| n   |        CPU usage        |      Memory usage      |
|---: | ----------------------: | ---------------------: |
|   1 |   1973872795  (19.7%)   |   84159   (0.6%)       |
|   2 |   2100792151  (21.0%)   |  135036   (1.0%)       |
|   3 |   2230496880  (22.3%)   |  195019   (1.4%)       |
|   4 |   2362986982  (23.6%)   |  264108   (1.9%)       |
|   5 |   2498262457  (25.0%)   |  342303   (2.4%)       |
|  10 |   3216420427  (32.2%)   |  869868   (6.2%)       |
|  15 |   4004964503  (40.0%)   | 1625111  (11.6%)       |
|  20 |   4863392938  (48.6%)   | 2608071  (18.6%)       |
|  25 |   5792519954  (57.9%)   | 3818759  (27.3%)       |
|  30 |   6792082043  (67.9%)   | 5257226  (37.6%)       |
|  35 |   7866543233  (78.7%)   | 6923466  (49.5%)       |
|  40 |   9034559189  (90.3%)   | 8817549  (63.0%)       |
|  45 |  10289941621 (102.9%)   | 10939465  (78.1%)      |

These percentages are given in comparison against the mainnet parameters. You can run the benchmark via
```bash
nix run .#bench-verifier
```