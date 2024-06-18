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
|   1 |  1318957338  (13.2%)    | 74376   (0.5%)    |
|   2 |  1459818859  (14.6%)    | 123483   (0.9%)   |
|   3 |  1602669903  (16.0%)    | 181711   (1.3%)   |
|   4 |  1747505703  (17.5%)    | 249060   (1.8%)   |
|   5 |  1894321492  (18.9%)    | 325530   (2.3%)   |
|  10 |  2658195612  (26.6%)    | 844695   (6.0%)   |
|  15 |  3471683865  (34.7%)    | 1591885  (11.4%)  |
|  20 |  4334795785  (43.3%)    | 2567100  (18.3%)  |
|  25 |  5247521838  (52.5%)    | 3770340  (26.9%)  |
|  30 |  6209871558  (62.1%)    | 5201605  (37.2%)  |
|  35 |  7221835411  (72.2%)    | 6860895  (49.0%)  |
|  40 |  8283422931  (82.8%)    | 8748210  (62.5%)  |
|  45 |  9394629351  (93.9%)    | 10863550  (77.6%) |

Some preliminary benchmarks for a batched non-membership proof of size `n`,

| n   |        CPU usage        |      Memory usage      |
|---: | ----------------------: | ---------------------: |
|   1 |  1703383729  (17.0%)    | 79368   (0.6%)         |
|   2 |  1843916604  (18.4%)    | 128460   (0.9%)        |
|   3 |  1986269336  (19.9%)    | 186664   (1.3%)        |
|   4 |  2130441925  (21.3%)    | 253980   (1.8%)        |
|   5 |  2276434371  (22.8%)    | 330408   (2.4%)        |
|  10 |  3033694456  (30.3%)    | 849228   (6.1%)        |
|  15 |  3836525891  (38.4%)    | 1595876  (11.4%)       |
|  20 |  4684867521  (46.8%)    | 2570391  (18.4%)       |
|  25 |  5578803976  (55.8%)    | 3772784  (26.9%)       |
|  30 |  6518297372  (65.2%)    | 5203106  (37.2%)       |
|  35 |  7504480025  (75.0%)    | 6861351  (49.0%)       |
|  40 |  8542196250  (85.4%)    | 8747589  (62.5%)       |
|  45 |  9629944970  (96.3%)    | 10861810  (77.6%)      |

These percentages are given in comparison against the mainnet parameters. You can run the benchmark via
```bash
nix run .#bench-verifier
```