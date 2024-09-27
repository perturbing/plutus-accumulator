# ⚠️ Disclaimer

**This repository is not audited and should be used only for educational purposes. Use at your own risk.**

## Overview

An accumulator is a cryptographic primitive that allows a prover to succinctly commit to a set of values while being able to provide proofs of (non-)membership. A batch proof is an accumulator proof that can be used to prove (non-)membership of multiple values simultaneously. An accumulator is said to be dynamic if it allows for the addition and removal of elements.

In this repository, we implement such a dynamic universal accumulator as described in [this](https://dl.acm.org/doi/pdf/10.1145/3548606.3560676) paper, using the BLS12-381 pairing curve in plutus V3. Though that paper also describes how one can make this scheme zero-knowledge, this work does not implement this, as the usage of a vector scheme onchain is generally not hiding anyway. If you want to understand the math behind this work, there is [this](https://hackmd.io/@CjIlIbTxRqWOCpWzxuWmkQ/BybaUlSN0) blog.

The main take-away of this exploratory work is that such an accumulator can be very efficient for onchain application where transaction size and memory usage is restricted. The trade-off here is that these costs are moved to the offchain and onchain CPU budget. Besides that, this work is also important for applications which need to assert (non-)membership of multiple elements in the context of one transaction, as batching does not increase the proof size. The size limit of the set is not constrained by the onchain verifier, which makes it ideal for large sets.

In short, this accumulator stores a commitment to a set of elements in one G2 element (96 bytes) and allows for constant size (non)-membership proofs. The membership proofs are also a G2 element (96 bytes) and the non-membership proof requires an extra G1 element (48 bytes).

The core strength of this scheme is that the subtraction of elements from the commitment does not require any more overhead than the required (batched) membership proof. That is because the proof, used for this, is the commitment of an accumulator that represents the old state minus the subset you are proving. Making this scheme ideal for situations where a large state needs to be fanned out in batched stages. The additive case requires a (batched) non-membership proof and a (batched) membership proof for the new accumulator.

## Benchmarks

Some preliminary benchmarks for a batched membership proof of size `n`,

```bash
n membership proofs aggregated verification

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    1     853   (5.2%)      1318957338  (13.2%)           74376   (0.5%)
    2     943   (5.8%)      1459818859  (14.6%)          123483   (0.9%)
    3    1033   (6.3%)      1602665136  (16.0%)          181711   (1.3%)
    4    1123   (6.9%)      1747496169  (17.5%)          249060   (1.8%)
    5    1213   (7.4%)      1894311958  (18.9%)          325530   (2.3%)
   10    1663  (10.2%)      2658176544  (26.6%)          844695   (6.0%)
   15    2113  (12.9%)      3471669564  (34.7%)         1591885  (11.4%)
   20    2563  (15.6%)      4334781484  (43.3%)         2567100  (18.3%)
   25    3013  (18.4%)      5247507537  (52.5%)         3770340  (26.9%)
   30    3463  (21.1%)      6209857257  (62.1%)         5201605  (37.2%)
   35    3913  (23.9%)      7221825877  (72.2%)         6860895  (49.0%)
   40    4363  (26.6%)      8283413397  (82.8%)         8748210  (62.5%)
   45    4813  (29.4%)      9394619817  (93.9%)        10863550  (77.6%)


n non-membership proofs aggregated verification

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    1    1050   (6.4%)      1703383729  (17.0%)           79368   (0.6%)
    2    1140   (7.0%)      1843916604  (18.4%)          128460   (0.9%)
    3    1230   (7.5%)      1986269336  (19.9%)          186664   (1.3%)
    4    1320   (8.1%)      2130441925  (21.3%)          253980   (1.8%)
    5    1410   (8.6%)      2276434371  (22.8%)          330408   (2.4%)
   10    1860  (11.4%)      3033694456  (30.3%)          849228   (6.1%)
   15    2310  (14.1%)      3836525891  (38.4%)         1595876  (11.4%)
   20    2760  (16.8%)      4684867521  (46.8%)         2570391  (18.4%)
   25    3210  (19.6%)      5578803976  (55.8%)         3772784  (26.9%)
   30    3660  (22.3%)      6518297372  (65.2%)         5203106  (37.2%)
   35    4110  (25.1%)      7504480025  (75.0%)         6861351  (49.0%)
   40    4560  (27.8%)      8542196250  (85.4%)         8747589  (62.5%)
   45    5010  (30.6%)      9629944970  (96.3%)        10861810  (77.6%)
```

These percentages are given in comparison against the mainnet parameters. You can run the benchmark via

```bash
nix run .#bench-verifier
```

## Aiken implementation

The folder `aiken-bilinear-accumulator` contains the accumulator in Aiken as implemented in the Haskell package `plutus-accumulator`. The cabal package `aiken-utils` is used to recreate the same testing vector as in the `plutus-benchmark`package. This is done to verify the correctness of the implementation and to compare the performance of the Aiken implementation. Here are the results of the benchmarks,

```bash

    ┍━ aiken_bilinear_accumulator/testing_vectors ━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem:    64636, cpu:  1542073613] check_membership_one_elements
    │ PASS [mem:   113543, cpu:  1742168266] check_membership_two_elements
    │ PASS [mem:   173613, cpu:  1945689909] check_membership_three_elements
    │ PASS [mem:   244853, cpu:  2152639238] check_membership_four_elements
    │ PASS [mem:   327272, cpu:  2363044393] check_membership_five_elements
    │ PASS [mem:   907276, cpu:  3466954298] check_membership_ten_elements
    │ PASS [mem:  1767771, cpu:  4658209641] check_membership_fifteen_elements
    │ PASS [mem:  2909623, cpu:  5937875110] check_membership_twenty_elements
    │ PASS [mem:  4333745, cpu:  7307121461] check_membership_twenty_five_elements
    │ PASS [mem:  6040973, cpu:  8766952650] check_membership_thirty_elements
    │ PASS [mem:  8032152, cpu: 10318445701] check_membership_thirty_five_elements
    │ PASS [mem: 10308209, cpu: 11962748882] check_membership_forty_elements
    │ PASS [mem: 12870053, cpu: 13701007497] check_membership_forty_five_elements
    │ PASS [mem:    67185, cpu:  1926509787] check_non_membership_one_elements
    │ PASS [mem:   116071, cpu:  2126536553] check_non_membership_two_elements
    │ PASS [mem:   176101, cpu:  2329936624] check_non_membership_three_elements
    │ PASS [mem:   247275, cpu:  2536710000] check_non_membership_four_elements
    │ PASS [mem:   329593, cpu:  2746856681] check_non_membership_five_elements
    │ PASS [mem:   908343, cpu:  3848189661] check_non_membership_ten_elements
    │ PASS [mem:  1765721, cpu:  5033930191] check_non_membership_fifteen_elements
    │ PASS [mem:  2901766, cpu:  6304017116] check_non_membership_twenty_elements
    │ PASS [mem:  4316489, cpu:  7658535066] check_non_membership_twenty_five_elements
    │ PASS [mem:  6009941, cpu:  9097446157] check_non_membership_thirty_elements
    │ PASS [mem:  7982116, cpu: 10620782945] check_non_membership_thirty_five_elements
    │ PASS [mem: 10233084, cpu: 12228578295] check_non_membership_forty_elements
    │ PASS [mem: 12762839, cpu: 13920794017] check_non_membership_forty_five_elements
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 26 tests | 26 passed | 0 failed
```

This library is available via the aiken package manager.

```bash
aiken packages add --version 0.1.0  jmagan/aiken-bilinear-accumulator
```
