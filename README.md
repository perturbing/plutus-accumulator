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
