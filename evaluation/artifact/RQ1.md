# RQ1: expressiveness of HE programs

RQ1 compares support for input data, HE operations, cryptographic contexts,
schemes and libraries. It is a comparison of supported features, not a count of
randomly generated programs. The representative example is
[logistic_regression_a4_fp_paper.t2](../../src/main/resources/paper/logistic_regression_a4_fp_paper.t2).

## Run the example

Complete the setup in [UBUNTU.md](UBUNTU.md), then run from the repository root:

```sh
bin/fhetest interp -file:src/main/resources/paper/logistic_regression_a4_fp_paper.t2 -n:32768 -m:65537
bin/fhetest run -file:src/main/resources/paper/logistic_regression_a4_fp_paper.t2 -b:OpenFHE -n:32768 -d:5 -m:65537 -openfhe:1.4.2
bin/fhetest run -file:src/main/resources/paper/logistic_regression_a4_fp_paper.t2 -b:SEAL -n:32768 -d:5 -m:65537 -seal:4.1.2
```

Run the backends sequentially because they share generated build files within
the checkout. The interpreter produces `209 2936 12467`: the weighted sums
are 5, 14 and 23, and the example evaluates `24 + 12x + x^3`.
The CKKS backend outputs are approximate; compare each with its reference using
the criterion in [Utils.scala](../../src/main/scala/fhetest/Utils/Utils.scala):
absolute difference below 0.001 for a zero reference, otherwise relative error
below 0.001 with the reference as denominator.

These commands use a ring dimension of 32768 and multiplicative depth of 5.
The pinned T2 code generator supplies the remaining backend defaults; no
`-libconfig:true` option is used. This is an executable example profile, not a
claim that it recovers every parameter of the historical RQ1 experiment. Retain
the commands, environment and generated C++ when documenting another profile.
The example demonstrates vector inputs and HE operations through both libraries;
one successful execution does not establish every feature in the RQ1 table.
