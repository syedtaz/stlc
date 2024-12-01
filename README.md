# λ→

OCaml implementation of variations of the SECD machine that evaluates simply typed
lambda calculus extended with units, products and sums. One-to-one translation
of Olivier Danvy's "A Rational Deconstruction of Landin’s SECD Machine" paper. [1]

| Machine       | File                      | Notes |
|---------------|---------------------------|-------|
| Basic         | stlc/lib/basic.ml         | Original SECD machine                     |
| Disentangled  | stlc/lib/disentangled.ml  | Converts basic into multiple functions    |
| Higher Order  | stlc/lib/higher_order.ml  | Refunctionalizes disentangled             |
| Dumpless      | stlc/lib/dumpless.ml      | Removes CPS from higher-order             |
| Controlless   | stlc/lib/controlless.ml   | Removes all continuations from dumpless   |
| Stackless     | stlc/lib/stackless.ml     | Removes the stack from the controlless    |

[^1]: Danvy, Olivier. “A Rational Deconstruction of Landin’s Secd Machine.” Lecture Notes in Computer Science, 2005, 52–71. https://doi.org/10.1007/11431664_4.