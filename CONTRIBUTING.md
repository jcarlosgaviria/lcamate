# Contributing to lcamate

Thank you for your interest. `lcamate` is a young package; reports from people who use it on real data are the
most useful contribution.

## Reporting a problem

Open an issue with a **minimal example**, the data source and its version, and `sessionInfo()`. If the problem
is in a database itself (for example an inconsistent uncertainty distribution), say so: those are reported to
the maintainers of the database, not fixed here.

## Proposing a change

- A change that alters a **scientific assumption** (for example how an unspecified compartment is mapped) needs
  a source, a measurement of how many results it changes, and a note in `NEWS.md`.
- A new function is discussed in an issue first. The package aims to stay small and simple for users.
- Do not report p-values computed on Monte Carlo draws (see the vignettes for the reason).

## Code

- Run `devtools::document()`, `devtools::test()` and `devtools::check()`; `check()` should report 0 errors,
  0 warnings and 0 notes.
- Every new behaviour has a test. Expected values in tests are derived independently of the code they test
  whenever possible.
- Documentation and messages are in English; the Spanish vignettes are adaptations of the English ones.

## Use of AI assistants

Parts of this package were written with an AI assistant (Claude, Anthropic); every result quoted in the
documentation was checked by the author against data or an independent computation. If you contribute with
AI help, say so in the pull request and check the output yourself.
