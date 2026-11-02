# lcamate: Matrix-Based Life Cycle Assessment in R

[![R-CMD-check](https://github.com/jcarlosgaviria/lcamate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jcarlosgaviria/lcamate/actions/workflows/R-CMD-check.yaml)

`lcamate` computes life cycle assessment (LCA) results with the matrix method of ISO 14044, in R. The name reads as *LCA* + *mate*: a companion for the whole analysis, from checking the input data to reporting.

> **About the name.** In this package, *LCA* means **life cycle assessment**. It is not related to *latent class
> analysis*, which is also abbreviated LCA in R. (Until version 0.0.0.9027 the package was called `lcar`.)

## What it does

- Builds the technosphere and biosphere matrices from a table of exchanges and solves for inventories, impact
  scores and process contributions (`lca_model()`, `lca_calc()`, `lca_contrib()`).
- Reads USLCI (openLCA JSON-LD), CSV tables and impact methods, including TRACI 2.2 (`read_uslci()`,
  `read_lca_csv()`, `read_olca_method()`, `read_traci()`).
- Checks the input data before calculating (`lca_check()`).
- Monte Carlo simulation with the error of the simulation, comparison of alternatives (A vs B), design of experiments
  and data-quality (pedigree) scenarios.
- Reproducible reports (`lca_report()`, `use_lca_article()`).

Results of the matrix algebra were compared with Brightway and openLCA on the same data; see the vignettes for
what was compared, with which tolerance and what was **not** checked.

## Status

Development version (`0.0.0.9xxx`). It is not on CRAN. Install from GitHub:

```r
# install.packages("pak")
pak::pak("jcarlosgaviria/lcamate")
# or: remotes::install_github("jcarlosgaviria/lcamate", build_vignettes = TRUE)
```

Please report problems and ask questions in the [issues](https://github.com/jcarlosgaviria/lcamate/issues);
see `CONTRIBUTING.md`.

## Known limits worth reading first

- TRACI 2.2 (Excel) lists metals as ions with an oxidation state, so metal emissions named as the element in USLCI
  get no ecotoxicity or human-health factor. `read_traci()` counts and announces them; see `?read_traci`.
- Compartment mapping for TRACI is an assumption, editable by the user.
- Unit scale problems are only detected if the exchanges carry a `unit` column.

## Author

Juan C. Gaviria-Chaverra, Universidad de Antioquia.
