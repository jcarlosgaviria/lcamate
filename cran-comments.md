## Submission

This is a new submission (first release of `lcamate` 0.1.0).

## Test environments

* Local: Windows (R version: FILL IN), `devtools::check()`: 0 errors | 0 warnings | 0 notes (0.0.0.9031).
* GitHub Actions (release R): ubuntu-latest, windows-latest, macos-latest: `R CMD check` Status: OK.
* win-builder: FILL IN (release and devel).
* R-hub: FILL IN.

## R CMD check results

0 errors | 0 warnings | 1 note (expected for a new submission: "New submission").
FILL IN any other note after running `R CMD check --as-cran`.

## Notes for the reviewers

* `LCA` in this package means life cycle assessment (not latent class analysis); the `Description` field says so.
* The package ships small example data in `inst/extdata`: excerpts of USLCI (MIT licence, see `inst/extdata/README_DATA.md`) and of TRACI 2.2
  (US EPA, public domain), plus invented toy data labelled as such. The full USLCI and TRACI files are not included.
* Vignettes use few Monte Carlo iterations to keep the build short.
* Names in the `Description` field that may look like misspellings are software or data sources: Brightway, openLCA, USLCI, TRACI.
