## Submission

This is a new submission (first release of `lcamate` 0.1.0).

## Test environments

* Local: Windows, R 4.5.3 (2026-03-11), `devtools::check()`: 0 errors | 0 warnings | 0 notes (0.1.0).
* GitHub Actions (release R): ubuntu-latest, windows-latest, macos-latest: `R CMD check` Status: OK.
* win-builder: R 4.6.2 (release) and R-devel (2026-11-02 r90827): Status: 1 NOTE each (see below).

## R CMD check results

0 errors | 0 warnings | 1 note

The note (win-builder, release and devel) is the one expected for a new submission:

    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Juan C. Gaviria-Chaverra <jcarlos.gaviria@udea.edu.co>'
    New submission
    Possibly misspelled words in DESCRIPTION:
      LCA (7:46)
      technosphere (8:26)

Both words are correct: `LCA` is life cycle assessment (defined in the Description itself) and
`technosphere` is a standard term of the method.

## Notes for the reviewers

* `LCA` in this package means life cycle assessment (not latent class analysis); the `Description` field says so.
* The package ships small example data in `inst/extdata`: excerpts of USLCI (MIT licence, see `inst/extdata/README_DATA.md`) and of TRACI 2.2
  (US EPA, public domain), plus invented toy data labelled as such. The full USLCI and TRACI files are not included.
* Vignettes use few Monte Carlo iterations to keep the build short.
* Names in the `Description` field that may look like misspellings are software or data sources: 'Brightway', 'openLCA'.

