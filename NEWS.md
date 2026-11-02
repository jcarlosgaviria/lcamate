# lcamate 0.0.0.9031

* Vignettes: fewer Monte Carlo iterations in the examples (500 instead of 2000; 200 or 300 instead of 500 or 1000 in the designs of experiments),
  so that rebuilding all vignettes takes about a third of the time (measured before the change: 284 s on the author's computer). The text now says that
  the numbers shown are illustrative and that a reported result should use at least the `n_needed` printed by `lca_mc_error()`. Vignette index titles now match
  their titles. No code of the package changed.

# lcamate 0.0.0.9030

* Documentation only. The wording "processes with a complete chain" used for the measurements quoted in `?read_traci` (metal gap) and in the
  notes of 0.0.0.9027 meant processes whose **own** technosphere inputs are all linked (`completeness` = 1), not processes whose whole upstream chain
  is closed. In USLCI v1.2026-03.0 only 27 of 1,341 processes are closed upstream, and 698 of the 733 processes with `completeness` = 1 have one of the 66
  "Bridge" processes (placeholders that point to USEEIO or USFS data not included in the export) upstream. The numbers quoted are unchanged; they describe
  scores computed from the data inside the export, which are lower bounds for those processes. No code changed.

# lcamate 0.0.0.9029

* `read_traci()`: ground emitted **without any compartment** (category `emission/ground`) now gets the **mean of the natural and
  agricultural soil factors** for ecotoxicity and human health (non-cancer and cancer), as TRACI 2.1 does in all the flows compared
  (USLCI v1.2026-03.0: 142, 80 and 37 flows; an observed pattern, not a rule documented by the EPA). Before, it was treated as natural soil.
  Results change only for flows emitted to unspecified ground; in USLCI this matters in about 9 of 716 processes, where those flows dominate the score.
  New code `soilM` for `compartments`; new element `traci_report$soil_unspecified_mean`. Industrial ground is still treated as natural soil.

# lcamate 0.0.0.9028

* The package was **renamed from `lcar` to `lcamate`**. Use `library(lcamate)`; the functions, arguments and results are
  unchanged. The vignettes are now `lcamate-tutorial`, `lcamate-tutorial-es`, `lcamate-pedigri`, `lcamate-uslci` and
  `lcamate-teaching-guide`. The element `versions$lcar` of the report data is now `versions$lcamate`. Reason: `lcar` is also the name of an
  older, unrelated repository, and a more distinctive name avoids confusion.

# lcar 0.0.0.9027

* `read_traci()`: air emitted **without any compartment** (category `emission/air`) now receives the **mean of the
  urban and rural TRACI 2.2 factors** for freshwater ecotoxicity and human health. Before, it was treated as rural air.
  **This changes results** of those three categories for flows of that kind. Measured with USLCI v1.2026-03.0 (716 processes
  whose own technosphere inputs are all linked, unit scores; upstream chains can still be cut off): human health (non-cancer) rises by a median factor of 3.6 (range 0.99 to 15), human health
  (cancer) by 1.6 (0.87 to 14) and freshwater ecotoxicity is practically unchanged (median 1.00; 0.99 to 1.16), because rural
  factors are much smaller than urban ones for the human-health categories. The categories that depend on air alone
  (global warming, acidification, smog, ozone, particulate matter) are unchanged. The rule is an assumption (supported
  by the TRACI 2.1 JSON-LD of the EPA, which gives unspecified air exactly this value in all the flows compared with USLCI 2026-03, and by
  an example in the USEtox manual; neither is a documented rule) and can be overridden with `compartments`. The number of flows classified as unspecified air is in
  `m$traci_report$air_unspecified_mean`. Unspecified soil is not changed.
* Metals named as the element are counted and announced, not matched (0.0.0.9023).
* `lca_check()` tells apart processes that are empty in the source from processes whose inputs were all cut off (0.0.0.9022).
