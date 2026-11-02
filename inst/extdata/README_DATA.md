# Data used in examples and tests

- `toy_*`: tiny example database and Brightway reference results (own).
- `uslci_subgraph_*` (the file `_processes.csv` lists all 583 processes with their names): closure of 12 processes of the U.S. Life Cycle Inventory
  (USLCI) database and the eLCI electricity baseline library, restricted to the
  elementary flows that have a GWP100 factor. `uslci_subgraph_openlca_gwp.csv`
  holds the GWP100 computed with the openLCA engine (olca-core 2.6.2) on the
  full databases.
- `uslci_mini/`: 7 USLCI processes in openLCA JSON-LD format (process
  `0aaf1e13...` reduced to 10 elementary exchanges and without allocation
  factors). `uslci_mini_expected_*`: result of an independent Python
  implementation of the same rules, used as a cross-check.
- `uslci_units.csv`: unit conversion factors read from the USLCI database.

USLCI source: https://github.com/uslci-admin/uslci-content (MIT License,
Copyright (c) 2017 uslci-admin). Cite the database when using these data.
- `uslci_unc/`: 7 USLCI processes in JSON-LD with log-normal, triangular and
  uniform uncertainty (three of them reduced: 3 elementary exchanges kept, no
  allocation factors or documentation). `uslci_unc_expected.csv`: exchange
  parameters derived by an independent Python implementation (loc/scale or
  minimum/maximum/mode after unit, property and reference normalisation).

- `cup_exchanges.csv`, `cup_cf.csv`: **illustrative, invented data** for teaching
  (disposable versus reusable cup; ten fictitious processes; GWP100 with CO2, CH4
  and N2O, factors 1, 29.8 and 273). Every row carries `source = "illustrative
  (invented)"`. The numbers are not measurements and must not be used to claim
  that one cup is better than the other. About 2/3 of the exchanges have a
  lognormal distribution (invented GSDs) so that Monte Carlo, coverage and
  comparison can be practised. Reference scores (independent numpy solve):
  disposable serving 0.03129444, reusable serving (30 uses) 0.0307368 kg CO2-eq.
- `olca_method_mini/`: a tiny impact method in openLCA JSON-LD format with
  **invented factors** (two methods, three categories) built on the flows of
  `uslci_mini/`, to test `read_olca_method()` (units, duplicated factor,
  formula-only factor, unknown unit, property mismatch, no match).
  `olca_method_mini_expected_cf.csv` and `_expected_scores.csv`: result of an
  independent Python implementation. **Not real TRACI/ReCiPe factors.**
- `traci22_mini_substances.csv`: 12 rows copied from the **real** TRACI 2.2 `Substances` sheet
  (US EPA, public domain data): carbon dioxide, methane, nitrous oxide, sulfur dioxide,
  formaldehyde, chloroform, carbon tetrachloride, nitrogen oxides, sulfur oxides, PM2.5 and
  mercury(II). Two edits for testing, both on purpose: a leading all-empty duplicate row for the
  formaldehyde CAS (tests "first non-missing value") and the text `n/a` in one numeric cell of the
  methane row (as in the 2022 release, where zero placeholders became `n/a`).
  `traci22_mini_flows.csv` / `_exchanges.csv`: 15 elementary flows with CAS numbers and categories
  (**invented amounts**) of a one-process model. `traci22_mini_expected_cf.csv` / `_expected_scores.csv`:
  factors and scores derived independently in Python by explicit flow-to-substance and
  flow-to-column mapping (not by the package rules).
