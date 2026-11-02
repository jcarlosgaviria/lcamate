# Internal: normalise a CAS number to digits without leading zeros; NA when absent.
# Values such as "n.a.", "x", "0" or "" are treated as "no CAS".
.cas_norm <- function(x) {
  x <- as.character(x)
  d <- gsub("[^0-9]", "", x)
  d <- sub("^0+", "", d)
  d[is.na(x) | !nzchar(d)] <- NA_character_
  d
}

# Internal: the six TRACI 2.2 emission compartments used by the ecotoxicity and
# human-health factors, and the text that identifies each in the column names.
# "airM" is not a TRACI column: it is the mean of the urban (airU) and rural (airC) factors, used for air
# emitted without any compartment ("emission/air"); see ?read_traci. "soilM" is the mean of the natural (nat)
# and agricultural (agr) soil factors, used for ground emitted without any compartment ("emission/ground").
.traci_comp_codes <- c("airU", "airC", "airM", "fresh", "sea", "nat", "agr", "soilM")

# Internal: default rules that send a model flow category (openLCA / FEDEFL style
# paths such as "Elementary flows/emission/air/troposphere/urban") to a TRACI
# compartment. Order matters: the first matching rule wins. These are
# ASSUMPTIONS, documented in ?read_traci, and can be overridden.
.traci_comp_rules <- function() {
  data.frame(
    pattern = c("/emission/air$", "/emission/.*air.*urban", "/emission/.*air",
                "/emission/.*water.*(saline|ocean|sea)", "/emission/.*water",
                "/emission/(non-FEDEFL/)?ground$", "/emission/.*ground.*agricultur", "/emission/.*ground"),
    compartment = c("airM", "airU", "airC", "sea", "fresh", "soilM", "agr", "nat"),
    stringsAsFactors = FALSE)
}

# Internal: classify flow keys. Returns the compartment code, NA when the flow is
# not an emission or cannot be classified. `rules` is a data frame (pattern,
# compartment); user rules are tried before the defaults.
.traci_classify <- function(fk, rules) {
  cat <- tolower(ifelse(is.na(fk$category), "", fk$category))
  out <- rep(NA_character_, length(cat))
  for (i in seq_len(nrow(rules))) {
    hit <- is.na(out) & grepl(rules$pattern[i], cat, ignore.case = TRUE, perl = TRUE)
    out[hit] <- rules$compartment[i]
  }
  out[!is.na(fk$dir) & fk$dir != "out"] <- NA_character_   # inputs are not emissions
  out
}

# Internal: names of the single-compartment (air) TRACI categories
.traci_air_cats <- function() {
  list(
    list(name = "Acidification", pat = "^Acidification", unit = "kg SO2 eq"),
    list(name = "Global warming", pat = "^Global Climate", unit = "kg CO2 eq"),
    list(name = "Human health - particulate matter", pat = "^HH Particulate", unit = "PM2.5 eq"),
    list(name = "Ozone depletion", pat = "^Ozone Depletion", unit = "kg CFC-11 eq"),
    list(name = "Smog formation", pat = "^Photochemical", unit = "kg O3 eq"))
}

# Internal: identify the compartment of a TRACI 2.2 column name
.traci_col_comp <- function(nm) {
  n <- tolower(nm)
  ifelse(grepl("em\\.airu|urban air", n), "airU",
  ifelse(grepl("em\\.airc|rural air", n), "airC",
  ifelse(grepl("fr\\.waterc|cont\\. freshwater", n) & !grepl("sea", n), "fresh",
  ifelse(grepl("sea water", n), "sea",
  ifelse(grepl("nat\\.soil|natural soil", n), "nat",
  ifelse(grepl("agr\\.soil|agric\\. soil", n), "agr", NA_character_))))))
}

# Internal: first non-NA value per group (numeric)
.first_nonna <- function(v, g) {
  r <- tapply(v, g, function(z) { z <- z[!is.na(z)]; if (length(z)) z[1] else NA })
  r[levels(g)]
}

#' Read the TRACI 2.2 impact method (EPA spreadsheet) into a model
#'
#' Reads the `Substances` sheet of the TRACI 2.2 workbook published by the U.S.
#' EPA and adds its impact categories to the model. TRACI 2.2 is published
#' **by substance (CAS number)**, not by flow identifier, so the elementary flows
#' of the model must carry a CAS number in `m$flow_info$cas` ([read_uslci()]
#' fills it from the database).
#'
#' @details
#' **Categories added**: Acidification, Global warming, Human health - particulate
#' matter, Ozone depletion, Smog formation (all for emissions to air), and
#' Freshwater ecotoxicity, Human health - cancer and Human health - non-cancer
#' (factors that depend on the emission compartment). TRACI 2.2 does **not**
#' include eutrophication or fossil-fuel depletion: the EPA removed them in 2022
#' (the workbook points to a separate eutrophication release). The TRACI 2.1
#' JSON-LD export read by [read_olca_method()] does include eutrophication.
#'
#' **Matching**. A flow is matched to a substance by CAS number. TRACI lists some
#' important pollutants without a CAS number (nitrogen oxides, sulfur oxides, PM2.5,
#' PM10 and a few hundred others), and some databases leave the CAS out of generic
#' flows (in the USLCI export, about a third of the elementary flows used by the
#' processes are missing from the flow folder, including "Carbon dioxide" emitted to
#' air). The flow **name** is therefore compared with the substance name
#' (case-insensitive, exact) in two cases: a flow with no CAS is compared with every
#' TRACI substance, and a flow whose CAS TRACI does not list is compared only with the
#' TRACI substances that have no CAS. A name shared by several TRACI substances is
#' ambiguous and not used. A small table of aliases is added (`aliases`). Every match by
#' name is counted in `m$traci_report`.
#'
#' **Compartments (an assumption)**. Ecotoxicity and human-health factors depend
#' on where the substance is emitted. The category path of each flow is mapped to
#' one of six TRACI compartments (urban air, rural/continental air, freshwater,
#' seawater, natural soil, agricultural soil). Air emitted **without any
#' compartment** (the category is just `emission/air`) gets the **mean of the urban
#' and rural factors** (50\\% / 50\\%), an assumption with two supports and no documented rule: in
#' the TRACI 2.1 JSON-LD of the EPA, unspecified air has exactly this value in all the flows compared (USLCI
#' v1.2026-03.0: 249 for ecotoxicity, 137 and 78 for human health non-cancer and cancer), and the USEtox manual
#' gives a 50/50 split of undifferentiated air as an example of a possible assumption. (Up to version 0.0.0.9026 this flow was treated as rural air, which gave lower human-health
#' values.) Other air paths that are not urban (for example stratosphere) are still treated as rural air;
#' unspecified water and subterranean water as freshwater; industrial and other ground as natural soil,
#' because TRACI has no industrial-soil compartment. Ground emitted **without any compartment** (the
#' category is just `emission/ground`) gets the **mean of the natural and agricultural soil factors**, again
#' an observed pattern and not a documented rule: in the TRACI 2.1 JSON-LD of the EPA, unspecified soil has exactly this value
#' in all the flows compared (USLCI v1.2026-03.0: 142 for ecotoxicity, where both factors are equal, and 80 and 37
#' for human health non-cancer and cancer). Up to version 0.0.0.9028 this flow was treated as natural soil.
#' Override with `compartments` (codes `airU`, `airC`,
#' `airM` for the mean, `fresh`, `sea`, `nat`, `agr`, `soilM` for the soil mean). The number of flows classified as unspecified air (they
#' receive the mean where TRACI has both factors) is in `m$traci_report$air_unspecified_mean`, and that of unspecified
#' ground in `m$traci_report$soil_unspecified_mean`.
#' Resources, inputs and flows that cannot be classified get no factor.
#'
#' **Global warming** in TRACI is based on IPCC AR4 values (for example CH4 25,
#' N2O 298), unlike [lca_method_gwp()] which offers AR5 and AR6. TRACI gives no
#' factor to carbon dioxide taken from the air (resource input), so biogenic
#' uptake is not credited.
#'
#' **Metals (not matched on purpose)**. TRACI 2.2 lists metals as ions with an oxidation state
#' (for example `zinc(II)`, `lead(II)`), while databases such as USLCI name the element ("Zinc").
#' These flows get **no factor**, because choosing an ion is an assumption (for chromium, arsenic and
#' antimony the factors of the ions differ a lot, and USLCI does not give the oxidation state). The
#' number of emission flows affected is in `m$traci_report$metals_without_factor` (names in
#' `metals_without_factor_names`), a message is printed and [lca_check()] adds a note. Freshwater
#' ecotoxicity and human-health results can therefore be underestimated. If you accept an ion, say so with
#' `aliases`, e.g. `aliases = c("zinc" = "zinc(ii)")`, and state it in your report.
#'
#' The gap can be large. In a test with USLCI v1.2026-03.0 (733 processes whose own technosphere inputs are all linked; their upstream chains can still be cut off, see `vignette("lcamate-uslci")`), the metal
#' emissions that TRACI 2.2 leaves without a factor accounted for more than half of the **TRACI 2.1** score of
#' human health (non-cancer) in 588 of 703 processes with a non-zero score, and for a median of 19\% of the
#' freshwater ecotoxicity score (more than half in 225 of 716 processes); for human health (cancer) the median
#' was 4\%. Scores from the two versions also differ when these metals are left out, but not because the
#' factors changed: for flows with an explicit compartment (urban or rural air, water, agricultural or
#' industrial soil) the factors compared are identical in the TRACI 2.1 JSON-LD and the TRACI 2.2 workbook. The
#' differences come from (1) the flows each route reaches (by flow identifier or by CAS and name) and (2) how
#' flows with an unspecified compartment are treated: in TRACI 2.1 an unspecified air flow gets exactly the mean
#' of the urban and rural factors (all flows compared); `read_traci()` now does
#' the same (before version 0.0.0.9027 it used rural air, which gave a median of about one seventh of that
#' value for human health (non-cancer) in the substances compared). If metals matter in your study, [read_olca_method()] with the
#' TRACI 2.1 JSON-LD of the EPA assigns factors to the element-named flows by flow identifier (as openLCA does);
#' state which version you used.
#'
#' **Flags**. For ecotoxicity and human health the workbook marks each factor
#' "Recommended" or "Interim". All are used by default; give `flags =
#' "Recommended"` to use only those.
#'
#' @param path Path to the TRACI 2.2 `.xlsx` (needs the `readxl` package) or to
#'   a `.csv` with the same columns as the `Substances` sheet.
#' @param m An [lca_model()] whose `flow_info` has columns `flow`, `id` and `cas`.
#' @param aliases Optional named character vector `c("flow name" = "TRACI substance
#'   name")` added to the built-in aliases (matching is case-insensitive).
#' @param compartments Optional data frame with columns `pattern` (a regular
#'   expression applied to the flow category) and `compartment` (one of `airU`,
#'   `airC`, `airM` (mean of urban and rural), `fresh`, `sea`, `nat`, `agr`, `soilM` (mean of natural and agricultural soil)), tried before the built-in rules.
#' @param flags Optional character vector: keep only ecotoxicity / human-health
#'   factors with these flags (`"Recommended"`, `"Interim"`).
#' @param prefix Optional text put before each category name.
#' @param sheet Sheet name of the `.xlsx` (default `"Substances"`).
#' @return The model with the categories added to its characterisation matrix.
#'   `m$method_report` has one row per category and `m$traci_report` counts
#'   matches by CAS and by name, flows without CAS and flows without compartment.
#' @references US EPA (2022). Tool for the Reduction and Assessment of Chemical
#'   and Other Environmental Impacts (TRACI) 2.2.
#' @export
read_traci <- function(path, m, aliases = NULL, compartments = NULL, flags = NULL,
                       prefix = NULL, sheet = "Substances") {
  if (!inherits(m, "lca_model")) stop("`m` must be an lca_model", call. = FALSE)
  fi <- m$flow_info
  if (is.null(fi) || !all(c("flow", "id") %in% names(fi)) || !"cas" %in% names(fi))
    stop("the model needs `flow_info` with a `cas` column (read_uslci() provides it)", call. = FALSE)
  if (!file.exists(path)) stop("file not found: ", path, call. = FALSE)
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    D <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("package 'readxl' is required to read .xlsx files (or give a .csv)", call. = FALSE)
    # readxl guesses column types from the first rows; the "x" placed by TRACI where a value
    # does not exist then becomes NA with one warning per cell. That NA is what we want
    # (no CAS / no factor), so only those warnings are silenced; any other warning is kept.
    D <- withCallingHandlers(
      as.data.frame(readxl::read_excel(path, sheet = sheet), check.names = FALSE,
                    stringsAsFactors = FALSE),
      warning = function(w) {
        if (grepl("^Expecting (numeric|date|logical|text)", conditionMessage(w)))
          invokeRestart("muffleWarning")
      })
  }
  nm <- names(D)
  find1 <- function(pat) { k <- grep(pat, nm); if (length(k) < 1) NA_integer_ else k[1] }
  ccas <- find1("^Formatted CAS"); cname <- find1("^Substance Name")
  if (is.na(ccas) || is.na(cname))
    stop("the table needs the columns 'Formatted CAS #' and 'Substance Name'", call. = FALSE)
  num <- function(j) suppressWarnings(as.numeric(as.character(D[[j]])))
  # keys of the TRACI rows
  tcas <- .cas_norm(D[[ccas]])
  tname <- tolower(trimws(gsub("\\s+", " ", as.character(D[[cname]]))))
  # flows of the model
  fk <- .split_flow_keys(fi$flow)
  fk$cas <- .cas_norm(fi$cas)
  al <- c("sulfur oxides" = "sulfur oxides (sox)",
          "particulate matter, \u2264 2.5\u03bcm" = "pm2.5",
          "particulate matter, \u2264 10\u03bcm" = "pm10")
  if (!is.null(aliases)) al[tolower(names(aliases))] <- tolower(unname(aliases))
  fname <- ifelse(fk$name %in% names(al), unname(al[fk$name]), fk$name)
  # match each flow to one TRACI row: by CAS first, then by name for rows without CAS
  by_cas <- match(fk$cas, tcas)
  by_cas[is.na(fk$cas)] <- NA_integer_
  nocas_rows <- which(is.na(tcas))
  # by name: flows WITHOUT a CAS are compared with every TRACI substance (databases often
  # leave the CAS out of generic flows such as "Carbon dioxide / emission to air"); flows with
  # a CAS that TRACI does not list are compared only with the TRACI rows that have no CAS.
  # A name shared by TRACI rows of different substances is ambiguous and is not used.
  grp0 <- ifelse(is.na(tcas), paste0("row", seq_along(tcas)), tcas)
  amb <- names(which(tapply(grp0, tname, function(z) length(unique(z))) > 1))
  by_name_all <- match(fname, tname)
  by_name_all[fname %in% amb] <- NA_integer_
  by_name_nocas <- nocas_rows[match(fname, tname[nocas_rows])]
  by_name_nocas[fname %in% amb] <- NA_integer_
  by_name <- ifelse(is.na(fk$cas), by_name_all, by_name_nocas)
  row <- ifelse(!is.na(by_cas), by_cas, by_name)
  how <- ifelse(!is.na(by_cas), "cas", ifelse(!is.na(by_name), "name", NA_character_))
  # groups of TRACI rows sharing a CAS: take the first non-missing value per column
  grp <- ifelse(is.na(tcas), paste0("row", seq_along(tcas)), tcas)
  g <- factor(grp, levels = unique(grp))
  gi <- as.integer(g)[row]                       # group index of each flow
  rules <- rbind(compartments[, c("pattern", "compartment")], .traci_comp_rules())
  bad <- setdiff(rules$compartment, .traci_comp_codes)
  if (length(bad)) stop("unknown compartment code(s): ", paste(bad, collapse = ", "), call. = FALSE)
  comp <- .traci_classify(fk, rules)
  is_air <- !is.na(comp) & comp %in% c("airU", "airC", "airM")
  nflow <- length(m$flows)
  put <- function(vals_by_group, sel) {
    v <- stats::setNames(numeric(nflow), m$flows)
    ok <- sel & !is.na(gi)
    x <- vals_by_group[gi[ok]]
    keep <- !is.na(x)
    v[fi$flow[ok][keep]] <- x[keep]
    list(v = v, n = sum(keep))
  }
  rows <- list()
  add <- function(label, unit, n_fac, v, n_hit) {
    m$C[[paste0(prefix, label)]] <<- v
    rows[[length(rows) + 1]] <<- data.frame(category = paste0(prefix, label), ref_unit = unit,
      n_factors = n_fac, n_flows_matched = n_hit, share_flows_matched = n_hit / max(nflow, 1),
      skipped_formula = 0L, skipped_unit = 0L, skipped_property = 0L, stringsAsFactors = FALSE)
  }
  # single-compartment (air) categories
  for (ac in .traci_air_cats()) {
    j <- find1(ac$pat)
    if (is.na(j)) next
    vals <- .first_nonna(num(j), g)
    r <- put(vals, is_air)
    add(ac$name, ac$unit, sum(!is.na(num(j))), r$v, r$n)
  }
  # compartment-dependent categories
  flagcol <- function(pat) { j <- find1(pat); if (is.na(j)) NULL else as.character(D[[j]]) }
  dep <- list(
    list(name = "Freshwater ecotoxicity", pat = "^Ecotox\\. CF", unit = "CTUeco", flag = "^CF Flag Ecotox"),
    list(name = "Human health - cancer", pat = "^Human health CF.*cancer", unit = "CTUcancer", flag = "^CF Flag HH carcinogenic"),
    list(name = "Human health - non-cancer", pat = "^Human health CF.*non-canc", unit = "CTUnoncancer", flag = "^CF Flag HH non-carcinogenic"))
  for (dc in dep) {
    cols <- grep(dc$pat, nm)
    if (dc$name == "Human health - cancer") cols <- cols[!grepl("non-canc", tolower(nm[cols]))]
    if (!length(cols)) next
    cc <- .traci_col_comp(nm[cols])
    fl <- flagcol(dc$flag)
    ok_flag <- if (is.null(flags) || is.null(fl)) rep(TRUE, nrow(D)) else fl %in% flags
    v <- stats::setNames(numeric(nflow), m$flows); nhit <- 0L; nfac <- 0L; byc <- list()
    for (k in seq_along(cols)) {
      if (is.na(cc[k])) next
      x <- num(cols[k]); x[!ok_flag] <- NA_real_
      nfac <- nfac + sum(!is.na(x))
      vals <- .first_nonna(x, g)
      byc[[cc[k]]] <- vals
      r <- put(vals, !is.na(comp) & comp == cc[k])
      v <- v + r$v; nhit <- nhit + r$n
    }
    # ground without compartment: mean of the natural and agricultural soil factors (only when both exist)
    if (all(c("nat", "agr") %in% names(byc))) {
      r <- put((byc$nat + byc$agr) / 2, !is.na(comp) & comp == "soilM")
      v <- v + r$v; nhit <- nhit + r$n
    }
    # air without compartment: mean of the urban and rural factors (only when both exist)
    if (all(c("airU", "airC") %in% names(byc))) {
      r <- put((byc$airU + byc$airC) / 2, !is.na(comp) & comp == "airM")
      v <- v + r$v; nhit <- nhit + r$n
    }
    add(dc$name, dc$unit, nfac, v, nhit)
  }
  rep <- do.call(rbind, rows)
  m$method_report <- rbind(m$method_report, rep)
  has_cas <- !is.na(fk$cas)
  # Metals named as the element ("Zinc") are not reached: TRACI lists them as ions with an oxidation state
  # ("zinc(ii)"). They are NOT matched (that would be an assumption); they are counted and reported instead.
  ion_metals <- sort(unique(sub("\\(.*$", "", tname[grepl("^[a-z]+\\([ivx0-9+]+\\)$", tname)])))
  metal_flow <- !is.na(comp) & is.na(how) & fk$name %in% ion_metals
  metal_names <- sort(unique(fk$name[metal_flow]))
  m$traci_report <- list(
    version = "TRACI 2.2 (US EPA, spreadsheet)",
    n_flows = nflow,
    flows_with_cas = sum(has_cas),
    matched_by_cas = sum(how == "cas", na.rm = TRUE),
    matched_by_name = sum(how == "name", na.rm = TRUE),
    ambiguous_names = sum(fname %in% amb),
    flows_unmatched = sum(is.na(how)),
    metals_without_factor = sum(metal_flow),
    metals_without_factor_names = metal_names,
    flows_without_compartment = sum(is.na(comp)),
    air_unspecified_mean = sum(!is.na(comp) & comp == "airM"),
    soil_unspecified_mean = sum(!is.na(comp) & comp == "soilM"),
    traci_rows_without_cas = sum(is.na(tcas)),
    notes = c("Eutrophication and fossil-fuel depletion are not in TRACI 2.2.",
              "Global warming factors are based on IPCC AR4.",
              "Compartment mapping of flow categories is an assumption (see ?read_traci).",
              "Air emitted without a compartment gets the mean of the urban and rural factors (assumption).",
              "Ground emitted without a compartment gets the mean of the natural and agricultural soil factors (assumption)."))
  if (any(metal_flow))
    message(sum(metal_flow), " emission flow(s) of metals named as the element (", paste(metal_names, collapse = ", "),
            ") have no TRACI 2.2 factor: TRACI lists metals as ions with an oxidation state, not as the element. ",
            "Freshwater ecotoxicity and human-health results may be underestimated. See ?read_traci.")
  if (all(rep$n_flows_matched == 0))
    warning("no flow of the model matched a TRACI 2.2 factor: check that `flow_info$cas` is filled ",
            "and that the flow categories identify the emission compartment", call. = FALSE)
  m
}
