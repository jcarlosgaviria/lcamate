# Internal: GWP100 values and the names under which each gas is recognised.
# AR5: IPCC AR5 WGI Table 8.A.1 (without climate-carbon feedback), as used since the first version.
# AR6: only values read from the official IPCC AR6 WGI Chapter 7 Supplementary
# Material, Table 7.SM.7 (CO2, N2O, HFC-134a, HFC-23, CFC-11, CFC-12, CFC-113,
# CFC-114, HCFC-22); methane is handled apart (see lca_method_gwp). Other gases
# are NOT included for AR6 because a secondary copy of the table was found to
# differ from the official one; add them with `extra`.
.gwp_table <- function() {
  g <- function(gas, alias, ar5, ar6) data.frame(gas = gas, alias = I(list(tolower(alias))),
                                                 ar5 = ar5, ar6 = ar6, stringsAsFactors = FALSE)
  rbind(
    g("CO2", c("carbon dioxide", "co2"), 1, 1),
    g("N2O", c("nitrous oxide", "dinitrogen monoxide", "n2o"), 265, 273),
    g("SF6", c("sulfur hexafluoride", "sf6"), 23500, NA),
    g("HFC-134a", c("hfc-134a", "1,1,1,2-tetrafluoroethane"), 1300, 1530),
    g("HFC-152a", c("hfc-152a"), 138, NA),
    g("HFC-23", c("hfc-23", "trifluoromethane"), 12400, 14600),
    g("CFC-11", c("cfc-11", "trichlorofluoromethane"), 4660, 6230),
    g("CFC-12", c("cfc-12", "dichlorodifluoromethane"), 10200, 12500),
    g("CFC-113", c("cfc-113"), 5820, 6520),
    g("CFC-114", c("cfc-114"), 8590, 9430),
    g("HCFC-22", c("hcfc-22", "chlorodifluoromethane"), 1760, 1960),
    g("CF4", c("carbon tetrafluoride", "cf4", "pfc-14"), 6630, NA),
    g("CCl4", c("carbon tetrachloride", "ccl4"), 1730, NA))
}

# Internal: split model flow keys ("name | category | in/out") into parts
.split_flow_keys <- function(keys) {
  parts <- strsplit(keys, " | ", fixed = TRUE)
  nm <- vapply(parts, function(p) p[1], "")
  dir <- vapply(parts, function(p) if (length(p) >= 2 && p[length(p)] %in% c("in", "out")) p[length(p)] else NA_character_, "")
  cat <- vapply(parts, function(p) if (length(p) >= 3) p[2] else NA_character_, "")
  data.frame(key = keys, name = tolower(trimws(nm)), dir = dir, category = cat, stringsAsFactors = FALSE)
}

#' Add a GWP100 method (IPCC AR5 or AR6) to a model
#'
#' Builds a global warming potential method by matching flow **names** with a
#' small table of gases and adds it to the model. It is a convenience for
#' climate-only studies; it is **not** a complete impact assessment method (for
#' those use [read_olca_method()]).
#'
#' Only air emissions are characterised. Where the flow key has a compartment,
#' it must start with `"Elementary flows/emission/air"`; flows whose key has no
#' compartment (for example from a CSV) are taken as air emissions. An input of
#' carbon dioxide from air gets the factor -1. Biogenic or non-fossil carbon
#' dioxide is not recognised and must be handled by the analyst.
#'
#' Values: AR5 uses IPCC AR5 Table 8.A.1 (without climate-carbon feedback) for 13
#' gases and carbon dioxide. For AR6 only the gases whose values were read from
#' the official Table 7.SM.7 of the Chapter 7 Supplementary Material are
#' included (carbon dioxide, nitrous oxide 273, HFC-134a, HFC-23, CFC-11, CFC-12,
#' CFC-113, CFC-114 and HCFC-22), and methane: `ch4 = "fossil"` 29.8 and
#' `"nonfossil"` 27.0 (Table 7.15) or `"sm7"` 27.9 (Table 7.SM.7). Verify the
#' values against the report before citing them. Other gases can be added with
#' `extra`. Methane flows whose name contains "fossil" (not "non-fossil" or
#' "biogenic") use the fossil value, those with "biogenic" or "non-fossil" the
#' non-fossil value, and the others the value chosen with `ch4`.
#'
#' @param m An `lca_model`.
#' @param version `"AR5"` or `"AR6"`.
#' @param ch4 For AR6, the methane value for unlabelled methane flows:
#'   `"fossil"` (29.8), `"nonfossil"` (27.0) or `"sm7"` (27.9). Ignored for AR5,
#'   which has the single value 28.
#' @param extra Named numeric vector of additional gases (names matched
#'   case-insensitively with the flow names), GWP100 in kg CO2-eq per kg.
#' @param name Name of the method in `m$C`; default `"GWP100_AR5"` or
#'   `"GWP100_AR6"`.
#' @return The model with the new method in `m$C` and a row in
#'   `m$method_report` (number of gases in the table, number of flows of the
#'   model characterised, and their share).
#' @export
lca_method_gwp <- function(m, version = c("AR5", "AR6"), ch4 = c("fossil", "nonfossil", "sm7"),
                           extra = NULL, name = NULL) {
  if (!inherits(m, "lca_model")) stop("`m` must be an lca_model", call. = FALSE)
  version <- match.arg(version); ch4 <- match.arg(ch4)
  tab <- .gwp_table()
  vals <- if (version == "AR5") tab$ar5 else tab$ar6
  ch4_val <- if (version == "AR5") list(fossil = 28, nonfossil = 28, plain = 28) else
    list(fossil = 29.8, nonfossil = 27.0,
         plain = c(fossil = 29.8, nonfossil = 27.0, sm7 = 27.9)[[ch4]])
  lk <- data.frame(alias = unlist(tab$alias), v = rep(vals, lengths(tab$alias)), stringsAsFactors = FALSE)
  lk <- lk[!is.na(lk$v), ]
  if (!is.null(extra)) {
    if (is.null(names(extra)) || !is.numeric(extra)) stop("`extra` must be a named numeric vector", call. = FALSE)
    lk <- rbind(lk[!lk$alias %in% tolower(names(extra)), ], data.frame(alias = tolower(names(extra)), v = unname(extra)))
  }
  fk <- .split_flow_keys(m$flows)
  air_em <- is.na(fk$category) | startsWith(fk$category, "Elementary flows/emission/air")
  is_out <- is.na(fk$dir) | fk$dir == "out"
  v <- rep(NA_real_, nrow(fk))
  hit <- match(fk$name, lk$alias)
  ok <- !is.na(hit) & air_em & is_out
  v[ok] <- lk$v[hit[ok]]
  # methane
  is_ch4 <- grepl("^(methane|ch4)\\b", fk$name)
  nonfossil <- grepl("non-fossil|biogenic", fk$name)
  fossil <- grepl("fossil", fk$name) & !nonfossil
  mk <- is_ch4 & air_em & is_out & !(fk$name %in% lk$alias)
  v[mk] <- ifelse(nonfossil[mk], ch4_val$nonfossil, ifelse(fossil[mk], ch4_val$fossil, ch4_val$plain))
  # plain "methane"/"ch4" names are not in the table, so mk covers them; user extras win
  if (!is.null(extra)) { e <- match(fk$name, tolower(names(extra))); v[!is.na(e) & air_em & is_out] <- extra[e[!is.na(e) & air_em & is_out]] }
  # CO2 taken up from air
  co2 <- fk$name %in% c("carbon dioxide", "co2")
  uptake <- co2 & !is.na(fk$dir) & fk$dir == "in" &
    (startsWith(ifelse(is.na(fk$category), "", fk$category), "Elementary flows/resource/air") |
     startsWith(ifelse(is.na(fk$category), "", fk$category), "Elementary flows/emission/air"))
  v[uptake] <- -1
  cf <- stats::setNames(ifelse(is.na(v), 0, v), m$flows)
  nm <- if (is.null(name)) paste0("GWP100_", version) else name
  m$C[[nm]] <- cf
  n_gas <- length(unique(lk$alias))
  rep1 <- data.frame(category = nm, ref_unit = "kg CO2 eq", n_factors = nrow(tab[!is.na(vals), ]) + 1L + length(extra),
                     n_flows_matched = sum(!is.na(v)), share_flows_matched = sum(!is.na(v)) / length(m$flows),
                     skipped_formula = 0L, skipped_unit = 0L, skipped_property = 0L, stringsAsFactors = FALSE)
  m$method_report <- rbind(m$method_report, rep1)
  m
}
