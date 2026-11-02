# Internal: first non-NULL element
`%||%` <- function(a, b) if (is.null(a)) b else a

# Internal: GWP100 (IPCC AR5, without climate-carbon feedback) for a few gases.
# This is a deliberately small set, enough to validate the reader; it is NOT a
# complete impact method.
.gwp100_ar5 <- c("Carbon dioxide" = 1, "Methane" = 28, "Nitrous oxide" = 265,
  "Sulfur hexafluoride" = 23500, "HFC-134a" = 1300, "HFC-152a" = 138,
  "HFC-23" = 12400, "CFC-11" = 4660, "CFC-12" = 10200, "CFC-113" = 5820,
  "CFC-114" = 8590, "HCFC-22" = 1760, "Carbon tetrafluoride" = 6630,
  "Carbon tetrachloride" = 1730)

.gwp100_cf <- function(keys) {
  emis_air <- startsWith(keys$category, "Elementary flows/emission/air")
  res_air <- startsWith(keys$category, "Elementary flows/resource/air")
  co2 <- keys$flow == "Carbon dioxide"
  g <- unname(.gwp100_ar5[keys$flow])
  out <- ifelse(keys$is_input == 0 & emis_air & !is.na(g), g,
         ifelse(keys$is_input == 1 & co2 & (emis_air | res_air), -1, NA_real_))
  ok <- !is.na(out)
  data.frame(method = rep("GWP100", sum(ok)), flow = keys$key[ok], cf = out[ok],
             stringsAsFactors = FALSE)
}

# Parameters of an olca-schema uncertainty: log-normal (geomMean, geomSd),
# triangle (minimum, mode, maximum), uniform (minimum, maximum)
.unc_field <- function(u, i) {
  if (is.null(u)) return(NA_real_)
  v <- switch(u$distributionType %||% "",
    LOG_NORMAL_DISTRIBUTION = list(u$geomMean, u$geomSd, NULL),
    TRIANGLE_DISTRIBUTION = list(u$minimum, u$mode, u$maximum),
    UNIFORM_DISTRIBUTION = list(u$minimum, u$maximum, NULL),
    list(NULL, NULL, NULL))
  x <- v[[i]]
  if (is.null(x)) NA_real_ else as.numeric(x)
}

# Convert the distribution of exchanges to lcamate columns. `k` multiplies the
# raw amounts to reach the normalised amount (unit, property and reference).
.unc_params <- function(d, raw, k) {
  n <- length(raw)
  out <- data.frame(utype = rep(0, n), loc = NA_real_, scale = NA_real_,
                    minimum = NA_real_, maximum = NA_real_)
  status <- rep("none", n)
  has <- d$dist != ""
  ln <- d$dist == "LOG_NORMAL_DISTRIBUTION"; tr <- d$dist == "TRIANGLE_DISTRIBUTION"
  un <- d$dist == "UNIFORM_DISTRIBUTION"
  status[has & !(ln | tr | un)] <- "unsupported"
  okk <- !is.na(k) & k > 0
  # log-normal
  ok <- ln & okk & raw > 0 & !is.na(d$u1) & d$u1 > 0 & !is.na(d$u2) & d$u2 >= 1
  status[ln & !ok] <- "unsupported"
  real <- ok & d$u2 > 1
  status[ok & !real] <- "degenerate"; status[real] <- "lognormal"
  out$utype[real] <- 2; out$loc[real] <- log(d$u1[real] * k[real]); out$scale[real] <- log(d$u2[real])
  # triangle
  ok <- tr & okk & !is.na(d$u1) & !is.na(d$u2) & !is.na(d$u3) & d$u1 <= d$u2 & d$u2 <= d$u3
  status[tr & !ok] <- "unsupported"
  real <- ok & d$u3 > d$u1
  status[ok & !real] <- "degenerate"; status[real] <- "triangular"
  out$utype[real] <- 5; out$minimum[real] <- d$u1[real] * k[real]
  out$loc[real] <- d$u2[real] * k[real]; out$maximum[real] <- d$u3[real] * k[real]
  # uniform
  ok <- un & okk & !is.na(d$u1) & !is.na(d$u2) & d$u1 <= d$u2
  status[un & !ok] <- "unsupported"
  real <- ok & d$u2 > d$u1
  status[ok & !real] <- "degenerate"; status[real] <- "uniform"
  out$utype[real] <- 4; out$minimum[real] <- d$u1[real] * k[real]; out$maximum[real] <- d$u2[real] * k[real]
  list(par = out, status = status)
}

.read_flows <- function(dir) {
  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
  rows <- lapply(files, function(f) {
    d <- jsonlite::fromJSON(f, simplifyVector = FALSE)
    fp <- d$flowProperties
    if (!length(fp)) return(NULL)
    data.frame(flow_id = d[["@id"]],
               prop = vapply(fp, function(x) x$flowProperty$name, ""),
               conv = vapply(fp, function(x) x$conversionFactor %||% NA_real_, 0),
               is_ref = vapply(fp, function(x) isTRUE(x$isRefFlowProperty), TRUE),
               cas = as.character(d$cas %||% NA_character_),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

.read_processes <- function(dir) {
  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
  hdr <- vector("list", length(files)); exl <- vector("list", length(files))
  for (k in seq_along(files)) {
    d <- jsonlite::fromJSON(files[k], simplifyVector = FALSE)
    pid <- d[["@id"]]
    hdr[[k]] <- data.frame(pid = pid, name = d$name, ptype = d$processType %||% "",
                           category = d$category %||% "",
                           location = (d$location %||% list(name = ""))$name %||% "",
                           stringsAsFactors = FALSE)
    ex <- d$exchanges
    if (!length(ex)) next
    chr <- function(f) vapply(ex, f, "")
    exl[[k]] <- data.frame(
      pid = pid,
      flow_id = chr(function(e) e$flow[["@id"]]),
      flow = chr(function(e) e$flow$name),
      category = chr(function(e) e$flow$category %||% ""),
      ftype = chr(function(e) e$flow$flowType),
      is_input = vapply(ex, function(e) as.integer(isTRUE(e$isInput)), 0L),
      amount = vapply(ex, function(e) as.numeric(e$amount), 0),
      is_ref = vapply(ex, function(e) as.integer(isTRUE(e$isQuantitativeReference)), 0L),
      provider = chr(function(e) e$defaultProvider[["@id"]] %||% ""),
      unit = chr(function(e) e$unit$name),
      prop = chr(function(e) e$flowProperty$name),
      dist = chr(function(e) e$uncertainty$distributionType %||% ""),
      u1 = vapply(ex, function(e) .unc_field(e$uncertainty, 1), 0),
      u2 = vapply(ex, function(e) .unc_field(e$uncertainty, 2), 0),
      u3 = vapply(ex, function(e) .unc_field(e$uncertainty, 3), 0),
      stringsAsFactors = FALSE)
  }
  list(P = do.call(rbind, hdr), X = do.call(rbind, exl))
}

#' Read a USLCI (openLCA JSON-LD) database into an LCA model
#'
#' Reads the folders `processes/` and `flows/` of an openLCA JSON-LD export
#' (for example the U.S. Life Cycle Inventory Database) and builds an
#' [lca_model()] in *cut-off mode*: technosphere inputs without a valid
#' provider inside the database are not followed, and each process reports
#' how complete its linking is.
#'
#' Rules applied:
#' \describe{
#'  \item{Units}{amounts are converted to the reference unit of their unit
#'    group with the factors of `units`; amounts given in a flow property other
#'    than the reference one are converted with the flow's conversion factor.
#'    Exchanges whose unit or property cannot be converted are skipped and
#'    reported.}
#'  \item{R1}{product inputs named `Transformation...` or `Occupation...`
#'    (land use) that have no provider in the database are treated as resource
#'    (elementary) flows.}
#'  \item{R2}{a default-provider link is accepted only if the reference flow of
#'    the provider is the flow being consumed; otherwise the link is refused
#'    and counted as not linked.}
#'  \item{Waste outputs}{waste flows leaving a process are treated as demands
#'    on their treatment provider, if it exists.}
#' }
#' A GWP100 method (IPCC AR5 values for 14 gases, CO2 uptake as -1) is attached
#' as a convenience and for validation; it is **not** a complete impact method.
#'
#' @param path Folder of the JSON-LD export (contains `processes/`, `flows/`).
#' @param units Optional data frame with columns `unit` and `factor` replacing
#'   or extending the built-in table of unit factors.
#' @param verbose Print progress.
#' @return An `lca_model` with extra elements `info` (one row per process:
#'   name, type, category, location, `n_tech`, `n_linked`, `completeness`) and
#'   `report` (counts of reclassified, refused and skipped items).
#' @export
read_uslci <- function(path, units = NULL, verbose = FALSE) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("package 'jsonlite' is required", call. = FALSE)
  uf <- utils::read.csv(system.file("extdata", "uslci_units.csv", package = "lcamate"),
                        stringsAsFactors = FALSE)
  if (!is.null(units)) {
    uf <- rbind(uf[!uf$unit %in% units$unit, c("unit", "factor")], units[, c("unit", "factor")])
  }
  UF <- stats::setNames(uf$factor, uf$unit)
  if (verbose) message("reading flows ...")
  fp <- .read_flows(file.path(path, "flows"))
  if (verbose) message("reading processes ...")
  rd <- .read_processes(file.path(path, "processes"))
  P <- rd$P; X <- rd$X
  X$provider[X$provider == ""] <- NA_character_

  # units and flow properties
  skipped <- list()
  X$raw <- X$amount
  f <- unname(UF[X$unit])
  bad_u <- is.na(f)
  skipped$unit <- X[bad_u, c("pid", "flow", "unit")]
  X$amount <- X$amount * f
  fp_key <- paste(fp$flow_id, fp$prop); refprop <- fp$prop[fp$is_ref]
  names(refprop) <- fp$flow_id[fp$is_ref]
  rp <- unname(refprop[X$flow_id])
  need_conv <- !bad_u & !is.na(rp) & X$prop != rp
  cv <- unname(fp$conv[match(paste(X$flow_id, X$prop), fp_key)])
  bad_p <- need_conv & (is.na(cv) | cv == 0)
  skipped$property <- X[bad_p, c("pid", "flow", "prop")]
  ok <- need_conv & !bad_p
  X$amount[ok] <- X$amount[ok] / cv[ok]
  X$k <- f; X$k[ok] <- f[ok] / cv[ok]
  X <- X[!bad_u & !bad_p, ]

  ids <- P$pid
  # R1
  land <- X$ftype == "PRODUCT_FLOW" & X$is_input == 1 &
    grepl("^(Transformation|Occupation)", X$flow) & !(X$provider %in% ids)
  X$ftype[land] <- "ELEMENTARY_FLOW"
  # processes with a non-zero reference amount
  refx <- X[X$is_ref == 1, ]
  refx <- refx[!duplicated(refx$pid), ]
  refamt <- stats::setNames(refx$amount, refx$pid)
  keep <- names(refamt)[refamt != 0]
  P <- P[P$pid %in% keep, ]; ids <- P$pid
  X <- X[X$pid %in% ids, ]
  rf <- stats::setNames(refx$flow_id, refx$pid)
  # technosphere
  tsel <- X$is_ref == 0 & X$amount != 0 &
    ((X$ftype == "PRODUCT_FLOW" & X$is_input == 1) |
     (X$ftype == "WASTE_FLOW" & X$is_input == 0))
  T <- X[tsel, ]
  in_db <- T$provider %in% ids
  same <- in_db & unname(rf[T$provider]) == T$flow_id
  T$linked <- !is.na(same) & same
  n_refused <- sum(in_db & !T$linked)
  n_tech <- tapply(rep(1L, nrow(T)), factor(T$pid, ids), sum)
  n_link <- tapply(as.integer(T$linked), factor(T$pid, ids), sum)
  n_tech[is.na(n_tech)] <- 0L; n_link[is.na(n_link)] <- 0L
  info <- data.frame(P, n_tech = as.integer(n_tech), n_linked = as.integer(n_link),
                     completeness = ifelse(n_tech > 0, n_link / pmax(n_tech, 1), 1),
                     row.names = NULL, stringsAsFactors = FALSE)
  TL <- T[T$linked & T$provider != T$pid, ]
  kk <- function(D) D$k / unname(refamt[D$pid])
  ut <- .unc_params(TL, TL$raw, kk(TL))
  tech <- data.frame(type = rep("technosphere", nrow(TL)), input = TL$provider,
                     output = TL$pid, amount = TL$amount / unname(refamt[TL$pid]),
                     ut$par, stringsAsFactors = FALSE)
  E <- X[X$ftype == "ELEMENTARY_FLOW" & X$amount != 0, ]
  E$key <- paste0(E$flow, " | ", E$category, ifelse(E$is_input == 1, " | in", " | out"))
  ub <- .unc_params(E, E$raw, kk(E))
  bio <- data.frame(type = rep("biosphere", nrow(E)), input = E$key, output = E$pid,
                    amount = E$amount / unname(refamt[E$pid]), ub$par, stringsAsFactors = FALSE)
  ex <- rbind(tech, bio)
  # exchanges without a varying distribution are summed per key; the others stay
  # separate because each is an independent random variable
  fixed <- ex$utype == 0
  exf <- ex[fixed, ]
  g <- paste(exf$type, exf$input, exf$output, sep = "\r")
  sm <- rowsum(exf$amount, g, reorder = FALSE)
  first <- !duplicated(g)
  exf <- exf[first, ]
  exf$amount <- as.numeric(sm[g[first], 1])
  ex <- rbind(exf, ex[!fixed, ])
  st <- c(ut$status, ub$status)
  n_ex <- length(st)
  keys <- E[!duplicated(E$key), c("key", "flow", "category", "is_input", "flow_id")]
  cf <- .gwp100_cf(keys)
  rpn <- unname(refprop[keys$flow_id])
  finfo <- data.frame(flow = keys$key, id = keys$flow_id, ref_prop = ifelse(is.na(rpn), NA_character_, rpn),
                      cas = unname(fp$cas[match(keys$flow_id, fp$flow_id)]),
                      stringsAsFactors = FALSE)
  m <- lca_model(ex, cf, processes = ids, info = info, flow_info = finfo)
  m$report <- list(r1_reclassified = sum(land), r2_refused = n_refused,
                   skipped_unit = nrow(skipped$unit), skipped_property = nrow(skipped$property),
                   n_processes = length(ids),
                   uncertainty = c(as.list(table(factor(st, c("lognormal", "triangular", "uniform",
                                                              "degenerate", "unsupported", "none")))),
                                   n_exchanges = n_ex,
                                   share_with_spread = sum(st %in% c("lognormal", "triangular", "uniform")) / max(n_ex, 1)))
  m
}
