# Internal sampler for the exchange distributions of a model.
# utype codes follow Brightway: 2 lognormal (loc = log(median), scale = log(GSD)),
# 4 uniform (minimum, maximum), 5 triangular (loc = mode, minimum, maximum).
.make_sampler <- function(m) {
  ex <- m$ex
  if (is.null(ex$utype)) stop("model has no uncertainty columns", call. = FALSE)
  u <- ex$utype; u[is.na(u)] <- 0
  ln <- u == 2; un <- u == 4; tr <- u == 5
  col <- function(nm) if (is.null(ex[[nm]])) rep(NA_real_, nrow(ex)) else ex[[nm]]
  loc <- col("loc"); sc <- col("scale"); mn <- col("minimum"); mx <- col("maximum")
  if (any(un | tr) && anyNA(c(mn[un | tr], mx[un | tr])))
    stop("uniform/triangular exchanges need `minimum` and `maximum`", call. = FALSE)
  list(mask = ln | un | tr,
       draw = function() {
         a <- ex$amount
         if (any(ln)) a[ln] <- exp(rnorm(sum(ln), loc[ln], sc[ln]))
         if (any(un)) a[un] <- stats::runif(sum(un), mn[un], mx[un])
         if (any(tr)) {
           lo <- mn[tr]; hi <- mx[tr]; md <- loc[tr]; v <- stats::runif(sum(tr))
           fc <- (md - lo) / (hi - lo)
           a[tr] <- ifelse(v < fc, lo + sqrt(v * (hi - lo) * (md - lo)),
                           hi - sqrt((1 - v) * (hi - lo) * (hi - md)))
         }
         a
       })
}

# Exchanges with a distribution that actually varies
.has_spread <- function(ex) {
  if (is.null(ex$utype)) return(rep(FALSE, nrow(ex)))
  u <- ex$utype; u[is.na(u)] <- 0
  sc <- if (is.null(ex$scale)) rep(NA_real_, nrow(ex)) else ex$scale
  mn <- if (is.null(ex$minimum)) rep(NA_real_, nrow(ex)) else ex$minimum
  mx <- if (is.null(ex$maximum)) rep(NA_real_, nrow(ex)) else ex$maximum
  (u == 2 & !is.na(sc) & sc > 0) | ((u == 4 | u == 5) & !is.na(mn) & !is.na(mx) & mx > mn)
}

#' Uncertainty coverage of a result
#'
#' A Monte Carlo interval only reflects the uncertainty that is *defined* in
#' the data. When few exchanges carry a distribution, a narrow interval means
#' lack of information, not low uncertainty. This function reports how much of
#' the calculation has a distribution that actually varies.
#'
#' @inheritParams lca_calc
#' @return A one-row data frame: `n_exchanges`, `n_uncertain`,
#'   `share_exchanges` (all exchanges), `share_active` (exchanges of the
#'   processes with non-zero supply for this demand) and
#'   `share_bio_contribution`, the share of the absolute direct contributions of
#'   the elementary-flow exchanges that have a varying distribution. The last
#'   one is indicative: it does not weigh technosphere exchanges.
#' @export
lca_uncertainty_coverage <- function(m, demand, method) {
  res <- lca_calc(m, demand, method)
  ex <- m$ex
  scen <- if (is.null(ex$pedigree_scenario)) rep(FALSE, nrow(ex)) else ex$pedigree_scenario
  unc_all <- .has_spread(ex); unc <- unc_all & !scen   # documented uncertainty only
  act <- res$supply[ex$col] != 0
  bio <- ex$type == "biosphere"
  contrib <- abs(m$C[[method]][ex$row[bio]] * ex$amount[bio] * res$supply[ex$col[bio]])
  tot <- sum(contrib)
  data.frame(n_exchanges = nrow(ex), n_uncertain = sum(unc),
             share_exchanges = mean(unc),
             share_active = if (any(act)) mean(unc[act]) else NA_real_,
             share_bio_contribution = if (tot > 0) sum(contrib[unc[bio]]) / tot else NA_real_,
             share_bio_scenario = if (tot > 0) sum(contrib[(unc_all & scen)[bio]]) / tot else NA_real_,
             row.names = NULL)
}

.coverage_warn <- function(cov, threshold = 0.5) {
  v <- cov$share_bio_contribution
  v <- v[!is.na(v)]
  if (length(v) && min(v) < threshold)
    warning(sprintf(paste0("only %.1f%% of the elementary-flow contributions have a varying ",
      "uncertainty distribution; the intervals reflect only the uncertainty defined in the data, ",
      "not the total uncertainty (see lca_uncertainty_coverage())"), 100 * min(v)), call. = FALSE)
  invisible(NULL)
}
