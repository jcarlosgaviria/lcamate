#' Monte Carlo simulation with lognormal uncertainty
#'
#' Exchanges with `utype == 2` are drawn as `exp(rnorm(loc, scale))`; other
#' exchanges keep their nominal amount. Draws are independent between
#' exchanges and the same sampled matrices are used for all methods
#' (paired simulation).
#'
#' @inheritParams lca_calc
#' @param methods Character vector of methods.
#' @param n Number of iterations.
#' @param seed Optional seed.
#' @param keep_draws If `TRUE`, the sampled amounts of the uncertain exchanges
#'   are kept (attribute `"draws"`, an `n` x k matrix) for [lca_sens()].
#' @return A matrix with `n` rows and one column per method, of class
#'   `lca_mc`.
#' @export
lca_mc <- function(m, demand, methods, n, seed = NULL, keep_draws = FALSE) {
  if (!is.null(seed)) set.seed(seed)
  sm <- .make_sampler(m)
  out <- matrix(NA_real_, n, length(methods), dimnames = list(NULL, methods))
  dr <- if (keep_draws) matrix(NA_real_, n, sum(sm$mask)) else NULL
  for (i in seq_len(n)) {
    a <- sm$draw()
    if (keep_draws) dr[i, ] <- a[sm$mask]
    mats <- lca_matrices(m, a)
    for (mm in methods) out[i, mm] <- lca_calc(m, demand, mm, mats)$score
  }
  if (keep_draws) {
    ex <- m$ex
    colnames(dr) <- paste0(ex$type[sm$mask], ": ", ex$input[sm$mask], " -> ", ex$output[sm$mask])
    attr(out, "draws") <- dr
  }
  cov <- do.call(rbind, lapply(methods, function(mm)
    cbind(method = mm, lca_uncertainty_coverage(m, demand, mm))))
  attr(out, "coverage") <- cov
  class(out) <- c("lca_mc", class(out))
  out
}

#' Monte Carlo error of the reported mean
#'
#' @param x Numeric vector of simulated scores.
#' @param target_rel Target relative standard error of the mean.
#' @param conf Confidence level of the interval for the mean.
#' @return A list with `n`, `mean`, `se`, `rel_se`, `ci` and `n_needed`
#'   (iterations needed to reach `target_rel`).
#' @export
lca_mc_error <- function(x, target_rel = 0.01, conf = 0.95) {
  n <- length(x); mu <- mean(x); se <- sd(x) / sqrt(n)
  z <- qnorm(1 - (1 - conf) / 2)
  list(n = n, mean = mu, se = se, rel_se = se / abs(mu),
       ci = c(mu - z * se, mu + z * se),
       n_needed = ceiling((sd(x) / (target_rel * abs(mu)))^2))
}

#' Summary table of a Monte Carlo run
#'
#' Reports effect sizes and uncertainty of the result, not p-values:
#' p-values computed on simulated draws only reflect the number of iterations.
#'
#' @param x An `lca_mc` matrix or a numeric vector.
#' @param probs Quantiles to report.
#' @return A data frame with one row per method: `n`, `mean`, `sd`, `cv`,
#'   `median`, the requested quantiles, the Monte Carlo relative standard
#'   error of the mean (`mc_rel_se`) and `unc_coverage`, the share of the
#'   elementary-flow contributions with a varying distribution (see
#'   [lca_uncertainty_coverage()]); a warning is given when it is below 50%.
#'   For a model with a pedigree scenario ([lca_pedigree_scenario()]) the column
#'   `unc_scenario` gives the additional share that varies only by assumption.
#' @export
lca_mc_summary <- function(x, probs = c(0.025, 0.975)) {
  cov <- attr(x, "coverage")
  x <- as.matrix(unclass(x)); attr(x, "draws") <- NULL; attr(x, "coverage") <- NULL
  if (is.null(colnames(x))) colnames(x) <- "score"
  rows <- lapply(colnames(x), function(nm) {
    v <- x[, nm]; q <- stats::quantile(v, probs, names = FALSE)
    d <- data.frame(method = nm, n = length(v), mean = mean(v), sd = sd(v),
                    cv = sd(v) / abs(mean(v)), median = stats::median(v),
                    stringsAsFactors = FALSE)
    for (i in seq_along(probs)) d[[paste0("q", format(100 * probs[i], trim = TRUE))]] <- q[i]
    d$mc_rel_se <- sd(v) / sqrt(length(v)) / abs(mean(v))
    if (!is.null(cov)) d$unc_coverage <- cov$share_bio_contribution[match(nm, cov$method)]
    if (!is.null(cov) && any(cov$share_bio_scenario > 0, na.rm = TRUE))
      d$unc_scenario <- cov$share_bio_scenario[match(nm, cov$method)]
    d
  })
  if (!is.null(cov)) .coverage_warn(cov)
  do.call(rbind, rows)
}

#' Rank-correlation sensitivity of a Monte Carlo result
#'
#' Spearman correlation between each uncertain exchange (as sampled) and the
#' score. It is a screening measure of which inputs drive the variance, not a
#' variance decomposition.
#'
#' @param x An `lca_mc` object created with `keep_draws = TRUE`.
#' @param method Name of the method (column of `x`); defaults to the first.
#' @param top Number of exchanges to return.
#' @return A data frame ordered by absolute correlation.
#' @export
lca_sens <- function(x, method = colnames(x)[1], top = 10) {
  dr <- attr(x, "draws")
  if (is.null(dr)) stop("run lca_mc(..., keep_draws = TRUE) first", call. = FALSE)
  y <- unclass(x)[, method]
  r <- vapply(seq_len(ncol(dr)), function(j)
    if (sd(dr[, j]) == 0) 0 else stats::cor(dr[, j], y, method = "spearman"), 0)
  out <- data.frame(exchange = colnames(dr), rho = r, stringsAsFactors = FALSE)
  out <- out[order(-abs(out$rho)), ]
  rownames(out) <- NULL
  utils::head(out, top)
}
