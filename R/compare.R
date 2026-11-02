#' Compare two alternatives with a paired Monte Carlo
#'
#' Both alternatives are evaluated on the *same* sampled matrices in every
#' iteration (`paired = TRUE`), so uncertainty in processes shared by A and B
#' cancels in the difference. With `paired = FALSE` alternative B uses its
#' own independent draws; this is only useful to show how much ignoring the
#' pairing inflates the apparent uncertainty.
#'
#' The result reports effect sizes and probabilities, not p-values: with
#' simulated draws a p-value only reflects the number of iterations.
#'
#' @param m An `lca_model`.
#' @param demand_a,demand_b Named numeric vectors (see [lca_calc()]).
#' @param methods Character vector of methods.
#' @param n Number of iterations.
#' @param seed Optional seed.
#' @param paired Use the same draws for A and B (default `TRUE`).
#' @return An object of class `lca_compare` with matrices `a`, `b` and
#'   `diff` (A minus B; `n` rows, one column per method) and the deterministic
#'   scores in `nominal`. Use [lca_compare_summary()] for a table.
#' @examples
#' m <- read_lca_csv(system.file("extdata", "toy_exchanges.csv", package = "lcamate"),
#'                   system.file("extdata", "toy_cf.csv", package = "lcamate"))
#' cmp <- lca_compare(m, c(steel = 1), c(coal = 1), "GWP100", n = 200, seed = 1)
#' lca_compare_summary(cmp)
#' @export
lca_compare <- function(m, demand_a, demand_b, methods, n, seed = NULL, paired = TRUE) {
  if (!is.null(seed)) set.seed(seed)
  sm <- .make_sampler(m)
  draw <- function() lca_matrices(m, sm$draw())
  A <- B <- matrix(NA_real_, n, length(methods), dimnames = list(NULL, methods))
  for (i in seq_len(n)) {
    ma <- draw()
    mb <- if (paired) ma else draw()
    for (mm in methods) {
      A[i, mm] <- lca_calc(m, demand_a, mm, ma)$score
      B[i, mm] <- lca_calc(m, demand_b, mm, mb)$score
    }
  }
  mats <- lca_matrices(m)
  nominal <- vapply(methods, function(mm)
    c(a = lca_calc(m, demand_a, mm, mats)$score, b = lca_calc(m, demand_b, mm, mats)$score),
    c(a = 0, b = 0))
  cov <- function(d) do.call(rbind, lapply(methods, function(mm)
    cbind(method = mm, lca_uncertainty_coverage(m, d, mm))))
  structure(list(a = A, b = B, diff = A - B, nominal = nominal, paired = paired,
                 demand_a = demand_a, demand_b = demand_b,
                 coverage = list(a = cov(demand_a), b = cov(demand_b))),
            class = "lca_compare")
}

#' Summary of a paired comparison
#'
#' @param x An `lca_compare` object.
#' @param level Coverage of the interval of the difference.
#' @return A data frame, one row per method, with the mean of A and B, the mean
#'   difference (A - B) and its central interval, `prob_a_lower` (share of
#'   iterations in which A has the lower impact), the median ratio A/B, and
#'   `dz` (mean difference divided by its standard deviation, a standardised
#'   effect size; `NA` when the difference does not vary) and `unc_coverage`
#'   (the lower of the two alternatives' share of elementary-flow contributions
#'   with a varying distribution; see [lca_uncertainty_coverage()]). A warning
#'   is given when it is below 50%.
#' @export
lca_compare_summary <- function(x, level = 0.95) {
  pr <- c((1 - level) / 2, 1 - (1 - level) / 2)
  rows <- lapply(colnames(x$diff), function(mm) {
    d <- x$diff[, mm]; q <- stats::quantile(d, pr, names = FALSE)
    s <- sd(d)
    data.frame(method = mm, n = length(d), mean_a = mean(x$a[, mm]), mean_b = mean(x$b[, mm]),
               mean_diff = mean(d), diff_lower = q[1], diff_upper = q[2],
               prob_a_lower = mean(d < 0),
               median_ratio = stats::median(x$a[, mm] / x$b[, mm]),
               dz = if (s > 0) mean(d) / s else NA_real_,
               unc_coverage = if (is.null(x$coverage)) NA_real_ else
                 min(x$coverage$a$share_bio_contribution[x$coverage$a$method == mm],
                     x$coverage$b$share_bio_contribution[x$coverage$b$method == mm]),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (!is.null(x$coverage)) .coverage_warn(data.frame(share_bio_contribution = out$unc_coverage))
  out
}

#' @export
print.lca_compare <- function(x, ...) {
  cat("<lca_compare>", nrow(x$diff), "iterations,", if (x$paired) "paired" else "UNPAIRED", "\n")
  print(lca_compare_summary(x), digits = 4, row.names = FALSE)
  invisible(x)
}

#' Plot the distribution of the difference A - B
#'
#' @param x An `lca_compare` object.
#' @param method Method to plot; defaults to the first.
#' @param bins Number of histogram bins.
#' @return A `ggplot` object. The dashed line marks zero (no difference).
#' @export
plot_compare <- function(x, method = colnames(x$diff)[1], bins = 40) {
  .need_gg()
  d <- x$diff[, method]
  s <- lca_compare_summary(x)[lca_compare_summary(x)$method == method, ]
  ggplot2::ggplot(data.frame(d = d), ggplot2::aes(x = .data$d)) +
    ggplot2::geom_histogram(bins = bins, fill = "#2c7fb8", colour = "white") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = mean(d), linewidth = 0.8) +
    ggplot2::labs(x = paste("Difference A - B in", method), y = "Iterations",
                  title = if (x$paired) "Paired Monte Carlo comparison" else "Unpaired Monte Carlo comparison",
                  subtitle = sprintf("P(A lower) = %.1f%%, mean difference = %s, median ratio A/B = %s",
                                     100 * s$prob_a_lower, format(s$mean_diff, digits = 4),
                                     format(s$median_ratio, digits = 3))) +
    ggplot2::theme_minimal()
}
