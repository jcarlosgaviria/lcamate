#' Contributions of processes to a score, with a "Rest" category
#'
#' @param res An `lca_result` from [lca_calc()].
#' @param n Number of processes shown individually (largest absolute
#'   contribution); the others are summed in `"Rest"`.
#' @param labels Optional named character vector (names = process ids,
#'   values = readable names) used instead of the ids.
#' @return A data frame with `process`, `contribution` and `share`
#'   (fraction of the total score).
#' @export
lca_contrib <- function(res, n = 10, labels = NULL) {
  v <- res$by_process
  if (!is.null(labels)) {
    l <- unname(labels[names(v)]); names(v) <- ifelse(is.na(l), names(v), l)
  }
  v <- v[order(-abs(v))]
  v <- v[v != 0]
  if (length(v) > n) v <- c(v[seq_len(n)], Rest = sum(v[-seq_len(n)]))
  data.frame(process = names(v), contribution = unname(v),
             share = unname(v) / res$score, row.names = NULL, stringsAsFactors = FALSE)
}

.need_gg <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("package 'ggplot2' is required for plots; install it with install.packages('ggplot2')",
         call. = FALSE)
}

#' Plot process contributions
#'
#' @inheritParams lca_contrib
#' @return A `ggplot` object (data in `$data`).
#' @export
plot_contributions <- function(res, n = 10, labels = NULL) {
  .need_gg()
  d <- lca_contrib(res, n, labels)
  d$process <- factor(d$process, levels = rev(d$process))
  d$label <- sprintf("%s (%.1f%%)", trimws(formatC(d$contribution, digits = 3, format = "g")),
                     100 * d$share)
  d$hj <- ifelse(d$contribution >= 0, -0.05, 1.05)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$contribution, y = .data$process)) +
    ggplot2::geom_col(fill = "#2c7fb8") +
    ggplot2::geom_text(ggplot2::aes(label = .data$label, hjust = .data$hj), size = 3.2) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.12, 0.28))) +
    ggplot2::labs(x = paste("Contribution to", res$method), y = NULL,
                  title = "Process contributions",
                  subtitle = paste("Total score:", format(res$score, digits = 4))) +
    ggplot2::theme_minimal()
}

#' Plot the distribution of a Monte Carlo result
#'
#' Histogram with the mean (solid line) and the central interval (dashed
#' lines).
#'
#' @param x An `lca_mc` object or numeric vector.
#' @param method Column to plot; defaults to the first.
#' @param level Coverage of the central interval.
#' @param bins Number of histogram bins.
#' @return A `ggplot` object.
#' @export
plot_mc <- function(x, method = NULL, level = 0.95, bins = 40) {
  .need_gg()
  if (is.matrix(x)) {
    method <- method %||% colnames(x)[1]
    v <- unclass(x)[, method]
  } else { v <- x; method <- method %||% "score" }
  q <- stats::quantile(v, c((1 - level) / 2, 1 - (1 - level) / 2), names = FALSE)
  ggplot2::ggplot(data.frame(v = as.numeric(v)), ggplot2::aes(x = .data$v)) +
    ggplot2::geom_histogram(bins = bins, fill = "#2c7fb8", colour = "white") +
    ggplot2::geom_vline(xintercept = mean(v), linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = q, linetype = "dashed") +
    ggplot2::labs(x = method, y = "Iterations",
                  title = "Monte Carlo distribution",
                  subtitle = sprintf("n = %d, mean = %s, %g%% interval [%s, %s]",
                                     length(v), format(mean(v), digits = 4), 100 * level,
                                     format(q[1], digits = 4), format(q[2], digits = 4))) +
    ggplot2::theme_minimal()
}

#' Plot the sensitivity screening
#'
#' @param x An `lca_mc` object created with `keep_draws = TRUE`.
#' @inheritParams lca_sens
#' @return A `ggplot` object.
#' @export
plot_sens <- function(x, method = colnames(x)[1], top = 10) {
  .need_gg()
  d <- lca_sens(x, method, top)
  d$exchange <- factor(d$exchange, levels = rev(d$exchange))
  d$label <- sprintf("%.2f", d$rho)
  d$hj <- ifelse(d$rho >= 0, -0.1, 1.1)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$rho, y = .data$exchange)) +
    ggplot2::geom_col(fill = "#2c7fb8") +
    ggplot2::geom_text(ggplot2::aes(label = .data$label, hjust = .data$hj), size = 3.2) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.12, 0.15))) +
    ggplot2::labs(x = "Spearman correlation with the score", y = NULL,
                  title = "Sensitivity screening") +
    ggplot2::theme_minimal()
}
