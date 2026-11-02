# Standard 2^(k-p) generators (Box-Hunter style), by position of the factors
.default_generators <- list(
  "3-1" = c(C = "A*B"), "4-1" = c(D = "A*B*C"), "5-1" = c(E = "A*B*C*D"),
  "5-2" = c(D = "A*B", E = "A*C"), "6-1" = c(F = "A*B*C*D*E"),
  "6-2" = c(E = "A*B*C", F = "B*C*D"), "6-3" = c(D = "A*B", E = "A*C", F = "B*C"),
  "7-1" = c(G = "A*B*C*D*E*F"), "7-2" = c(F = "A*B*C*D", G = "A*B*D*E"),
  "7-3" = c(E = "A*B*C", F = "B*C*D", G = "A*C*D"),
  "7-4" = c(D = "A*B", E = "A*C", F = "B*C", G = "A*B*C"))

# Words of the defining relation (all products of the generator words)
.defining_words <- function(gen_words) {
  words <- list(); n <- length(gen_words)
  for (s in seq_len(2^n - 1)) {
    pick <- which(bitwAnd(s, 2^(seq_len(n) - 1)) > 0)
    w <- Reduce(function(a, b) union(setdiff(a, b), setdiff(b, a)), gen_words[pick])
    words[[length(words) + 1]] <- sort(w)
  }
  words
}

#' Build an experimental design
#'
#' Full factorial or two-level fractional factorial (2^(k-p)) designs for the
#' factors of an LCA study. Factors can be physical (amounts, temperatures)
#' or scenario choices (supplier, material); in fractional designs the first
#' level of each factor is coded -1 and the second +1.
#'
#' @param factors Named list; each element is the vector of levels of a factor.
#' @param type `"full"` (all combinations) or `"frac"` (two-level fraction).
#' @param p For `type = "frac"`, number of generators (fraction `1/2^p`).
#' @param generators For `type = "frac"`, named character vector such as
#'   `c(D = "A*B*C")` (the factor on the left is generated from the product of
#'   the others). If `NULL`, standard generators by position are used
#'   (3-7 factors).
#' @param replicates Number of replicates of the whole design.
#' @param randomize Randomise the run order.
#' @param seed Seed for the randomisation.
#' @return An object of class `lca_design`: a data frame with columns `run`,
#'   `std_order`, `replicate` and one column per factor (actual levels), with
#'   attributes `coded` (the -1/+1 matrix for two-level designs) and
#'   `generators`.
#' @examples
#' d <- lca_doe_design(list(A = c(1, 2), B = c("x", "y"), C = c(0, 1)))
#' d
#' f <- lca_doe_design(list(A = 0:1, B = 0:1, C = 0:1, D = 0:1), type = "frac")
#' lca_doe_aliases(f)
#' @export
lca_doe_design <- function(factors, type = c("full", "frac"), p = 1, generators = NULL,
                           replicates = 1, randomize = FALSE, seed = NULL) {
  type <- match.arg(type)
  if (!is.list(factors) || is.null(names(factors)) || any(names(factors) == ""))
    stop("`factors` must be a named list of levels", call. = FALSE)
  if (any(lengths(factors) < 2)) stop("each factor needs at least two levels", call. = FALSE)
  k <- length(factors); nm <- names(factors)
  coded <- NULL; gens <- NULL
  if (type == "full") {
    idx <- do.call(expand.grid, c(lapply(factors, seq_along), KEEP.OUT.ATTRS = FALSE))
    if (all(lengths(factors) == 2)) coded <- as.matrix(2 * idx - 3)
  } else {
    if (any(lengths(factors) != 2)) stop("fractional designs need two levels per factor", call. = FALSE)
    if (is.null(generators)) {
      key <- paste0(k, "-", p)
      g <- .default_generators[[key]]
      if (is.null(g)) stop("no default generators for ", k, " factors and p = ", p,
                           "; supply `generators`", call. = FALSE)
      names(g) <- nm[match(names(g), LETTERS)]
      generators <- vapply(g, function(s) paste(nm[match(strsplit(s, "\\*")[[1]], LETTERS)],
                                                collapse = "*"), "")
    }
    if (!all(names(generators) %in% nm)) stop("generator names must be factor names", call. = FALSE)
    base <- setdiff(nm, names(generators))
    if (length(base) + length(generators) != k) stop("inconsistent generators", call. = FALSE)
    g0 <- as.matrix(expand.grid(stats::setNames(rep(list(c(-1, 1)), length(base)), base),
                                KEEP.OUT.ATTRS = FALSE))
    coded <- g0
    for (gn in names(generators)) {
      parts <- strsplit(generators[[gn]], "\\*")[[1]]
      if (!all(parts %in% colnames(coded))) stop("generator uses unknown or later factor: ", gn, call. = FALSE)
      newcol <- apply(coded[, parts, drop = FALSE], 1, prod)
      coded <- cbind(coded, newcol)
      colnames(coded)[ncol(coded)] <- gn
    }
    coded <- coded[, nm, drop = FALSE]
    idx <- as.data.frame((coded + 3) / 2)
    gens <- generators
  }
  D <- as.data.frame(Map(function(lv, i) lv[i], factors, idx), stringsAsFactors = FALSE)
  names(D) <- nm
  std <- seq_len(nrow(D))
  reps <- rep(seq_len(replicates), each = nrow(D))
  D <- D[rep(std, replicates), , drop = FALSE]
  if (!is.null(coded)) coded <- coded[rep(std, replicates), , drop = FALSE]
  out <- data.frame(run = seq_len(nrow(D)), std_order = rep(std, replicates),
                    replicate = reps, D, row.names = NULL, stringsAsFactors = FALSE)
  if (randomize) {
    if (!is.null(seed)) set.seed(seed)
    o <- sample(nrow(out))
    out <- out[o, ]; if (!is.null(coded)) coded <- coded[o, , drop = FALSE]
    out$run <- seq_len(nrow(out)); rownames(out) <- NULL
  }
  structure(out, coded = coded, generators = gens, factors = factors, class = c("lca_design", "data.frame"))
}

#' @export
print.lca_design <- function(x, ...) {
  g <- attr(x, "generators")
  cat("<lca_design>", nrow(x), "runs,", length(attr(x, "factors")), "factors",
      if (!is.null(g)) paste0("(fraction; generators: ", paste(names(g), "=", g, collapse = ", "), ")") else "(full)", "\n")
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}

#' Aliases (confounding) of a fractional design
#'
#' @param design An `lca_design` created with `type = "frac"`.
#' @return A data frame with the design `resolution` and, for every main
#'   effect and two-factor interaction, the effects it is aliased with.
#'   Main effects of a resolution III design are confounded with two-factor
#'   interactions; read the effects accordingly.
#' @export
lca_doe_aliases <- function(design) {
  g <- attr(design, "generators")
  if (is.null(g)) stop("not a fractional design: no aliases", call. = FALSE)
  nm <- names(attr(design, "factors"))
  gw <- lapply(names(g), function(n) c(n, strsplit(g[[n]], "\\*")[[1]]))
  words <- .defining_words(gw)
  res <- min(lengths(words))
  terms <- c(as.list(nm), combn(nm, 2, simplify = FALSE))
  lab <- function(s) paste(sort(s), collapse = ":")
  al <- vapply(terms, function(t) {
    a <- unique(vapply(words, function(w) lab(union(setdiff(t, w), setdiff(w, t))), ""))
    a <- a[a != "" & lengths(strsplit(a, ":")) <= 2]
    if (length(a)) paste(a, collapse = ", ") else ""
  }, "")
  out <- data.frame(term = vapply(terms, lab, ""), aliased_with = al, stringsAsFactors = FALSE)
  attr(out, "resolution") <- res
  out
}

#' Scale an exchange of a model (helper for design factors)
#'
#' Multiplies the amount of the exchange(s) from `input` to `output` by `by`.
#' For lognormal uncertainty the location is shifted by `log(by)`, so the
#' geometric standard deviation is preserved; uniform and triangular
#' distributions (minimum, maximum, mode) are multiplied by `by`.
#'
#' @param m An `lca_model`.
#' @param input,output Names of the exchange (see [lca_model()]).
#' @param by Positive multiplier.
#' @return The modified `lca_model`.
#' @export
lca_scale <- function(m, input, output, by) {
  if (!is.numeric(by) || length(by) != 1 || by <= 0) stop("`by` must be a positive number", call. = FALSE)
  i <- which(m$ex$input == input & m$ex$output == output)
  if (!length(i)) stop("no exchange ", input, " -> ", output, call. = FALSE)
  m$ex$amount[i] <- m$ex$amount[i] * by
  if (!is.null(m$ex$utype)) {
    ln <- i[!is.na(m$ex$utype[i]) & m$ex$utype[i] == 2]
    m$ex$loc[ln] <- m$ex$loc[ln] + log(by)
    ut <- i[!is.na(m$ex$utype[i]) & m$ex$utype[i] %in% c(4, 5)]
    if (length(ut)) { m$ex$minimum[ut] <- m$ex$minimum[ut] * by; m$ex$maximum[ut] <- m$ex$maximum[ut] * by }
    tr <- i[!is.na(m$ex$utype[i]) & m$ex$utype[i] == 5]
    m$ex$loc[tr] <- m$ex$loc[tr] * by
  }
  m
}

#' Run a design on an LCA model
#'
#' For every run, `fun` turns the factor levels into a modified model (and
#' optionally a different demand); the score of each run is then computed.
#' With `n` Monte Carlo iterations all runs use the same random draws (common
#' random numbers), so differences between runs reflect the factors, not the
#' sampling noise. Requires the same exchanges in every modified model.
#'
#' @param m An `lca_model`.
#' @param design An `lca_design`.
#' @param fun Function `function(m, levels)`; `levels` is a named list with the
#'   run's factor levels. Returns an `lca_model`, or a list with elements
#'   `model` and/or `demand`.
#' @param demand Default named demand vector.
#' @param method Impact method.
#' @param n Number of Monte Carlo iterations; `NULL` for the deterministic score.
#' @param seed Seed (used when `n` is given).
#' @return An object of class `lca_doe` (a list with `design`, `scores` and
#'   `method`): `scores` is a vector (deterministic) or a runs x `n` matrix.
#' @export
lca_doe_run <- function(m, design, fun, demand, method, n = NULL, seed = NULL) {
  fnames <- names(attr(design, "factors"))
  one <- function(r) {
    lv <- as.list(design[r, fnames, drop = FALSE]); lv <- lapply(lv, function(z) z[[1]])
    res <- fun(m, lv)
    if (inherits(res, "lca_model")) res <- list(model = res)
    list(model = res$model %||% m, demand = res$demand %||% demand)
  }
  sc <- vector("list", nrow(design))
  for (r in seq_len(nrow(design))) {
    s <- one(r)
    sc[[r]] <- if (is.null(n)) lca_calc(s$model, s$demand, method)$score
               else as.numeric(unclass(lca_mc(s$model, s$demand, method, n, seed = seed %||% 1))[, 1])
  }
  scores <- if (is.null(n)) unlist(sc) else do.call(rbind, sc)
  cov <- if (is.null(n)) NULL else cbind(method = method, lca_uncertainty_coverage(m, demand, method))
  structure(list(design = design, scores = scores, method = method, n = n, coverage = cov), class = "lca_doe")
}

.effect_terms <- function(coded, interactions) {
  nm <- colnames(coded)
  X <- coded
  if (interactions >= 2 && length(nm) > 1) {
    pr <- combn(nm, 2, simplify = FALSE)
    for (p in pr) { X <- cbind(X, coded[, p[1]] * coded[, p[2]]); colnames(X)[ncol(X)] <- paste(p, collapse = ":") }
  }
  key <- apply(X, 2, paste, collapse = ",")
  X[, !duplicated(key), drop = FALSE]
}

#' Effects of the factors of a two-level design
#'
#' Main effects and (optionally) two-factor interactions, defined as the change
#' in the response when the factor goes from its first to its second level.
#'
#' Two modes, with different statistical meaning:
#' \describe{
#'  \item{`"model"`}{the responses come from a model, deterministic or with
#'    Monte Carlo (an `lca_doe` object). Effects are computed in every
#'    iteration; the table gives the mean effect, its central interval
#'    across iterations (the uncertainty of the inputs, not sampling error),
#'    and `prob_positive`, the share of iterations with a positive effect.
#'    **No p-values are given**: with simulated draws a p-value only reflects the
#'    number of iterations (Heijungs, 2020).}
#'  \item{`"experimental"`}{the responses `y` are measured (or computed from
#'    measured inventories) on real replicates. The effects are fitted by least
#'    squares and the table includes standard errors, confidence intervals
#'    and p-values. Needs replicates (residual degrees of freedom > 0).}
#' }
#' In fractional designs aliased effects share one column; see
#' [lca_doe_aliases()].
#'
#' @param x An `lca_doe` (from [lca_doe_run()]) or an `lca_design`.
#' @param y Numeric responses, one per run, when `x` is an `lca_design`.
#' @param mode `"auto"` (model for an `lca_doe`, experimental for a design with
#'   `y`), `"model"` or `"experimental"`.
#' @param interactions 1 for main effects only, 2 to add two-factor interactions.
#' @param level Confidence/interval level.
#' @return A data frame of class `lca_effects` ordered by absolute effect.
#' @export
lca_doe_effects <- function(x, y = NULL, mode = c("auto", "model", "experimental"),
                            interactions = 2, level = 0.95) {
  mode <- match.arg(mode)
  design <- if (inherits(x, "lca_doe")) x$design else x
  coded <- attr(design, "coded")
  if (is.null(coded)) stop("effects need a two-level design", call. = FALSE)
  resp <- if (inherits(x, "lca_doe")) x$scores else y
  if (is.null(resp)) stop("supply responses `y`", call. = FALSE)
  if (mode == "auto") mode <- if (inherits(x, "lca_doe")) "model" else "experimental"
  X <- .effect_terms(coded, interactions)
  N <- nrow(X); pr <- c((1 - level) / 2, 1 - (1 - level) / 2)
  if (mode == "model") {
    Y <- if (is.matrix(resp)) resp else matrix(resp, ncol = 1)
    if (nrow(Y) != N) stop("responses do not match the design", call. = FALSE)
    E <- (2 / N) * crossprod(X, Y)
    gm <- mean(Y)
    out <- data.frame(term = colnames(X), effect = rowMeans(E),
                      lower = if (ncol(Y) > 1) apply(E, 1, stats::quantile, pr[1], names = FALSE) else NA_real_,
                      upper = if (ncol(Y) > 1) apply(E, 1, stats::quantile, pr[2], names = FALSE) else NA_real_,
                      prob_positive = if (ncol(Y) > 1) rowMeans(E > 0) else NA_real_,
                      relative = rowMeans(E) / gm, row.names = NULL, stringsAsFactors = FALSE)
  } else {
    if (is.matrix(resp)) stop("experimental mode needs one response per run", call. = FALSE)
    if (length(resp) != N) stop("responses do not match the design", call. = FALSE)
    df <- N - ncol(X) - 1
    if (df <= 0) stop("no residual degrees of freedom: use replicates or `interactions = 1`", call. = FALSE)
    fit <- stats::lm(resp ~ X)
    co <- summary(fit)$coefficients[-1, , drop = FALSE]
    tq <- stats::qt(1 - (1 - level) / 2, df)
    out <- data.frame(term = colnames(X), effect = 2 * co[, 1], se = 2 * co[, 2],
                      lower = 2 * (co[, 1] - tq * co[, 2]), upper = 2 * (co[, 1] + tq * co[, 2]),
                      p_value = co[, 4], relative = 2 * co[, 1] / mean(resp),
                      row.names = NULL, stringsAsFactors = FALSE)
  }
  out <- out[order(-abs(out$effect)), ]; rownames(out) <- NULL
  cov <- if (inherits(x, "lca_doe")) x$coverage else NULL
  if (!is.null(cov)) .coverage_warn(cov)
  structure(out, mode = mode, level = level, coverage = cov, class = c("lca_effects", "data.frame"))
}

#' @export
print.lca_effects <- function(x, ...) {
  cat("<lca_effects> mode:", attr(x, "mode"), "\n")
  if (attr(x, "mode") == "model") cat("(no p-values: effects over the Monte Carlo uncertainty of the inputs)\n")
  cv <- attr(x, "coverage")
  if (!is.null(cv)) cat(sprintf("uncertainty coverage of the baseline model: %.1f%% of elementary-flow contributions\n", 100 * cv$share_bio_contribution))
  print.data.frame(x, digits = 4, row.names = FALSE)
  invisible(x)
}

#' Plot the effects of a design
#'
#' Bars with the value of each effect; error bars show the interval when
#' available.
#'
#' @param x An `lca_effects` object.
#' @param top Number of effects shown.
#' @return A `ggplot` object.
#' @export
plot_effects <- function(x, top = 15) {
  .need_gg()
  d <- as.data.frame(unclass(x)); d <- utils::head(d, top)
  d$term <- factor(d$term, levels = rev(d$term))
  f <- function(v) trimws(formatC(v, digits = 3, format = "g"))
  has_ci <- !all(is.na(d$lower))
  d$label <- if (has_ci) sprintf("%s [%s, %s]", f(d$effect), f(d$lower), f(d$upper)) else f(d$effect)
  hi <- ifelse(is.na(d$upper), d$effect, pmax(d$effect, d$upper))
  lo <- ifelse(is.na(d$lower), d$effect, pmin(d$effect, d$lower))
  d$pos <- ifelse(d$effect >= 0, hi, lo)
  d$hj <- ifelse(d$effect >= 0, -0.05, 1.05)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$effect, y = .data$term)) +
    ggplot2::geom_col(fill = "#2c7fb8") +
    ggplot2::geom_vline(xintercept = 0)
  if (has_ci)
    p <- p + ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$lower, xmax = .data$upper), width = 0.25, orientation = "y")
  p + ggplot2::geom_text(ggplot2::aes(x = .data$pos, label = .data$label, hjust = .data$hj), size = 3.2) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.15, 0.4))) +
    ggplot2::labs(x = "Effect (second level minus first level)", y = NULL,
                  title = "Effects of the design",
                  subtitle = if (attr(x, "mode") == "model") "Model mode: interval = uncertainty of the inputs; no p-values"
                             else "Experimental mode: confidence intervals") +
    ggplot2::theme_minimal()
}
