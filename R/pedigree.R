.ped_ind <- c("reliability", "completeness", "temporal", "geographical", "technological")

# Default uncertainty factors U_i of the pedigree matrix (rows = score 1..5).
# Values follow ecoinvent (Frischknecht et al. 2007). All 25 cells were checked against
# Table 10.5 of Weidema et al. (2013), which gives the variance s2 of the underlying
# normal distribution; U = exp(2 * sqrt(s2)) agrees with these rounded factors to
# within 0.6 % (largest difference: score 3 of reliability, completeness, temporal,
# 1.0936 against 1.10). The v3 report works with the variances; here the rounded
# factors are used, as in the earlier ecoinvent tables.
.ped_default_factors <- function() {
  f <- cbind(reliability   = c(1.00, 1.05, 1.10, 1.20, 1.50),
             completeness  = c(1.00, 1.02, 1.05, 1.10, 1.20),
             temporal      = c(1.00, 1.03, 1.10, 1.20, 1.50),
             geographical  = c(1.00, 1.01, 1.02, 1.05, 1.10),
             technological = c(1.00, 1.05, 1.20, 1.50, 2.00))
  rownames(f) <- 1:5
  f
}

#' Assign pedigree-matrix scores to the exchanges of a model
#'
#' The pedigree matrix describes the quality of a datum with five indicators
#' scored from 1 (best) to 5 (worst): reliability, completeness, temporal,
#' geographical and technological correlation (Weidema and Wesnaes 1996). The
#' scores are **judgements made by the analyst**; this function only attaches
#' them to the exchanges of a model. It never assigns a score by itself, and
#' exchanges without scores are counted as such.
#'
#' @param m An `lca_model`.
#' @param scores A data frame with column `output` (process id) and the five
#'   score columns `reliability`, `completeness`, `temporal`, `geographical`
#'   and `technological` (integers 1 to 5, `NA` allowed). If column `input` is
#'   present and not `NA`, the row applies only to that exchange of the
#'   process; otherwise it applies to all exchanges of the process. Specific
#'   rows override general ones.
#' @return An object of class `lca_pedigree`: a list with `scores` (one row per
#'   exchange of `m$ex`, with `type`, `input`, `output` and the five scores) and
#'   `n_unmatched`, the number of rows of `scores` that matched no exchange.
#' @references Weidema BP, Wesnaes MS (1996). Data quality management for life
#'   cycle inventories. J Clean Prod 4:167-174.
#' @export
lca_pedigree <- function(m, scores) {
  if (!inherits(m, "lca_model")) stop("`m` must be an lca_model", call. = FALSE)
  if (!is.data.frame(scores) || !"output" %in% names(scores) ||
      !all(.ped_ind %in% names(scores)))
    stop("`scores` needs columns: output, ", paste(.ped_ind, collapse = ", "), call. = FALSE)
  v <- as.matrix(scores[, .ped_ind, drop = FALSE])
  if (!is.numeric(v) || any(!is.na(v) & !(v %in% 1:5)))
    stop("scores must be integers from 1 to 5 (or NA)", call. = FALSE)
  out <- as.character(scores$output)
  if (!all(out %in% m$processes)) stop("`output` has unknown processes: ",
    paste(utils::head(setdiff(out, m$processes), 3), collapse = ", "), call. = FALSE)
  inp <- if ("input" %in% names(scores)) as.character(scores$input) else rep(NA_character_, nrow(scores))
  ex <- m$ex
  P <- matrix(NA_real_, nrow(ex), 5, dimnames = list(NULL, .ped_ind))
  unmatched <- 0L
  apply_rows <- function(rows) for (r in rows) {
    idx <- if (is.na(inp[r])) which(ex$output == out[r]) else which(ex$output == out[r] & ex$input == inp[r])
    if (!length(idx)) unmatched <<- unmatched + 1L
    for (i in .ped_ind) P[idx, i] <<- v[r, i]
  }
  apply_rows(which(is.na(inp)))
  apply_rows(which(!is.na(inp)))
  structure(list(scores = data.frame(type = ex$type, input = ex$input, output = ex$output,
                                     P, row.names = NULL, stringsAsFactors = FALSE),
                 n_unmatched = unmatched), class = "lca_pedigree")
}

#' @export
print.lca_pedigree <- function(x, ...) {
  s <- x$scores[, .ped_ind, drop = FALSE]
  complete <- stats::complete.cases(s)
  cat("<lca_pedigree>", nrow(s), "exchanges;", sum(complete), "with all five scores;",
      sum(rowSums(!is.na(s)) > 0 & !complete), "partly scored;",
      sum(rowSums(!is.na(s)) == 0), "without scores\n")
  if (x$n_unmatched) cat(x$n_unmatched, "row(s) of `scores` matched no exchange\n")
  invisible(x)
}

# Contribution of each biosphere exchange to |score| for a demand (as in coverage)
.bio_contrib <- function(m, demand, method) {
  res <- lca_calc(m, demand, method)
  ex <- m$ex; bio <- ex$type == "biosphere"
  v <- numeric(nrow(ex))
  v[bio] <- abs(m$C[[method]][ex$row[bio]] * ex$amount[bio] * res$supply[ex$col[bio]])
  v
}

#' Summarise the data quality described by a pedigree
#'
#' Reports, for a given demand and impact method, how much of the result rests on
#' exchanges that have pedigree scores and how much on poor-quality scores (4 or
#' 5). The basis is the share of the absolute direct contributions of the
#' elementary-flow exchanges, as in [lca_uncertainty_coverage()]. This is a
#' communication of data quality, not a measure of uncertainty.
#'
#' @param m An `lca_model`.
#' @param ped An `lca_pedigree` from [lca_pedigree()].
#' @inheritParams lca_calc
#' @return A data frame with one row per indicator plus `any_indicator`:
#'   `share_exchanges_scored`, `share_contribution_scored`, `mean_score`
#'   (contribution-weighted, among scored exchanges) and
#'   `share_contribution_poor` (share of the total contribution with a score of
#'   4 or 5). The last row uses all five scores: scored = all five present,
#'   poor = at least one indicator is 4 or 5.
#' @export
lca_pedigree_summary <- function(m, ped, demand, method) {
  if (!inherits(ped, "lca_pedigree")) stop("`ped` must come from lca_pedigree()", call. = FALSE)
  w <- .bio_contrib(m, demand, method)
  bio <- m$ex$type == "biosphere"
  S <- as.matrix(ped$scores[, .ped_ind])
  tot <- sum(w)
  frac <- function(sel) if (tot > 0) sum(w[sel]) / tot else NA_real_
  row <- function(name, scored, mean_score, poor)
    data.frame(indicator = name, share_exchanges_scored = mean(scored[bio]),
               share_contribution_scored = frac(scored), mean_score = mean_score,
               share_contribution_poor = frac(poor), stringsAsFactors = FALSE)
  rows <- lapply(.ped_ind, function(i) {
    sc <- !is.na(S[, i])
    ms <- if (sum(w[sc]) > 0) sum(w[sc] * S[sc, i]) / sum(w[sc]) else NA_real_
    row(i, sc, ms, sc & S[, i] >= 4)
  })
  comp <- stats::complete.cases(S)
  anypoor <- comp & apply(S, 1, function(z) any(z >= 4))
  rows[[6]] <- row("any_indicator", comp, NA_real_, anypoor)
  do.call(rbind, rows)
}

#' Pedigree-derived uncertainty scenario
#'
#' Converts pedigree scores into lognormal distributions and returns a copy of
#' the model in which those exchanges vary. **This is a scenario, not
#' documented uncertainty**: the geometric standard deviation comes from a
#' generic table of factors, not from data on the exchange. The model is labelled
#' (`m$scenario`) and the exchanges are flagged, so that
#' [lca_uncertainty_coverage()] keeps reporting the share of the result with
#' *documented* uncertainty separately (`share_bio_contribution`) from the share
#' added by the scenario (`share_bio_scenario`). Intervals from this model should
#' not be used for significance tests (Heijungs 2020).
#'
#' For an exchange with scores \eqn{i = 1..5} the squared geometric standard
#' deviation is \eqn{\exp\sqrt{(\ln U_b)^2 + \sum_i (\ln U_i)^2}}, with the
#' median set to the nominal amount (ecoinvent convention, Frischknecht et al.
#' 2007).
#'
#' @param m An `lca_model`.
#' @param ped An `lca_pedigree`.
#' @param factors Optional 5 x 5 numeric matrix (rows = score 1 to 5, columns =
#'   the five indicators, with those names) replacing the default table of
#'   ecoinvent factors. `NA` marks a score without a factor; exchanges that
#'   would use it are left fixed and counted.
#'   The default is: reliability 1.00, 1.05, 1.10, 1.20, 1.50; completeness 1.00,
#'   1.02, 1.05, 1.10, 1.20; temporal 1.00, 1.03, 1.10, 1.20, 1.50; geographical
#'   1.00, 1.01, 1.02, 1.05, 1.10; technological 1.00, 1.05, 1.20, 1.50, 2.00.
#'   Every cell agrees with the variances of Table 10.5 of Weidema et al. (2013)
#'   (as \eqn{\exp(2\sqrt{\sigma^2})}) to within 0.6 percent; the rounded factors
#'   are used, as in the earlier ecoinvent tables.
#' @param basic Basic uncertainty factor \eqn{U_b}: a single number or a named
#'   vector with names `technosphere` and `biosphere`. The default, 1.05, is a
#'   placeholder for both; ecoinvent's flow-specific basic factors are not
#'   reproduced.
#' @param overwrite Replace distributions already documented in the data? By
#'   default exchanges with a documented, varying distribution are kept.
#' @return An `lca_model` with `utype`, `loc` and `scale` set for the exchanges
#'   with all five scores and a positive amount, a logical column
#'   `pedigree_scenario` in `m$ex`, and `m$scenario` (counts of what was applied
#'   and skipped).
#' @references Frischknecht R et al. (2007). Overview and methodology.
#'   ecoinvent report No. 1. Weidema BP et al. (2013). Overview and methodology:
#'   data quality guideline for the ecoinvent database version 3. Heijungs R
#'   (2020). On the number of Monte Carlo runs in comparative probabilistic LCA.
#'   Int J Life Cycle Assess 25:394-402.
#' @export
lca_pedigree_scenario <- function(m, ped, factors = NULL, basic = c(technosphere = 1.05, biosphere = 1.05),
                                  overwrite = FALSE) {
  if (!inherits(ped, "lca_pedigree")) stop("`ped` must come from lca_pedigree()", call. = FALSE)
  F <- if (is.null(factors)) .ped_default_factors() else as.matrix(factors)
  if (!identical(dim(F), c(5L, 5L)) || !all(.ped_ind %in% colnames(F)))
    stop("`factors` must be a 5 x 5 matrix with columns: ", paste(.ped_ind, collapse = ", "), call. = FALSE)
  if (any(F[!is.na(F)] < 1)) stop("factors must be >= 1", call. = FALSE)
  if (length(basic) == 1) basic <- c(technosphere = basic, biosphere = basic)
  if (!all(c("technosphere", "biosphere") %in% names(basic)) || any(basic < 1))
    stop("`basic` must be >= 1, a number or named technosphere/biosphere", call. = FALSE)
  ex <- m$ex; n <- nrow(ex)
  S <- as.matrix(ped$scores[, .ped_ind])
  complete <- stats::complete.cases(S)
  lnU <- matrix(NA_real_, n, 5)
  for (j in seq_along(.ped_ind)) lnU[complete, j] <- log(F[S[complete, j], .ped_ind[j]])
  has_factor <- complete & !is.na(rowSums(lnU))
  positive <- ex$amount > 0
  documented <- .has_spread(ex)
  if (is.null(ex$utype)) ex$utype <- 0
  ex$utype[is.na(ex$utype)] <- 0
  for (nm in c("loc", "scale", "minimum", "maximum")) if (is.null(ex[[nm]])) ex[[nm]] <- NA_real_
  apply_to <- has_factor & positive & (overwrite | !documented)
  lnUb <- log(unname(basic[ex$type]))
  sigma <- sqrt(lnUb^2 + rowSums(lnU^2)) / 2   # ln(GSD)
  ex$utype[apply_to] <- 2
  ex$loc[apply_to] <- log(ex$amount[apply_to])
  ex$scale[apply_to] <- sigma[apply_to]
  ex$minimum[apply_to] <- NA_real_; ex$maximum[apply_to] <- NA_real_
  ex$pedigree_scenario <- apply_to
  m$ex <- ex
  m$scenario <- list(type = "pedigree", n_applied = sum(apply_to),
                     n_not_scored = sum(!complete),
                     n_no_factor = sum(complete & !has_factor),
                     n_nonpositive = sum(has_factor & !positive),
                     n_kept_documented = sum(has_factor & positive & documented & !overwrite),
                     basic = basic, factors = F, overwrite = overwrite)
  m
}
