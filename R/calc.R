#' Calculate an LCA result for a demand
#'
#' @param m An `lca_model`.
#' @param demand Named numeric vector: process names and amounts demanded.
#' @param method Name of the impact method (a name in `names(m$C)`).
#' @param mats Matrices from [lca_matrices()]; computed if missing.
#' @return An object of class `lca_result` with `supply`, `inventory`,
#'   `score` and `by_process` (contribution of each process to the score).
#' @export
lca_calc <- function(m, demand, method, mats = lca_matrices(m)) {
  if (!method %in% names(m$C))
    stop("unknown method: ", method, call. = FALSE)
  if (is.null(names(demand)) || !all(names(demand) %in% m$processes))
    stop("`demand` must be a named vector of known processes", call. = FALSE)
  f <- numeric(length(m$processes)); names(f) <- m$processes
  f[names(demand)] <- demand
  s <- as.numeric(solve(mats$A, f))
  g <- as.numeric(mats$B %*% s)
  C <- m$C[[method]]
  structure(list(supply = setNames(s, m$processes),
                 inventory = setNames(g, m$flows),
                 score = sum(C * g),
                 by_process = setNames(as.numeric(crossprod(mats$B, C)) * s, m$processes),
                 method = method, demand = demand),
            class = "lca_result")
}

#' Unit scores of every process
#'
#' Impact score of one unit of the reference product of every process, in a
#' single sparse solve (`A' x = B' C`).
#' @inheritParams lca_calc
#' @return Named numeric vector with one score per process.
#' @export
lca_scores <- function(m, method, mats = lca_matrices(m)) {
  if (!method %in% names(m$C))
    stop("unknown method: ", method, call. = FALSE)
  h <- as.numeric(crossprod(mats$B, m$C[[method]]))
  setNames(as.numeric(solve(t(mats$A), h)), m$processes)
}

#' @export
print.lca_result <- function(x, ...) {
  cat("<lca_result> method:", x$method, " score:", format(x$score, digits = 6), "\n")
  top <- sort(abs(x$by_process), decreasing = TRUE)
  top <- names(top)[seq_len(min(5, length(top)))]
  cat("largest contributions:\n")
  print(signif(x$by_process[top], 4))
  invisible(x)
}
