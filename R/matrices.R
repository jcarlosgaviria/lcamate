#' Technosphere and biosphere matrices (sparse)
#'
#' @param m An `lca_model`.
#' @param amounts Optional numeric vector with one amount per row of
#'   `m$ex` (for example one Monte Carlo draw). Defaults to the nominal amounts.
#' @return A list with sparse matrices `A` (processes x processes, unit
#'   diagonal minus consumption) and `B` (flows x processes).
#' @export
lca_matrices <- function(m, amounts = m$ex$amount) {
  n <- length(m$processes); k <- length(m$flows)
  t <- m$ex$type == "technosphere"
  A <- Diagonal(n) -
    sparseMatrix(i = m$ex$row[t], j = m$ex$col[t], x = amounts[t], dims = c(n, n))
  B <- sparseMatrix(i = m$ex$row[!t], j = m$ex$col[!t], x = amounts[!t], dims = c(k, n))
  list(A = A, B = B)
}
