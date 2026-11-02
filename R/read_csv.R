#' Read an LCA model from CSV files
#'
#' @param exchanges Path to a CSV file with the columns described in
#'   [lca_model()].
#' @param cf Path to a CSV file with columns `method`, `flow`, `cf`.
#' @param processes Optional character vector of all process names.
#' @return An `lca_model`.
#' @examples
#' m <- read_lca_csv(system.file("extdata", "toy_exchanges.csv", package = "lcamate"),
#'                   system.file("extdata", "toy_cf.csv", package = "lcamate"))
#' lca_calc(m, c(product = 1), "GWP100")$score
#' @export
read_lca_csv <- function(exchanges, cf, processes = NULL) {
  ex <- utils::read.csv(exchanges, stringsAsFactors = FALSE)
  cfd <- utils::read.csv(cf, stringsAsFactors = FALSE)
  lca_model(ex, cfd, processes = processes)
}
