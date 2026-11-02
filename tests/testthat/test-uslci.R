ext <- function(...) system.file("extdata", ..., package = "lcamate")

test_that("USLCI subgraph reproduces openLCA GWP100 (12 processes)", {
  procs <- read.csv(ext("uslci_subgraph_processes.csv"), stringsAsFactors = FALSE)$process
  m <- read_lca_csv(ext("uslci_subgraph_exchanges.csv"), ext("uslci_subgraph_cf.csv"), processes = procs)
  ref <- read.csv(ext("uslci_subgraph_openlca_gwp.csv"), stringsAsFactors = FALSE)
  expect_equal(nrow(ref), 12)
  mats <- lca_matrices(m)
  sc <- vapply(ref$pid, function(p) lca_calc(m, setNames(1, p), "GWP100", mats)$score, 0)
  expect_equal(unname(sc), ref$gwp_olca, tolerance = 1e-9)
  all_sc <- lca_scores(m, "GWP100", mats)
  expect_equal(unname(all_sc[ref$pid]), ref$gwp_olca, tolerance = 1e-9)
})

test_that("read_uslci matches an independent Python implementation on a mini database", {
  skip_if_not_installed("jsonlite")
  m <- read_uslci(ext("uslci_mini"))
  expect_s3_class(m, "lca_model")
  expect_equal(m$report$r1_reclassified, 2)
  expect_equal(m$report$r2_refused, 3)
  expect_equal(m$report$skipped_unit, 0)
  ref <- read.csv(ext("uslci_mini_expected_exchanges.csv"), stringsAsFactors = FALSE)
  ex <- m$ex[, c("type", "input", "output", "amount")]
  k1 <- paste(ex$type, ex$input, ex$output); k2 <- paste(ref$type, ref$input, ref$output)
  expect_setequal(k1, k2)
  expect_equal(ex$amount[match(k2, k1)], ref$amount, tolerance = 1e-10)
  comp <- read.csv(ext("uslci_mini_expected_completeness.csv"), stringsAsFactors = FALSE)
  i <- match(comp$pid, m$info$pid)
  expect_equal(m$info$n_tech[i], comp$n_tech)
  expect_equal(m$info$n_linked[i], comp$n_linked)
  expect_equal(m$info$completeness[i], comp$completeness)
})

test_that("read_uslci leaves match openLCA", {
  skip_if_not_installed("jsonlite")
  m <- read_uslci(ext("uslci_mini"))
  ref <- read.csv(ext("uslci_subgraph_openlca_gwp.csv"), stringsAsFactors = FALSE)
  leaves <- c("6e19103d-ed39-3b71-8c51-0c65b85002a4", "53fe40de-ea70-4506-9196-eeed6bfe9a5e")
  mats <- lca_matrices(m)
  for (p in leaves)
    expect_equal(lca_calc(m, setNames(1, p), "GWP100", mats)$score,
                 ref$gwp_olca[ref$pid == p], tolerance = 1e-9)
})

test_that("lca_model keeps processes without exchanges", {
  ex <- data.frame(type = "biosphere", input = "CO2", output = "a", amount = 1)
  cf <- data.frame(method = "m", flow = "CO2", cf = 1)
  m <- lca_model(ex, cf, processes = c("a", "b"))
  expect_equal(m$processes, c("a", "b"))
  expect_equal(lca_calc(m, c(b = 1), "m")$score, 0)
  expect_error(lca_model(ex, cf, processes = "b"))
})
