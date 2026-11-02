ext <- function(...) system.file("extdata", ..., package = "lcamate")

test_that("flow identifiers are kept by read_uslci", {
  m <- read_uslci(ext("uslci_mini"))
  expect_true(all(c("flow", "id", "ref_prop") %in% names(m$flow_info)))
  expect_true(all(m$flow_info$flow %in% m$flows))
  expect_false(anyDuplicated(m$flow_info$id) > 0)
  expect_equal(nrow(m$flow_info), length(m$flows))
  m2 <- lca_scale(m, m$ex$input[m$ex$type == "biosphere"][1], m$ex$output[m$ex$type == "biosphere"][1], 2)
  expect_equal(m2$flow_info, m$flow_info)
  expect_error(lca_model(m$ex[, 1:4], data.frame(method = "a", flow = "x", cf = 1),
                         processes = m$processes, flow_info = data.frame(flow = "a")), "flow, id")
})

test_that("method factors are linked by identifier and match an independent derivation", {
  m <- read_uslci(ext("uslci_mini"))
  mm <- read_olca_method(ext("olca_method_mini"), m, method = "method A")
  exp_cf <- read.csv(ext("olca_method_mini_expected_cf.csv"), stringsAsFactors = FALSE)
  for (cat in unique(exp_cf$category)) {
    e <- exp_cf[exp_cf$category == cat, ]
    v <- mm$C[[cat]]
    expect_equal(sum(v != 0), nrow(e))
    expect_equal(unname(v[e$flow]), e$cf, tolerance = 1e-12)
  }
  exp_sc <- read.csv(ext("olca_method_mini_expected_scores.csv"), stringsAsFactors = FALSE)
  for (cat in unique(exp_sc$category)) {
    e <- exp_sc[exp_sc$category == cat, ]
    s <- lca_scores(mm, cat)
    expect_equal(unname(s[e$process]), e$score, tolerance = 1e-10)
  }
  expect_true("GWP100" %in% names(mm$C))      # the built-in method is kept
})

test_that("the report counts what could not be applied", {
  m <- read_uslci(ext("uslci_mini"))
  expect_warning(mm <- read_olca_method(ext("olca_method_mini"), m, method = "method A"), NA)
  r <- mm$method_report
  expect_equal(r$n_factors, c(3, 5, 1))
  expect_equal(r$n_flows_matched, c(3, 2, 0))
  expect_equal(r$skipped_formula, c(0, 1, 0))
  expect_equal(r$skipped_unit, c(0, 1, 0))
  expect_equal(r$skipped_property, c(0, 1, 0))
  expect_equal(r$share_flows_matched, r$n_flows_matched / length(m$flows))
})

test_that("method selection, prefix and errors", {
  m <- read_uslci(ext("uslci_mini"))
  expect_error(read_olca_method(ext("olca_method_mini"), m), "several methods")
  expect_error(read_olca_method(ext("olca_method_mini"), m, method = "zzz"), "matches 0")
  expect_error(read_olca_method(ext("olca_method_mini"), m, method = "Invented"), "matches 2")
  b <- read_olca_method(ext("olca_method_mini"), m, method = "method B", prefix = "B: ")
  expect_true("B: Test climate (invented)" %in% names(b$C))
  expect_equal(nrow(b$method_report), 1)
  toy <- lca_model(read.csv(ext("toy_exchanges.csv")), read.csv(ext("toy_cf.csv")))
  expect_error(read_olca_method(ext("olca_method_mini"), toy, method = "A"), "flow identifiers")
  expect_error(read_olca_method(tempfile(), m), "does not exist")
  expect_error(read_olca_method(ext("uslci_mini"), m), "lcia_categories")
})

test_that("a method with no matching flow gives a warning", {
  m <- read_uslci(ext("uslci_mini"))
  m$flow_info$id <- paste0("x", seq_len(nrow(m$flow_info)))
  expect_warning(read_olca_method(ext("olca_method_mini"), m, method = "method A"), "no flow of the model")
})
