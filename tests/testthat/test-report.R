toy <- function() {
  ex <- read.csv(system.file("extdata", "toy_exchanges.csv", package = "lcamate"))
  cf <- read.csv(system.file("extdata", "toy_cf.csv", package = "lcamate"))
  lca_model(ex, cf)
}
dat <- function(...) .report_data(toy(), c(product = 1), "GWP100", n = 100, seed = 1,
  labels = NULL, compare = NULL, doe = NULL, title = "T", author = "A", goal = NULL,
  functional_unit = NULL, assumptions = NULL, limitations = NULL, ...)

test_that("report data has the expected content and matches direct calls", {
  m <- toy(); d <- dat()
  expect_equal(d$scores$score, lca_calc(m, c(product = 1), "GWP100")$score)
  expect_equal(d$mc$summary$mean,
               lca_mc_summary(lca_mc(m, c(product = 1), "GWP100", 100, seed = 1))$mean)
  expect_equal(d$model$n_processes, length(m$processes))
  expect_match(d$functional_unit, "product")
  expect_true(all(c("method", "n", "rel_se", "n_needed") %in% names(d$mc$error)))
  expect_true(any(grepl("p-values", d$limitations)))
  expect_true(any(grepl("not measurements", d$measured)))
})

test_that("compare and doe sections are included when supplied", {
  m <- toy()
  cmp <- lca_compare(m, c(product = 1), c(steel = 1), "GWP100", 80, seed = 2)
  des <- lca_doe_design(list(steel = c(0.8, 1.2), gas = c(0.8, 1.2)))
  fun <- function(mm, lv) lca_scale(lca_scale(mm, "steel", "product", lv[["steel"]]), "gas", "elec", lv[["gas"]])
  d <- .report_data(m, c(product = 1), "GWP100", 50, 1, NULL, cmp, NULL, "T", NULL, NULL, NULL, NULL, NULL)
  expect_equal(d$compare$summary$mean_diff, lca_compare_summary(cmp)$mean_diff)
  expect_true(d$compare$paired)
  expect_null(d$doe)
  ef <- lca_doe_effects(des, y = c(1, 2, 3, 5), mode = "experimental", interactions = 1)
  d2 <- suppressWarnings(.report_data(m, c(product = 1), "GWP100", 50, 1, NULL, NULL, ef, "T", NULL, NULL, NULL, NULL, NULL))
  expect_equal(d2$doe$mode, "experimental")
  expect_error(.report_data(m, c(product = 1), "GWP100", 50, 1, NULL, NULL, data.frame(), "T", NULL, NULL, NULL, NULL, NULL),
               "lca_doe_effects")
})

test_that("low coverage is reported as a limitation and warning", {
  d <- dat()
  if (!is.null(d$mc$coverage) && any(d$mc$coverage$share_bio_contribution < 0.5, na.rm = TRUE))
    expect_true(any(grepl("below 50%", d$limitations)))
  m <- toy(); m$ex$gsd <- NULL
  expect_error(.report_data(m, c(product = 1), "nope", 10, 1, NULL, NULL, NULL, "T", NULL, NULL, NULL, NULL, NULL), "unknown method")
  expect_error(.report_data(m, c(1), "GWP100", 10, 1, NULL, NULL, NULL, "T", NULL, NULL, NULL, NULL, NULL), "named")
})

test_that("use_lca_article writes a skeleton and refuses to overwrite", {
  f <- tempfile(fileext = ".Rmd")
  expect_equal(use_lca_article(f), f)
  expect_true(file.exists(f))
  expect_match(paste(readLines(f), collapse = "\n"), "Uncertainty analysis")
  expect_error(use_lca_article(f), "already exists")
  expect_silent(use_lca_article(f, overwrite = TRUE))
})

test_that("lca_report renders html and docx (needs rmarkdown and pandoc)", {
  skip_if_not_installed("rmarkdown"); skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available())
  m <- toy()
  h <- lca_report(m, c(product = 1), n = 100, seed = 1, file = tempfile(fileext = ".html"))
  expect_true(file.exists(h))
  expect_match(paste(readLines(h, warn = FALSE), collapse = " "), "Uncertainty")
  w <- lca_report(m, c(product = 1), n = 100, seed = 1, format = "docx", file = tempfile(fileext = ".docx"))
  expect_true(file.exists(w))
})
