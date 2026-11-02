toy <- function() {
  ex <- read.csv(system.file("extdata", "toy_exchanges.csv", package = "lcamate"))
  cf <- read.csv(system.file("extdata", "toy_cf.csv", package = "lcamate"))
  lca_model(ex, cf)
}

test_that("paired A uses the same draws as lca_mc", {
  m <- toy()
  cmp <- lca_compare(m, c(product = 1), c(steel = 1), "GWP100", 60, seed = 5)
  mc <- lca_mc(m, c(product = 1), "GWP100", 60, seed = 5)
  expect_equal(unname(cmp$a[, 1]), unname(unclass(mc)[, 1]), tolerance = 1e-12)
  expect_equal(cmp$diff, cmp$a - cmp$b)
})

test_that("a process against itself: paired difference is exactly zero, unpaired is not", {
  m <- toy()
  p <- lca_compare(m, c(product = 1), c(product = 1), "GWP100", 50, seed = 1)
  u <- lca_compare(m, c(product = 1), c(product = 1), "GWP100", 50, seed = 1, paired = FALSE)
  expect_true(all(p$diff == 0))
  expect_gt(sd(u$diff[, 1]), 0)
  s <- lca_compare_summary(p)
  expect_true(is.na(s$dz)); expect_equal(s$prob_a_lower, 0)
})

test_that("double demand: A lower with certainty, ratio 0.5", {
  m <- toy()
  cmp <- lca_compare(m, c(product = 1), c(product = 2), c("GWP100", "AP"), 40, seed = 2)
  s <- lca_compare_summary(cmp)
  expect_equal(s$prob_a_lower, c(1, 1))
  expect_equal(s$median_ratio, c(0.5, 0.5), tolerance = 1e-10)
  expect_equal(unname(cmp$nominal["a", ]), unname(cmp$nominal["b", ] / 2), tolerance = 1e-12)
})

test_that("summary columns and plot", {
  m <- toy()
  cmp <- lca_compare(m, c(steel = 1), c(coal = 1), "GWP100", 80, seed = 3)
  s <- lca_compare_summary(cmp)
  expect_true(s$diff_lower < s$mean_diff && s$mean_diff < s$diff_upper)
  expect_true(s$prob_a_lower >= 0 && s$prob_a_lower <= 1)
  expect_output(print(cmp), "paired")
  skip_if_not_installed("ggplot2")
  expect_s3_class(plot_compare(cmp), "ggplot")
  expect_no_error(ggplot2::ggplot_build(plot_compare(cmp)))
})
