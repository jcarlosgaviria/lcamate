toy <- function() {
  ex <- read.csv(system.file("extdata", "toy_exchanges.csv", package = "lcamate"))
  cf <- read.csv(system.file("extdata", "toy_cf.csv", package = "lcamate"))
  lca_model(ex, cf)
}

test_that("model has the expected structure", {
  m <- toy()
  expect_s3_class(m, "lca_model")
  expect_equal(m$processes, c("elec", "gas", "steel", "coal", "diesel", "transport", "product"))
  mats <- lca_matrices(m)
  expect_s4_class(mats$A, "Matrix")
  expect_equal(as.numeric(Matrix::diag(mats$A)), rep(1, 7))
})

test_that("results match Brightway on every demand", {
  m <- toy()
  det <- read.csv(system.file("extdata", "toy_brightway_det.csv", package = "lcamate"))
  mats <- lca_matrices(m)
  for (d in unique(det$demand)) {
    nm <- sub("x[0-9]+$", "", d); amt <- as.numeric(sub("^.*x", "", d))
    r <- lca_calc(m, setNames(amt, nm), "GWP100", mats)
    ref <- function(q) det$value[det$demand == d & det$quantity == q]
    expect_equal(r$score, ref("GWP100"), tolerance = 1e-6, info = d)
    expect_equal(unname(r$supply), ref("supply")[order(det$index[det$demand == d & det$quantity == "supply"])], tolerance = 1e-6, info = d)
    expect_equal(sum(r$by_process), r$score, tolerance = 1e-12)
  }
})

test_that("lca_scores equals one calc per process", {
  m <- toy(); mats <- lca_matrices(m)
  s <- lca_scores(m, "GWP100", mats)
  one <- vapply(m$processes, function(p) lca_calc(m, setNames(1, p), "GWP100", mats)$score, 0)
  expect_equal(unname(s), unname(one), tolerance = 1e-12)
})

test_that("input validation", {
  m <- toy()
  expect_error(lca_calc(m, c(nope = 1), "GWP100"))
  expect_error(lca_calc(m, c(elec = 1), "nope"))
})

test_that("Monte Carlo is reproducible and error helper works", {
  m <- toy()
  a <- lca_mc(m, c(product = 1), "GWP100", 50, seed = 1)
  b <- lca_mc(m, c(product = 1), "GWP100", 50, seed = 1)
  expect_identical(a, b)
  e <- lca_mc_error(a[, 1])
  expect_true(e$ci[1] < e$mean && e$mean < e$ci[2])
  expect_gt(e$n_needed, 0)
})

test_that("Monte Carlo summary, sensitivity and contributions", {
  m <- toy()
  x <- lca_mc(m, c(product = 1), c("GWP100", "AP"), 200, seed = 2, keep_draws = TRUE)
  expect_s3_class(x, "lca_mc")
  s <- lca_mc_summary(x)
  expect_equal(s$method, c("GWP100", "AP"))
  expect_equal(s$mean, unname(colMeans(unclass(x))), tolerance = 1e-12)
  expect_true(all(s$q2.5 < s$median & s$median < s$q97.5))
  se <- lca_sens(x, "GWP100", top = 3)
  expect_equal(nrow(se), 3)
  expect_true(abs(se$rho[1]) >= abs(se$rho[3]))
  expect_error(lca_sens(lca_mc(m, c(product = 1), "GWP100", 5, seed = 1)))
  r <- lca_calc(m, c(product = 1), "GWP100")
  cb <- lca_contrib(r, n = 2)
  expect_equal(nrow(cb), 3)
  expect_equal(cb$process[3], "Rest")
  expect_equal(sum(cb$contribution), r$score, tolerance = 1e-12)
  expect_equal(sum(cb$share), 1, tolerance = 1e-12)
})

test_that("plots return ggplot objects", {
  skip_if_not_installed("ggplot2")
  m <- toy()
  r <- lca_calc(m, c(product = 1), "GWP100")
  x <- lca_mc(m, c(product = 1), "GWP100", 100, seed = 3, keep_draws = TRUE)
  expect_s3_class(plot_contributions(r), "ggplot")
  expect_s3_class(plot_mc(x), "ggplot")
  expect_s3_class(plot_sens(x), "ggplot")
  expect_no_error(ggplot2::ggplot_build(plot_mc(x)))
})

test_that("bar plots carry value labels", {
  skip_if_not_installed("ggplot2")
  m <- toy()
  r <- lca_calc(m, c(product = 1), "GWP100")
  p <- plot_contributions(r, 3)
  expect_true("label" %in% names(p$data))
  expect_match(p$data$label[1], "%\\)$")
  expect_false(any(grepl("\\.[0-9]*0 \\(", p$data$label)))
  x <- lca_mc(m, c(product = 1), "GWP100", 100, seed = 3, keep_draws = TRUE)
  expect_true("label" %in% names(plot_sens(x)$data))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("contribution labels replace process ids", {
  m <- toy()
  r <- lca_calc(m, c(product = 1), "GWP100")
  lab <- c(steel = "Steel, at plant", gas = "Natural gas")
  cb <- lca_contrib(r, n = 3, labels = lab)
  expect_true("Steel, at plant" %in% cb$process)
  expect_equal(sum(cb$contribution), r$score, tolerance = 1e-12)
  expect_false("steel" %in% cb$process)
  skip_if_not_installed("ggplot2")
  expect_true("Steel, at plant" %in% plot_contributions(r, 3, labels = lab)$data$process)
})
