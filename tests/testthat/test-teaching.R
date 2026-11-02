cup <- function() {
  ex <- read.csv(system.file("extdata", "cup_exchanges.csv", package = "lcamate"))
  cf <- read.csv(system.file("extdata", "cup_cf.csv", package = "lcamate"))
  lca_model(ex, cf)
}

test_that("teaching data are labelled as invented and match an independent solve", {
  ex <- read.csv(system.file("extdata", "cup_exchanges.csv", package = "lcamate"))
  expect_true(all(ex$source == "illustrative (invented)"))
  m <- cup()
  expect_equal(length(m$processes), 10)
  expect_equal(lca_calc(m, c(serving_disposable = 1), "GWP100")$score, 0.03129444, tolerance = 1e-9)
  expect_equal(lca_calc(m, c(serving_reusable = 1), "GWP100")$score, 0.0307368, tolerance = 1e-9)
})

test_that("the reusable cup score falls with the number of uses (python values)", {
  m <- cup()
  f <- function(k) lca_calc(lca_scale(m, "cup_reusable", "serving_reusable", 30 / k),
                            c(serving_reusable = 1), "GWP100")$score
  expect_equal(f(5), 0.0591158, tolerance = 1e-9)
  expect_equal(f(10), 0.0420884, tolerance = 1e-9)
  expect_equal(f(50), 0.02846648, tolerance = 1e-9)
})

test_that("teaching workflow runs: mc, coverage, paired comparison, design, pedigree", {
  m <- cup()
  cv <- lca_uncertainty_coverage(m, c(serving_reusable = 1), "GWP100")
  expect_gt(cv$share_bio_contribution, 0.5)
  cmp <- lca_compare(m, c(serving_reusable = 1), c(serving_disposable = 1), "GWP100", 200, seed = 1)
  expect_s3_class(cmp, "lca_compare")
  des <- lca_doe_design(list(uses = c(10, 50), wash = c(0.5, 1.5)))
  fun <- function(mm, lv) lca_scale(lca_scale(mm, "cup_reusable", "serving_reusable", 30 / lv$uses),
                                    "electricity", "washing", lv$wash)
  run <- lca_doe_run(m, des, fun, c(serving_reusable = 1, serving_disposable = -1), "GWP100", n = 50, seed = 1)
  ef <- suppressWarnings(lca_doe_effects(run))
  expect_true(all(c("uses", "wash") %in% ef$term))
  plain <- m; plain$ex$utype <- NULL
  sc <- data.frame(output = "cup_reusable", reliability = 3, completeness = 3, temporal = 2,
                   geographical = 2, technological = 2)
  s <- lca_pedigree_scenario(plain, lca_pedigree(plain, sc))
  expect_gt(s$scenario$n_applied, 0)
})
