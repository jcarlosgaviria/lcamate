toy_plain <- function() {
  ex <- read.csv(system.file("extdata", "toy_exchanges.csv", package = "lcamate"))
  ex <- ex[, c("type", "input", "output", "amount")]
  cf <- read.csv(system.file("extdata", "toy_cf.csv", package = "lcamate"))
  lca_model(ex, cf)
}
sc <- function(output, input = NA, r = 1, c = 1, t = 1, g = 1, te = 1)
  data.frame(output = output, input = input, reliability = r, completeness = c,
             temporal = t, geographical = g, technological = te, stringsAsFactors = FALSE)

test_that("scores attach to exchanges; specific rows override general ones", {
  m <- toy_plain()
  s <- rbind(sc("steel", r = 2), sc("steel", input = "CO2", r = 4))
  p <- lca_pedigree(m, s)
  expect_s3_class(p, "lca_pedigree")
  st <- p$scores[p$scores$output == "steel", ]
  expect_equal(st$reliability[st$input == "CO2"], 4)
  expect_true(all(st$reliability[st$input != "CO2"] == 2))
  expect_true(all(is.na(p$scores$reliability[p$scores$output != "steel"])))
  expect_output(print(p), "with all five scores")
  expect_equal(lca_pedigree(m, rbind(s, sc("steel", input = "nonexistent")))$n_unmatched, 1)
})

test_that("invalid scores and unknown processes are rejected", {
  m <- toy_plain()
  expect_error(lca_pedigree(m, sc("steel", r = 6)), "1 to 5")
  expect_error(lca_pedigree(m, sc("nope")), "unknown processes")
  expect_error(lca_pedigree(m, data.frame(output = "steel")), "needs columns")
})

test_that("scenario GSD matches an independent derivation (Python)", {
  m <- toy_plain()
  # all scores 1: only the basic uncertainty, ln(GSD) = ln(1.05)/2
  p1 <- lca_pedigree(m, sc("steel"))
  s1 <- lca_pedigree_scenario(m, p1)
  a <- s1$ex$pedigree_scenario
  expect_equal(sum(a), sum(m$ex$output == "steel" & m$ex$amount > 0))
  expect_equal(unique(s1$ex$scale[a]), 0.024395082084716024, tolerance = 1e-12)
  expect_equal(s1$ex$loc[a], log(m$ex$amount[a]))
  # scores (2,3,2,1,3)
  p2 <- lca_pedigree(m, sc("steel", r = 2, c = 3, t = 2, g = 1, te = 3))
  expect_equal(unique(lca_pedigree_scenario(m, p2)$ex$scale[a]), 0.10155825079820735, tolerance = 1e-12)
  # all 5 and basic 1.05
  p5 <- lca_pedigree(m, sc("steel", r = 5, c = 5, t = 5, g = 5, te = 5))
  expect_equal(unique(lca_pedigree_scenario(m, p5)$ex$scale[a]), 0.4620504794582288, tolerance = 1e-12)
  # basic 2.0 with (2,3,2,1,3)
  expect_equal(unique(lca_pedigree_scenario(m, p2, basic = 2)$ex$scale[a]), 0.3603223719876715, tolerance = 1e-12)
})

test_that("scenario is labelled and kept apart from documented uncertainty", {
  m <- toy_plain()
  p <- lca_pedigree(m, rbind(sc("steel"), sc("gas")))
  s <- lca_pedigree_scenario(m, p)
  expect_equal(s$scenario$type, "pedigree")
  expect_output(print(s), "SCENARIO")
  cv <- lca_uncertainty_coverage(s, c(product = 1), "GWP100")
  expect_equal(cv$share_bio_contribution, 0)          # nothing documented
  expect_gt(cv$share_bio_scenario, 0)
  expect_lte(cv$share_bio_scenario, 1)
  # warning for low documented coverage still applies
  x <- suppressWarnings(lca_mc(s, c(product = 1), "GWP100", 50, seed = 1))
  expect_warning(lca_mc_summary(x), "only 0.0%")
  expect_true("unc_scenario" %in% names(suppressWarnings(lca_mc_summary(x))))
  # the scenario produces a spread; the original does not
  expect_gt(sd(unclass(x)[, 1]), 0)
  expect_error(lca_mc(m, c(product = 1), "GWP100", 20, seed = 1), "no uncertainty columns")
})

test_that("documented distributions are kept unless overwrite = TRUE; edge cases counted", {
  ex <- read.csv(system.file("extdata", "toy_exchanges.csv", package = "lcamate"))
  cf <- read.csv(system.file("extdata", "toy_cf.csv", package = "lcamate"))
  m <- lca_model(ex, cf)                      # has documented lognormals
  p <- lca_pedigree(m, sc("steel"))
  k <- lca_pedigree_scenario(m, p)
  expect_equal(k$scenario$n_applied, 0)
  expect_gt(k$scenario$n_kept_documented, 0)
  expect_equal(k$ex$scale, m$ex$scale)
  o <- lca_pedigree_scenario(m, p, overwrite = TRUE)
  expect_gt(o$scenario$n_applied, 0)
  # unscored exchanges are counted, not guessed
  m2 <- toy_plain()
  s2 <- lca_pedigree_scenario(m2, lca_pedigree(m2, sc("steel")))
  expect_equal(s2$scenario$n_not_scored, sum(m2$ex$output != "steel"))
  # non-positive amounts are skipped
  m3 <- m2; m3$ex$amount[m3$ex$output == "steel"][1] <- -1
  s3 <- lca_pedigree_scenario(m3, lca_pedigree(m3, sc("steel")))
  expect_equal(s3$scenario$n_nonpositive, 1)
  # a score without a factor leaves the exchange fixed
  F <- matrix(1.05, 5, 5, dimnames = list(1:5, c("reliability","completeness","temporal","geographical","technological")))
  F[3, "temporal"] <- NA
  s4 <- lca_pedigree_scenario(m2, lca_pedigree(m2, sc("steel", t = 3)), factors = F)
  expect_equal(s4$scenario$n_applied, 0)
  expect_equal(s4$scenario$n_no_factor, sum(m2$ex$output == "steel"))
  expect_error(lca_pedigree_scenario(m2, p, basic = 0.9), ">= 1")
  expect_error(lca_pedigree_scenario(m2, p, factors = diag(3)), "5 x 5")
})

test_that("pedigree summary reports shares of the result by data quality", {
  m <- toy_plain()
  p <- lca_pedigree(m, rbind(sc("steel", r = 4, te = 5), sc("gas", r = 2)))
  s <- lca_pedigree_summary(m, p, c(product = 1), "GWP100")
  expect_equal(s$indicator, c(.ped_ind, "any_indicator"))
  expect_true(all(s$share_contribution_scored >= 0 & s$share_contribution_scored <= 1))
  # independent: contribution shares from lca_calc by process
  res <- lca_calc(m, c(product = 1), "GWP100")
  bio <- m$ex[m$ex$type == "biosphere", ]
  w <- abs(m$C$GWP100[bio$input] * bio$amount * res$supply[bio$output])
  expect_equal(s$share_contribution_scored[s$indicator == "reliability"],
               sum(w[bio$output %in% c("steel", "gas")]) / sum(w), tolerance = 1e-12)
  expect_equal(s$share_contribution_poor[s$indicator == "reliability"],
               sum(w[bio$output == "steel"]) / sum(w), tolerance = 1e-12)
  expect_equal(s$mean_score[s$indicator == "reliability"],
               (4 * sum(w[bio$output == "steel"]) + 2 * sum(w[bio$output == "gas"])) /
                 sum(w[bio$output %in% c("steel", "gas")]), tolerance = 1e-12)
  expect_error(lca_pedigree_summary(m, list(), c(product = 1), "GWP100"), "lca_pedigree")
})

test_that("report states the scenario", {
  m <- toy_plain()
  s <- lca_pedigree_scenario(m, lca_pedigree(m, sc("steel")))
  d <- suppressWarnings(.report_data(s, c(product = 1), "GWP100", 30, 1, NULL, NULL, NULL,
                                     "T", NULL, NULL, NULL, NULL, NULL))
  expect_true(any(grepl("^SCENARIO", d$limitations)))
})

test_that("report on a model without uncertainty skips Monte Carlo and says so", {
  m <- toy_plain()
  d <- .report_data(m, c(product = 1), "GWP100", 30, 1, NULL, NULL, NULL, "T", NULL, NULL, NULL, NULL, NULL)
  expect_false(d$mc$available)
  expect_null(d$mc$summary)
  expect_true(any(grepl("no uncertainty distributions", d$measured)))
  expect_equal(d$scores$score, lca_calc(m, c(product = 1), "GWP100")$score)
})

test_that("report text distinguishes documented and scenario uncertainty", {
  m <- toy_plain()
  s <- lca_pedigree_scenario(m, lca_pedigree(m, rbind(sc("steel"), sc("gas"))))
  d <- suppressWarnings(.report_data(s, c(product = 1), "GWP100", 30, 1, NULL, NULL, NULL,
                                     "T", NULL, NULL, NULL, NULL, NULL))
  expect_true(d$mc$scenario)
  expect_true(any(grepl("pedigree-derived distributions", d$measured)))
  expect_false(any(grepl("propagate only", d$measured)))
})

test_that("default factors agree with the variances of Weidema et al. (2013), Table 10.5", {
  v <- cbind(reliability   = c(0, 0.0006, 0.002, 0.008, 0.04),
             completeness  = c(0, 0.0001, 0.0006, 0.002, 0.008),
             temporal      = c(0, 0.0002, 0.002, 0.008, 0.04),
             geographical  = c(0, 2.5e-5, 1e-4, 6e-4, 0.002),
             technological = c(0, 6e-4, 0.008, 0.04, 0.12))
  F <- .ped_default_factors()[, colnames(v)]
  expect_false(anyNA(F))
  expect_lt(max(abs(F / exp(2 * sqrt(v)) - 1)), 0.006)
})
