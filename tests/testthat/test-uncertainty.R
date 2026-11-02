ext <- function(...) system.file("extdata", ..., package = "lcamate")
one_proc <- function(extra) {
  ex <- rbind(data.frame(type = "biosphere", input = "CO2", output = "a", amount = 1,
                         utype = 0, loc = NA, scale = NA, minimum = NA, maximum = NA), extra)
  lca_model(ex, data.frame(method = "m", flow = c("CO2", "CH4", "N2O", "X"), cf = 1))
}

test_that("uniform and triangular samplers match theory", {
  m <- one_proc(rbind(
    data.frame(type = "biosphere", input = "CH4", output = "a", amount = 1, utype = 4,
               loc = NA, scale = NA, minimum = 2, maximum = 6),
    data.frame(type = "biosphere", input = "N2O", output = "a", amount = 1, utype = 5,
               loc = 3, scale = NA, minimum = 1, maximum = 9),
    data.frame(type = "biosphere", input = "X", output = "a", amount = 1, utype = 2,
               loc = log(5), scale = log(1.5), minimum = NA, maximum = NA)))
  sm <- .make_sampler(m)
  expect_equal(sum(sm$mask), 3)
  set.seed(1)
  d <- t(vapply(seq_len(20000), function(i) sm$draw()[sm$mask], numeric(3)))
  expect_true(all(d[, 1] >= 2 & d[, 1] <= 6)); expect_equal(mean(d[, 1]), 4, tolerance = 0.02)
  expect_true(all(d[, 2] >= 1 & d[, 2] <= 9)); expect_equal(mean(d[, 2]), (1 + 3 + 9) / 3, tolerance = 0.02)
  expect_equal(median(d[, 3]), 5, tolerance = 0.02)
  # triangular mode: density is highest near 3
  expect_gt(sum(d[, 2] > 2.5 & d[, 2] < 3.5), sum(d[, 2] > 6 & d[, 2] < 7))
})

test_that("lca_scale handles uniform and triangular", {
  m <- one_proc(rbind(
    data.frame(type = "biosphere", input = "CH4", output = "a", amount = 1, utype = 4,
               loc = NA, scale = NA, minimum = 2, maximum = 6),
    data.frame(type = "biosphere", input = "N2O", output = "a", amount = 1, utype = 5,
               loc = 3, scale = NA, minimum = 1, maximum = 9)))
  m2 <- lca_scale(lca_scale(m, "CH4", "a", 2), "N2O", "a", 3)
  e <- m2$ex
  expect_equal(c(e$minimum[2], e$maximum[2]), c(4, 12))
  expect_equal(c(e$minimum[3], e$loc[3], e$maximum[3]), c(3, 9, 27))
})

test_that("coverage: full for the toy model, low when little is uncertain", {
  ex <- read.csv(ext("toy_exchanges.csv")); cf <- read.csv(ext("toy_cf.csv"))
  m <- lca_model(ex, cf)
  cv <- lca_uncertainty_coverage(m, c(product = 1), "GWP100")
  expect_equal(cv$share_bio_contribution, 1)
  m2 <- one_proc(data.frame(type = "biosphere", input = "CH4", output = "a", amount = 0.01,
                            utype = 4, loc = NA, scale = NA, minimum = 0.005, maximum = 0.015))
  cv2 <- lca_uncertainty_coverage(m2, c(a = 1), "m")
  expect_equal(cv2$share_bio_contribution, 0.01 / 1.01, tolerance = 1e-12)
  expect_equal(cv2$n_uncertain, 1)
  x <- lca_mc(m2, c(a = 1), "m", 50, seed = 1)
  expect_warning(s <- lca_mc_summary(x), "elementary-flow contributions")
  expect_equal(s$unc_coverage, 0.01 / 1.01, tolerance = 1e-12)
  expect_no_warning(lca_mc_summary(lca_mc(m, c(product = 1), "GWP100", 20, seed = 1)))
  cmp <- lca_compare(m2, c(a = 1), c(a = 2), "m", 30, seed = 1)
  expect_warning(lca_compare_summary(cmp), "elementary-flow contributions")
})

test_that("USLCI uncertainty import matches an independent derivation", {
  skip_if_not_installed("jsonlite")
  m <- read_uslci(ext("uslci_unc"))
  u <- m$report$uncertainty
  expect_equal(c(u$lognormal, u$triangular, u$uniform), c(4, 2, 1))
  # one triangular exchange of USLCI has its mode outside [minimum, maximum]
  expect_equal(u$unsupported, 1)
  expect_equal(u$share_with_spread, 7 / u$n_exchanges)
  exp <- read.csv(ext("uslci_unc_expected.csv"), stringsAsFactors = FALSE)
  got <- m$ex[, c("type", "input", "output", "amount", "utype", "loc", "scale", "minimum", "maximum")]
  k <- function(d) paste(d$type, d$input, d$output, d$utype)
  # rows with a varying distribution: one by one
  eu <- exp[exp$utype != 0, ]; gu <- got[got$utype != 0, ]
  expect_false(anyDuplicated(k(eu)) > 0)
  expect_setequal(k(gu), k(eu))
  gu <- gu[match(k(eu), k(gu)), ]
  for (cn in c("amount", "loc", "scale", "minimum", "maximum"))
    expect_equal(gu[[cn]], eu[[cn]], tolerance = 1e-10, info = cn)
  # rows without distribution: summed per key
  ef <- exp[exp$utype == 0, ]; gf <- got[got$utype == 0, ]
  se <- tapply(ef$amount, paste(ef$type, ef$input, ef$output), sum)
  sg <- tapply(gf$amount, paste(gf$type, gf$input, gf$output), sum)
  expect_setequal(names(sg), names(se))
  expect_equal(unname(sg[names(se)]), unname(se), tolerance = 1e-10)
  # the model samples and keeps the distribution parameters
  set.seed(1)
  sm <- .make_sampler(m)
  a <- sm$draw()
  expect_equal(sum(sm$mask), 7)
  expect_true(all(is.finite(a)))
})

test_that("a USLCI export with no distribution gives zero coverage-relevant exchanges", {
  skip_if_not_installed("jsonlite")
  m <- read_uslci(ext("uslci_mini"))
  expect_equal(m$report$uncertainty$share_with_spread, 0)
  expect_equal(sum(unlist(m$report$uncertainty[c("lognormal", "triangular", "uniform")])), 0)
})
