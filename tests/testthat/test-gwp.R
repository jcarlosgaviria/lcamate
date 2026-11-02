ext <- function(...) system.file("extdata", ..., package = "lcamate")
mini_keys <- function(nm, cat = "Elementary flows/emission/air", dir = "out") paste(nm, cat, dir, sep = " | ")
mk <- function(flows) {
  ex <- data.frame(type = "biosphere", input = flows, output = "p", amount = 1)
  lca_model(ex, data.frame(method = "x", flow = flows[1], cf = 1))
}

test_that("AR5 reproduces the GWP100 of read_uslci", {
  m <- read_uslci(ext("uslci_mini"))
  g <- lca_method_gwp(m, "AR5")
  expect_equal(unname(g$C$GWP100_AR5), unname(m$C$GWP100))
  expect_equal(g$method_report$category, "GWP100_AR5")
  g2 <- lca_method_gwp(read_uslci(ext("uslci_unc")), "AR5")
  expect_true(all(g2$C$GWP100_AR5 == 0))
})

test_that("AR6 values, methane variants and CO2 uptake", {
  fl <- c(mini_keys("Carbon dioxide"), mini_keys("Methane"), mini_keys("Methane, fossil"),
          mini_keys("Methane, non-fossil"), mini_keys("Nitrous oxide"), mini_keys("CFC-11"),
          mini_keys("Carbon dioxide", "Elementary flows/resource/air", "in"),
          mini_keys("Carbon dioxide", "Elementary flows/emission/water", "out"),
          mini_keys("Sulfur hexafluoride"), mini_keys("Toluene"))
  m <- mk(fl)
  a6 <- lca_method_gwp(m, "AR6")$C$GWP100_AR6
  expect_equal(unname(a6), c(1, 29.8, 29.8, 27.0, 273, 6230, -1, 0, 0, 0))   # SF6 not in AR6 table
  expect_equal(unname(lca_method_gwp(m, "AR6", ch4 = "nonfossil")$C$GWP100_AR6[2]), 27.0)
  expect_equal(unname(lca_method_gwp(m, "AR6", ch4 = "sm7")$C$GWP100_AR6[2]), 27.9)
  a5 <- lca_method_gwp(m, "AR5")$C$GWP100_AR5
  expect_equal(unname(a5), c(1, 28, 28, 28, 265, 4660, -1, 0, 23500, 0))
})

test_that("names without compartment (CSV) are emissions; extras are added and override", {
  m <- mk(c("CO2", "CH4", "NOx", "HFC-134a"))
  g <- lca_method_gwp(m, "AR6", extra = c(NOx = 5, "hfc-134A" = 1000), name = "mine")$C$mine
  expect_equal(unname(g), c(1, 29.8, 5, 1000))
  expect_equal(unname(lca_method_gwp(m, "AR5")$C$GWP100_AR5), c(1, 28, 0, 1300))
  expect_error(lca_method_gwp(m, extra = c(1, 2)), "named numeric")
  expect_error(lca_method_gwp(list()), "lca_model")
})

test_that("score of the toy model with AR5 follows hand calculation", {
  ex <- read.csv(ext("toy_exchanges.csv")); cf <- read.csv(ext("toy_cf.csv"))
  m <- lca_method_gwp(lca_model(ex, cf), "AR5")
  r <- lca_calc(m, c(product = 1), "GWP100_AR5")
  ref <- lca_calc(m, c(product = 1), "GWP100")
  # CH4 is 28 here and 29.8 in the toy CSV: the difference is exactly the CH4 inventory times 1.8
  ch4 <- r$inventory["CH4"]
  expect_equal(unname(ref$score - r$score), unname(1.8 * ch4), tolerance = 1e-12)
})
