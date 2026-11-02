ext <- function(...) system.file("extdata", ..., package = "lcamate")

.mini_traci_model <- function() {
  fl <- utils::read.csv(ext("traci22_mini_flows.csv"), stringsAsFactors = FALSE,
                        fileEncoding = "UTF-8", colClasses = "character")
  ex <- utils::read.csv(ext("traci22_mini_exchanges.csv"), stringsAsFactors = FALSE,
                        fileEncoding = "UTF-8")
  lca_model(ex, data.frame(method = "dummy", flow = fl$flow[1], cf = 0),
            processes = "P", flow_info = fl)
}

test_that("CAS numbers are normalised", {
  expect_equal(.cas_norm(c("124-38-9", "0000124-38-9", "n.a.", "x", NA, "", "0")),
               c("124389", "124389", NA, NA, NA, NA, NA))
})

test_that("read_uslci keeps the CAS number of each flow", {
  m <- read_uslci(ext("uslci_mini"))
  expect_true("cas" %in% names(m$flow_info))
})

test_that("TRACI 2.2 factors match an independent derivation", {
  m <- .mini_traci_model()
  mm <- read_traci(ext("traci22_mini_substances.csv"), m)
  exp_cf <- utils::read.csv(ext("traci22_mini_expected_cf.csv"), stringsAsFactors = FALSE,
                            fileEncoding = "UTF-8")
  exp_sc <- utils::read.csv(ext("traci22_mini_expected_scores.csv"), stringsAsFactors = FALSE)
  for (cat in unique(exp_cf$category)) {
    e <- exp_cf[exp_cf$category == cat, ]
    v <- mm$C[[cat]]
    expect_equal(sum(v != 0), nrow(e), info = cat)
    expect_equal(unname(v[e$flow]), e$cf, tolerance = 1e-12, info = cat)
  }
  for (i in seq_len(nrow(exp_sc))) {
    s <- lca_scores(mm, exp_sc$category[i])
    expect_equal(unname(s["P"]), exp_sc$score[i], tolerance = 1e-10, info = exp_sc$category[i])
  }
  expect_true(all(c("Acidification", "Global warming", "Freshwater ecotoxicity",
                    "Human health - cancer", "Human health - non-cancer",
                    "Human health - particulate matter", "Ozone depletion",
                    "Smog formation") %in% names(mm$C)))
})

test_that("the report counts matches by CAS and by name, and states the limits", {
  m <- .mini_traci_model()
  mm <- read_traci(ext("traci22_mini_substances.csv"), m)
  r <- mm$traci_report
  expect_equal(r$n_flows, 16)
  expect_equal(r$matched_by_name, 5)       # nitrogen oxides, sulfur oxides, PM2.5, mercury(II), and methane without CAS
  expect_equal(r$matched_by_cas, 10)       # includes the CO2 resource input, which has a CAS
  expect_equal(r$flows_unmatched, 1)       # 'Unknown substance': no CAS, no name match
  expect_true(any(grepl("Eutrophication", r$notes)))
  expect_true(all(c("category", "n_flows_matched") %in% names(mm$method_report)))
  expect_equal(nrow(mm$method_report), 8)
})

test_that("resources and inputs get no factor and unknown compartments are counted", {
  m <- .mini_traci_model()
  mm <- read_traci(ext("traci22_mini_substances.csv"), m)
  co2_in <- grep("resource/air \\| in$", m$flows, value = TRUE)
  expect_length(co2_in, 1)
  for (cat in names(mm$C)[names(mm$C) != "dummy"]) expect_equal(unname(mm$C[[cat]][co2_in]), 0)
  expect_equal(mm$traci_report$flows_without_compartment, 1)   # only the resource input
})

test_that("flags restrict the compartment-dependent factors", {
  m <- .mini_traci_model()
  a <- read_traci(ext("traci22_mini_substances.csv"), m)
  b <- read_traci(ext("traci22_mini_substances.csv"), m, flags = "Recommended")
  expect_true(sum(b$C[["Freshwater ecotoxicity"]] != 0) <= sum(a$C[["Freshwater ecotoxicity"]] != 0))
  expect_equal(b$C[["Global warming"]], a$C[["Global warming"]])
})

test_that("aliases, compartment overrides and prefix work", {
  m <- .mini_traci_model()
  x <- read_traci(ext("traci22_mini_substances.csv"), m, prefix = "T22: ")
  expect_true("T22: Global warming" %in% names(x$C))
  # an alias for an otherwise unmatched flow
  y <- read_traci(ext("traci22_mini_substances.csv"), m,
                  aliases = c("unknown substance" = "PM2.5"))
  k <- "Unknown substance | Elementary flows/emission/air | out"
  expect_true(k %in% m$flows)
  expect_equal(unname(y$C[["Human health - particulate matter"]][k]), 1)
  # send urban formaldehyde to the agricultural-soil compartment
  z <- read_traci(ext("traci22_mini_substances.csv"), m,
                  compartments = data.frame(pattern = "troposphere/urban", compartment = "agr"))
  kf <- "Formaldehyde | Elementary flows/emission/air/troposphere/urban | out"
  expect_true(kf %in% m$flows)
  expect_gt(unname(x$C[["T22: Smog formation"]][kf]), 0)       # urban air: air factor applies
  expect_equal(unname(z$C[["Smog formation"]][kf]), 0)    # moved to soil: no air factor
  expect_error(read_traci(ext("traci22_mini_substances.csv"), m,
                          compartments = data.frame(pattern = "x", compartment = "moon")),
               "unknown compartment")
})

test_that("inputs are checked", {
  m <- .mini_traci_model()
  expect_error(read_traci("no_such_file.csv", m), "not found")
  expect_error(read_traci(ext("traci22_mini_substances.csv"), list()), "lca_model")
  m2 <- m; m2$flow_info$cas <- NULL
  expect_error(read_traci(ext("traci22_mini_substances.csv"), m2), "cas")
})

test_that("metals named as the element are not matched, but counted, announced and noted by lca_check", {
  fl <- utils::read.csv(ext("traci22_mini_flows.csv"), stringsAsFactors = FALSE,
                        fileEncoding = "UTF-8", colClasses = "character")
  ex <- utils::read.csv(ext("traci22_mini_exchanges.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  em <- "Mercury | Elementary flows/emission/water/fresh water body | out"
  rs <- "Mercury | Elementary flows/resource/ground | in"
  fl <- rbind(fl, data.frame(flow = c(em, rs), id = c("flow-90", "flow-91"), ref_prop = "Mass", cas = "",
                             stringsAsFactors = FALSE))
  ex <- rbind(ex, data.frame(type = "biosphere", input = c(em, rs), output = "P", amount = c(0.5, 0.2)))
  m <- lca_model(ex, data.frame(method = "dummy", flow = fl$flow[1], cf = 0), processes = "P", flow_info = fl)
  expect_message(mm <- read_traci(ext("traci22_mini_substances.csv"), m), "no TRACI 2.2 factor")
  r <- mm$traci_report
  expect_equal(r$metals_without_factor, 1)          # the emission; the resource input is not counted
  expect_equal(r$metals_without_factor_names, "mercury")
  for (cat in grep("ecotox|Human health - (non-)?cancer", names(mm$C), value = TRUE, ignore.case = TRUE))
    expect_equal(unname(mm$C[[cat]][em]), 0)         # no factor was invented
  f <- lca_check(mm)$findings
  expect_equal(f$severity[f$check == "metals_without_factor"], "note")
  # the plain mini model has no such flow: no message, no finding
  expect_silent(m0 <- read_traci(ext("traci22_mini_substances.csv"), .mini_traci_model()))
  expect_equal(m0$traci_report$metals_without_factor, 0)
  expect_false("metals_without_factor" %in% lca_check(m0)$findings$check)
})

test_that("air without a compartment gets the mean of the urban and rural factors", {
  fl <- utils::read.csv(ext("traci22_mini_flows.csv"), stringsAsFactors = FALSE,
                        fileEncoding = "UTF-8", colClasses = "character")
  ex <- utils::read.csv(ext("traci22_mini_exchanges.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  gen <- "Formaldehyde | Elementary flows/emission/air | out"
  rur <- "Formaldehyde | Elementary flows/emission/air/troposphere/rural | out"
  fl <- rbind(fl, data.frame(flow = c(gen, rur), id = c("flow-92", "flow-93"), ref_prop = "Mass",
                             cas = "50-00-0", stringsAsFactors = FALSE))
  ex <- rbind(ex, data.frame(type = "biosphere", input = c(gen, rur), output = "P", amount = c(0.3, 0.1)))
  m <- lca_model(ex, data.frame(method = "dummy", flow = fl$flow[1], cf = 0), processes = "P", flow_info = fl)
  mm <- read_traci(ext("traci22_mini_substances.csv"), m)
  u <- 27.624987078132335; r <- 25.982363625231965      # formaldehyde ecotoxicity: urban and rural air
  expect_equal(unname(mm$C[["Freshwater ecotoxicity"]][gen]), (u + r) / 2, tolerance = 1e-12)
  expect_equal(unname(mm$C[["Freshwater ecotoxicity"]][rur]), r, tolerance = 1e-12)   # an explicit path is unchanged
  expect_equal(mm$traci_report$air_unspecified_mean, 3)      # classified as unspecified air: formaldehyde, and methane and 'Unknown substance' of the fixture
  # categories that depend on air alone treat it as before
  expect_equal(unname(mm$C[["Smog formation"]][gen]), 9.455846153846153, tolerance = 1e-12)
  # the user can still send it to rural air
  m2 <- read_traci(ext("traci22_mini_substances.csv"), m,
                   compartments = data.frame(pattern = "/emission/air$", compartment = "airC"))
  expect_equal(unname(m2$C[["Freshwater ecotoxicity"]][gen]), r, tolerance = 1e-12)
})

test_that("ground without a compartment gets the mean of the natural and agricultural soil factors", {
  fl <- utils::read.csv(ext("traci22_mini_flows.csv"), stringsAsFactors = FALSE,
                        fileEncoding = "UTF-8", colClasses = "character")
  ex <- utils::read.csv(ext("traci22_mini_exchanges.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  gen <- "Formaldehyde | Elementary flows/emission/ground | out"
  nat <- "Formaldehyde | Elementary flows/emission/ground/terrestrial/forest | out"
  agr <- "Formaldehyde | Elementary flows/emission/ground/human-dominated/agricultural | out"   # already in the fixture
  fl <- rbind(fl, data.frame(flow = c(gen, nat), id = c("flow-94", "flow-95"), ref_prop = "Mass",
                             cas = "50-00-0", stringsAsFactors = FALSE))
  ex <- rbind(ex, data.frame(type = "biosphere", input = c(gen, nat), output = "P", amount = c(0.3, 0.1)))
  m <- lca_model(ex, data.frame(method = "dummy", flow = fl$flow[1], cf = 0), processes = "P", flow_info = fl)
  mm <- read_traci(ext("traci22_mini_substances.csv"), m)
  n <- 9.961097097371929e-08; a <- 1.4784393045317133e-07   # formaldehyde non-cancer: natural and agricultural soil
  hh <- mm$C[["Human health - non-cancer"]]
  expect_equal(unname(hh[gen]), (n + a) / 2, tolerance = 1e-12)
  expect_equal(unname(hh[nat]), n, tolerance = 1e-12)       # explicit paths are unchanged
  expect_equal(unname(hh[agr]), a, tolerance = 1e-12)
  expect_equal(mm$traci_report$soil_unspecified_mean, 1)
  # the user can still send it to natural soil
  m2 <- read_traci(ext("traci22_mini_substances.csv"), m,
                   compartments = data.frame(pattern = "/emission/ground$", compartment = "nat"))
  expect_equal(unname(m2$C[["Human health - non-cancer"]][gen]), n, tolerance = 1e-12)
  expect_equal(m2$traci_report$soil_unspecified_mean, 0)
})
