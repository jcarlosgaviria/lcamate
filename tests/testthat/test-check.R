ext <- function(...) system.file("extdata", ..., package = "lcamate")

.cup <- function() {
  ex <- utils::read.csv(ext("cup_exchanges.csv"), stringsAsFactors = FALSE)
  cf <- utils::read.csv(ext("cup_cf.csv"), stringsAsFactors = FALSE)
  list(ex = ex, cf = cf)
}

.has <- function(chk, name) name %in% chk$findings$check
.n <- function(chk, name) chk$findings$n[chk$findings$check == name]

test_that("clean teaching data raise no error or warning", {
  d <- .cup(); m <- lca_model(d$ex, d$cf)
  chk <- lca_check(m)
  expect_s3_class(chk, "lca_check")
  expect_equal(chk$n_error, 0)
  expect_equal(chk$n_warning, 0)
  expect_true(length(chk$not_checked) >= 3)
  expect_output(print(chk), "Not checked")
})

test_that("each planted problem is found with the planted count", {
  d <- .cup(); ex <- d$ex
  add <- function(type, input, output, amount, utype = 0, loc = NA, scale = NA, minimum = NA, maximum = NA)
    data.frame(type = type, input = input, output = output, amount = amount, utype = utype,
               loc = loc, scale = scale, source = "planted", minimum = minimum, maximum = maximum,
               stringsAsFactors = FALSE)
  ex$minimum <- NA_real_; ex$maximum <- NA_real_
  ex <- rbind(ex,
    ex[1, ],                                                         # one repeated row
    add("biosphere", "Ethane", "washing", 0.002),                    # flow with no factor
    add("biosphere", "CO2", "Lid.", 0.5),                            # process 'Lid.' ~ 'lid'
    add("biosphere", "CO2", "serving_reusable", 0.1, 2, NA, NA),     # lognormal without parameters
    add("biosphere", "CH4", "serving_reusable", -0.2, 2, -1.6, 0.3), # lognormal, negative amount
    add("biosphere", "N2O", "serving_reusable", 0.3, 5, 0.2, NA, 0.4, 0.1), # triangular: min > max
    add("biosphere", "N2O", "plastic", 0.3, 7),                      # unknown utype
    add("biosphere", "N2O", "paperboard", 0, 0))                     # zero amount
  m <- lca_model(ex, d$cf)
  chk <- lca_check(m)
  expect_equal(.n(chk, "repeated_rows"), 1)
  expect_equal(.n(chk, "no_characterisation"), 1)
  expect_true(grepl("Ethane", chk$findings$examples[chk$findings$check == "no_characterisation"]))
  expect_equal(.n(chk, "process_names_same_after_normalising"), 1)
  expect_equal(.n(chk, "lognormal_parameters"), 1)
  expect_equal(.n(chk, "lognormal_negative_amount"), 1)
  expect_equal(.n(chk, "range_inverted"), 1)
  expect_equal(.n(chk, "unknown_utype"), 1)
  expect_equal(.n(chk, "zero_amounts"), 1)
  expect_gt(chk$n_error, 0)
  expect_equal(chk$n_error + chk$n_warning + chk$n_note, nrow(chk$findings))
})

test_that("empty processes are reported (and weighed with a demand); aggregators are notes", {
  d <- .cup(); ex <- d$ex
  # 'emptyp' is bought by the disposable serving but has no data of its own
  ex <- rbind(ex, data.frame(type = "technosphere", input = "emptyp", output = "serving_disposable",
                             amount = 0.5, utype = 0, loc = NA, scale = NA, source = "planted"))
  m <- lca_model(ex, d$cf, processes = c(unique(d$ex$output), "emptyp"))
  a <- lca_check(m)
  expect_equal(.n(a, "empty_process"), 1)
  expect_equal(a$findings$severity[a$findings$check == "empty_process"], "warning")
  # five assemblies in the cup data have no elementary flow of their own
  expect_equal(.n(a, "aggregator_without_elementary_flows"), 5)
  expect_equal(a$findings$severity[a$findings$check == "aggregator_without_elementary_flows"], "note")
  b <- lca_check(m, c(serving_disposable = 1), "GWP100")
  expect_equal(b$findings$severity[b$findings$check == "empty_process"], "warning")
  expect_true(grepl("1 of", b$findings$message[b$findings$check == "empty_process"]))
  # a demand whose chain does not use 'emptyp' leaves it as a note
  cc <- lca_check(m, c(serving_reusable = 1), "GWP100")
  expect_equal(cc$findings$severity[cc$findings$check == "empty_process"], "note")
})

test_that("a method that characterises nothing is an error, and large uncharacterised flows are listed", {
  d <- .cup(); m <- lca_model(d$ex, data.frame(method = "none", flow = "not_a_flow", cf = 1))
  chk <- lca_check(m)
  expect_true(.has(chk, "no_characterisation"))
  expect_equal(chk$findings$severity[chk$findings$check == "no_characterisation"], "error")
  d <- .cup(); cf <- d$cf[d$cf$flow != "N2O", ]
  m2 <- lca_model(d$ex, cf)
  chk2 <- lca_check(m2, c(serving_disposable = 1), "GWP100")
  expect_true(.has(chk2, "largest_uncharacterised"))
  expect_true(grepl("N2O", chk2$findings$examples[chk2$findings$check == "largest_uncharacterised"]))
})

test_that("names one character apart are suggested, and the search can be capped", {
  ex <- data.frame(type = c("technosphere", "biosphere", "biosphere"),
                   input = c("steel_plate_a", "CO2", "CO2"),
                   output = c("product", "steel_plate_a", "steel_plate_b"),
                   amount = c(1, 2, 3), stringsAsFactors = FALSE)
  m <- lca_model(ex, data.frame(method = "GWP", flow = "CO2", cf = 1), processes = c("product", "steel_plate_a", "steel_plate_b"))
  chk <- lca_check(m)
  expect_true(.has(chk, "process_names_one_character_apart"))
  chk2 <- lca_check(m, max_names = 2)
  expect_false(.has(chk2, "process_names_one_character_apart"))
  expect_true(any(grepl("one-character", chk2$not_checked)))
})

test_that("inputs are validated and models stay unchanged", {
  d <- .cup(); m <- lca_model(d$ex, d$cf)
  m0 <- m
  lca_check(m)
  expect_identical(m, m0)
  expect_error(lca_check(list()), "lca_model")
  expect_error(lca_check(m, method = "nope"), "unknown method")
})

test_that("flows without a CAS number are reported when the model has flow_info", {
  fl <- utils::read.csv(ext("traci22_mini_flows.csv"), stringsAsFactors = FALSE,
                        fileEncoding = "UTF-8", colClasses = "character")
  ex <- utils::read.csv(ext("traci22_mini_exchanges.csv"), stringsAsFactors = FALSE,
                        fileEncoding = "UTF-8")
  m <- lca_model(ex, data.frame(method = "d", flow = fl$flow[1], cf = 0), processes = "P", flow_info = fl)
  chk <- lca_check(m)
  expect_equal(.n(chk, "flows_without_cas"), 4)   # unknown substance, generic methane, sulfur oxides and PM2.5 ('n.a.')
  expect_true(grepl("Methane", chk$findings$examples[chk$findings$check == "flows_without_cas"]))
  d <- .cup(); chk2 <- lca_check(lca_model(d$ex, d$cf))
  expect_false(.has(chk2, "flows_without_cas"))
})

test_that("repeated rows that all carry a distribution are a note, not a warning", {
  d <- .cup(); ex <- d$ex
  expect_equal(ex$utype[3], 2)
  ex <- rbind(ex, ex[3, ])                                  # the lognormal row, twice
  chk <- lca_check(lca_model(ex, d$cf))
  expect_equal(.n(chk, "repeated_uncertain_rows"), 1)
  expect_false(.has(chk, "repeated_rows"))
  expect_equal(chk$findings$severity[chk$findings$check == "repeated_uncertain_rows"], "note")
})

test_that("names that differ in a meaningful symbol are not treated as the same", {
  expect_false(identical(.chk_norm("Particulate matter, \u2264 10um"), .chk_norm("Particulate matter, > 10um")))
  expect_identical(.chk_norm("Steel,  PLATE."), .chk_norm("steel plate"))
})

# ---- solvability and units -------------------------------------------------
.two <- function(a, b, ...) {
  data.frame(type = c("technosphere", "technosphere", "biosphere"),
             input = c("P2", "P1", "CO2"), output = c("P1", "P2", "P1"),
             amount = c(a, b, 1), stringsAsFactors = FALSE, ...)
}
.cf2 <- data.frame(method = "GWP", flow = "CO2", cf = 1, stringsAsFactors = FALSE)

test_that("teaching data have no solvability finding", {
  d <- .cup(); chk <- lca_check(lca_model(d$ex, d$cf))
  expect_false(any(c("self_consumption", "not_productive", "singular_system", "ill_conditioned") %in%
                     chk$findings$check))
})

test_that("a process consuming all of its own product is an error", {
  ex <- rbind(.two(0.2, 0.2), data.frame(type = "technosphere", input = "P1", output = "P1", amount = 1))
  chk <- lca_check(lca_model(ex, .cf2))
  expect_true(.has(chk, "self_consumption"))
  expect_true(chk$n_error >= 1)
})

test_that("a cycle that returns exactly what it uses is singular", {
  chk <- lca_check(lca_model(.two(1, 1), .cf2))
  expect_true(.has(chk, "singular_system"))         # eigenvalue exactly 1
  expect_false(.has(chk, "not_productive"))
  expect_output(print(chk), "singular_system")
})

test_that("an unproductive cycle (eigenvalue 2) is an error", {
  chk <- lca_check(lca_model(.two(2, 2), .cf2))
  expect_true(.has(chk, "not_productive"))
  expect_equal(chk$findings$severity[chk$findings$check == "not_productive"], "error")
})

test_that("negative amounts downgrade productivity to a warning", {
  chk <- lca_check(lca_model(.two(2, -2), .cf2))
  expect_true(.has(chk, "possibly_not_productive"))
  expect_false(.has(chk, "not_productive"))
})

test_that("a productive cycle is fine and a near-unit cycle is ill-conditioned", {
  expect_false(any(c("not_productive", "ill_conditioned") %in%
                     lca_check(lca_model(.two(0.5, 0.5), .cf2))$findings$check))
  chk <- lca_check(lca_model(.two(1 - 1e-10, 1 - 1e-10), .cf2))
  expect_true(.has(chk, "ill_conditioned"))
  expect_equal(chk$findings$severity[chk$findings$check == "ill_conditioned"], "warning")
})

test_that("an unsolvable system gives no result for a demand", {
  chk <- lca_check(lca_model(.two(2, 2), .cf2), demand = c(P1 = 1))
  expect_true(any(grepl("cannot be solved", chk$not_checked)))
})

test_that("unit consistency is checked only when a unit column exists", {
  d <- .cup()
  expect_true(any(grepl("no `unit` column", lca_check(lca_model(d$ex, d$cf))$not_checked)))
  ex <- d$ex; ex$unit <- "kg"
  chk <- lca_check(lca_model(ex, d$cf))
  expect_false(any(c("unit_mismatch", "unit_scale_mismatch", "unit_missing") %in% chk$findings$check))
  expect_false(any(grepl("no `unit` column", chk$not_checked)))
})

test_that("kg and g on the same flow is a warning; kg and MJ is an error; empty is a note", {
  d <- .cup(); ex <- d$ex; ex$unit <- "kg"
  bio <- which(ex$type == "biosphere")
  fl <- ex$input[bio][1]
  ex$unit[bio[ex$input[bio] == fl][1]] <- "g"
  chk <- lca_check(lca_model(ex, d$cf))
  expect_equal(.n(chk, "unit_scale_mismatch"), 1)
  expect_false(.has(chk, "unit_mismatch"))
  ex$unit[bio[ex$input[bio] == fl][1]] <- "MJ"
  chk <- lca_check(lca_model(ex, d$cf))
  expect_equal(.n(chk, "unit_mismatch"), 1)
  expect_equal(chk$findings$severity[chk$findings$check == "unit_mismatch"], "error")
  ex$unit[bio[ex$input[bio] == fl][1]] <- ""
  chk <- lca_check(lca_model(ex, d$cf))
  expect_equal(.n(chk, "unit_missing"), 1)
  expect_false(any(c("unit_mismatch", "unit_scale_mismatch") %in% chk$findings$check))
})

test_that("extreme but harmless scales are not reported as ill-conditioned", {
  chk <- lca_check(lca_model(.two(1e9, 1e-10), .cf2))      # product of the cycle is 0.1
  expect_false(.has(chk, "ill_conditioned"))
  expect_false(.has(chk, "not_productive"))
})

test_that("empty processes of a database model say whether their inputs were cut off", {
  d <- .cup(); ex <- d$ex
  ex <- rbind(ex, data.frame(type = "technosphere", input = "e1", output = "serving_disposable",
                             amount = 0.5, utype = 0, loc = NA, scale = NA, source = "planted"),
              data.frame(type = "technosphere", input = "e2", output = "serving_disposable",
                         amount = 0.5, utype = 0, loc = NA, scale = NA, source = "planted"))
  procs <- c(unique(d$ex$output), "e1", "e2")
  info <- data.frame(pid = procs, n_tech = 0L, n_linked = 0L, stringsAsFactors = FALSE)
  info$n_tech[info$pid == "e1"] <- 3L          # e1: three inputs in the source, none linked
  m <- lca_model(ex, d$cf, processes = procs, info = info)
  msg <- lca_check(m)$findings$message
  msg <- msg[lca_check(m)$findings$check == "empty_process"]
  expect_true(grepl("1 of them do have inputs", msg))
  expect_true(grepl("1 have no input at all", msg))
  # without that information the original wording stays
  m2 <- lca_model(ex, d$cf, processes = procs)
  msg2 <- lca_check(m2)$findings$message[lca_check(m2)$findings$check == "empty_process"]
  expect_false(grepl("cut-off", msg2))
})
