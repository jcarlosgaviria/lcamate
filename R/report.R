# Internal: format a number with 4 significant digits
.fmt <- function(x) ifelse(is.na(x), "NA", trimws(formatC(signif(x, 4), format = "g", digits = 4)))

# Internal: run an expression and keep its warnings instead of printing them
.collect_warnings <- function(expr) {
  w <- character()
  val <- withCallingHandlers(expr, warning = function(cnd) {
    w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning")
  })
  list(value = val, warnings = w)
}

# Internal: compute everything the report template shows (no rendering).
.report_data <- function(m, demand, methods, n, seed, labels, compare, doe,
                         title, author, goal, functional_unit, assumptions,
                         limitations, target_rel = 0.01) {
  if (!inherits(m, "lca_model")) stop("`m` must be an lca_model", call. = FALSE)
  if (is.null(names(demand))) stop("`demand` must be a named vector", call. = FALSE)
  if (!all(methods %in% names(m$C))) stop("unknown method(s): ",
    paste(setdiff(methods, names(m$C)), collapse = ", "), call. = FALSE)
  lab <- function(id) { l <- if (is.null(labels)) NA else unname(labels[id]); ifelse(is.na(l), id, l) }
  warn <- character()

  fu <- if (!is.null(functional_unit)) functional_unit else
    paste(sprintf("%s of %s", .fmt(unname(demand)), lab(names(demand))), collapse = "; ")

  model <- list(n_processes = length(m$processes), n_flows = length(m$flows),
                n_links = sum(m$ex$type == "technosphere"),
                n_exchanges = nrow(m$ex), methods = names(m$C), report = m$report,
                completeness = if (!is.null(m$info$completeness)) m$info$completeness else NULL)

  results <- lapply(methods, function(mm) lca_calc(m, demand, mm))
  names(results) <- methods
  scores <- data.frame(method = methods, score = vapply(results, function(r) r$score, 0),
                       row.names = NULL, stringsAsFactors = FALSE)
  contrib <- lapply(results, lca_contrib, n = 8, labels = labels)

  has_unc <- !is.null(m$ex$utype) && any(.has_spread(m$ex))
  if (has_unc) {
    mcr <- .collect_warnings(lca_mc(m, demand, methods, n = n, seed = seed, keep_draws = TRUE))
    mc <- mcr$value
    sm <- .collect_warnings(lca_mc_summary(mc))
    warn <- c(warn, mcr$warnings, sm$warnings)
    err <- do.call(rbind, lapply(methods, function(mm) {
      e <- lca_mc_error(unclass(mc)[, mm], target_rel = target_rel)
      data.frame(method = mm, n = e$n, rel_se = e$rel_se, n_needed = e$n_needed)
    }))
    coverage <- attr(mc, "coverage")
    sens <- tryCatch(lca_sens(mc, method = methods[1], top = 5), error = function(e) NULL)
  } else {
    mc <- NULL; sm <- list(value = NULL); err <- NULL; sens <- NULL
    coverage <- do.call(rbind, lapply(methods, function(mm)
      cbind(method = mm, lca_uncertainty_coverage(m, demand, mm))))
  }

  cmp <- NULL
  if (!is.null(compare)) {
    cs <- .collect_warnings(lca_compare_summary(compare))
    cmp <- list(summary = cs$value, paired = isTRUE(compare$paired), n = nrow(compare$diff))
    warn <- c(warn, cs$warnings)
  }
  eff <- NULL
  if (!is.null(doe)) {
    if (!inherits(doe, "lca_effects")) stop("`doe` must come from lca_doe_effects()", call. = FALSE)
    tb <- doe; class(tb) <- "data.frame"
    eff <- list(table = tb, mode = attr(doe, "mode"), level = attr(doe, "level"))
  }

  have_gg <- requireNamespace("ggplot2", quietly = TRUE)
  plots <- list()
  if (have_gg) {
    plots$contrib <- lapply(results, plot_contributions, n = 8, labels = labels)
    if (has_unc) plots$mc <- tryCatch(plot_mc(mc, method = methods[1]), error = function(e) NULL)
    if (!is.null(compare)) plots$compare <- tryCatch(plot_compare(compare), error = function(e) NULL)
    if (!is.null(doe)) plots$effects <- tryCatch(plot_effects(doe), error = function(e) NULL)
  }

  low_cov <- has_unc && !is.null(coverage) && any(coverage$share_bio_contribution < 0.5, na.rm = TRUE)
  scen <- !is.null(m$scenario)
  pct <- function(v) if (is.null(coverage)) "unknown share" else paste0(.fmt(100 * min(v, na.rm = TRUE)), "%")
  measured <- c(
    "Deterministic scores are computed exactly from the inventory and characterization factors supplied; they are not measurements made by the authors of this report.",
    if (!has_unc) "The data contain no uncertainty distributions: only deterministic scores are reported and no Monte Carlo was run."
    else if (scen) sprintf("Monte Carlo varies the documented distributions (%s of elementary-flow contributions) and, as an explicit scenario, pedigree-derived distributions (a further %s); the latter are assumptions, not data.",
                           pct(coverage$share_bio_contribution), pct(coverage$share_bio_scenario))
    else sprintf("Monte Carlo results propagate only the uncertainty distributions documented in the data (%s of elementary-flow contributions covered).", pct(coverage$share_bio_contribution)))
  limits <- c(
    if (low_cov) "Uncertainty coverage is below 50%: narrow intervals reflect missing information about the uncertainty of the data, not low uncertainty.",
    "Monte Carlo intervals describe the stated input distributions; no p-values or significance tests are derived from simulated draws.",
    "Results are valid only for the system boundary, impact methods and database version used.",
    limitations)

  if (!is.null(m$scenario)) {
    sc <- m$scenario
    limits <- c(sprintf("SCENARIO: %d exchanges vary according to a pedigree-derived uncertainty assumption (generic factors, not data on the exchanges); the share of the result with documented uncertainty is reported separately (%s).",
                        sc$n_applied, if (is.null(coverage)) "unknown" else paste0(.fmt(100 * min(coverage$share_bio_contribution, na.rm = TRUE)), "%")), limits)
  }
  list(title = title, author = author, date = format(Sys.Date()),
       goal = goal, functional_unit = fu, demand = demand,
       model = model, scores = scores, contrib = contrib, mc = list(available = has_unc, scenario = scen,
         summary = sm$value, error = err, coverage = coverage, sens = sens, n = n, seed = seed,
         target_rel = target_rel),
       compare = cmp, doe = eff, plots = plots, warnings = unique(warn),
       measured = measured, assumed = assumptions, limitations = limits,
       versions = list(lcamate = tryCatch(as.character(utils::packageVersion("lcamate")), error = function(e) "unknown"),
                       R = R.version.string),
       session = utils::capture.output(print(utils::sessionInfo())))
}

#' Generate a reproducible LCA report
#'
#' Computes scores, process contributions, Monte Carlo uncertainty (with error
#' and number of iterations needed) and, optionally, a paired comparison and a
#' design-of-experiments analysis, and renders them in a structured report that
#' follows the sections usually required to report an LCA (goal and scope,
#' inventory, results, uncertainty, data quality, limitations). The report
#' always separates what was *computed from the data* from what is *assumed*,
#' reports the uncertainty coverage, and gives no p-values for simulated draws.
#'
#' Requires the suggested packages `rmarkdown` and `knitr` and
#' [pandoc](https://pandoc.org). Plots need `ggplot2`; without it the tables
#' are still produced.
#'
#' @param m An `lca_model`.
#' @param demand Named numeric vector of demanded processes and amounts.
#' @param methods Impact methods to report (names in `m$C`); default all.
#' @param n Monte Carlo iterations.
#' @param seed Random seed (recorded in the report).
#' @param labels Optional named character vector of readable process names.
#' @param compare Optional object from [lca_compare()].
#' @param doe Optional object from [lca_doe_effects()].
#' @param title,author Report title and author.
#' @param goal Text with the goal of the study (a placeholder is shown if `NULL`).
#' @param functional_unit Text; by default built from `demand`.
#' @param assumptions Character vector of assumptions made by the analyst.
#' @param limitations Character vector of extra limitations to list.
#' @param format `"html"` or `"docx"`. PDF is not offered because it needs a
#'   LaTeX installation.
#' @param file Output path. By default a file in [tempdir()] (the path is
#'   reported in a message).
#' @param quiet Passed to `rmarkdown::render()`.
#' @return The path of the report, invisibly.
#' @examples
#' if (requireNamespace("rmarkdown", quietly = TRUE) &&
#'     requireNamespace("knitr", quietly = TRUE) && rmarkdown::pandoc_available()) {
#'   ex <- read.csv(system.file("extdata", "toy_exchanges.csv", package = "lcamate"))
#'   cf <- read.csv(system.file("extdata", "toy_cf.csv", package = "lcamate"))
#'   m <- lca_model(ex, cf)
#'   out <- lca_report(m, c(product = 1), n = 200, seed = 1,
#'                     file = tempfile(fileext = ".html"))
#' }
#' @export
lca_report <- function(m, demand, methods = names(m$C), n = 1000, seed = 1,
                       labels = NULL, compare = NULL, doe = NULL,
                       title = "Life cycle assessment report", author = NULL,
                       goal = NULL, functional_unit = NULL, assumptions = NULL,
                       limitations = NULL, format = c("html", "docx"),
                       file = NULL, quiet = TRUE) {
  format <- match.arg(format)
  for (p in c("rmarkdown", "knitr"))
    if (!requireNamespace(p, quietly = TRUE))
      stop("package '", p, "' is required; install it with install.packages('", p, "')", call. = FALSE)
  if (!rmarkdown::pandoc_available())
    stop("pandoc is required to render the report (https://pandoc.org)", call. = FALSE)
  d <- .report_data(m, demand, methods, n, seed, labels, compare, doe, title, author,
                    goal, functional_unit, assumptions, limitations)
  if (is.null(file)) file <- file.path(tempdir(), paste0("lca-report.", format))
  tpl <- system.file("report", "lca_report.Rmd", package = "lcamate")
  if (!nzchar(tpl)) stop("report template not found", call. = FALSE)
  work <- tempfile("lcamate-report-"); dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  file.copy(tpl, file.path(work, "lca_report.Rmd"))
  fmt <- if (format == "html")
    rmarkdown::html_document(toc = TRUE, toc_float = FALSE, self_contained = TRUE, theme = "default")
  else rmarkdown::word_document(toc = TRUE)
  out <- rmarkdown::render(file.path(work, "lca_report.Rmd"), output_format = fmt,
                           output_file = basename(file), output_dir = dirname(normalizePath(file, mustWork = FALSE)),
                           params = list(data = d), envir = new.env(parent = globalenv()), quiet = quiet)
  message("Report written to ", out)
  invisible(out)
}

#' Create a manuscript skeleton for an LCA article
#'
#' Writes an editable R Markdown file in English with the usual sections of a
#' research article (introduction, methods, results, discussion) and
#' reproducible chunks that call `lcamate`. It is a starting point to be
#' rewritten by the author, not a finished text.
#'
#' @param path File to create (default `"lca-article.Rmd"` in the working directory).
#' @param overwrite Overwrite an existing file?
#' @return The path, invisibly.
#' @examples
#' f <- use_lca_article(tempfile(fileext = ".Rmd"))
#' file.exists(f)
#' @export
use_lca_article <- function(path = "lca-article.Rmd", overwrite = FALSE) {
  tpl <- system.file("templates", "lca-article.Rmd", package = "lcamate")
  if (!nzchar(tpl)) stop("article template not found", call. = FALSE)
  if (file.exists(path) && !overwrite)
    stop("`", path, "` already exists; use overwrite = TRUE", call. = FALSE)
  ok <- file.copy(tpl, path, overwrite = overwrite)
  if (!ok) stop("could not write `", path, "`", call. = FALSE)
  invisible(path)
}
