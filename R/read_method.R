# Internal: directory that holds an openLCA JSON-LD export (unzips if needed)
.method_root <- function(path) {
  if (!file.exists(path)) stop("`path` does not exist: ", path, call. = FALSE)
  if (dir.exists(path)) return(path)
  if (!grepl("\\.zip$", path, ignore.case = TRUE))
    stop("`path` must be a folder or a .zip file", call. = FALSE)
  d <- tempfile("lcamate-method-"); dir.create(d)
  utils::unzip(path, exdir = d)
  d
}

# Internal: find a subfolder called `name` anywhere below `root`
.find_dir <- function(root, name) {
  ds <- list.dirs(root, recursive = TRUE, full.names = TRUE)
  ds <- ds[basename(ds) == name]
  if (length(ds)) ds[1] else NA_character_
}

# Internal: conversion factors of units (name -> factor to the reference unit)
.method_unit_factors <- function(root) {
  uf <- utils::read.csv(system.file("extdata", "uslci_units.csv", package = "lcamate"),
                        stringsAsFactors = FALSE)
  UF <- stats::setNames(uf$factor, uf$unit)
  gd <- .find_dir(root, "unit_groups")
  if (!is.na(gd)) for (f in list.files(gd, pattern = "\\.json$", full.names = TRUE)) {
    d <- jsonlite::fromJSON(f, simplifyVector = FALSE)
    for (u in d$units) if (!is.null(u$name) && !is.null(u$conversionFactor)) UF[u$name] <- u$conversionFactor
  }
  UF
}

# Internal: read one impact category file
.read_impact_category <- function(f) {
  d <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  fx <- d$impactFactors
  num <- function(e) { v <- e$value; if (is.null(v) || !is.numeric(v)) NA_real_ else as.numeric(v) }
  chr <- function(g) vapply(fx, function(e) { v <- g(e); if (is.null(v)) NA_character_ else as.character(v) }, "")
  fac <- if (length(fx)) data.frame(
    flow_id = chr(function(e) e$flow[["@id"]]),
    flow = chr(function(e) e$flow$name),
    value = vapply(fx, num, 0),
    has_formula = vapply(fx, function(e) !is.null(e$formula) && nzchar(e$formula), TRUE),
    unit = chr(function(e) e$unit$name),
    prop = chr(function(e) e$flowProperty$name),
    stringsAsFactors = FALSE) else NULL
  list(id = d[["@id"]], name = d$name, ref_unit = d$refUnit %||% d$referenceUnitName %||% NA_character_,
       factors = fac)
}

#' Read an impact method in openLCA JSON-LD format
#'
#' Adds the impact categories of a method (for example TRACI or ReCiPe as
#' published in openLCA JSON-LD by the U.S. EPA) to a model whose elementary
#' flows carry identifiers, such as one read with [read_uslci()]. Factors are
#' linked **by flow identifier (UUID)**, not by name, so the flow list of the
#' method and of the database must be the same (for USLCI, the Federal LCA
#' Commons Elementary Flow List). The package does not distribute any impact
#' method: each user downloads the files and is responsible for respecting
#' their license.
#'
#' A factor is applied to every elementary flow of the model with the same
#' identifier, whatever its direction, to the amount expressed in the reference
#' unit of the flow's reference property. Factors that cannot be applied are
#' counted, not guessed: unit not known, flow property different from the
#' reference property of the model's flow, or a value given only as a formula.
#' Uncertainty of the factors is ignored.
#'
#' @param path Folder or `.zip` file with the openLCA JSON-LD export of the
#'   method (folders `lcia_categories/` and, optionally, `lcia_methods/` and
#'   `unit_groups/`).
#' @param m An `lca_model` with `m$flow_info` (identifiers of the flows).
#' @param method Name of the method (exact or a unique part of it) when the
#'   export has several; `NULL` uses the only method, or all categories if
#'   there is no `lcia_methods/` folder.
#' @param prefix Optional text put before the name of each category.
#' @return The model with one new entry in `m$C` per impact category and
#'   `m$method_report`, a data frame with, per category, the reference unit,
#'   `n_factors` in the method, `n_flows_matched` (flows of the model with a
#'   factor), `share_flows_matched` (of all flows of the model) and the counts
#'   of factors skipped. A low share means the method says nothing about most
#'   of the flows of the model; check it before interpreting a score.
#' @export
read_olca_method <- function(path, m, method = NULL, prefix = NULL) {
  if (!inherits(m, "lca_model")) stop("`m` must be an lca_model", call. = FALSE)
  if (is.null(m$flow_info) || !nrow(m$flow_info))
    stop("the model has no flow identifiers (`m$flow_info`); read it with read_uslci() ",
         "or give `flow_info` to lca_model()", call. = FALSE)
  root <- .method_root(path)
  cdir <- .find_dir(root, "lcia_categories")
  if (is.na(cdir)) stop("no `lcia_categories` folder found in `path`", call. = FALSE)
  cats <- lapply(list.files(cdir, pattern = "\\.json$", full.names = TRUE), .read_impact_category)
  names(cats) <- vapply(cats, function(x) x$id, "")
  mdir <- .find_dir(root, "lcia_methods")
  sel <- names(cats); n_missing <- 0L
  if (!is.na(mdir) && length(list.files(mdir, pattern = "\\.json$"))) {
    meth <- lapply(list.files(mdir, pattern = "\\.json$", full.names = TRUE),
                   function(f) jsonlite::fromJSON(f, simplifyVector = FALSE))
    mn <- vapply(meth, function(x) x$name %||% "", "")
    if (is.null(method)) {
      if (length(meth) > 1) stop("the export has several methods; give `method`. Available: ",
                                 paste(utils::head(mn, 10), collapse = "; "), call. = FALSE)
      k <- 1
    } else {
      k <- which(mn == method)
      if (!length(k)) k <- which(grepl(tolower(method), tolower(mn), fixed = TRUE))
      if (length(k) != 1) stop("`method` matches ", length(k), " methods. Available: ",
                               paste(utils::head(mn, 10), collapse = "; "), call. = FALSE)
    }
    ids <- vapply(meth[[k]]$impactCategories, function(x) x[["@id"]], "")
    n_missing <- sum(!ids %in% names(cats))
    sel <- ids[ids %in% names(cats)]
  } else if (!is.null(method)) {
    warning("`method` ignored: the export has no lcia_methods folder", call. = FALSE)
  }
  if (!length(sel)) stop("no impact categories found for the selected method", call. = FALSE)
  UF <- .method_unit_factors(root)
  fi <- m$flow_info
  rows <- list()
  for (id in sel) {
    ct <- cats[[id]]; fac <- ct$factors
    nm <- paste0(prefix, ct$name)
    if (is.null(fac) || !nrow(fac)) {
      rows[[length(rows) + 1]] <- data.frame(category = nm, ref_unit = ct$ref_unit, n_factors = 0L,
        n_flows_matched = 0L, share_flows_matched = 0, skipped_formula = 0L, skipped_unit = 0L,
        skipped_property = 0L, stringsAsFactors = FALSE)
      m$C[[nm]] <- stats::setNames(numeric(length(m$flows)), m$flows)
      next
    }
    fac <- fac[!duplicated(fac$flow_id), , drop = FALSE]   # first factor per flow
    g <- unname(UF[fac$unit])
    bad_u <- !is.na(fac$value) & (is.na(g) | g == 0)
    use <- !is.na(fac$value) & !is.na(g) & g != 0
    pos <- match(fi$id, fac$flow_id)
    hit <- !is.na(pos)
    prop_ok <- is.na(fi$ref_prop) | is.na(fac$prop[pos]) | fi$ref_prop == fac$prop[pos]
    ok <- hit & !is.na(use[pos]) & use[pos] & prop_ok
    v <- stats::setNames(numeric(length(m$flows)), m$flows)
    v[fi$flow[ok]] <- fac$value[pos[ok]] / g[pos[ok]]
    rows[[length(rows) + 1]] <- data.frame(category = nm, ref_unit = ct$ref_unit,
      n_factors = nrow(fac), n_flows_matched = sum(ok),
      share_flows_matched = sum(ok) / length(m$flows),
      skipped_formula = sum(fac$has_formula & is.na(fac$value)),
      skipped_unit = sum(bad_u),
      skipped_property = sum(hit & !prop_ok),
      stringsAsFactors = FALSE)
    m$C[[nm]] <- v
  }
  rep <- do.call(rbind, rows)
  m$method_report <- rbind(m$method_report, rep)
  if (all(rep$n_flows_matched == 0))
    warning("no flow of the model matched a factor of the method: check that both use the ",
            "same elementary flow list (identifiers)", call. = FALSE)
  if (n_missing) warning(n_missing, " impact categor", if (n_missing > 1) "ies" else "y",
                         " listed by the method have no file in `lcia_categories`", call. = FALSE)
  m
}
