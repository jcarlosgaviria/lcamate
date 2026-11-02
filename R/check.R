# Internal: add one finding to a list
.chk_add <- function(acc, check, severity, n, message, examples = character()) {
  acc[[length(acc) + 1]] <- data.frame(
    check = check, severity = severity, n = as.integer(n), message = message,
    examples = paste(utils::head(unique(as.character(examples)), 5), collapse = "; "),
    stringsAsFactors = FALSE)
  acc
}

# Internal: normalise a name for comparison (case, accents, punctuation, spaces)
.chk_norm <- function(x) {
  # symbols that change the meaning of a name are spelled out before punctuation is dropped
  x <- gsub("\u2264", " le ", x, fixed = TRUE); x <- gsub("\u2265", " ge ", x, fixed = TRUE)
  x <- gsub(">", " gt ", x, fixed = TRUE); x <- gsub("<", " lt ", x, fixed = TRUE)
  x <- gsub("=", " eq ", x, fixed = TRUE); x <- gsub("+", " plus ", x, fixed = TRUE)
  y <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
  y[is.na(y)] <- x[is.na(y)]
  y <- tolower(y)
  y <- gsub("[^a-z0-9]+", " ", y)
  trimws(gsub("\\s+", " ", y))
}

# Internal: unit families for the unit-consistency check (lower case)
.chk_unit_family <- function(u) {
  fam <- list(mass = c("kg", "g", "mg", "t", "tonne", "ton", "lb", "oz", "ug"),
              energy = c("mj", "kwh", "j", "gj", "tj", "btu", "mmbtu", "kj", "wh", "mwh"),
              volume = c("m3", "l", "ml", "gal", "cm3", "dm3"),
              area = c("m2", "ha", "km2", "cm2"),
              length = c("m", "km", "cm", "mm"),
              transport = c("tkm", "kg km", "kgkm", "tonne km", "t km", "pkm"),
              time = c("h", "hr", "s", "min", "day", "d", "year", "a", "yr"),
              item = c("p", "pcs", "unit", "item", "items", "piece", "pieces"))
  out <- rep(NA_character_, length(u))
  for (f in names(fam)) out[u %in% fam[[f]]] <- f
  out
}

# Internal: largest eigenvalue of |T| (power iteration on |T| + I, which also handles cycles)
.chk_spectral_radius <- function(S, maxit = 2000, tol = 1e-10) {
  n <- nrow(S); v <- rep(1 / sqrt(n), n); lam <- 0; conv <- FALSE
  for (it in seq_len(maxit)) {
    w <- as.numeric(S %*% v) + v
    nw <- sqrt(sum(w^2)); if (nw == 0) { lam <- 0; conv <- TRUE; break }
    new <- nw
    v <- w / nw
    if (abs(new - lam) < tol * max(1, new)) { lam <- new; conv <- TRUE; break }
    lam <- new
  }
  list(rho = max(lam - 1, 0), converged = conv)
}

#' Check the input data of a model before calculating
#'
#' Looks for problems in the table of exchanges that do not stop the
#' calculation but change the result without any warning: repeated rows,
#' elementary flows that no impact method characterises, processes without
#' elementary flows, inconsistent uncertainty parameters, and names that
#' differ only in case, accents, punctuation or one character. It reads the
#' model; it never changes it.
#'
#' @details
#' Each finding has a severity: `error` (the calculation or the Monte Carlo
#' cannot be trusted or will fail), `warning` (probably wrong or likely to
#' mislead) or `note` (worth knowing, often legitimate). Findings list a few
#' examples, not every case; use the model tables to see all of them.
#'
#' A flow with a characterisation factor of exactly zero is treated as "not
#' characterised", because [lca_model()] fills zero for flows a method does
#' not mention; a method that deliberately gives zero cannot be told apart.
#' In large databases most elementary flows are not characterised by any
#' method; that is normal, and the useful question is whether the flows that
#' matter are. When `demand` and `method` are given, the largest uncharacterised
#' flows of that result are listed (their units may differ).
#'
#' **Solvability.** Before anything else the technosphere matrix is examined:
#' a process that consumes all of its own product (`self_consumption`), a
#' technosphere that is not productive (largest eigenvalue of the absolute
#' technosphere above 1 with non-negative amounts), an exactly or numerically
#' singular matrix, and an estimated condition number (after scaling rows and
#' columns to a maximum of 1, because the raw value depends on the units chosen
#' for the processes) above 1e8 (warning) or 1e12 (error). The two thresholds are conventions, not theorems: they mean
#' that about 8 or 12 of the roughly 16 digits of double precision are lost.
#' Cycles are allowed (they are solved by the matrix); only unproductive ones
#' are reported. If the system cannot be solved, no result is computed for
#' `demand`.
#'
#' **Units.** If `exchanges` has a `unit` column (optional; [lca_model()] keeps
#' it), the same flow or product must use the same unit everywhere. Units of
#' the same quantity but a different scale (kg and g) are a warning; units of
#' different quantities, or unknown to the check, are an error. The check
#' knows only common units of mass, energy, volume, area, length, transport,
#' time and items. [read_uslci()] converts all amounts to the reference unit of
#' their unit group, so it needs no such column. The check cannot know whether
#' a unit is the right one for its flow.
#'
#' **What it does not check** is printed with the result, for example the
#' correctness of the data themselves.
#'
#' @param m An [lca_model()].
#' @param demand Optional named numeric vector (as in [lca_calc()]). With
#'   `method`, adds how many of the processes in the supply chain are flagged.
#' @param method Optional method name (a name in `names(m$C)`).
#' @param max_names Largest number of distinct names for which names one
#'   character apart are searched (the search compares every pair).
#' @return An object of class `lca_check` with `findings` (a data frame with
#'   `check`, `severity`, `n`, `message`, `examples`), counts per severity and
#'   `not_checked`.
#' @export
lca_check <- function(m, demand = NULL, method = NULL, max_names = 1500) {
  if (!inherits(m, "lca_model")) stop("`m` must be an lca_model", call. = FALSE)
  if (!is.null(demand) && is.null(method)) method <- names(m$C)[1]
  if (!is.null(method) && !method %in% names(m$C))
    stop("unknown method: ", method, call. = FALSE)
  ex <- m$ex; acc <- list(); notchk <- character()
  tech <- ex$type == "technosphere"; bio <- !tech
  col <- function(nm) if (is.null(ex[[nm]])) rep(NA_real_, nrow(ex)) else ex[[nm]]

  # 0. can the technosphere be solved?
  solv_ok <- TRUE
  if (any(tech)) {
    n <- length(m$processes)
    tt <- sparseMatrix(i = ex$row[tech], j = ex$col[tech], x = ex$amount[tech], dims = c(n, n))
    d <- Matrix::diag(tt)
    selfc <- which(d >= 1)
    if (length(selfc)) {
      solv_ok <- FALSE
      acc <- .chk_add(acc, "self_consumption", "error", length(selfc),
        "A process consumes at least as much of its own product as it makes (net output <= 0); the system cannot be solved.",
        m$processes[selfc])
    }
    sr <- .chk_spectral_radius(abs(tt))
    allpos <- all(ex$amount[tech] >= 0)
    if (sr$rho > 1 + 1e-6) {
      if (allpos) {
        solv_ok <- FALSE
        acc <- .chk_add(acc, "not_productive", "error", 1L,
          paste0("The technosphere is not productive: its largest eigenvalue is ", format(signif(sr$rho, 6)),
                 " (> 1). With non-negative amounts, the processes consume as much as they produce around some cycle, ",
                 "so no non-negative supply meets a positive demand."), character())
      } else {
        acc <- .chk_add(acc, "possibly_not_productive", "warning", 1L,
          paste0("The largest eigenvalue of the absolute technosphere is ", format(signif(sr$rho, 6)),
                 " (> 1). Because some amounts are negative (co-products or avoided burdens) this does not prove the ",
                 "system is unsolvable, but check the cycles."), character())
      }
    } else if (!sr$converged) {
      notchk <- c(notchk, "productivity: the eigenvalue iteration did not converge; treat its estimate as approximate")
    }
    if (solv_ok) {
      A <- sparseMatrix(i = c(seq_len(n), ex$row[tech]), j = c(seq_len(n), ex$col[tech]),
                        x = c(rep(1, n), -ex$amount[tech]), dims = c(n, n))
      # The condition number of the raw matrix depends on the units chosen for the processes
      # (a database with amounts from 1e-11 to 1e6 looks ill-conditioned only because of that).
      # Rows and columns are therefore scaled to a maximum of 1 before it is estimated.
      ii <- A@i + 1L; jj <- rep(seq_len(n), diff(A@p)); xx <- A@x
      rs <- 1 / as.numeric(tapply(abs(xx), factor(ii, seq_len(n)), max)); rs[!is.finite(rs)] <- 1
      x1 <- xx * rs[ii]
      cs <- 1 / as.numeric(tapply(abs(x1), factor(jj, seq_len(n)), max)); cs[!is.finite(cs)] <- 1
      A <- sparseMatrix(i = ii, j = jj, x = as.numeric(x1 * cs[jj]), dims = c(n, n))
      ce <- tryCatch(Matrix::condest(A)$est, error = function(e) NA_real_, warning = function(w) NA_real_)
      if (is.na(ce)) {
        solv_ok <- FALSE
        acc <- .chk_add(acc, "singular_system", "error", 1L,
          "The matrix (I - technosphere) is singular or numerically singular: the system has no unique solution. Look for cycles that return exactly what they use.",
          character())
      } else if (ce > 1e12) {
        acc <- .chk_add(acc, "ill_conditioned", "error", 1L,
          paste0("Estimated condition number ", format(ce, digits = 3), " after scaling (> 1e12): results are not reliable ",
                 "(about ", floor(log10(ce)), " of the ~16 digits of double precision are lost)."), character())
      } else if (ce > 1e8) {
        acc <- .chk_add(acc, "ill_conditioned", "warning", 1L,
          paste0("Estimated condition number ", format(ce, digits = 3), " after scaling (> 1e8): about ", floor(log10(ce)),
                 " of the ~16 digits of double precision are lost; small changes in amounts change results a lot."),
          character())
      }
    }
  }

  # 1. repeated rows. Rows that all carry a varying distribution are independent random
  # variables by design (read_uslci keeps them apart); the others are added silently.
  key <- paste(ex$type, ex$input, ex$output, sep = "\r")
  dg <- unique(key[duplicated(key)])
  if (length(dg)) {
    uu <- if (is.null(ex$utype)) rep(0, nrow(ex)) else ex$utype
    uu[is.na(uu)] <- 0
    all_spread <- vapply(dg, function(k) all(uu[key == k] %in% c(2, 4, 5)), TRUE)
    lab_dg <- gsub("\r", " -> ", dg, fixed = TRUE)
    if (any(!all_spread))
      acc <- .chk_add(acc, "repeated_rows", "warning", sum(!all_spread),
        paste(sum(!all_spread), "combination(s) of type, input and output appear more than once with at least",
              "one row that has no distribution. Their amounts are added in the matrices."),
        lab_dg[!all_spread])
    if (any(all_spread))
      acc <- .chk_add(acc, "repeated_uncertain_rows", "note", sum(all_spread),
        paste(sum(all_spread), "combination(s) of type, input and output repeat, all with a distribution:",
              "each row is an independent random variable (as read_uslci() keeps them)."),
        lab_dg[all_spread])
  }

  # 2. elementary flows no method characterises
  for (mm in names(m$C)) {
    v <- m$C[[mm]]
    if (!length(v)) next
    zero <- names(v)[v == 0]
    if (length(zero) == length(v)) {
      acc <- .chk_add(acc, "no_characterisation", "error", length(zero),
        paste0("Method '", mm, "' gives no factor to any elementary flow of the model."), head(zero, 5))
    } else if (length(zero)) {
      sh <- mean(ex$input[bio] %in% zero)
      ord <- zero[order(-tapply(abs(ex$amount[bio]), factor(ex$input[bio], zero), sum, default = 0)[zero])]
      acc <- .chk_add(acc, "no_characterisation", "warning", length(zero),
        paste0("Method '", mm, "': ", length(zero), " of ", length(v), " elementary flows have no factor (",
               format(round(100 * sh, 1), nsmall = 1), "% of the elementary-flow exchanges). Normal for large databases; ",
               "check that the flows that matter are covered."),
        ord)
    }
  }
  res <- if (!is.null(demand) && solv_ok) lca_calc(m, demand, method) else NULL
  if (!is.null(demand) && !solv_ok)
    notchk <- c(notchk, "results for `demand` (the system cannot be solved)")
  if (!is.null(res)) {
    v <- m$C[[method]]
    unch <- names(v)[v == 0]
    inv <- abs(res$inventory[unch])
    inv <- inv[inv > 0]
    if (length(inv)) {
      top <- names(sort(inv, decreasing = TRUE))
      acc <- .chk_add(acc, "largest_uncharacterised", "note", length(inv),
        paste0("In this result, ", length(inv), " flows with a non-zero inventory have no factor in '",
               method, "'. Largest by absolute inventory (units may differ):"), top)
    }
  }

  # 2b. elementary flows without a CAS number (matters for methods linked by CAS, such as TRACI 2.2)
  if (!is.null(m$flow_info) && "cas" %in% names(m$flow_info)) {
    nocas <- is.na(.cas_norm(m$flow_info$cas))
    if (any(nocas))
      acc <- .chk_add(acc, "flows_without_cas", "note", sum(nocas),
        paste0(sum(nocas), " of ", nrow(m$flow_info), " elementary flows have no CAS number. Methods linked by CAS ",
               "(read_traci) can only reach them by exact name; in USLCI exports, flows missing from the flow folder ",
               "lose their CAS."), m$flow_info$flow[nocas])
  }

  # 2c. metals named as the element that TRACI 2.2 (listed as ions) could not reach
  if (!is.null(m$traci_report$metals_without_factor) && m$traci_report$metals_without_factor > 0)
    acc <- .chk_add(acc, "metals_without_factor", "note", m$traci_report$metals_without_factor,
      paste0(m$traci_report$metals_without_factor, " emission flow(s) of metals named as the element have no TRACI 2.2 factor ",
             "(TRACI lists ions with an oxidation state, and matching one would be an assumption). Freshwater ecotoxicity and ",
             "human-health results can be underestimated."), m$traci_report$metals_without_factor_names)

  # 3. processes with no data at all; aggregators without elementary flows
  procs <- m$processes
  has_tech_in <- unique(ex$output[tech])
  nobio <- setdiff(procs, unique(ex$output[bio]))
  empty <- setdiff(nobio, has_tech_in)
  aggr <- intersect(nobio, has_tech_in)
  active <- if (!is.null(res)) names(res$supply)[res$supply != 0] else NULL
  if (length(empty)) {
    msg <- paste(length(empty), "of", length(procs), "process(es) have no exchange of their own (no inputs and no",
                 "elementary flows): they contribute nothing, which is usually missing data.")
    # Models read from a database (read_uslci) keep, per process, how many inputs the source has and how
    # many were linked: an empty process may then be one whose inputs were all cut off, not one with no data.
    inf <- m$info
    if (is.data.frame(inf) && all(c("pid", "n_tech", "n_linked") %in% names(inf))) {
      i <- match(empty, inf$pid)
      cut <- !is.na(i) & inf$n_tech[i] > 0 & inf$n_linked[i] == 0
      if (any(cut))
        msg <- paste(length(empty), "of", length(procs), "process(es) have no exchange in the model (no linked",
                     "inputs and no elementary flows). In the source,", sum(cut), "of them do have inputs, but none",
                     "has a provider in the database (cut-off): they are not empty in the source, only unlinked.",
                     if (sum(!cut)) paste(sum(!cut), "have no input at all in the source.") else "")
    }
    sev <- "warning"
    if (!is.null(active)) {
      hit <- intersect(empty, active)
      msg <- paste0(msg, " In the supply chain of this demand: ", length(hit), " of ", length(active), " active processes.")
      if (!length(hit)) sev <- "note"
    }
    acc <- .chk_add(acc, "empty_process", sev, length(empty), msg, empty)
  }
  if (length(aggr))
    acc <- .chk_add(acc, "aggregator_without_elementary_flows", "note", length(aggr),
      paste(length(aggr), "process(es) only combine other processes and have no elementary flow of their own.",
            "Legitimate for assemblies; check that none is missing its own emissions."), aggr)

  # 4. uncertainty parameters
  if (!is.null(ex$utype)) {
    u <- ex$utype; u[is.na(u)] <- 0
    lab <- paste(ex$input, "->", ex$output)
    bad <- !u %in% c(0, 2, 4, 5)
    if (any(bad))
      acc <- .chk_add(acc, "unknown_utype", "error", sum(bad),
        "`utype` must be 0 (none), 2 (lognormal), 4 (uniform) or 5 (triangular).", lab[bad])
    loc <- col("loc"); sc <- col("scale"); mn <- col("minimum"); mx <- col("maximum")
    ln <- u == 2
    b <- ln & (!is.finite(loc) | !is.finite(sc) | (is.finite(sc) & sc < 0))
    if (any(b))
      acc <- .chk_add(acc, "lognormal_parameters", "error", sum(b),
        "Lognormal exchanges need finite `loc` and a non-negative `scale`.", lab[b])
    b <- ln & is.finite(sc) & sc == 0
    if (any(b))
      acc <- .chk_add(acc, "lognormal_no_spread", "note", sum(b),
        "Lognormal exchanges with `scale` = 0 have no spread (GSD = 1); they add no uncertainty.", lab[b])
    b <- ln & ex$amount < 0
    if (any(b))
      acc <- .chk_add(acc, "lognormal_negative_amount", "warning", sum(b),
        "Lognormal exchanges with a negative `amount`: the Monte Carlo draws positive values, so the sign is lost.", lab[b])
    ut <- u %in% c(4, 5)
    b <- ut & (!is.finite(mn) | !is.finite(mx))
    if (any(b))
      acc <- .chk_add(acc, "range_missing", "error", sum(b),
        "Uniform and triangular exchanges need finite `minimum` and `maximum`.", lab[b])
    b <- ut & is.finite(mn) & is.finite(mx) & mn > mx
    if (any(b))
      acc <- .chk_add(acc, "range_inverted", "error", sum(b), "`minimum` is greater than `maximum`.", lab[b])
    tr <- u == 5
    b <- tr & is.finite(loc) & is.finite(mn) & is.finite(mx) & (loc < mn | loc > mx)
    if (any(b))
      acc <- .chk_add(acc, "mode_outside_range", "error", sum(b),
        "The mode (`loc`) of a triangular exchange is outside [minimum, maximum].", lab[b])
    b <- tr & !is.finite(loc)
    if (any(b))
      acc <- .chk_add(acc, "mode_missing", "error", sum(b), "Triangular exchanges need a finite mode in `loc`.", lab[b])
    b <- ut & is.finite(mn) & is.finite(mx) & mn <= mx & (ex$amount < mn | ex$amount > mx)
    if (any(b))
      acc <- .chk_add(acc, "amount_outside_range", "warning", sum(b),
        "The nominal `amount` is outside [minimum, maximum] of its distribution.", lab[b])
  }
  # 4b. units (only when the exchanges have a `unit` column)
  if (!is.null(ex$unit)) {
    un <- trimws(tolower(as.character(ex$unit)))
    miss <- is.na(un) | !nzchar(un)
    if (any(miss))
      acc <- .chk_add(acc, "unit_missing", "note", sum(miss),
        "Exchanges with an empty `unit`; they are left out of the unit comparison.",
        paste(ex$input, "->", ex$output)[miss])
    ok <- !miss
    ukey <- paste(ex$type, ex$input, sep = "\r")[ok]
    uvals <- tapply(un[ok], ukey, function(z) sort(unique(z)))
    multi <- uvals[vapply(uvals, length, 1L) > 1]
    if (length(multi)) {
      fam <- lapply(multi, .chk_unit_family)
      mixed_qty <- vapply(fam, function(f) anyNA(f) || length(unique(f)) > 1, TRUE)
      lab <- paste0(sub("^[a-z]+\r", "", names(multi)), ": ", vapply(multi, paste, "", collapse = " | "))
      if (any(mixed_qty))
        acc <- .chk_add(acc, "unit_mismatch", "error", sum(mixed_qty),
          paste(sum(mixed_qty), "flow(s) or product(s) appear with units that are different quantities, or units",
                "the check does not know. An amount in one unit cannot be added to an amount in another."),
          lab[mixed_qty])
      if (any(!mixed_qty))
        acc <- .chk_add(acc, "unit_scale_mismatch", "warning", sum(!mixed_qty),
          paste(sum(!mixed_qty), "flow(s) or product(s) appear in units of the same quantity but a different scale",
                "(for example kg and g). The model adds the numbers as they are, without converting."),
          lab[!mixed_qty])
    }
  } else {
    notchk <- c(notchk, "units: the exchanges have no `unit` column, so a mix of kg and g, or of MJ and kWh, cannot be detected")
  }

  zero_amt <- ex$amount == 0
  if (any(zero_amt))
    acc <- .chk_add(acc, "zero_amounts", "note", sum(zero_amt), "Exchanges with `amount` = 0.",
                    paste(ex$input, "->", ex$output)[zero_amt])

  # 5. names that differ only in form, or by one character
  nm_check <- function(names_vec, what) {
    nm <- unique(names_vec)
    if (length(nm) < 2) return(NULL)
    nn <- .chk_norm(nm)
    grp <- tapply(nm, nn, function(z) if (length(z) > 1) paste(z, collapse = " <> ") else NA_character_)
    grp <- grp[!is.na(grp)]
    if (length(grp))
      acc <<- .chk_add(acc, paste0(what, "_names_same_after_normalising"), "warning", length(grp),
        paste0(length(grp), " group(s) of ", what, " names are equal after ignoring case, accents, punctuation and spaces; ",
               "they may be the same thing written twice."), unname(grp))
    if (length(nm) > max_names) {
      notchk <<- c(notchk, paste0("one-character differences among ", what, " names (", length(nm),
                                  " names; raise `max_names` to search)"))
      return(NULL)
    }
    un <- unique(nn[nchar(nn) >= 6])
    if (length(un) < 2) return(NULL)
    d <- utils::adist(un)
    d[upper.tri(d, diag = TRUE)] <- Inf
    w <- which(d == 1, arr.ind = TRUE)
    if (nrow(w)) {
      pairs <- paste(un[w[, 1]], "<>", un[w[, 2]])
      acc <<- .chk_add(acc, paste0(what, "_names_one_character_apart"), "note", nrow(w),
        paste0(nrow(w), " pair(s) of ", what, " names differ by one character. Possible typos; many are legitimate ",
               "(for example sizes or numbers)."), pairs)
    }
    NULL
  }
  nm_check(procs, "process")
  nm_check(m$flows, "flow")

  notchk <- c(notchk,
    "that a unit is the right one for its flow (only that the same flow does not use different units)",
    "the correctness of the data themselves (only their consistency is examined)")
  f <- if (length(acc)) do.call(rbind, acc) else
    data.frame(check = character(), severity = character(), n = integer(),
               message = character(), examples = character(), stringsAsFactors = FALSE)
  rownames(f) <- NULL
  structure(list(findings = f,
                 n_error = sum(f$severity == "error"), n_warning = sum(f$severity == "warning"),
                 n_note = sum(f$severity == "note"), not_checked = notchk,
                 demand = demand, method = method), class = "lca_check")
}

#' @export
print.lca_check <- function(x, ...) {
  cat("<lca_check>", x$n_error, "error(s),", x$n_warning, "warning(s),", x$n_note, "note(s)\n")
  tag <- c(error = "[ERROR]", warning = "[WARN] ", note = "[NOTE] ")
  f <- x$findings
  if (nrow(f)) {
    f <- f[order(match(f$severity, c("error", "warning", "note"))), ]
    for (i in seq_len(nrow(f))) {
      cat("\n", tag[[f$severity[i]]], " ", f$check[i], " (n = ", f$n[i], ")\n", sep = "")
      cat("  ", f$message[i], "\n", sep = "")
      if (nzchar(f$examples[i])) cat("  e.g. ", f$examples[i], "\n", sep = "")
    }
  } else cat("\nNo problems found by these checks.\n")
  cat("\nNot checked:\n")
  cat(paste0("  - ", x$not_checked), sep = "\n")
  invisible(x)
}
