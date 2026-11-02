#' Build an LCA model from a table of exchanges
#'
#' @description
#' Converts a long table of exchanges and a table of characterization factors
#' into a model object. Technosphere exchanges are normalised per unit of the
#' reference product of the consuming process (the diagonal of the
#' technosphere matrix is 1).
#'
#' @param exchanges A data frame with columns `type` (`"technosphere"` or
#'   `"biosphere"`), `input`, `output`, and `amount`. For technosphere rows
#'   `input` is the supplying process and `output` the consuming process; for
#'   biosphere rows `input` is the elementary flow and `output` the process
#'   that emits (or consumes) it. Optional columns `utype`, `loc` and `scale`
#'   describe a lognormal uncertainty (`utype == 2`, `loc = log(median)`,
#'   `scale = log(GSD)`), as in Brightway.
#' @param cf A data frame with columns `method`, `flow` and `cf`.
#' @param processes Optional character vector with all process names, in the
#'   order wanted. Needed when some processes have no exchanges at all;
#'   by default the processes are those appearing in `output`.
#' @param info Optional data frame with one row per process (any descriptive
#'   columns); stored as `m$info`.
#' @param flow_info Optional data frame describing the elementary flows, with
#'   columns `flow` (the name used in `exchanges$input`), `id` (a unique
#'   identifier, for example the UUID of the flow) and optionally `ref_prop`
#'   (name of the reference flow property). It lets [read_olca_method()] link
#'   impact factors by identifier. Stored as `m$flow_info`.
#' @return An object of class `lca_model`.
#' @examples
#' ex <- read.csv(system.file("extdata", "toy_exchanges.csv", package = "lcamate"))
#' cf <- read.csv(system.file("extdata", "toy_cf.csv", package = "lcamate"))
#' m <- lca_model(ex, cf)
#' m
#' @export
lca_model <- function(exchanges, cf, processes = NULL, info = NULL, flow_info = NULL) {
  need <- c("type", "input", "output", "amount")
  if (!all(need %in% names(exchanges)))
    stop("`exchanges` needs columns: ", paste(need, collapse = ", "), call. = FALSE)
  if (!all(c("method", "flow", "cf") %in% names(cf)))
    stop("`cf` needs columns: method, flow, cf", call. = FALSE)
  if (!all(exchanges$type %in% c("technosphere", "biosphere")))
    stop("`type` must be 'technosphere' or 'biosphere'", call. = FALSE)
  if (anyNA(exchanges$amount) || any(!is.finite(exchanges$amount)))
    stop("`amount` must be finite", call. = FALSE)
  exchanges$input <- as.character(exchanges$input)
  exchanges$output <- as.character(exchanges$output)
  procs <- if (is.null(processes)) unique(exchanges$output) else as.character(processes)
  if (anyDuplicated(procs)) stop("`processes` has duplicated names", call. = FALSE)
  if (!all(exchanges$output %in% procs))
    stop("exchanges refer to processes not in `processes`", call. = FALSE)
  tech <- exchanges$type == "technosphere"
  unknown <- setdiff(exchanges$input[tech], procs)
  if (length(unknown))
    stop("technosphere inputs without a process: ",
         paste(head(unknown, 5), collapse = ", "), call. = FALSE)
  flows <- unique(exchanges$input[!tech])
  ex <- exchanges
  ex$row <- ifelse(tech, match(ex$input, procs), match(ex$input, flows))
  ex$col <- match(ex$output, procs)
  methods <- unique(as.character(cf$method))
  C <- lapply(methods, function(mm) {
    v <- numeric(length(flows)); names(v) <- flows
    s <- cf[cf$method == mm, ]
    s <- s[s$flow %in% flows, ]
    v[as.character(s$flow)] <- s$cf
    v
  })
  names(C) <- methods
  if (!is.null(flow_info)) {
    if (!all(c("flow", "id") %in% names(flow_info)))
      stop("`flow_info` needs columns: flow, id", call. = FALSE)
    flow_info <- as.data.frame(flow_info, stringsAsFactors = FALSE)
    flow_info$flow <- as.character(flow_info$flow); flow_info$id <- as.character(flow_info$id)
    if (anyDuplicated(flow_info$flow)) stop("`flow_info$flow` has duplicated names", call. = FALSE)
    flow_info <- flow_info[flow_info$flow %in% flows, , drop = FALSE]
    rownames(flow_info) <- NULL
  }
  structure(list(processes = procs, flows = flows, ex = ex, C = C, info = info,
                 flow_info = flow_info), class = "lca_model")
}

#' @export
print.lca_model <- function(x, ...) {
  cat("<lca_model>", length(x$processes), "processes,", length(x$flows),
      "elementary flows,", sum(x$ex$type == "technosphere"), "technosphere links\n")
  cat("methods:", paste(names(x$C), collapse = ", "), "\n")
  if (!is.null(x$scenario))
    cat("SCENARIO: pedigree-derived uncertainty on", x$scenario$n_applied,
        "exchanges (not documented uncertainty)\n")
  invisible(x)
}
