#' @name dt
#' @encoding UTF-8
#' @family wraps
#' @title Wrappers of `data.table` functions
#' @details
#' \tabular{ll}{  `dt_merge`   \tab Thinly wraps \code{\link[data.table]{merge}}.         \cr
#'                `dt_wide`    \tab Thinly wraps \code{\link[data.table]{dcast}}.         \cr
#'                `dt_rows`    \tab Selects rows.                                         \cr
#'                `dt_cols`    \tab Selects columns without `x[ , ..var]`.                \cr
#'                `dt_sub`     \tab Selects a subtable without `x[row.var, ..col.var]`.   \cr
#'                `as_dt`      \tab Thinly wraps \code{\link[data.table]{as.data.table}}. \cr
#'                `is_dt`      \tab Thinly wraps \code{\link[data.table]{is.data.table}}. \cr
#'                `ie_dt`      \tab Convert to `data.table`, if needed.                     }
#' @param x An object for `as_dt`, `is_dt`, and `ie_dt`. A \code{\link[data.table]{data.table}} for all others.
#' @param ir,ic \link[=cmp_ind_vec]{Complete indexer vecs} or \link[=cmp_chr_vec]{complete character vecs} identifying rows and columns of `x`, respectively.
#' @param say `TRUE` or `FALSE` indicating whether to notify user of starting a potentially time-consuming process (converting to `data.table`, joining two large `data.table`s, and reshaping from long to wide).
#' @return A data.table.
#' @inherit data.table::as.data.table
#' @export
as_dt <- function(x, keep.rownames = FALSE, ..., say = TRUE) {if (say) {uj::say("\n| coerce to data.table")}; data.table::as.data.table(x)}

#' @rdname dt
#' @inherit data.table::is.data.table
#' @export
is_dt <- function(...) {uj::run_alias("data.table", "is.data.table")}

#' @rdname dt
#' @export
ie_dt <- function(x, say = TRUE) {if (uj::is_dt(x)) {x} else {uj::as_dt(x, say)}}

#' @rdname dt
#' @export
dt_sub <- function(x, ir, ic) {x[ir , ic, with = FALSE]}

#' @rdname dt
#' @export
dt_cols <- function(x, ic) {x[ , ic, with = FALSE]}

#' @rdname dt
#' @inherit data.table::merge
#' @export
dt_merge <- function(x, y, by = NULL, by.x = NULL, by.y = NULL, all = FALSE, all.x = all, all.y = all, sort = TRUE, suffixes = c(".x", ".y"), no.dups = TRUE, allow.cartesian = base::getOption("datatable.allow.cartesian"), say = TRUE) {
  if (say) {uj::say("\n| merge data.tables")}
  if (base::is.null(by) & base::is.null(by.x) & base::is.null(by.y)) {data.table::merge.data.table(x, y, all = all, all.x = all.x, all.y = all.y, sort = sort, suffixes = suffixes, no.dups = no.dups, allow.cartesian = allow.cartesian)}
  else if (!base::is.null(by)) {data.table::merge.data.table(x, y, by = by, all = all, all.x = all.x, all.y = all.y, sort = sort, suffixes = suffixes, no.dups = no.dups, allow.cartesian = allow.cartesian)}
  else {data.table::merge.data.table(x, y, by.x = by.x, by.y = by.y, all = all, all.x = all.x, all.y = all.y, sort = sort, suffixes = suffixes, no.dups = no.dups, allow.cartesian = allow.cartesian)}
}

#' @rdname dt
#' @export
dt_rows <- function(x, ir) {x[ir, ]}

#' @rdname dt
#' @inherit data.table::dcast
#' @export
dt_wide <- function(data, formula, fun.aggregate = NULL, sep = "_", ..., margins = NULL, subset = NULL, fill = NULL, drop = TRUE, value.var = data.table:::guess(data), verbose = base::getOption("datatable.verbose"), say = TRUE) {
  if (say) {uj::say("\n| reshape data.table from long to wide")}
  code <- base::paste0("data.table::dcast.data.table(data, formula, sep = sep, ..., drop = drop, value.var = value.var, verbose = verbose",
                       uj::f0(base::is.null(fun.aggregate), "", ", fun.aggregate = fun.aggregate"),
                       uj::f0(base::is.null(margins), "", ", margins = margins"),
                       uj::f0(base::is.null(subset), "", ", subset = subset"),
                       uj::f0(base::is.null(fill), "", ", fill = fill"), ")")
  uj::run(code)
}
