#' @name wraps_data_table
#' @family wraps
#' @title Wrappers of package `data.table` functions
#' @description \itemize{
#'   \item **`dtas`**: reduced functionality wrapper for \code{\link[data.table]{as.data.table}}.
#'   \item **`dtis`**: reduced functionality wrapper for \code{\link[data.table]{is.data.table}}.
#'   \item **`dtmerge`**: reduced functionality wrapper for \code{\link[data.table]{merge}}.
#'   \item **`dtwide`**: reduced functionality wrapper for \code{\link[data.table]{dcast}}.
#' }
#' \cr\cr
#' The following wrappers combine multiple `data.table` operations or address extraction without using `data.table`'a non-standard `[` extraction functions.
#' \itemize{
#'   \item **`dtie`**: evaluates whether `x` is already a `data.table`. If so, returns it as is, otherwise converts it to a `data.table`.
#'   \item **`dtcols`**: select columns without `data.table`'s non-standard `[` extraction functions (i.e., `x[ , ..var]`).
#'   \item **`dtrows`**: is a corollary for row extraction (but is not strictly necessary; there is no special `data.table` syntax row extraction).
#'   \item **`dtsub`**: selects a sub-`data.table` without non-standard `[` extraction functions (i.e., `x[row.var, ..col.var]`).
#' }
#' @param x An R object for `dtas`, `dtis`, and `dtie`. A \code{\link[data.table]{data.table}} for all others.
#' @param ir,ic \link[=cmp_ind_vec]{Complete indexer vecs} or \link[=cmp_chr_vec]{complete character vecs} naming rows and columns of `x`, respectively.
#' @param say Non-`NA` logical scalar indicating whether to notify user of starting a potentially time-consuming process (converting to `data.table`, joining two large `data.table`s, and reshaping from long to wide).
#' @return A data.table.
#' @inherit data.table::as.data.table
#' @export
dtas <- function(x, say = TRUE) {if (say) {say("\n| coerce to data.table")}; data.table::as.data.table(x)}

#' @rdname wraps_data_table
#' @inherit data.table::is.data.table
#' @export
dtis <- function(x) {data.table::is.data.table(x)}

#' @rdname wraps_data_table
#' @export
dtie <- function(x, say = TRUE) {if (data.table::is.data.table(x)) {x} else {dtas(x, say)}}

#' @rdname wraps_data_table
#' @export
dtsub <- function(x, ir, ic) {x[ir , ..ic]}

#' @rdname wraps_data_table
#' @export
dtcols <- function(x, ic) {x[ , ..ic]}

#' @rdname wraps_data_table
#' @inherit data.table::merge
#' @export
dtmerge <- function(x, y, by = NULL, by.x = NULL, by.y = NULL, all = FALSE, all.x = all, all.y = all, sort = TRUE, suffixes = c(".x", ".y"), no.dups = TRUE, allow.cartesian = getOption("datatable.allow.cartesian"), say = TRUE) {
  if (say) {say("\n| merge data.tables")}
  data.table::merge(x, y, by = by, by.x = by.x, by.y = by.y, all = all, all.x = all.x, all.y = all.y, sort = sort, suffixes = suffixes, no.dups = no.dups, allow.cartesian = allow.cartesian)
}

#' @rdname wraps_data_table
#' @export
dtrows <- function(x, ir) {x[ir, ]}

#' @rdname wraps_data_table
#' @inherit data.table::dcast
#' @export
dtwide <- function(data, formula, fun.aggregate = NULL, sep = "_", ..., margins = NULL, subset = NULL, fill = NULL, drop = TRUE, value.var = data.table:::guess(data), verbose = getOption("datatable.verbose"), say = TRUE) {
  if (say) {say("\n| reshape data.table from long to wide")}
  data.table::dcast(data, formula, value.var = value.var, fun.aggregate = fun.aggregate, sep = sep, ..., margins = margins, subset = subset, fill = fill, drop = drop, value.var = value.var, verbose = verbose)
}

