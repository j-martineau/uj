#' @name wraps_data_table
#' @family wraps
#' @title Wraps of Functions from Package `data.table`
#' @description The following table describes thin wraps in this group of
#'   functions:\tabular{ll}{
#'     WRAPPER     \tab FUNCTION                                             \cr
#'     `dtas`      \tab \code{\link[data.table]{as.data.table}}              \cr
#'     `dtis`      \tab \code{\link[data.table]{is.data.table}}              \cr
#'     `dtmerge`   \tab \code{\link[data.table]{merge}}                      \cr
#'     `dtwide`    \tab \code{\link[data.table]{dcast}}                        }
#'   Functionality is stripped down significantly for these wraps.
#'   \cr\cr
#'   There are a few additional wrappers that combine multiple `data.table`
#'   operations or that address extraction of data from a data.table without
#'   using the non-standard `data.table` indexing functions. The are
#'   described in the table below.
#'   \tabular{ll}{
#'     WRAPPER    \tab FUNCTIONALITY                                         \cr
#'     `dtie`     \tab If `x` is already a data table, return it as is,
#'                     otherwise, convert it to a data.table.                \cr
#'     `dtcols`   \tab Select columns from a data.table without non-standard
#'                     data.table indexing functions (i.e., `x[ , ..var]`).  \cr
#'     `dtrows`   \tab Select rows from a data.table with similar syntax as
#'                     `dtcols` (not strictly necessary as there is no special
#'                     data.table syntax for extracting rows).               \cr
#'     `dtsub`    \tab Select a sub-table from a data.table without using
#'                     non-standard data.table indexing functions (i.e.,
#'                     `x[row.var, ..col.var]`).                               }
#' @param x An R object (`dtas`, `dtis`, and `dtie`). A
#'   \code{\link[data.table]{data.table}} for all other functions.
#' @param ir,ic \link[=cmp_ind_vec]{Complete indexer vecs} or
#'   \link[=cmp_chr_vec]{complete character vecs} identifying rows and columns
#'   of `x`, respectively.
#' @param say Non-`NA` logical scalar indicating whether to notify user of
#'   starting a potentially time-consuming process (i.e., converting to
#'   data.table, joining two large data.tables, and reshaping from long to
#'   wide).
#' @return A \code{\link[data.table]{data.table}}.
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

