#' @name dt.
#' @family wraps
#' @title Wraps of functions from package \code{data.table}.
#' @description The following table describes thin wraps in this group of
#'   functions:\tabular{ll}{
#'   WRAPPER         \tab FUNCTION                                           \cr
#'   \code{dtas}     \tab \code{\link[data.table]{as.data.table}}            \cr
#'   \code{dtis}     \tab \code{\link[data.table]{is.data.table}}            \cr
#'   \code{dtmerge}  \tab \code{\link[data.table]{merge}}                    \cr
#'   \code{dtwide}   \tab \code{\link[data.table]{dcast}}                      }
#'   Functionality is stripped down significantly for these wraps.
#'   \cr\cr
#'   There are a few additional wrappers that combine multiple \code{data.table}
#'   operations or that address extraction of data from a data.table without
#'   using the non-standard \code{data.table} indexing functions. The are
#'   described in the table below.
#'   \tabular{ll}{
#'   WRAPPER        \tab FUNCTIONALITY                                       \cr
#'   \code{dtie}    \tab If \code{x} is already a data table, return it as is,
#'                       otherwise, convert it to a data.table.              \cr
#'   \code{dtcols}  \tab Select columns from a data.table without non-standard
#'                       data.table indexing functions (i.e.,
#'                       \code{x[ , ..var]}).                                \cr
#'   \code{dtrows}  \tab Select rows from a data.table with similar syntax as
#'                       \code{dtcols} (not strictly necessary as there is no
#'                       special data.table syntax for extracting rows).     \cr
#'   \code{dtsub}   \tab Select a sub-table from a data.table without using
#'                       non-standard data.table indexing functions (i.e.,
#'                       \code{x[row.var, ..col.var]}).                        }
#' @param x An R object (\code{dtas}, \code{dtis}, and \code{dtie}). A
#'   \code{\link[data.table]{data.table}} for all other functions.
#' @param ir,ic Logical or numeric indices or variables naming rows and columns
#'   of \code{x}, respectively.
#' @param say Logical scalar indicating whether to notify user of starting a
#'   potentially time-consuming process (i.e., converting to data.table, joining
#'   two large data.tables, and reshaping from long to wide).
#' @return A \code{\link[data.table]{data.table}}.
#' @export
dt. <- function() {help("dt.", package = "uj")}

#' @describeIn dt. Coerce to data.table.
#' @inherit data.table::as.data.table
#' @export
dtas <- function(x, say = TRUE) {if (say) {say("\n| coerce to data.table")}; data.table::as.data.table(x)}

#' @describeIn dt. Evaluate whether an object is a data.table
#' @inherit data.table::is.data.table
#' @export
dtis <- function(x) {data.table::is.data.table(x)}

#' @describeIn dt. If an object is not already a data.table, coerce it.
#' @export
dtie <- function(x, say = TRUE) {if (data.table::is.data.table(x)) {x} else {dtas(x, say)}}

#' @describeIn dt. Extract a subset of a data.table.
#' @export
dtsub <- function(x, ir, ic) {x[ir , ..ic]}

#' @describeIn dt. Extract columns of a data.table.
#' @export
dtcols <- function(x, ic) {x[ , ..ic]}

#' @describeIn dt. Merge two data.tables by specified columns.
#' @inherit data.table::merge
#' @export
dtmerge <- function(x, y, by = NULL, by.x = NULL, by.y = NULL, all = FALSE, all.x = all, all.y = all, sort = TRUE, suffixes = c(".x", ".y"), no.dups = TRUE, allow.cartesian = getOption("datatable.allow.cartesian"), say = TRUE) {
  if (say) {say("\n| merge data.tables")}
  data.table::merge(x, y, by = by, by.x = by.x, by.y = by.y, all = all, all.x = all.x, all.y = all.y, sort = sort, suffixes = suffixes, no.dups = no.dups, allow.cartesian = allow.cartesian)
}

#' @describeIn dt. Extract rows of a data.table.
#' @export
dtrows <- function(x, ir) {x[ir, ]}

#' @describeIn dt. Reshape a data.table from long to wide.
#' @inherit data.table::dcast
#' @export
dtwide <- function(data, formula, fun.aggregate = NULL, sep = "_", ..., margins = NULL, subset = NULL, fill = NULL, drop = TRUE, value.var = data.table:::guess(data), verbose = getOption("datatable.verbose"), say = TRUE) {
  if (say) {say("\n| reshape data.table from long to wide")}
  data.table::dcast(data, formula, value.var = value.var, fun.aggregate = fun.aggregate, sep = sep, ..., margins = margins, subset = subset, fill = fill, drop = drop, value.var = value.var, verbose = verbose)
}

