#' @name dt_tools
#' @title Aliases and wrappers for functions in package \code{data.table}.
#' @description Alias for \code{\link[data.table]{data.table}}.
#' @export
dtmk <- data.table::data.table

#' @describeIn dt_tools Alias for \code{\link[data.table]{is.data.table}}.
#' @export
dtis <- data.table::is.data.table

#' @describeIn dt_tools Alias for \code{\link[data.table]{dcast}} (long-to-wide
#'   reshaping function).
#' @export
dtcast <- data.table::dcast

#' @describeIn dt_tools Alias for \code{\link[data.table]{merge}}.
#' @export
dtmerge <- data.table::merge.data.table

#' @describeIn dt_tools Thin wrapper for
#'   \code{\link[data.table]{as.data.table}}.
#' @export
dtas <- function(x) {say("\n| as data.table"); data.table::as.data.table(x)}

#' @describeIn dt_tools If \code{x} is not already a data.table, coerce it to
#'   a data.table.
#' @export
dtie <- function(x) {if (dtis(x)) {x} else {dtas(x)}}

#' @describeIn dt_tools Extract rows and columns of a data table using indices
#'   or names rather than the formal data.table syntax for column selection.
#' @export
dtsub <- function(x, ir, ic) {x[ir , ..ic]}

#' @describeIn dt_tools Extract columns of a data table using indices or names
#'   rather than the formal data.table syntax for column selection.
#' @export
dtcols <- function(x, ic) {x[ , ..ic]}

#' @describeIn dt_tools merge two tables by specified columns.
#' @export
dtjoin <- function(x, y, by) {say("\n| merge"); dtmerge(x, y, by = by)}

#' @describeIn dt_tools Extract rows of a data table using indices or names.
#' @export
dtrows <- function(x, ir) {x[ir, ]}

#' @describeIn dt_tools Reshape \code{x} from long to wide.
#' @export
dtwide <- function(x, f, v) {say("\n| reshape"); dtcast(x, f, value.var = v)}     # cast data table long to wide
