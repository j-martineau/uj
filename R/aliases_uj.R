#' @name aliases_uj
#' @family wrappers
#' @title Wrappers for functions in other packages
#' @param x A data.table object (for aliases beginning with \code{dt}. For
#'   aliases of base package functions, as expected for those functions.
#' @param ir,ic Indexing vector giving rows, columns of \code{x} to select.
#' @param f data.table casting formula
#' @param v data.table variable name of column containing values to be cast
#'   wide.
#' @param first,last Positive integers indicating first and last characters to
#'   extract.
#' @export
aliases_uj <- function() {help("aliases_uj", package = "uj")}

#' @describeIn aliases_uj Thin wrapper for
#'   \code{\link[data.table]{as.data.table}}.
#' @export
dtas <- function(x) {say("\n| as data.table"); data.table::as.data.table(x)}

#' @describeIn aliases_uj If \code{x} is not already a data.table, coerce it to
#'   a data.table.
#' @export
dtie <- function(x) {if (data.table::is.data.table(x)) {x} else {data.table::as.data.table(x)}}

#' @describeIn aliases_uj Extract rows and columns of a data.table using indices
#'   or names rather than the formal data.table syntax for column selection.
#' @export
dtsub <- function(x, ir, ic) {x[ir , ..ic]}

#' @describeIn aliases_uj Extract columns of a data.table using indices or names
#'   rather than the formal data.table syntax for column selection.
#' @export
dtcols <- function(x, ic) {x[ , ..ic]}

#' @describeIn aliases_uj merge two data.tables by specified columns.
#' @export
dtjoin <- function(x, y, by) {say("\n| merge"); data.table::merge(x, y, by = by)}

#' @describeIn aliases_uj Extract rows of a data.table using indices or names.
#' @export
dtrows <- function(x, ir) {x[ir, ]}

#' @describeIn aliases_uj Reshape data.table \code{x} from long to wide.
#' @export
dtwide <- function(x, f, v) {say("\n| reshape"); data.table::dcast(x, f, value.var = v)}

#' @describeIn aliases_uj Limited functionality thin wrapper for dplyr function
#'   \code{\link[dplyr]{group_by}} (group cases for group-based analyses).
#' @export
dpgrp <- function(x, keys) {run("dplyr::group_by(x, ", dw(keys, w. = v(comma)), ")")}

#' @describeIn aliases_uj Alias for \code{\link[readr]{read_csv}}.
#' @export
rdcsv <- function(file = NULL) {
  if (inll(file)) {file <- doc("comma-separated-values (.csv) data file")}
  if (!cmp_chr_vec(file)) {stop("\n • [file] must be NULL or a complete character vector.")}
  file <- dw0(file)
  if (!file.exists(file)) {stop("\n • the specified file ('", file, "') does not exist.")}
  readr::read_csv(dw0(file))
}

#' @describeIn aliases_uj Alias for \code{\link[readr]{clipboard}} to read text
#'   from the system clipboard.
#' @export
rdclip <- function() {readr::clipboard}

#' @describeIn aliases_uj Alias for \code{\link[readr]{read_delim}} to read
#'   (arbitrary) delimiter-separated values from text files.
#' @export
rdxsv <- function(file = NULL, delim = ",") {
  if (inll(file)) {file <- doc("comma-separated-values (.csv) data file")}
  if (!cmp_chr_vec(file)) {stop("\n • [file] must be NULL or a complete character vector.")}
  file <- dw0(file)
  if (!file.exists(file)) {stop("\n • the specified file ('", file, "') does not exist.")}
  if (inll(file)) {file <- doc(".csv data file")}
  readr::read_delim(file, delim = delim)
}

#' @describeIn aliases_uj Alias for \code{\link[readr]{read_tsv}} to read tab
#'   separated values from text files.
#' @export
rdtsv <- function(file = NULL) {
  if (inll(file)) {file <- doc(".csv data file")}
  readr::read_tsv(file)
}

#' @describeIn aliases_uj Alias for \code{base::ceiling}.
#' @export
up <- function(x) {ceiling(x)}

#' @describeIn aliases_uj Alias for \code{base::floor}.
#' @export
dn <- function(x) {floor(x)}

#' @describeIn aliases_uj Alias for \code{base::sprintf}.
#' @export
spf <- function(fmt, ...) {sprintf(fmt, ...)}

#' @describeIn aliases_uj Alias for \code{base::substr(x, first, last)}.
#' @export
mid <- function(x, first, last) {substr(x, first, last)}

