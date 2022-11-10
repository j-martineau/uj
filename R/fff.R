#' @name fff.
#' @family props
#' @title Object form properties
#' @description Gets a vector of form properties from \code{fff_vals()} that are
#'   applicable to \code{x}. An object's form properties have to do with the
#'   number of \link[ddd]{defined dimensions} and/or \link[eee]{effective
#'   dimensions} plus some additional restrictions:\tabular{lll}{
#'   FUNDAMENTAL   \tab FUNDAMENTAL   \tab FUNDAMENTAL                       \cr
#'   TYPE VALUE    \tab TYPE NAME     \tab TYPE DEFINITION                   \cr
#'   \code{'emp'}\tab empty         \tab Non-\code{NULL}, length = 0.        \cr
#'   \code{'pop'}\tab populated     \tab Length > 0.                         \cr
#'   \code{'pnt'}\tab point         \tab Containing 1 element (includes dtfs
#'                                       of dimension \code{1 × 1}).         \cr
#'   \code{'lin'}\tab linear        \tab Effectively 1-dimensional.          \cr
#'   \code{'row'}\tab 1-row         \tab Row matrix or row
#'                                       \code{\link[is_dtf]{dtf}}.          \cr
#'   \code{'col'}\tab 1-column      \tab Column matrix or column
#'                                       \code{\link[is_dtf]{dtf}}.          \cr
#'   \code{'rct'}\tab rectangular   \tab Matrix or \code{\link[is_dtf]{dtf}}
#'                                       with multiple rows and multiple
#'                                       columns.                            \cr
#'   \code{'sqr'}\tab square        \tab Square atomic matrix                \cr
#'   \code{'sld'}\tab solid         \tab Effectively hyper-dimensional (having
#'                                       indexing positions in 3+ dimensions). }
#' @param x An object.
#' @param xxx \link[cmp_chr_scl]{Complete character scalar} containing one or
#'   more values from \code{fff_vals()} separated by pipes and/or underscores
#'   ("."). Combinations of forms can be specified by separating them with
#'   underscores. Separating forms or combinations of forms with pipes will
#'   result in a value of \code{TRUE} if any of them applies to \code{x}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @section Submitting additional arguments to \code{ifff} via \code{...}:
#'   Allows for checking not just the form but whether length, number of rows,
#'   number of columns, and element values meet flexible criteria.
#' @return \code{fff_vals} and \code{fff} return a character vector. All
#'   others return either \code{TRUE} or \code{FALSE}.
#' @export
fff. <- function() {help("fff.", "uj")}

#' @describeIn fff. Evaluates whether \code{x} is empty.
#' @export
fff <- function(x) {
  nr <- nrow(x); nc <- ncol(x); nl <- is.null(x); rm <- nr > 1; r1 <- nr == 1
  nd <- nddd(x); ne <- neee(x); n  <- length( x); cm <- cn > 1; c1 <- cn == 1
  eq <- nr == nc
  c(f0(nd != 2, NULL, f0(rm & c1, 'col', NULL)),
    f0(n  != 0, NULL, f0(nl     , 'emp', NULL)),
    f0(ne != 1, NULL,             'lin'       ),
    f0(ne != 0, NULL,             'pnt'       ),
    f0(nd != 2, NULL, f0(rm & cm, 'rct', NULL)),
    f0(nd != 2, NULL, f0(cm & r1, 'row', NULL)),
    f0(ne <= 2, NULL,             'sld'       ),
    f0(nd != 2, NULL, f0(rm & eq, 'sqr', NULL)))
}

#' @describeIn fff. Evaluates whether \code{x} is empty.
#' @export
iemp <- function(x) {f0(length(x) != 0, F, !is.null(x))}

#' @describeIn fff. Evaluates whether \code{x} is a point.
#' @export
ipnt <- function(x) {neee(x) == 0}

#' @describeIn fff. Evaluates whether \code{x} is linear.
#' @export
ilin <- function(x) {neee(x) == 1}

#' @describeIn fff. Evaluates whether \code{x} is a row object.
#' @export
irow <- function(x) {f0(!nddd(x) == 2, F, nrow(x) == 1 & ncol(x) > 1)}

#' @describeIn fff. Evaluates whether \code{x} is a column object.
#' @export
icol <- function(x) {f0(!nddd(x) == 2, F, nrow(x) > 1 & ncol(x) == 1)}

#' @describeIn fff. Evaluates whether \code{x} is a rectangular object.
#' @export
irct <- function(x) {f0(!nddd(x) == 2, F, nrow(x) > 1 & ncol(x) > 1)}

#' @describeIn fff. Evaluates whether \code{x} is a square object.
#' @export
isqr <- function(x) {f0(!nddd(x) == 2, F, nrow(x) > 1 & ncol(x) == nrow(x))}

#' @describeIn fff. Evaluates whether \code{x} is a solid object.
#' @export
isld <- function(x) {neee(x) > 2}

#' @describeIn fff. Get a list of all possible form property values.
#' @export
fff_vals <- function() {x <- sort(c('emp', 'pnt', 'lin', 'row', 'col', 'rct', 'sqr', 'sld')); names(x) <- rep.int("fff", length(x)); x}

#' @describeIn fff. Evaluates whether any property in \code{xxx} is a form
#'   property applicable to \code{x} (subject to any additional restrictions
#'   in \code{...}).
#' @export
ifff <- function(x, xxx, ...) {
  if (!cmp_chr_scl(xxx)) {stop("\n • [xxx] must be a non-NA character scalar.")}
  valid <- fff_vals()
  combos <- unlist(strsplit(xxx, "|", fixed = T)[[1]], T, F)
  newxxx <- unlist(strsplit(combos, "_", fixed = T)     , T, F)
  valid <- all(newxxx %in% valid)
  if (!valid) {stop("\n • [xxx] contains a value not in fff_vals() (after splitting [xxx] along pipes and underscores).")}
  ixxx(x, xxx, ...)
}
