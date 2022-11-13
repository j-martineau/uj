#' @name ccc.
#' @family props
#' @title Extended class properties
#' @description Extended classes are not formally defined, but are dynamically
#' evaluated. Extended classes come in two varieties: universal and atomic. The
#' following two tables define each extended class in universal and atomic form,
#' respectively.:\tabular{lll}{
#' EXTENDED     \tab PROPERTY     \tab QUALIFYING                            \cr
#' CLASS        \tab VALUE        \tab OBJECTS                               \cr
#' vlist        \tab \code{'vls'} \tab Vector-list (i.e., not a data.frame). \cr
#' data.frame   \tab \code{'dtf'} \tab Data.frame.                           \cr
#' generic      \tab \code{'gen'} \tab Vector, array, or vlist.              \cr
#' matrix       \tab \code{'mat'} \tab Matrix.                               \cr
#' array+       \tab \code{'arr'} \tab Array or vector of any length.        \cr
#' scalar       \tab \code{'scl'} \tab Array or vector of any length.        \cr
#' vector+      \tab \code{'vec'} \tab Vector of length 1+ or array of length 1+
#'                                     with multiple index positions in either 0
#'                                     or 1 dimensions.                      \cr
#' multivec     \tab \code{'mvc'} \tab Vector of length 2+ or array of length 2+
#'                                     with multiple index positions in exactly
#'                                     1 dimension.                            }
#' @param x An object.
#' @param ccc \code{NULL} or \link[cmp_chr_scl]{complete character scalar}
#'   containing one or more values from \code{ccc_vals()} separated by pipes
#'   and/or underscores. Combinations of extended classes can be specified by
#'   separating them with underscores. Separating extended classes or
#'   combinations of extended classes with pipes will result in a value of
#'   \code{TRUE} if any of them applies to \code{x}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @section Submitting additional arguments to \code{ccc} via \code{...}:
#'   Allows for checking not just the ccc but whether length, number of rows,
#'   number of columns, and element values meet flexible criteria.
#' @return \code{ccc_vals} returns a character vector containing all valid
#'   extended class property values. \code{ccc} returns a character scalar
#'   or vector containing all extended class properties from
#'   \code{ccc_vals()} applicable to \code{x}. All others return either
#'   \code{TRUE} or \code{FALSE}.
#' @export
ccc. <- function() {help("ccc.", package = "uj")}

#' @describeIn ccc. Is \code{x} an array+?
#' @export
iarr <- function(x) {is.array(x) | is.vector(x)}

#' @describeIn ccc. Is \code{x} a data.frame?
#' @export
idtf <- function(x) {is.data.frame(x)}

#' @describeIn ccc. Is \code{x} a generic?
#' @export
igen <- function(x) {is.vector(x) | is.array(x) | (is.list(x) & !is.data.frame(x))}

#' @describeIn ccc. Is \code{x} a matrix?
#' @export
imat <- function(x) {is.matrix(x)}

#' @describeIn ccc. Is \code{x} a multivec?
#' @export
imvc <- function(x) {length(x) > 1 & (is.vector(x) | (is.array(x) & length(which(dim(x) > 1)) == 1))}

#' @describeIn ccc. Is \code{x} a scalar?
#' @export
iscl <- function(x) {length(x) == 1 & (is.array(x) | is.vector(x))}

#' @describeIn ccc. Is \code{x} a vector+?
#' @export
ivec <- function(x) {length(x) > 0 & (is.vector(x) | (is.array(x) & length(which(dim(x) > 1)) <= 1))}

#' @describeIn ccc. Is \code{x} a vlist?
#' @export
ivls <- function(x) {is.list(x) & !is.data.frame(x)}

#' @describeIn ccc. Get a character vector of all possible atomic extended
#'   classes.
#' @export
ccc_vals <- function() {c('arr', 'dtf', 'gen', 'mat', 'mvc', 'scl', 'vec', 'vls')}

#' @describeIn ccc. Gets a vector of properties from \code{ccc_vals()} that are
#'   applicable to \code{x}.
#' @export
ccc <- function(x) {
  c(if (iarr(x)) {'arr'} else {NULL}, if (idtf(x)) {'dtf'} else {NULL},
    if (igen(x)) {'gen'} else {NULL}, if (imat(x)) {'mat'} else {NULL},
    if (imvc(x)) {'mvc'} else {NULL}, if (iscl(x)) {'scl'} else {NULL},
    if (ivec(x)) {'vec'} else {NULL}, if (ivls(x)) {'vls'} else {NULL})
}

#' @describeIn ccc. Evaluates whether any (combination) property in \code{ccc}
#'   is an extended class property applicable to \code{x}.
#' @export
iccc <- function(x, ccc, ...) {
  if (!cmp_chr_scl(ccc)) {stop("\n • [ccc] must be a complete character scalar (?cmp_chr_scl).")}
  valid <- ccc_vals()
  combos <- av(strsplit(ccc, "|", fixed = T))
  newccc <- av(strsplit(combos, "_", fixed = T))
  valid <- all(newccc %in% valid)
  if (!valid) {stop("\n • [ccc] contains a value not in ccc_vals(), after splitting [ccc] on pipes and underscores.")}
  ippp(x, ccc, ...)
}
