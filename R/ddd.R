#' @name ddd.
#' @family props
#' @title Defined dimensionality properties
#' @description An object's defined dimensionality (def. dim.) is the number of
#'   dimensions on which its components can be indexed. The following table
#'   gives levels of defined dimensionality, property values assigned to levels
#'   of defined dimensionality, property names, and a definition of each value
#'   of defined dimensionality.\tabular{llll}{
#'           \tab DEFINED          \tab DEFINED          \tab                \cr
#'   NUMBER  \tab DIMENSIONALITY   \tab DIMENSIONALITY   \tab CHARACTERISTICS\cr
#'   OF DEFINED\tab PROPERTY       \tab PROPERTY         \tab OF QUALIFYING  \cr
#'   DIMENSIONS   \tab VALUE       \tab NAME             \tab OBJECTS        \cr
#'   \code{0}     \tab\code{'d0D'} \tab 0-dimensional structure
#'                                 \tab\code{NULL} \cr
#'   \code{1}     \tab\code{'d1D'} \tab 1-dimensional structure
#'                                 \tab vector, vlist, 1-dimensional array   \cr
#'   \code{2}     \tab\code{'d2D'} \tab 2-dimensional structure
#'                                 \tab \code{\link[is_dtf]{dtf}} or matrix  \cr
#'   \code{≥ 3}   \tab\code{'dHD'} \tab Hyper-dimensional structure
#'                                 \tab Array with 3+ dimensions               }
#'   Submitting additional arguments to \code{is_ddd} via \code{...} allows
#'   for checking not just the defined dimensionality but whether length, number
#'   of rows, number of columns, and element values meet flexible criteria.
#' @param x An object.
#' @param xxx \link[cmp_chr_scl]{Complete character scalar} containing one or
#'   more values from \code{ddd_vals()} separated by pipes and/or underscores.
#'   Combinations of defined dimensionality properties can be specified by
#'   separating them with underscores. Separating defined dimensionality
#'   properties or combinations of defined dimensionality properties with pipes
#'   will result in a value of \code{TRUE} if any of them applies to \code{x}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @return \code{nddd} returns a non-negative whole-number scalar.
#'   \code{ddd_vals} and \code{ddd} return a character scalar or vector. All
#'   others return either \code{TRUE} or \code{FALSE}.
#' @export
ddd. <- function() {help("ddd.", package = "uj")}

#' @describeIn ddd. Gets a vector of all defined dimensionality properties
#'   from \code{ddd_vals()} that are applicable to \code{x}.
#' @export
ddd <- function(x) {
  d <- f0(inll(x), 0, f0(is.vector(x), 1, length(dim(x))))
  f0(d == 0, 'd0D', f0(d == 1, 'd1D',
  f0(d == 2, 'd2D', f0(d >= 3, 'dHD', NULL))))
}

#' @describeIn ddd. Gets the \strong{number} of defined dimensions of
#'   \code{x} (i.e., not a character scalar defined dimensionality property
#'   from \code{ddd_vals}).
#' @export
nddd <- function(x) {f0(inll(x), 0, f0(is.vector(x), 1, length(dim(x))))}

#' @describeIn ddd. Evaluate whether \code{x} has \code{0} defined
#'   dimensions.
#' @export
id0D <- function(x) {nddd(x) == 0}

#' @describeIn ddd. Evaluate whether \code{x} has \code{1} defined dimension.
#' @export
id1D <- function(x) {nddd(x) == 1}

#' @describeIn ddd. Evaluate whether \code{x} has \code{2} defined
#'   dimensions.
#' @export
id2D <- function(x) {nddd(x) == 2}

#' @describeIn ddd. Evaluate whether \code{x} has \code{3+} defined
#'   dimensions.
#' @export
idHD <- function(x) {nddd(x) > 2}

#' @describeIn ddd. Get a character vector of all possible defined
#'   dimensionality property values.
#' @export
ddd_vals <- function() {x <- c('d0D', 'd1D', 'd2D', 'dHD'); names(x) <- rep.int("ddd", length(x)); x}

#' @describeIn ddd. Evaluates whether at least one of the defined
#'   dimensionality properties in \code{xxx} is applicable to \code{x}.
#' @export
iddd <- function(x, xxx, ...) {
  if (!cmp_chr_scl(xxx)) {stop("\n • [xxx] must be a complete character scalar.")}
  valid <- ddd_vals()
  combos <- strsplit(xxx , "|", fixed = T)[[1]]
  newxxx <- strsplit(combos, ".", fixed = T)[[1]]
  valid <- all(newxxx %in% valid)
  if (!valid) {stop("\n • [xxx] contains a value not in ddd_vals() after splitting [xxx] on pipes and underscores.")}
  ixxx(x, xxx, ...)
}
