#' @name ddd
#' @family props
#' @title Defined Dimensionality (ddd) Property Family
#' @description An object's defined dimensionality is the number of dimensions
#'   on which its components can be indexed. The following table gives levels of
#'   defined dimensionality, property values assigned to levels of defined
#'   dimensionality, property names, and a definition of each value of defined
#'   dimensionality.
#'   \tabular{llll}{
#'     \strong{Defined Dim}      \tab\strong{Property}    \tab\strong{Property}
#'                               \tab\strong{Qualifying}
#'     \cr\strong{Dimensionality}\tab\strong{Value}       \tab\strong{Name}
#'                               \tab\strong{Objects}
#'     \cr\code{0}  \tab\code{'d0D'}\tab 0-dimensional    \tab\code{NULL}.
#'     \cr\code{1}  \tab\code{'d1D'}\tab 1-dimensional    \tab vector, vlist, or
#'                                                             1-dimensional
#'                                                             array.
#'     \cr\code{2}  \tab\code{'d2D'}\tab 2-dimensional    \tab tibble or matrix.
#'     \cr\code{≥ 3}\tab\code{'dHD'}\tab Hyper-dimensional\tab array with 3 or
#'                                                             more dimensions.
#'   }
#' @details \strong{\code{ddd_vals}}
#'   \cr Gets all valid defined dimensionality property values.
#'   \cr\cr
#'   \strong{\code{nddd}}
#'   \cr Gets the \strong{number} of defined dimensions of \code{x} (i.e., not a
#'   character scalar defined dimensionality property from \code{ddd_vals}).
#'   \cr\cr
#'   \strong{\code{ddd}}
#'   \cr Gets a vector of all defined dimensionality properties from
#'   \code{ddd_vals()} that are applicable to \code{x}.
#'   \cr\cr
#'   \strong{\code{is_ddd}}
#'   \cr Evaluates whether at least one of the defined dimensionality properties
#'   in \code{ddd} is applicable to \code{x}.
#'   \cr\cr
#'   \strong{\code{xd0D}, \code{xd1D}, \code{xd2D}, and \code{xdHD}}
#'   \cr Evaluate whether \code{x} has, respectively, \code{0}, \code{1},
#'   \code{2}, and \code{3+} defined dimensions.
#'   \cr\cr
#'   \strong{Additional Arguments in \code{...}}
#'   \cr Submitting additional arguments to \code{is_ddim} via \code{...} allows
#'   for checking not just the defined dimensionality but whether length, number
#'   of rows, number of columns, and element values meet flexible criteria.
#' @param x An object.
#' @param xxx A character scalar containing one or more values from
#'   \code{ddd_vals()} separated by pipes and/or underscores. Combinations of
#'   defined dimensionality properties can be specified by separating them with
#'   underscores. Separating defined dimensionality properties or combinations
#'   of defined dimensionality properties with pipes will result in a value of
#'   \code{TRUE} if any of them applies to \code{x}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @return \code{nddd} returns a non-negative whole-number scalar.
#'   \code{ddd_vals} and \code{ddd} return a character scalar or vector. All
#'   others return either \code{TRUE} or \code{FALSE}.
#' @export
ddd_vals <- function() {
  x <- c('d0D', 'd1D', 'd2D', 'dHD')
  names(x) <- rep.int("ddd", length(x))
  x
}

#' @rdname ddd
#' @export
nddd <- function(x) {f0(xnll(x), 0, f0(is.vector(x), 1, length(dim(x))))}

#' @rdname ddd
#' @export
xd0D <- function(x) {ddd(x) == 0}

#' @rdname ddd
#' @export
xd1D <- function(x) {ddd(x) == 1}

#' @rdname ddd
#' @export
xd2D <- function(x) {ddd(x) == 2}

#' @rdname ddd
#' @export
xdHD <- function(x) {ddd(x) > 2}

#' @rdname ddd
#' @export
ddd <- function(x) {
  D <- ddd(x)
  f0(D == 0, 'd0D', f0(D == 1, 'd1D', f0(D == 2, 'd2D', f0(D >= 3, 'dHD', NULL))))
}

#' @rdname ddd
#' @export
is_ddd <- function(x, xxx, ...) {
  if (!cmp_chr_scl(x)) {stop("\n  * [xxx] must be a complete character scalar.")}
  ValidVec <- ddd_vals()
  Combos   <- strsplit(xxx , "|", fixed = T)[[1]]
  XXX      <- strsplit(Combos, ".", fixed = T)[[1]]
  Valid    <- all(XXX %in% ValidVec)
  if (!Valid) {stop("\n  * [xxx] contains a value not in ddd_vals() after splitting [xxx] on pipes and underscores.")}
  is_xxx(x, xxx, ...)
}
