#' @name ddim
#' @family props
#' @title Defined Dimensionality (ddim) Property Family
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
#' @details \strong{\code{ddim_vals}}
#'   \cr Gets all valid defined dimensionality property values.
#'   \cr\cr
#'   \strong{\code{ddim}}
#'   \cr Gets the \strong{number} of defined dimensions of \code{x} (i.e., not a
#'   character scalar ddim property from \code{ddim_vals}).
#'   \cr\cr
#'   \strong{\code{ddims}}
#'   \cr Gets a vector of all defined dimensionality properties from
#'   \code{ddim_vals()} that are applicable to \code{x}.
#'   \cr\cr
#'   \strong{\code{is_ddim}}
#'   \cr Evaluates whether one of the defined dimensionality properties in
#'   \code{ddims} is applicable to \code{x}.
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
#' @param ddims A character scalar containing one or more values from
#'   \code{ddim_vals()} separated by pipes and/or underscores.
#'   Combinations of ddim properties can be specified by separating them with
#'   underscores. Separating ddim properties or combinations of ddim properties
#'   with pipes will result in a value of \code{TRUE} if any of them applies to
#'   \code{x}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @return \code{ddim} returns a non-negative whole-number scalar.
#'   \code{ddim_vals} and \code{ddims} return a character scalar or vector. All
#'   others return either \code{TRUE} or \code{FALSE}.
#' @export
ddim_vals <- function() {
  x <- c('d0D', 'd1D', 'd2D', 'dHD')
  names(x) <- rep.int("ddim", length(x))
  x
}

#' @rdname ddim
#' @export
ddim <- function(x) {f0(xnll(x), 0, f0(is.vector(x), 1, length(dim(x))))}

#' @rdname ddim
#' @export
xd0D <- function(x) {ddim(x) == 0}

#' @rdname ddim
#' @export
xd1D <- function(x) {ddim(x) == 1}

#' @rdname ddim
#' @export
xd2D <- function(x) {ddim(x) == 2}

#' @rdname ddim
#' @export
xdHD <- function(x) {ddim(x) > 2}

#' @rdname ddim
#' @export
ddims <- function(x) {
  D <- ddim(x)
  f0(D == 0, 'd0D', f0(D == 1, 'd1D', f0(D == 2, 'd2D', f0(D >= 3, 'dHD', NULL))))
}

#' @rdname ddim
#' @export
is_ddim <- function(x, ddims, ...) {
  if (!cmp_chr_scl(x)) {stop("\n  * [ddims] must be a complete character scalar.")}
  ValidVec <- ddim_vals()
  Combos   <- strsplit(ddims , "|", fixed = T)[[1]]
  PropVec  <- strsplit(Combos, ".", fixed = T)[[1]]
  Valid    <- all(PropVec %in% ValidVec)
  if (!Valid) {stop("\n  * [ddims] contains a value not in ddim_vals() after splitting [ddims] on pipes and underscores.")}
  is_prop(x, ddims, ...)
}
