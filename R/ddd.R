#' @name ddd
#' @family props
#' @title Defined dimensionality properties
#' @description NOTE: \code{DDD} is used as a wildcard representing any given
#'   effective dimensionality property.
#'   \cr\cr
#'   An object's defined dimensionality is the number of dimensions
#'   on which its components can be indexed. The following table gives levels of
#'   defined dimensionality, property values assigned to levels of defined
#'   dimensionality, property names, and a definition of each value of defined
#'   dimensionality.\tabular{llll}{
#'           \tab DEFINED          \tab DEFINED          \tab                \cr
#'   NUMBER  \tab DIMENSIONALITY   \tab DIMENSIONALITY   \tab CHARACTERISTICS\cr
#'   OF DEFINED\tab PROPERTY       \tab PROPERTY         \tab OF QUALIFYING  \cr
#'   DIMENSIONS   \tab VALUE       \tab NAME             \tab OBJECTS        \cr
#'   \code{0}     \tab\code{'d0D'} \tab 0-dimensional structure.
#'                                 \tab \code{NULL}.                         \cr
#'   \code{1}     \tab\code{'d1D'} \tab 1-dimensional structure.
#'                                 \tab Vector, \link[=ivls]{vlist},
#'                                      1-dimensional array.                 \cr
#'   \code{2}     \tab\code{'d2D'} \tab 2-dimensional structure.
#'                                 \tab data.frame or matrix.                \cr
#'   \code{≥ 3}   \tab\code{'dHD'} \tab Hyper-dimensional structure.
#'                                 \tab Array with 3+ dimensions.              }
#'   Functions related to defined dimensionality properties are as follows:
#'   \tabular{ll}{
#'     DDD FUNCTION         \tab WHAT IT DOES                                \cr
#'     \code{iDDD}          \tab Evaluates whether \code{x} matches the
#'                               defined dimensionality property \code{DDD}
#'                               (subject to any restrictions in \code{...}).\cr
#'     \code{ddd}           \tab Gets a character vector containing all
#'                               defined dimensionality properties matching
#'                               \code{x}.                                   \cr
#'     \code{iddd}          \tab Evaluates \code{x} against the
#'                               defined dimensionality property specification
#'                               in \code{spec} (subject to any restrictions in
#'                               \code{...}).                                \cr
#'     \code{ddd_props}     \tab Gets a character vector of all possible
#'                               defined dimensionality property values.     \cr
#'     \code{is_ddd_spec}   \tab Evaluates whether \code{spec} is a valid
#'                               defined dimensionality property
#'                               specification.                                }
#' @param x An R object.
#' @param spec \code{NULL} or a \link[=cmp_chr_scl]{complete character vec}
#'   containing one or more defined dimensionality properties (i.e., from
#'   \code{ddd_vals()}). \strong{NOTE}: properties may be pipe-separated. If
#'   If there are multiple properties in \code{spec}, \code{x} is inspected for
#'   a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying Count and Value Restrictions
#' @return \strong{\code{ddd_vals}}: A character vector.
#'   \cr\cr\strong{\code{ddd}}: A character scalar or character vector.
#'   \cr\cr\strong{\code{iDDD, iddd, is_ddd_spec}}: A logical scalar.
#' @export
ddd <- function(x) {
  props <- .ddd_props()
  out <- NULL
  for (prop in props) {out <- c(out, f0(run('.is_', prop, '(x)'), prop, NULL))}
  out
}

#' @rdname ddd
#' @export
ddd_props <- function() {.ddd_props()}

#' @rdname ddd
#' @export
is_ddd_spec <- function(spec) {spec <- .spec_vals(); f0(length(spec) == 0, F, all(spec %in% .ddd_props()))}

#' @rdname ddd
#' @export
iddd <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...), f0(is_ddd_spec(spec), NULL, '\n \u2022 [spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from ddd_props().'))
  if (!is.null(errs)) {stop(errs)}
  if (!meets(x, ...)) {return(F)}
  for (prop in .spec_vals(spec)) {if (run('.is_', prop, '(x)')) {return(T)}}
  F
}

#' @rdname ddd
#' @export
id0D <- function(x, ...) {iddd(x, 'd0D', ...)}

#' @rdname ddd
#' @export
id1D <- function(x, ...) {iddd(x, 'd1D', ...)}

#' @rdname ddd
#' @export
id2D <- function(x, ...) {iddd(x, 'd2D', ...)}

#' @rdname ddd
#' @export
idHD <- function(x, ...) {iddd(x, 'dHD', ...)}

#' @rdname ddd
#' @export
nddd <- function(x) {f0(is.null(x), 0, f0(is.vector(x), 1, length(dim(x))))}
