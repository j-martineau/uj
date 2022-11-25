.id0D <- function(x) {is.null(x)}
.id1D <- function(x) {is.vector(x)}
.id2D <- function(x) {is.matrix(x) | is.data.frame(x)}
.idHD <- function(x) {length(dim(x)) > 2}
.ddds <- c("d0D", "d1D", "d2D", "dHD")

#' @name ddd
#' @family props
#' @title Defined dimensionality properties
#' @description An object's defined dimensionality is the number of dimensions
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
#'   Functions in this family are:\tabular{ll}{
#'     FUNCTION   \tab WHAT IT DOES \cr
#'     `ddd`           \tab Get a character vector containing all defined
#'                          dimensionality properties possessed by `x`       \cr
#'     `ixxx`          \tab Evaluate whether `x` possesses the defined
#'                          dimensionality property `xxx` (a placeholder for any
#'                          given defined dimensionality property value),
#'                          subject to any restrictions in `...`.            \cr
#'     `iddd`          \tab Evaluate whether `x` possesses one or more
#'                          (possibly pipe-delimited) defined dimensionality
#'                          properties in `spec`, subject to any restrictions in
#'                          `...`.                                           \cr
#'     `nddd`          \tab Get the number of defined dimensions of `x`.     \cr
#'     `ddd_props`     \tab Get a character vector of all possible defined
#'                          dimensionality property values.                  \cr
#'     `is_ddd_spec`   \tab Evaluate whether `spec` is a valid defined
#'                          dimensionality property specification.             }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec}
#'   containing one or more defined dimensionality properties (i.e., from
#'   `ddd_props()`). Defined dimensionality properties may be pipe-delimited. If
#'   there are multiple properties in `spec`, `x` is inspected for a match to
#'   any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying Count and Value Restrictions
#' @return \tabular{ll}{
#'   FUNCTIONS                       \tab RETURN VALUE                       \cr
#'   `ddd_vals`                      \tab A character vector.                \cr
#'   `ddd`                           \tab A character scalar or vector.      \cr
#'   `nddd`                          \tab A numeric scalar.
#'   `iddd`, `ixxx`, `is_ddd_spec`   \tab A logical scalar.                   }
#' @export
ddd <- function(x) {
  out <- NULL
  for (d in .ddds) {out <- c(out, f0(run('.i', d, '(x)'), d, NULL))}
  out
}

#' @rdname ddd
#' @export
ddd_props <- function() {.ddds}

#' @rdname ddd
#' @export
is_ddd_spec <- function(spec) {spec <- .spec_vals(spec); f0(length(spec) == 0, F, all(spec %in% .ddds))}

#' @rdname ddd
#' @export
iddd <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...), f0(is_ddd_spec(spec), NULL, '\n \u2022 [spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from ddd_props().'))
  if (!is.null(errs)) {stop(errs)}
  if (!meets(x, ...)) {return(F)}
  for (prop in .spec_vals(spec)) {if (run('.i', prop, '(x)')) {return(T)}}
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
