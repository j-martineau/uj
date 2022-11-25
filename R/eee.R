.ie0D <- function(x) {NROW(x) * NCOL(x) == 1 & length(x) == 1}
.ie1D <- function(x) {1 %in% c(NROW(x), NCOL(x)) & NROW(x) * NCOL(x) > 1}
.ie2D <- function(x) {f0(!is.array(x) & !is.data.frame(x), F, length(which(dim(x) > 1)) == 2)}
.ieHD <- function(x) {f0(!is.array(x), F, length(which(dim(x) > 1)) > 2)}
.ieUD <- function(x) {length(x) == 0}
.eees <- c("e0D", "e1D", "e2D", "eHD", "eUD")

#' @name eee
#' @family props
#' @title Effective dimensionality properties
#' @description Effective dimensionality of a non-empty object is defined as
#'   the number of dimensions with multiple indexed positions. Effective
#'   dimensionality is undefined for empty objects. The following table
#'   describes values of effective dimensionality, property names assigned to
#'   them, and a definition of those values.\tabular{llll}{
#'   NUMBER OF    \tab EFFECTIVE        \tab EFFECTIVE
#'                                      \tab CHARACTERISTICS\cr
#'   EFFECTIVE    \tab DIMENSIONALITY   \tab DIMENSIONALITY   
#'                                      \tab OF QUALIFYING\cr
#'   DIMENSIONS   \tab PROPERTY VALUE   \tab PROPERTY NAME   
#'                                      \tab OBJECTS      \cr
#'   \code{NaN}\tab\code{'eUD'}\tab Effectively dimensionally undefined
#'                             \tab \code{NULL} or of length 0.              \cr
#'   \code{0}  \tab\code{'e0D'}\tab Effectively \code{0}-dimensional
#'                             \tab Vector of length 1, vlist of length 1, array
#'                                  of length 1, or code{1 × 1} data.frame   \cr
#'   \code{1}  \tab\code{'e1D'}\tab Effectively \code{1}-dimensional
#'                             \tab Vector or vlist of length 2+ or
#'                                  non-empty array with multiple index
#'                                  positions in \code{1} dimension.         \cr
#'   \code{2}  \tab\code{'e2D'}\tab Effectively \code{2}-dimensional
#'                             \tab Data frames or matrices with multiple rows
#'                                  and multiple columns and non-empty arrays
#'                                  with multiple index positions in exactly
#'                                  \code{2} dimensions.                     \cr
#'   \code{≥ 3}\tab\code{'eHD'}\tab Effectively hyper-dimensional
#'                             \tab Non-empty array with multiple index
#'                                  positions in at least 3 dimensions.        }
#'   Functions in this family are:\tabular{ll}{
#'     FUNCTION   \tab WHAT IT DOES \cr
#'     `eee`           \tab Get a character vector containing all effective
#'                          dimensionality properties possessed by `x`       \cr
#'     `ixxx`          \tab Evaluate whether `x` possesses the effective
#'                          dimensionality property `xxx` (a placeholder for any
#'                          given effective dimensionality property value),
#'                          subject to any restrictions in `...`.            \cr
#'     `ieee`          \tab Evaluate whether `x` possesses one or more
#'                          (possibly pipe-delimited) effective dimensionality
#'                          properties in `spec`, subject to any restrictions in
#'                          `...`.                                           \cr
#'     `neee`          \tab Get the number of effective dimensions of `x`.   \cr
#'     `eee_props`     \tab Get a character vector of all possible effective
#'                          dimensionality property values.                  \cr
#'     `is_eee_spec`   \tab Evaluate whether `spec` is a valid effective
#'                          dimensionality property specification.             }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec}
#'   containing one or more effective dimensionality properties (i.e., from
#'   `eee_props()`). Effective dimensionality properties may be pipe-delimited.
#'   If there are multiple properties in `spec`, `x` is inspected for a match
#'   to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying Count and Value Restrictions
#' @return \tabular{ll}{
#'   FUNCTIONS                       \tab RETURN VALUE                       \cr
#'   `eee_vals`                      \tab A character vector.                \cr
#'   `eee`                           \tab A character scalar or vector.      \cr
#'   `neee`                          \tab A numeric scalar.                  \cr
#'   `ieee`, `ixxx`, `is_eee_spec`   \tab A logical scalar.                    }
#' @export
eee <- function(x) {
  out <- NULL
  for (e in .eees) {out <- c(out, f0(run('.i', e, '(x)'), e, NULL))}
  out
}

#' @rdname eee
#' @export
eee_props <- function() {.eees}

#' @rdname eee
#' @export
is_eee_spec <- function(spec) {spec <- .spec_vals(spec); f0(length(spec) == 0, F, all(spec %in% .eees))}

#' @rdname eee
#' @export
ieee <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...), f0(is_eee_spec(spec), NULL, '\n \u2022 [spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from eee_props().'))
  if (!is.null(errs)) {stop(errs)}
  if (!meets(x, ...)) {return(F)}
  for (prop in .spec_vals(spec)) {if (run('.i', prop, '(x)')) {return(T)}}
  F
}

#' @rdname eee
#' @export
ie0D <- function(x, ...) {ieee(x, 'e0D', ...)}

#' @rdname eee
#' @export
ie1D <- function(x, ...) {ieee(x, 'e1D', ...)}

#' @rdname eee
#' @export
ie2D <- function(x, ...) {ieee(x, 'e2D', ...)}

#' @rdname eee
#' @export
ieHD <- function(x, ...) {ieee(x, 'eHD', ...)}

#' @rdname eee
#' @export
ieUD <- function(x, ...) {ieee(x, 'eUD', ...)}

#' @rdname eee
#' @export
neee <- function(x) {f0(length(x) == 0, NaN, f0(NROW(x) * NCOL(x) == 1, 0, f0(is.vector(x), 1, length(which(dim(x) > 1)))))}
