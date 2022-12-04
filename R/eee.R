#' @name eee
#' @family props
#' @title Effective dimensionality properties
#' @description Effective dimensionality of a non-empty object is defined as the number of dimensions with `2+` positions. Effective dimensionality is undefined for empty objects.
#' \itemize{
#'   \item **`'eUD'`**: Effectively `NaN`-dimensional (`NULL` or otherwise of length-`0` objects; effective dimensionality is undefined).
#'   \item **`'e0D'`**: Effectively `0`-dimensional (vectors, vlists, and arrays of length `1` and `1 × 1` data.frames).
#'   \item **`'e1D'`**: Effectively `1`-dimensional (vectors or vlists of length `2+`, length `2+` arrays with `2+` positions in just `1` dimension, and `1 x 2+` or `2+ x 1` data.frames).
#'   \item **`'e2D'`**: Effectively `2`-dimensional (`2+ x 2+` data frames or matrices and length `4+` arrays with `2+` positions in just `2` dimensions).
#'   \item **`'eHD'`**: Effectively hyper-dimensional (length-`8+` array with `2+` positions in `3+` dimensions).
#' }
#' **Functions**
#' \itemize{
#'   \item **`eee`**: Gets a character vector containing all effective dimensionality properties possessed by `x`.
#'   \item **`ixxx`**: Evaluates whether `x` possesses the effective dimensionality property `xxx` (a placeholder for any given effective dimensionality property value), subject to any restrictions in `...`.
#'   \item **`ieee`**: Evaluates whether `x` possesses one or more (possibly pipe-delimited) effective dimensionality properties in `spec`, subject to any restrictions in `...`.
#'   \item **`neee`**: Get the number of effective dimensions of `x`.
#'   \item **`eee_props`**: Gets a character vector of all possible effective dimensionality property values.
#'   \item **`is_eee_spec`**: Evaluates whether `spec` is a valid effective dimensionality property specification.
#' }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more effective dimensionality properties (i.e., from `eee_props()`). Effective dimensionality properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \itemize{
#'   \item **`eee_vals`**: a character vector.
#'   \item **`eee`**: a character scalar/vector.
#'   \item **`neee`**: a numeric scalar.
#'   \item **`ieee, ixxx`**: a logical scalar.
#'   \item **`is_eee_spec`**: a logical scalar.
#' }
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
