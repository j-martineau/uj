.id0D <- function(x) {is.null(x)}
.id1D <- function(x) {is.vector(x)}
.id2D <- function(x) {is.matrix(x) | is.data.frame(x)}
.idHD <- function(x) {length(dim(x)) > 2}
.ddds <- c("d0D", "d1D", "d2D", "dHD")

#' @name ddd
#' @family props
#' @title Defined dimensionality properties
#' @description An object's defined dimensionality is the number of dimensions on which its elements can be indexed.
#' \itemize{
#'   \item **`'d0D'`**: `0`-dimensional structure (the `NULL` object).
#'   \item **`'d1D'`**: `1`-dimensional structure (vectors, \link[=ivls]{vlists}, and `1`-dimensional arrays).
#'   \item **`'d2D'`**: `2`-dimensional structure (data.frames and matrices).
#'   \item **`'dHD'`**: Hyper-dimensional structure (arrays with `3+` dimensions).
#' }
#' **Functions**
#' \itemize{
#'   \item **`ddd`**: gets a character vector containing all defined dimensionality properties possessed by `x`.
#'   \item **`ixxx`**: evaluates whether `x` possesses the defined dimensionality property `xxx` (a placeholder for any given defined dimensionality property value), subject to any restrictions in `...`.
#'   \item **`iddd`**: evaluates whether `x` possesses one or more (possibly pipe-delimited) defined dimensionality properties in `spec`, subject to any restrictions in `...`.
#'   \item **`nddd`**: gets the number of defined dimensions of `x`.
#'   \item **`ddd_props`**: gets a character vector of all possible defined dimensionality property values.
#'   \item **`is_ddd_spec`**: evaluates whether `spec` is a valid defined dimensionality property specification.
#' }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more defined dimensionality properties from `ddd_props()`. Defined dimensionality properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \itemize{
#'   \item **`ddd_vals`**: a character vector.
#'   \item **`ddd`**: a character scalar/vector.
#'   \item **`nddd`**: a numeric scalar.
#'   \item **`iddd, ixxx, is_ddd_spec`**: a logical scalar.
#' }
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
