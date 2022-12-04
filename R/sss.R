#' @name sss
#' @family props
#' @title Shape properties
#' @description Shape property values, names, and qualifying objects are as follows:
#' \itemize{
#'   \item **`'emp'`** (empty): non-`NULL`, length-`1` objects.
#'   \item **`'pnt'`** (point): `1 x 1` data/frames and arrays, vectors, \link[=ivls]{vlists} with `1` element.
#'   \item **`'col'`** (column): `1 x 2+` data.frames and matrices.
#'   \item **`'row'`** (row): `2+ x 1` data.frames and matrices.
#'   \item **`'lin'`** (linear): length `2+` vectors and vlists, length `2+` arrays elements in just `1` dimension, and column and row data.frames and matrices.
#'   \item **`'rct'`** (rectangular): `2+ x 2+` data.frames and matrices.
#'   \item **`'sqr'`** (square): `n × n` matrices where `n > 1`.
#'   \item **`'sld'`** (solid): arrays with `2+` elements in `3+` dimensions.
#' }
#' **Functions**
#' \itemize{
#'   \item **`sss`**: gets a vector of shape properties applicable to `x`.
#'   \item **`isss`**: evaluates whether `x` has one or more (possibly pipe-delimited) shape properties in `spec`, subject to any restrictions in `...`.
#'   \item **`ixxx`**: evaluates whether `x` has the shape property `xxx` subject to any restrictions in `...` (where `xxx` is a placeholder for any shape property).
#'   \item **`sss_props`**: gets a vector of all possible shape properties.
#'   \item **`is_sss_spec`**: evaluates whether `spec` is a valid shape property specification.
#' }
#' @param x An R object.
#' @param spec Either`NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more shape properties (i.e., from `sss_vals()`). Shape properties in `spec` may be pipe-delimited. If there are multiple shape properties in `spec`, `x` is inspected for any match to any shape property in `spec`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \itemize{
#'   \item **`sss`**: a character scalar/vector.
#'   \item **`sss_vals`**: a character vector.
#'   \item **`ixxx, isss, is_sss_spec`**: a logical scalar.
#' }
#' @export
sss <- function(x) {
  out <- NULL
  for (s in .ssss) {out <- c(out, f0(run('.i', s, '(x)'), s, NULL))}
  out
}

#' @rdname sss
#' @export
sss_props <- function() {.ssss}

#' @rdname sss
#' @export
is_sss_spec <- function(spec) {spec <- .spec_vals(spec); f0(length(spec) == 0, F, all(spec %in% .ssss))}

#' @rdname sss
#' @export
isss <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...), f0(is_sss_spec(spec), NULL, '\n \u2022 [spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from sss_props().'))
  if (!is.null(errs)) {stop(errs)}
  if (!meets(x, ...)) {return(F)}
  for (prop in .spec_vals(spec)) {if (run('.i', prop, '(x)')) {return(T)}}
  F
}

#' @rdname sss
#' @export
icol <- function(x, ...) {isss(x, 'col', ...)}

#' @rdname sss
#' @export
iemp <- function(x, ...) {isss(x, 'emp', ...)}

#' @rdname sss
#' @export
ilin <- function(x, ...) {isss(x, 'lin', ...)}

#' @rdname sss
#' @export
ipnt <- function(x, ...) {isss(x, 'pnt', ...)}

#' @rdname sss
#' @export
irct <- function(x, ...) {isss(x, 'rct', ...)}

#' @rdname sss
#' @export
irow <- function(x, ...) {isss(x, 'row', ...)}

#' @rdname sss
#' @export
isld <- function(x, ...) {isss(x, 'sld', ...)}

#' @rdname sss
#' @export
isqr <- function(x, ...) {isss(x, 'sqr', ...)}
