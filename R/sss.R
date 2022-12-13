#' @name sss
#' @family props
#' @title Shape properties
#' @description **Shape properties** \tabular{rcl}{
#'       `'emp'`   \tab empty  \tab   `0` length (but not `NULL`).
#'   \cr `'pnt'`   \tab point  \tab   `1` element or cell\eqn{^1}.
#'   \cr `'col'`   \tab column \tab   `1` row by `2+` columns.
#'   \cr `'row'`   \tab row    \tab   `2+` rows by `1` column.
#'   \cr `'lin'`   \tab linear \tab   `2+` positions in 1 dimension\eqn{^2}.
#'   \cr `'rct'`   \tab rect   \tab   `2+` positions in `2` dimensions.
#'   \cr `'sqr'`   \tab square \tab   `N × N` matrix where `N ≥ 2`.
#'   \cr `'sld'`   \tab solid  \tab   `2+` positions in `3+` dimensions.
#' }
#'       \eqn{^{1.}} `1x1` data.frames; length-`1` arrays, vectors, \link[=ivls]{vlists}.
#' \cr   \eqn{^{2.}} Length-`2+` vectors, arrays of `2+` positions in `1` dimension, row/column data.frames.
#' \cr\cr **Shape property functions** \tabular{rl}{
#'     `is_sss_spec`   \tab Is `spec` is a shape property spec?
#'   \cr `sss_props`   \tab Gets all possible shape properties.
#'   \cr      `isss`   \tab Does `x` match shape spec `spec`?
#'   \cr      `iSSS`   \tab Does `x` match shape property `SSS`?
#'   \cr       `sss`   \tab Gets all shape properties matched to `x`.
#' }
#' @param x An R object.
#' @param spec Either `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more shape properties (i.e., from `sss_vals()`). Shape properties in `spec` may be pipe-delimited. If there are multiple shape properties in `spec`, `x` is inspected for any match to any shape property in `spec`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \tabular{rl}{
#'     `is_sss_spec`   \tab A logical
#'   \cr      `iSSS`   \tab scalar.
#'   \cr      `isss`   \tab   
#'   \cr               \tab   
#'   \cr  `sss_vals`   \tab A character
#'   \cr       `sss`   \tab vector.
#' }
#' @examples
#' is_sss_spec("emp|pnt")
#' sss_props()
#' sss(matrix(letters, nrow = 1))
#' isss(matrix(letters, nrow = 2), "rct")
#' ipnt(data.frame(a = 1))
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
  errs <- c(.meets_errs(x, ...), f0(is_sss_spec(spec), NULL, '[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from sss_props().'))
  if (!is.null(errs)) {stop(.errs(errs))}
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
