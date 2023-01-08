#' @encoding UTF-8
#' @family properties
#' @title Shape properties
#' @description Shape properties are defined as follows: \tabular{rll}{
#'       `'emp'`   \tab empty  \tab   Of length `0` (but not `NULL`).
#'   \cr `'pnt'`   \tab point  \tab   Of length `1`\eqn{^a}.
#'   \cr `'col'`   \tab column \tab   `1 × 2+` matrix/data.frame.
#'   \cr `'row'`   \tab row    \tab   `2+ × 1` matrix/data.frame
#'   \cr `'lin'`   \tab linear \tab   `2+` positions in `1` dimension\eqn{^b}.
#'   \cr `'rct'`   \tab rect   \tab   `2+` positions in `2` dimensions.
#'   \cr `'sqr'`   \tab square \tab   `N × N` matrix where `N ≥ 2`.
#'   \cr `'sld'`   \tab solid  \tab   `2+` positions in `3+` dimensions.
#' }
#'     \eqn{^{a.}} Includes `1x1` data.frames.
#' \cr \eqn{^{b.}} Length-`2+` vectors, arrays of `2+` positions in `1` dimension, row/column data.frames.
#' \cr\cr
#' **Shape property functions**
#' \tabular{rl}{
#'     `is_sss_spec`   \tab Is `spec` a shape specification?
#'   \cr `sss_props`   \tab What shape properties are there?
#'   \cr  `sss_funs`   \tab What shape property functions are there?
#'   \cr      `isss`   \tab Does `x` match the shape spec in argument `spec`?
#'   \cr      `iSSS`   \tab Does `x` match single shape property `'SSS'`?
#'   \cr       `sss`   \tab What are `x`'s shape properties?
#' }
#' @param x An R object.
#' @param spec Either `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more `shape` properties (i.e., from `sss_vals()`). `Shape` properties in `spec` may be pipe-delimited. If there are multiple `shape` properties in `spec`, `x` is inspected for any match to any shape property in `spec`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return *A logical scalar*
#'   \cr   `is_sss_spec`
#'   \cr   `iSSS`
#'   \cr   `isss`
#'   \cr
#'   \cr *A character vector*
#'   \cr   `sss_props`
#'   \cr   `sss_funs`
#' @examples
#' sss_funs()
#' sss_props()
#' is_sss_spec("emp|pnt")
#' sss(matrix(letters, nrow = 1))
#' isss(matrix(letters, nrow = 2), "rct")
#' ipnt(data.frame(a = 1))
#' @export
sss <- function(x) {
  out <- NULL
  for (s in uj:::.ssss) {out <- base::c(out, uj::f0(uj::run('uj:::.i', s, '(x)'), s, NULL))}
  out
}

#' @rdname sss
#' @export
sss_props <- function() {uj:::.ssss}

#' @rdname sss
#' @export
sss_funs <- function() {base::paste0("i", uj:::.ssss)}

#' @rdname sss
#' @export
is_sss_spec <- function(spec) {spec <- uj:::.spec_vals(spec); uj::f0(base::length(spec) == 0, F, base::all(spec %in% .ssss))}

#' @rdname sss
#' @export
isss <- function(x, spec, ...) {
  errs <- base::c(uj:::.meets_errs(x, ...), uj::f0(uj::is_sss_spec(spec), NULL, '[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from sss_props().'))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  if (!uj::meets(x, ...)) {base::return(F)}
  for (prop in uj:::.spec_vals(spec)) {if (uj::run('uj:::.i', prop, '(x)')) {base::return(T)}}
  F
}

#' @rdname sss
#' @export
icol <- function(x, ...) {uj::isss(x, 'col', ...)}

#' @rdname sss
#' @export
iemp <- function(x, ...) {uj::isss(x, 'emp', ...)}

#' @rdname sss
#' @export
ilin <- function(x, ...) {uj::isss(x, 'lin', ...)}

#' @rdname sss
#' @export
ipnt <- function(x, ...) {uj::isss(x, 'pnt', ...)}

#' @rdname sss
#' @export
irct <- function(x, ...) {uj::isss(x, 'rct', ...)}

#' @rdname sss
#' @export
irow <- function(x, ...) {uj::isss(x, 'row', ...)}

#' @rdname sss
#' @export
isld <- function(x, ...) {uj::isss(x, 'sld', ...)}

#' @rdname sss
#' @export
isqr <- function(x, ...) {uj::isss(x, 'sqr', ...)}
