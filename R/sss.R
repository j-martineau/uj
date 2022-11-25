.icol <- function(x) {f0(!(is.matrix(x) | is.data.frame(x)), F, nrow > 1 & ncol == 1)}
.iemp <- function(x) {length(x) == 0 & !is.null(x)}
.ilin <- function(x) {.ie1D(x)}
.ipnt <- function(x) {.ie0D(x)}
.irct <- function(x) {f0(!.ie2D(x), F, is.matrix(x) | is.data.frame(x))}
.irow <- function(x) {f0(!(is.matrix(x) | is.data.frame(x)), F, nrow == 1 & ncol > 1)}
.isld <- function(x) {.ieUD(x)}
.isqr <- function(x) {is.matrix(x) & NROW(x) > 1 & NCOL(x) > 1 & NROW(x) == NCOL(x)}
.ssss <- c("col", "emp", "lin", "pnt", "rct", "row", "sld", "sqr")

#' @name sss
#' @family props
#' @title Shape properties
#' @description An object's shape properties have to do with the number of
#'   \link[=ddd]{defined dimensions} and/or \link[=eee]{effective dimensions}
#'   with some additional restrictions as follows:\tabular{lll}{
#'   NAME          \tab VALUE     \tab QUALIFYING OBJECTS                    \cr
#'   Empty         \tab `'emp'`   \tab Non-`NULL`, length-`1` objects.       \cr
#'   Point         \tab `'pnt'`   \tab Length-`1` vectors, length-`1`
#'                                     \link[=ivls]{vlists}, length-`1` arrays,
#'                                     and `1 x 1` data.frames.              \cr
#'   Linear        \tab `'lin'`   \tab Length `2+` vectors, length `2+`
#'                                     \link[=ivls]{vlists}, length `2+` arrays
#'                                     with multiple index positions in just
#'                                     `1` dimension, and data.frames with `1`
#'                                     row and multiple columns or `1` column
#'                                     and multiple rows.                    \cr
#'   Row           \tab `'row'`   \tab Matrices and data.frames with `1` row and
#'                                     multiple columns.                     \cr
#'   Column        \tab `'col'`   \tab Matrices and data.frames with `1` column
#'                                     and multiple rows.                    \cr
#'   Rectangular   \tab `'rct'`   \tab Matrices and data.frames with multiple
#'                                     rows and multiple columns.            \cr
#'   Square        \tab `'sqr'`   \tab Matrices and data.frames with multiple
#'                                     rows, multiple columns, and the same
#'                                     number of rows and columns.           \cr
#'   Solid         \tab `'sld'`   \tab Arrays with multiple index positions in
#'                                     `3+` dimensions.                        }
#'   Functions in this family are:\tabular{ll}{
#'   FUNCTION        \tab WHAT IT DOES                                       \cr
#'   `sss`           \tab Gets a character vector containing all shape
#'                        properties possessed by `x`                        \cr
#'   `ixxx`          \tab Evaluates whether `x` possesses the shape property
#'                        `xxx` (a placeholder for any given shape property
#'                        value), subject to any restrictions in `...`.      \cr
#'   `isss`          \tab Evaluates whether `x` possesses one or more
#'                        (possibly pipe-delimited) shape properties in
#'                        `spec`, subject to any restrictions in `...`.      \cr
#'   `sss_props`     \tab Gets a character vector of all possible shape
#'                        property values.                                   \cr
#'   `is_sss_spec`   \tab Evaluates whether `spec` is a valid shape property
#'                        specification.                                       }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec}
#'   containing one or more shape properties (i.e., from `sss_vals()`). Shape
#'   properties in `spec` may be pipe-delimited. If there are multiple shape
#'   properties in `spec`, `x` is inspected for any match to any shape property
#'   in `spec`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying Count and Value Restrictions
#' @return \tabular{ll}{
#'   FUNCTIONS                       \tab RETURN VALUE                       \cr
#'   `sss_vals`                      \tab A character vector.                \cr
#'   `sss`                           \tab A character scalar or vector.      \cr
#'   `ixxx`, `isss`, `is_sss_spec`   \tab A logical scalar.                    }
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
