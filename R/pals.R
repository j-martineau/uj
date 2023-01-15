.pals_errs <- function(n) {uj::f0(uj::cmp_psw_scl(n), NULL, uj:::.errs("[n] must be a positive whole-number scalar."))}

.pals_vals <- function(type, n) {
  x <- uj::run("v(", type, ")")
  while (base::length(x) < n) {x <- c(x, x)}
  x[1:n]
}

#' @name pals
#' @encoding UTF-8
#' @family plots
#' @title Plotting palettes
#' @description This family of functions uses `'dots'` to subsume plotting to `'points'`, `'shapes'`, and `'pch'`.
#' \cr\cr The functions in this family create palettes of various types from the first `n`\eqn{^1} values of source vectors as defined below:
#' \tabular{rll}{
#'           *Function*   \tab   *Type*.             \tab   *Source*
#'   \cr   `pal_colors`   \tab   colors              \tab   `v(colors)`
#'   \cr    `pal_lines`   \tab   line types          \tab   `v(lines)`
#'   \cr    `pal_adots`   \tab   alpha dots          \tab   `v(adots)`
#'   \cr    `pal_Adots`   \tab   ALPHA dots          \tab   `v(Adots)`
#'   \cr    `pal_ndots`   \tab   numeral dots        \tab   `v(ndots)`
#'   \cr    `pal_pdots`   \tab   punctuation dots    \tab   `v(pdots)`
#'   \cr    `pal_ldots`   \tab   line dots\eqn{^2}   \tab   `v(ldots)`
#'   \cr    `pal_edots`   \tab   empty dots\eqn{^3}  \tab   `v(edots)`
#'   \cr    `pal_fdots`   \tab   filled dots\eqn{^4} \tab   `v(fdots)`
#'   \cr    `pal_sdots`   \tab   solid dots          \tab   `v(sdots)`
#' }
#' ` `\eqn{^{1.}} Recycled if `n > length(v(.))`.
#' \cr ` `\eqn{^{2.}} Non-enclosing shape.
#' \cr ` `\eqn{^{3.}} Enclosing shape with empty interior.
#' \cr ` `\eqn{^{4.}} Enclosing shape with filled interior.
#' @param n A \link[=cmp_psw_scl]{complete positive whole-number scalar} indicating the number of unique values to return.
#' @return *A character vector*
#'   \cr   `pal_colors, pal_lines`
#'   \cr   `pal_adots,  pal_Adots`
#'   \cr   `pal_ndots,  pal_pdots`
#'   \cr
#'   \cr *A positive whole number vector*
#'   \cr   `pal_edots, pal_fdots`
#'   \cr   `pal_ldots, pal_sdots`
#' @export
pal_colors <- function(n) {uj::f0(base::is.null(uj:::.pals_errs(n)), uj:::.pals_vals("colors", n), stop(uj:::.pals_errs(n)))}

#' @rdname pals
#' @export
pal_lines <- function(n) {uj::f0(base::is.null(uj:::.pals_errs(n)), uj:::.pals_vals("lines", n), stop(uj:::.pals_errs(n)))}

#' @rdname pals
#' @export
pal_fdots <- function(n) {uj::f0(base::is.null(uj:::.pals_errs(n)), uj:::.pals_vals("fdots", n), stop(uj:::.pals_errs(n)))}

#' @rdname pals
#' @export
pal_ldots <- function(n) {uj::f0(base::is.null(uj:::.pals_errs(n)), uj:::.pals_vals("ldots", n), stop(uj:::.pals_errs(n)))}

#' @rdname pals
#' @export
pal_edots <- function(n) {uj::f0(base::is.null(uj:::.pals_errs(n)), uj:::.pals_vals("edots", n), stop(uj:::.pals_errs(n)))}

#' @rdname pals
#' @export
pal_sdots <- function(n) {uj::f0(base::is.null(uj:::.pals_errs(n)), uj:::.pals_vals("sdots", n), stop(uj:::.pals_errs(n)))}
