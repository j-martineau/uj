.pals_errs <- function(n) {f0(cmp_psw_scl(n), NULL, .errs("[n] must be a positive whole-number scalar."))}

.pals_vals <- function(type, n) {
  x <- run("v(", type, ")")
  while (length(x) < n) {x <- c(x, x)}
  x[1:n]
}

#' @name pals
#' @encoding UTF-8
#' @family plots
#' @title Plotting palettes
#' @description This family of functions uses `'dots'` to subsume plotting to `'points'`, `'shapes'`, and `'pch'`.
#' \cr\cr The functions in this family create palettes of various types from the first `n`\eqn{^a} values of source vectors as defined below:
#' \tabular{rll}{
#'         **Function**   \tab   **Type**            \tab   **Source**
#'   \cr                  \tab                       \tab  
#'   \cr   `pal_colors`   \tab   colors              \tab   `v(colors)`
#'   \cr                  \tab                       \tab  
#'   \cr    `pal_lines`   \tab   line types          \tab   `v(lines)`
#'   \cr                  \tab                       \tab  
#'   \cr    `pal_adots`   \tab   alpha dots          \tab   `v(adots)`
#'   \cr    `pal_Adots`   \tab   ALPHA dots          \tab   `v(Adots)`
#'   \cr    `pal_ndots`   \tab   numeral dots        \tab   `v(ndots)`
#'   \cr    `pal_pdots`   \tab   punctuation dots    \tab   `v(pdots)`
#'   \cr                  \tab                       \tab  
#'   \cr    `pal_edots`   \tab   empty dots\eqn{^c}  \tab   `v(edots)`
#'   \cr    `pal_fdots`   \tab   filled dots\eqn{^d} \tab   `v(fdots)`
#'   \cr    `pal_ldots`   \tab   line dots\eqn{^b}   \tab   `v(ldots)`
#'   \cr    `pal_sdots`   \tab   solid dots          \tab   `v(sdots)`
#' }
#' \eqn{^{a.}} Recycled if `n > length(v(.))`.
#' \cr\eqn{^{b.}} Non-enclosing shape.
#' \cr\eqn{^{c.}} Enclosing shape with empty interior.
#' \cr\eqn{^{d.}} Enclosing shape with filled interior.
#' @param n A \link[=cmp_psw_scl]{complete positive whole-number scalar} indicating the number of unique values to return.
#' @return *A character vector*
#'   \cr    `pal_colors, pal_lines`
#'   \cr    `pal_adots, pal_Adots`
#'   \cr    `pal_ndots, pal_pdots`
#'   \cr\cr *A positive whole number vector*
#'   \cr    `pal_edots, pal_fdots`
#'   \cr    `pal_ldots, pal_sdots`
#' @export
pal_colors <- function(n) {f0(is.null(.pals_errs(n)), .pals_vals("colors", n), stop(.pals_errs(n)))}

#' @rdname pals
#' @export
pal_lines <- function(n) {f0(is.null(.pals_errs(n)), .pals_vals("lines", n), stop(.pals_errs(n)))}

#' @rdname pals
#' @export
pal_fdots <- function(n) {f0(is.null(.pals_errs(n)), .pals_vals("fdots", n), stop(.pals_errs(n)))}

#' @rdname pals
#' @export
pal_ldots <- function(n) {f0(is.null(.pals_errs(n)), .pals_vals("ldots", n), stop(.pals_errs(n)))}

#' @rdname pals
#' @export
pal_edots <- function(n) {f0(is.null(.pals_errs(n)), .pals_vals("edots", n), stop(.pals_errs(n)))}

#' @rdname pals
#' @export
pal_sdots <- function(n) {f0(is.null(.pals_errs(n)), .pals_vals("sdots", n), stop(.pals_errs(n)))}
