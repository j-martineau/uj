.pals_errs <- function(n) {f0(cmp_psw_scl(n), NULL, .errx("[n] must be a positive whole-number scalar."))}

.pals_vals <- function(type, n) {
  x <- run("v(", type, ")")
  while (length(x) < n) {x <- c(x, x)}
  x[1:n]
}

#' @name pals
#' @family colors
#' @family plotting
#' @title Plotting palettes
#' @description This family of functions refers to `points` and `shapes` as dots.
#' \cr\cr The functions in this family create palettes of various types from the first `n`\eqn{^1} values of source vectors as defined below:\tabular{rcl}{
#'     \strong{Function}   \tab \strong{ Type}      \tab   \strong{Source}
#'   \cr    `pal_colors`   \tab colors              \tab   `v(colors)`
#'   \cr     `pal_lines`   \tab line types          \tab   `v(lines)`
#'   \cr     `pal_sdots`   \tab solid dots          \tab   `v(sdots)`
#'   \cr     `pal_ldots`   \tab line dots\eqn{^2}   \tab   `v(ldots)`
#'   \cr     `pal_edots`   \tab empty dots\eqn{^3}  \tab   `v(edots)`
#'   \cr     `pal_fdots`   \tab filled dots\eqn{^4} \tab   `v(fdots)`
#' }
#'    \eqn{^{1.}} Recycled if `n > length(v(.))`.
#' \cr\eqn{^{2.}} Non-enclosing.
#' \cr\eqn{^{3.}} Enclosing with empty interior.
#' \cr\eqn{^{4.}} Enclosing with filled interior.
#' @param n A \link[=cmp_psw_scl]{complete positive whole-number scalar} indicating the number of unique values to return.
#' @return \tabular{rl}{
#'      `pal_colors`   \tab A character vector.
#'   \cr `pal_lines`   \tab A character vector.
#'   \cr `pal_edots`   \tab A positive whole-number vector.
#'   \cr `pal_fdots`   \tab A positive whole-number vector.
#'   \cr `pal_ldots`   \tab A positive whole-number vector.
#'   \cr `pal_sdots`   \tab A positive whole-number vector.
#' }
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
