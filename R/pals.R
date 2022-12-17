.pals_errs <- function(n) {f0(cmp_psw_scl(n), NULL, .errs("[n] must be a positive whole-number scalar."))}

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
#' \cr
#' \cr The functions in this family create palettes of various types from the first `n`\eqn{^a} values of source vectors as defined below:
#' \tabular{rll}{
#'       **Function** \tab   **Type**            \tab   **Source**
#'   \cr `pal_colors` \tab   colors              \tab   `v(colors)`
#'   \cr  `pal_lines` \tab   line types          \tab   `v(lines)`
#'   \cr  `pal_sdots` \tab   solid dots          \tab   `v(sdots)`
#'   \cr  `pal_ldots` \tab   line dots\eqn{^b}   \tab   `v(ldots)`
#'   \cr  `pal_edots` \tab   empty dots\eqn{^c}  \tab   `v(edots)`
#'   \cr  `pal_fdots` \tab   filled dots\eqn{^d} \tab   `v(fdots)`
#' }
#'     \eqn{^{a.}} Recycled if `n > length(v(.))`.
#' \cr \eqn{^{b.}} Non-enclosing shape.
#' \cr \eqn{^{c.}} Enclosing shape with empty interior.
#' \cr \eqn{^{d.}} Enclosing shape with filled interior.
#' @param n A \link[=cmp_psw_scl]{complete positive whole-number scalar} indicating the number of unique values to return.
#' @return \tabular{rl}{
#'      `pal_colors` \tab   A character vector.
#'   \cr `pal_lines` \tab   A character vector.
#'   \cr `pal_edots` \tab   A positive whole number vector.
#'   \cr `pal_fdots` \tab   A positive whole number vector.
#'   \cr `pal_ldots` \tab   A positive whole number vector.
#'   \cr `pal_sdots` \tab   A positive whole number vector.
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
