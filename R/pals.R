#' @name pals.
#' @family plots
#' @family colors
#' @title Plotting palettes
#' @param n \link[cmp_psw_scl]{Complete positive whole-number scalar} indicating
#'   the number of unique values to return.
#' @return Character vector
#' @export
pals. <- function() {help("pals.", package = "uj")}

#' @describeIn pals. Create palette with \code{n} colors from the default
#'   color palette. Colors will be recycled if \code{n} is greater than
#'   \code{length(v(colors))}.
#' @export
pal_colors <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  first_n(n, v(colors))
}

#' @describeIn pals. Create palette with \code{n} line types from the default
#'   line type palette. Line types will be recycled if \code{n} is greater than
#'   \code{length(v(lines))}.
#' @export
pal_lines <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  first_n(n, v(lines))
}

#' @describeIn pals. Create palette with \code{n} fillable dots/shapes from
#'   the default line type palette. Dots/shapes will be recycled if \code{n} is
#'   greater than \code{length(v(fdots))}.
#' @export
pal_fdots <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  first_n(n, v(fdots))
}

#' @describeIn pals. Create palette with \code{n} line-based dots/shapes from
#'   the default line type palette. Dots/shapes will be recycled if \code{n} is
#'   greater than \code{length(v(ldots))}.
#' @export
pal_ldots <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  first_n(n, v(ldots))
}

#' @describeIn pals. Create palette with \code{n} empty dots/shapes from the
#'   default line type palette. Dots/shapes will be recycled if \code{n} is
#'   greater than \code{length(v(edots))}.
#' @export
pal_edots <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  first_n(n, v(edots))
}

#' @describeIn pals. Create palette with \code{n} solid dots/shapes from the
#'   default line type palette. Dots/shapes will be recycled if \code{n} is
#'   greater than \code{length(v(sdots))}.
#' @export
pal_sdots <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  first_n(n, v(sdots))
}
