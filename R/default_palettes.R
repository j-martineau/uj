#' @name pals_uj
#' @family plots
#' @title Plotting palettes
#' @param n Number of unique values to return.
#' @return Character vector
#' @export
pals_uj <- function() {help("pals_uj", package = "uj")}

#' @describeIn pals_uj Create palette with \code{n} colors from the default
#'   color palette. Colors will be recycled if \code{n} is greater than
#'   \code{length(v(colors))}.
#' @export
pal_colors <- function(n) {first_n(n, l(colors))}

#' @describeIn pals_uj Create palette with \code{n} line types from the default
#'   line type palette. Line types will be recycled if \code{n} is greater than
#'   \code{length(v(lines))}.
#' @export
pal_lines <- function(n) {first_n(n, l(lines))}

#' @describeIn pals_uj Create palette with \code{n} fillable dots/shapes from
#'   the default line type palette. Dots/shapes will be recycled if \code{n} is
#'   greater than \code{length(v(fdots))}.
#' @export
pal_fdots <- function(n) {first_n(n, l(fdots))}

#' @describeIn pals_uj Create palette with \code{n} line-based dots/shapes from
#'   the default line type palette. Dots/shapes will be recycled if \code{n} is
#'   greater than \code{length(v(ldots))}.
#' @export
pal_ldots <- function(n) {first_n(n, l(ldots))}

#' @describeIn pals_uj Create palette with \code{n} empty dots/shapes from the
#'   default line type palette. Dots/shapes will be recycled if \code{n} is
#'   greater than \code{length(v(edots))}.
#' @export
pal_edots <- function(n) {first_n(n, l(edots))}

#' @describeIn pals_uj Create palette with \code{n} solid dots/shapes from the
#'   default line type palette. Dots/shapes will be recycled if \code{n} is
#'   greater than \code{length(v(sdots))}.
#' @export
pal_sdots <- function(n) {first_n(n, l(sdots))}
