#' @name pals_uj
#' @family plots
#' @family colors
#' @title Plotting Palettes
#' @section Functions in This Family:
#'   \strong{\code{pal_colors}}
#'   \cr Creates a color palette with the first \code{n} color values from
#'   \code{v(colors)}.
#'   \cr\cr
#'   \strong{\code{pal_lines}}
#'   \cr Creates a line-type palette with the first \code{n} line-type values
#'   from \code{v(lines)}.
#'   \cr\cr
#'   \strong{\code{pal_edots}}
#'   \cr Creates an empty-dots/shapes palette with the first \code{n} empty-dot
#'   values from \code{v(edots)}. Empty dots are closed shapes with an unfilled
#'   interior.
#'   \cr\cr
#'   \strong{\code{pal_fdots}}
#'   \cr Creates a fillable-dots/shapes palette with the first \code{n}
#'   empty-dot values from \code{v(fdots)}. Fillable dots are closed shapes with
#'   a fillable interior.
#'   \cr\cr
#'   \strong{\code{pal_ldots}}
#'   \cr Creates a line-only dots/shapes palette with the first \code{n}
#'   line-only dot values from \code{v(ldots)}. Line-only dots are non-enclosing
#'   shapes.
#'   \cr\cr
#'   \strong{\code{pal_sdots}}
#'   \cr Creates a solid-dots/shapes palette with the first first \code{n}
#'   solid-dot values from \code{v(sdots)}.
#'   \cr\cr
#'   NOTE: If \code{n > v(.)}, values are recycled.
#' @param n \link[=cmp_psw_scl]{Complete positive whole-number scalar}
#'   indicating the number of unique values to return.
#' @return \strong{\code{pal_colors, pal_lines}}
#'   \cr A character vector.
#'   \cr\cr
#'   \strong{\code{pal_edots, pal_fdots, pal_ldots, pal_sdots}}
#'   \cr A positive whole number vector.
#' @export
pals_uj <- NULL

#' @rdname pals_uj
#' @export
pal_colors <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  first_n(n, v(colors))
}

#' @rdname pals_uj
#' @export
pal_lines <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  first_n(n, v(lines))
}

#' @rdname pals_uj
#' @export
pal_fdots <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  first_n(n, v(fdots))
}

#' @rdname pals_uj
#' @export
pal_ldots <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  first_n(n, v(ldots))
}

#' @rdname pals_uj
#' @export
pal_edots <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  first_n(n, v(edots))
}

#' @rdname pals_uj
#' @export
pal_sdots <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  first_n(n, v(sdots))
}
