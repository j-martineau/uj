#' @name pals_uj
#' @family plots
#' @family colors
#' @title Plotting palettes
#' @description NOTE: This package
#'   \tabular{ll}{
#'   \code{pal_colors}   \tab Creates a color palette with the first \code{n}
#'                            color values from \code{v(colors)}.            \cr
#'   \code{pal_lines}    \tab Creates a line-type palette with the first
#'                            \code{n} line-type values from \code{v(lines)}.\cr
#'   \code{pal_edots}    \tab Creates an empty-dots/shapes palette with the
#'                            first \code{n} empty-dot values from
#'                            \code{v(edots)}. Empty dots are closed shapes with
#'                            an unfilled interior.                          \cr
#'   \code{pal_fdots}    \tab Creates a fillable-dots/shapes palette with the
#'                            first \code{n} empty-dot values from
#'                            \code{v(fdots)}. Fillable dots are closed shapes
#'                            with a fillable interior.                      \cr
#'   \code{pal_ldots}    \tab Creates a line-only dots/shapes palette with the
#'                            first \code{n} line-only dot values from
#'                            \code{v(ldots)}. Line-only dots are non-enclosing
#'                            shapes.                                        \cr
#'   \code{pal_sdots}    \tab Creates a solid-dots/shapes palette with the first
#'                            first \code{n} solid-dot values from
#'                            \code{v(sdots)}.                                 }
#' If \code{n > v(.)}, values are recycled.
#' @param n \link[=cmp_psw_scl]{Complete positive whole-number scalar}
#'   indicating the number of unique values to return.
#' @return \tabular{lll}{
#'  \code{pal_colors},\code{pal_lines}\tab    \tab A character vector.       \cr
#'  \code{pal_edots}, \code{pal_fdots}, \code{pal_ldots}, \code{pal_sdots}\tab  
#'  \tab A positive whole number vector.                                       }
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
