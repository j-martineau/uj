#' @name pals_uj
#' @family plots
#' @family colors
#' @title Plotting Palettes
#' @description \tabular{ll}{
#'   FUNCTION     \tab WHAT IT DOES                                          \cr
#'   pal_colors   \tab Creates a color palette with the first `n` color values
#'                     from `v(colors)`.                                     \cr
#'   pal_lines    \tab Creates a line-type palette with the first `n` line-type
#'                     values from `v(lines)`.                               \cr
#'   pal_edots    \tab Creates an empty-dots/shapes palette with the first `n`
#'                     empty-dot values from `v(edots)`. Empty dots are closed
#'                     shapes with an unfilled interior.                     \cr
#'   pal_fdots    \tab Creates a fillable-dots/shapes palette with the first `n`
#'                     empty-dot values from `v(fdots)`. Fillable dots are
#'                     closed shapes with a fillable interior.               \cr
#'   pal_ldots    \tab Creates a line-only dots/shapes palette with the first
#'                     `n` line-only dot values from `v(ldots)`. Line-only dots
#'                     are non-enclosing shapes.                             \cr
#'   pal_sdots    \tab Creates a solid-dots/shapes palette with the first first
#'                     `n` solid-dot values from `v(sdots)`.                   }
#'   NOTE: Palette values are recycled if `n > length(v(.))`.
#' @param n A \link[=cmp_psw_scl]{complete positive whole-number scalar}
#'   indicating the number of unique values to return.
#' @return \tabular{ll}{
#'   FUNCTIONS                   \tab RETURN VALUE                           \cr
#'   `pal_colors`, `pal_lines`   \tab A character vector.                    \cr
#'   `pal_edots`, `pal_fdots`    \tab A positive whole-number vector.        \cr
#'   `pal_ldots`, `pal_sdots`    \tab A positive whole-number vector.          }
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
