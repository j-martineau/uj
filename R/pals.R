#' @name pals
#' @family plots
#' @family colors
#' @title Plotting palettes
#' @description \itemize{
#'   \item **`pal_colors`**: creates a color palette with the first `n` color values from `v(colors)`.
#'   \item **`pal_lines`**: creates a line-type palette with the first `n` line-type values from `v(lines)`.
#'   \item **`pal_edots`**: creates an empty-dots/shapes palette with the first `n` empty-dot values from `v(edots)` (empty dots are closed shapes with an unfilled interior).
#'   \item **`pal_fdots`**: creates a fillable-dots/shapes palette with the first `n` empty-dot values from `v(fdots)` (fillable dots are closed shapes with a fillable interior).
#'   \item **`pal_ldots`**: creates a line-only dots/shapes palette with the first `n` line-only dot values from `v(ldots)` (line-only dots are non-enclosing shapes).
#'   \item **`pal_sdots`**: creates a solid-dots/shapes palette with the first `n` solid-dot values from `v(sdots)`.
#' }
#' NOTE: Palette values are recycled if `n > length(v(.))`.
#' @param n A \link[=cmp_psw_scl]{complete positive whole-number scalar} indicating the number of unique values to return.
#' @return \itemize{
#'   \item **`pal_colors`, `pal_lines`**: a character vector.
#'   \item **`pal_edots`, `pal_fdots`**: a positive whole-number vector.
#'   \item **`pal_ldots`, `pal_sdots`**: a positive whole-number vector.
#' }
#' @export
pal_colors <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  first_n(n, v(colors))
}

#' @rdname pals
#' @export
pal_lines <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  first_n(n, v(lines))
}

#' @rdname pals
#' @export
pal_fdots <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  first_n(n, v(fdots))
}

#' @rdname pals
#' @export
pal_ldots <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  first_n(n, v(ldots))
}

#' @rdname pals
#' @export
pal_edots <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  first_n(n, v(edots))
}

#' @rdname pals
#' @export
pal_sdots <- function(n) {
  if (!cmp_psw_scl(n)) {stop("\n \u2022 [n] must be a positive whole-number scalar.")}
  first_n(n, v(sdots))
}
