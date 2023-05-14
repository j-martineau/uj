#' @name pals
#' @encoding UTF-8
#' @family plots
#' @title Plotting palettes
#' @description Build line type, shape, and color palettes.
#' @details The functions in this family create palettes of various types from values of source vectors as defined below:
#' \tabular{lll}{*Function*             \tab *Palette Type*                     \tab *Source*              \cr
#'               `line_types`           \tab line types                         \tab `v(line.types)`       \cr
#'               `open_shapes`          \tab `+ x ~ ^ v < >` as shapes          \tab `v(open.shapes)`      \cr
#'               `punc_shapes`          \tab `! @ # $ % & ?` as shapes          \tab `v(punc.shapes)`      \cr
#'               `fill_shapes`          \tab The `5` R fillable shapes          \tab `v(fill.shapes)`      \cr
#'               `empty_shapes`         \tab The `5` R empty shapes             \tab `v(empty.shapes)`     \cr
#'               `solid_shapes`         \tab The `4` R solid shapes             \tab `v(solid.shapes)`     \cr
#'               `digit_shapes`         \tab `1` to `9` as shapes               \tab `v(digit.shapes)`     \cr
#'               `digit_shapes0`        \tab `0` to `9` as shapes               \tab `v(digit.shapes0)`    \cr
#'               `letter_shapes`        \tab `a` to `z` as shapes               \tab `v(letter.shapes)`    \cr
#'               `LETTER_shapes`        \tab `A` to `Z` as shapes               \tab `v(LETTER.shapes)`    \cr
#'               `colors_standard`      \tab 10 distinctive colors              \tab `v(colors.standard)`  \cr
#'               `colors_sensitive`     \tab 10 colorblind sensitive colors     \tab `v(colors.sensitive)` \cr
#'               `build_palette`        \tab *any of the above*                 \tab *any of the above*    \cr
#'               `blank_shape`          \tab Space as a shape                   \tab `v(blank.shape)`        }
#' *NOTE*: `UnqN` defaults to the smaller of `TotN` or the number of unique values available for the palette. If `UnqN` is larger than the number of unique values available for the palette, an error is generated, except in `create_palette` where `UnqN` is reduced to the number of available unique values if necessary.
#' @param Type A character scalar from `c('line.types', 'open.shapes', 'punc.shapes', 'fill.shapes','empty.shapes', 'solid.shapes', 'digit.shapes', 'digit.shapes0', 'letter.shapes', 'LETTER.shapes', 'colors.standard', 'colors.sensitive')`.
#' @param TotN A \link[=cmp_psw_scl]{complete positive whole-number scalar} indicating the number of values to return.
#' @param UnqN A positive whole-number scalar less than or equal to `TotN` giving the number of unique values to be repeated until there are `TotN` total values.
#' @return **A positive whole number vector** \cr\cr `fill_shapes`, `open_shapes`, `punc_shapes`, `empty_shapes`, `solid_shapes`, `digit_shapes`, `digit_shapes0`, `letter_shapes`, `LETTER_shapes,` *and possibly* `build_palette`
#' \cr\cr  **A character vector**             \cr\cr `line_types`, `colors_standard`, `colors_sensitive` *and possibly* `build_palette`
#' \cr\cr  **A positive whole number scalar** \cr\cr `blank_shape`
#' @export
pals <- function() {utils::help("pals", "uj")}

#' @rdname pals
#' @export
line_types <- function(TotN, UnqN = min(9, TotN)) {
  uj:::.pal_errs(TotN, UnqN, 9, uj::callers())
  uj:::.pal_vals("linetypes", TotN, UnqN)
}

#' @rdname pals
#' @export
fill_shapes <- function(TotN, UnqN = min(5, TotN)) {
  uj:::.pal_errs(TotN, UnqN, 5, uj::callers())
  uj:::.pal_vals("fill.shapes", TotN, UnqN)
}

#' @rdname pals
#' @export
open_shapes <- function(TotN, UnqN = min(7, TotN)) {
  uj:::.pal_errs(TotN, UnqN, 7, uj::callers())
  uj:::.pal_vals("open.shapes", TotN, UnqN)
}

#' @rdname pals
#' @export
punc_shapes <- function(TotN, UnqN = min(7, TotN)) {
  uj:::.pal_errs(TotN, UnqN, 7, uj::callers())
  uj:::.pal_vals("punc.shapes", TotN, UnqN)
}

#' @rdname pals
#' @export
empty_shapes <- function(TotN, UnqN = min(5, TotN)) {
  uj:::.pal_errs(TotN, UnqN, 5, uj::callers())
  uj:::.pal_vals("empty.shapes", TotN, UnqN)
}

#' @rdname pals
#' @export
solid_shapes <- function(TotN, UnqN = min(4, TotN)) {
  uj:::.pal_errs(TotN, UnqN, 4, uj::callers())
  uj:::.pal_vals("empty.shapes", TotN, UnqN)
}

#' @rdname pals
#' @export
digit_shapes <- function(TotN, UnqN = min(9, TotN)) {
  uj:::.pal_errs(TotN, UnqN, 9, uj::callers())
  uj:::.pal_vals("digit.shapes", TotN, UnqN)
}

#' @rdname pals
#' @export
digit_shapes0 <- function(TotN, UnqN = min(10, TotN)) {
  uj:::.pal_errs(TotN, UnqN, 10, uj::callers())
  uj:::.pal_vals("digit.shapes0", TotN, UnqN)
}

#' @rdname pals
#' @export
letter_shapes <- function(TotN, UnqN = min(26, TotN)) {
  uj:::.pal_errs(TotN, UnqN, 26, uj::callers())
  uj:::.pal_vals("letter.shapes", TotN, UnqN)
}

#' @rdname pals
#' @export
LETTER_shapes <- function(TotN, UnqN = min(26, TotN)) {
  uj:::.pal_errs(TotN, UnqN, 26, uj::callers())
  uj:::.pal_vals("LETTER.shapes", TotN, UnqN)
}

#' @rdname pals
#' @export
colors_standard <- function(TotN, UnqN = min(10, TotN)) {
  uj:::.pal_errs(TotN, UnqN, 10, uj::callers())
  uj:::.pal_vals("colors.standard", TotN, UnqN)
}

#' @rdname pals
#' @export
colors_sensitive <- function(TotN, UnqN = min(10, TotN)) {
  uj:::.pal_errs(TotN, UnqN, 10, uj::callers())
  uj:::.pal_vals("colors.sensitive", TotN, UnqN)
}

#' @rdname pals
#' @export
build_palette <- function(Type, TotN, UnqN) {
  ValidTypes <- base::c('colors.sensitive', 'colors.standard', 'linetypes', 'letter.shapes', 'LETTER.shapes', 'digit.shapes0', 'digit.shapes', 'open.shapes', 'punc.shapes', 'empty.shapes', 'fill.shapes', 'solid.shapes')
  ValidUnqNs <- base::c(colors.sensitive = 10, colors.standard = 10, linetypes = 9, letter.shapes = 26, LETTER.shapes = 26, digit.shapes0 = 10, digit.shapes = 9, open.shapes = 7, punch.shapes = 7, empty.shapes = 5, fill.shapes = 5, solid.shapes = 4)
  Errs <- NULL
  if      (!uj::.cmp_chr_scl(Type)) {Errs <- base::c(Errs, "[Type] must be a character scalar in c('colors.sensitive', 'colors.standard', 'linetypes', 'letter.shapes', 'LETTER.shapes', 'digit.shapes0', 'digit.shapes', 'open.shapes', 'punc.shapes', 'empty.shapes', 'fill.shapes', 'solid.shapes').")}
  else if (!(Type %in% ValidTypes)) {Errs <- base::c(Errs, "[Type] must be a character scalar in c('colors.sensitive', 'colors.standard', 'linetypes', 'letter.shapes', 'LETTER.shapes', 'digit.shapes0', 'digit.shapes', 'open.shapes', 'punc.shapes', 'empty.shapes', 'fill.shapes', 'solid.shapes').")}
  if      (!uj::.cmp_psw_scl(TotN)) {Errs <- base::c(Errs, "[TotN] must be positive whole-number scalar.")}
  if      (!uj::.cmp_psw_scl(UnqN)) {Errs <- base::c(Errs, "[UnqN] must be positive whole-number scalar.")}
  if (!base::is.null(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (UnqN > TotN) {uj::stopperr("[UnqN] is greater than [TotN]", PKG = "uj")}
  UnqN < base::min(UnqN, ValidUnqNs[Type])
  uj::pal_vals(Type, TotN, UnqN)
}

#' @rdname pals
#' @export
blank_shape <- function() {uj::v(blank.shape)}

