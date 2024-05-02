# internals ####

.pal_errs <- function(tot, unq, max.unq, stack) {
  fun <- uj::caller()
  errs <- NULL
  if (!ppp::.cmp_psw_scl(tot)) {errs <- base::c(errs, "[tot] must be a positive whole number scalar.")}
  if (!ppp::.cmp_psw_scl(unq)) {errs <- base::c(errs, "[unq] must be a positive whole number scalar.")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, fun = fun, pkg = "uj", stack = stack)}
  if (unq > max.unq) {ppp::stopperr("[unq] is greater than the number of unique values available for the designated palette.", fun = fun, pkg = "uj", stack = stack)}
}

.pal_vals <- function(type, tot, unq) {
  vals <- uj::run("uj::v(", type, ")")[1:unq]
  while (base::length(vals) < tot) {vals <- base::c(vals, vals)}
  vals[1:tot]
}

# exported ####

#' @name pals
#' @encoding UTF-8
#' @family plots
#' @title Plotting palettes
#' @description Build line type, shape, and color palettes.
#' @details The functions in this family create palettes of various types from values of source vectors as defined below:
#' \tabular{lll}{
#'   **Function**           \tab **Palette type**                                  \tab **Source**            \cr
#'   `colors_sensitive`     \tab 6 dark + 6 bright colorblind sensitive colors     \tab `v(colors.sensitive)` \cr
#'   `colors_bright`        \tab 6 colorblind sensitive bright colors              \tab `v(colors.bright)`    \cr
#'   `colors_dark`          \tab 6 colorblind-sensitive dark colors                \tab `v(colors.dark)`      \cr
#'   `line_types`           \tab line types                                        \tab `v(line.types)`       \cr
#'   `open_shapes`          \tab `+ x ~ ^ v < >` as shapes                         \tab `v(open.shapes)`      \cr
#'   `punc_shapes`          \tab `! @ # $ % & ?` as shapes                         \tab `v(punc.shapes)`      \cr
#'   `fill_shapes`          \tab The `5` R fillable shapes                         \tab `v(fill.shapes)`      \cr
#'   `empty_shapes`         \tab The `5` R empty shapes                            \tab `v(empty.shapes)`     \cr
#'   `solid_shapes`         \tab The `4` R solid shapes                            \tab `v(solid.shapes)`     \cr
#'   `digit_shapes`         \tab `1` to `9` as shapes                              \tab `v(digit.shapes)`     \cr
#'   `digit_shapes0`        \tab `0` to `9` as shapes                              \tab `v(digit.shapes0)`    \cr
#'   `letter_shapes`        \tab `a` to `z` as shapes                              \tab `v(letter.shapes)`    \cr
#'   `LETTER_shapes`        \tab `A` to `Z` as shapes                              \tab `v(LETTER.shapes)`    \cr
#'   `build_palette`        \tab *any of the above*                                \tab *any of the above*    \cr
#'   `blank_shape`          \tab Space as a shape                                  \tab `v(blank.shape)`        }
#' *NOTE*: `unq` defaults to the smaller of `tot` or the number of unique values available for the palette. If `unq` is larger than the number of unique values available for the palette, an error is generated, except in `create_palette` where `unq` is reduced to the number of available unique values if necessary.
#' @param type A character scalar from `c('line.types', 'open.shapes', 'punc.shapes', 'fill.shapes','empty.shapes', 'solid.shapes', 'digit.shapes', 'digit.shapes0', 'letter.shapes', 'LETTER.shapes', 'colors.standard', 'colors.sensitive')`.
#' @param tot A \link[=cmp_psw_scl]{complete positive whole-number scalar} indicating the number of values to return.
#' @param unq A positive whole-number scalar less than or equal to `tot` giving the number of unique values to be repeated until there are `tot` total values.
#' @return **A positive whole number vector** \cr\cr `fill_shapes`, `open_shapes`, `punc_shapes`, `empty_shapes`, `solid_shapes`, `digit_shapes`, `digit_shapes0`, `letter_shapes`, `LETTER_shapes,` *and possibly* `build_palette`
#' \cr\cr  **A character vector**             \cr\cr `line_types`, `colors_dark`, `colors_bright`, `colors_sensitive` *and possibly* `build_palette`
#' \cr\cr  **A positive whole number scalar** \cr\cr `blank_shape`
#' @export
pals <- function() {utils::help("pals", "uj")}

#' @rdname pals
#' @export
line_types <- function(tot, unq = min(9, tot)) {
  uj:::.pal_errs(tot, unq, 9, uj::callers())
  uj:::.pal_vals("line.types", tot, unq)
}

#' @rdname pals
#' @export
fill_shapes <- function(tot, unq = min(5, tot)) {
  uj:::.pal_errs(tot, unq, 5, uj::callers())
  uj:::.pal_vals("fill.shapes", tot, unq)
}

#' @rdname pals
#' @export
open_shapes <- function(tot, unq = min(5, tot)) {
  uj:::.pal_errs(tot, unq, 5, uj::callers())
  uj:::.pal_vals("open.shapes", tot, unq)
}

#' @rdname pals
#' @export
punc_shapes <- function(tot, unq = min(7, tot)) {
  uj:::.pal_errs(tot, unq, 7, uj::callers())
  uj:::.pal_vals("punc.shapes", tot, unq)
}

#' @rdname pals
#' @export
empty_shapes <- function(tot, unq = min(5, tot)) {
  uj:::.pal_errs(tot, unq, 5, uj::callers())
  uj:::.pal_vals("empty.shapes", tot, unq)
}

#' @rdname pals
#' @export
solid_shapes <- function(tot, unq = min(4, tot)) {
  uj:::.pal_errs(tot, unq, 4, uj::callers())
  uj:::.pal_vals("solid.shapes", tot, unq)
}

#' @rdname pals
#' @export
digit_shapes <- function(tot, unq = min(9, tot)) {
  uj:::.pal_errs(tot, unq, 9, uj::callers())
  uj:::.pal_vals("digit.shapes", tot, unq)
}

#' @rdname pals
#' @export
digit_shapes0 <- function(tot, unq = min(10, tot)) {
  uj:::.pal_errs(tot, unq, 10, uj::callers())
  uj:::.pal_vals("digit.shapes0", tot, unq)
}

#' @rdname pals
#' @export
letter_shapes <- function(tot, unq = min(26, tot)) {
  uj:::.pal_errs(tot, unq, 26, uj::callers())
  uj:::.pal_vals("letter.shapes", tot, unq)
}

#' @rdname pals
#' @export
LETTER_shapes <- function(tot, unq = min(26, tot)) {
  uj:::.pal_errs(tot, unq, 26, uj::callers())
  uj:::.pal_vals("LETTER.shapes", tot, unq)
}

#' @rdname pals
#' @export
colors_dark <- function(tot, unq = min(6, tot)) {
  uj:::.pal_errs(tot, unq, 6, uj::callers())
  uj:::.pal_vals("colors.dark", tot, unq)
}

#' @rdname pals
#' @export
colors_bright <- function(tot, unq = min(6, tot)) {
  uj:::.pal_errs(tot, unq, 6, uj::callers())
  uj:::.pal_vals("colors.bright", tot, unq)
}

#' @rdname pals
#' @export
colors_sensitive <- function(tot, unq = min(12, tot)) {
  uj:::.pal_errs(tot, unq, 12, uj::callers())
  uj:::.pal_vals("colors.sensitive", tot, unq)
}

#' @rdname pals
#' @export
build_palette <- function(type, tot, unq) {
  validTypes <- base::c('colors.dark', 'colors.bright', 'colors.sensitive', 'line.types', 'letter.shapes', 'LETTER.shapes', 'digit.shapes0', 'digit.shapes', 'open.shapes', 'punc.shapes', 'empty.shapes', 'fill.shapes', 'solid.shapes')
  validUnqNs <- base::c(colors.sensitive = 10, colors.standard = 10, linetypes = 9, letter.shapes = 26, LETTER.shapes = 26, digit.shapes0 = 10, digit.shapes = 9, open.shapes = 7, punch.shapes = 7, empty.shapes = 5, fill.shapes = 5, solid.shapes = 4)
  errs <- NULL
  if      (!ppp::.cmp_chr_scl(type)) {errs <- base::c(errs, "[type] must be a character scalar in c('colors.sensitive', 'colors.standard', 'linetypes', 'letter.shapes', 'LETTER.shapes', 'digit.shapes0', 'digit.shapes', 'open.shapes', 'punc.shapes', 'empty.shapes', 'fill.shapes', 'solid.shapes').")}
  else if (!(type %in% validTypes )) {errs <- base::c(errs, "[type] must be a character scalar in c('colors.sensitive', 'colors.standard', 'linetypes', 'letter.shapes', 'LETTER.shapes', 'digit.shapes0', 'digit.shapes', 'open.shapes', 'punc.shapes', 'empty.shapes', 'fill.shapes', 'solid.shapes').")}
  if      (!ppp::.cmp_psw_scl(tot )) {errs <- base::c(errs, "[tot] must be positive whole-number scalar.")}
  if      (!ppp::.cmp_psw_scl(unq )) {errs <- base::c(errs, "[unq] must be positive whole-number scalar.")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "uj")}
  if (unq > tot) {ppp::stopperr("[unq] is greater than [tot]", pkg = "uj")}
  unq < base::min(unq, validUnqNs[type])
  uj::pal_vals(type, tot, unq)
}

#' @rdname pals
#' @export
blank_shape <- function() {uj::v(blank.shape)}

