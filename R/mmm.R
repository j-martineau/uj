#' @encoding UTF-8
#' @family properties
#' @title Extended mode (xmode) properties
#' @description xmodes are defined for non-empty atomic objects. For all other objects, the xmode is `NULL`. These are not formally defined modes, but are evaluated dynamically based on the current characteristics of an object.
#' \cr
#' \cr Atomic objects that contain only `NA` values are of every xmode (they can be coerced to any mode without introducing new `NA` values).
#' \cr
#' \cr **Generic xmode**
#' \tabular{rll}{
#'      `'atm'`   \tab atomic   \tab Both a \code{\link[=bbb]{basic}} and an xmode property.
#' }
#' \cr
#' **Character xmodes**
#' \tabular{rl}{
#'       `'chr'` \tab   character
#'   \cr `'ch1'` \tab   1-char, or `all(nchar(x) == 1)`
#'   \cr `'ch3'` \tab   3-char, or `all(nchar(x) == 3)`
#'   \cr `'clr'` \tab   color (valid color values)
#'   \cr `'str'` \tab   string (no blanks)
#' }
#' \cr
#' **Categorical xmodes**
#' \tabular{rl}{
#'       `'fac'` \tab   factor
#'   \cr `'lgl'` \tab   logical
#'   \cr `'ord'` \tab   ordered factor
#'   \cr `'uno'` \tab   unordered factor
#' }
#' \cr
#' **Basic numeric xmodes**
#' \tabular{rl}{
#'       `'num'` \tab   numeric
#'   \cr `'frc'` \tab   fractional
#'   \cr `'whl'` \tab   whole number
#' }
#' \cr
#' **Value-restricted numeric xmodes**
#' \tabular{rl}{
#'       `'pct'` \tab   percentage (`0-100`)
#'   \cr `'ppn'` \tab   proportion (`0-1`)
#'   \cr `'pos'` \tab   positive
#'   \cr `'neg'` \tab   negative
#'   \cr `'nng'` \tab   non-negative
#'   \cr `'nps'` \tab   non-positive
#' }
#' \cr
#' **Whole-number value-restricted xmodes**
#' \tabular{rl}{
#'       `'evn'` \tab   even
#'   \cr `'odd'` \tab   odd
#'   \cr `'ngw'` \tab   negative whole
#'   \cr `'psw'` \tab   positive whole
#'   \cr `'nnw'` \tab   non-negative whole
#'   \cr `'npw'` \tab   non-positive whole
#' }
#' \cr
#' **Combination xmodes**
#' \tabular{rl}{
#'       `'ind'` \tab   indexer (`lgl`, `psw`)
#'   \cr `'srt'` \tab   sortable (`chr`, `num`, `ord`)
#'   \cr `'nst'` \tab   non-sortable atomic
#' }
#' \cr
#' **xmode functions**
#' \tabular{rl}{
#'     `is_mmm_spec`   \tab Is `spec` an xmode specification?
#'   \cr `mmm_props`   \tab What xmode properties are there?
#'   \cr  `mmm_funs`   \tab What xmode property functions are there?
#'   \cr      `immm`   \tab Is `x` a match to the xmode property spec in `spec`?
#'   \cr      `iMMM`   \tab Is `x` a match to the single xmode property `'MMM'`?
#'   \cr       `mmm`   \tab What are `x`'s xmode properties?
#' }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more xmode properties from `mmm_props()`. xmode specs may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return *A character vector*
#'   \cr   `mmm_props`
#'   \cr   `mmm_funs`
#'   \cr   `mmm`
#'   \cr\cr *A logical scalar 
#'   \cr   `is_mmm_spec`
#'   \cr   `iMMM`
#'   \cr   `immm`
#' @examples
#' mmm_funs()
#' mmm_props()
#' mmm(letters)
#' mmm(1:10)
#' mmm(c(pi, log(10), exp(1)))
#' is_mmm_spec("invalid")
#' is_mmm_spec("psw|srt")
#' immm(1:10, "psw|srt")
#' immm(letters, "ch1")
#' immm(letters, "srt")
#' immm(1:10, "ch3")
#' ich1(letters)
#' isrt(letters)
#' iclr(1:10)
#' iclr("blue")
#' iclr("#1077ACFF")
#' @export
mmm <- function(x) {
  out <- NULL
  for (m in uj:::.mmms) {out <- base::c(out, uj::f0(uj::run('uj:::.i', m, '(x)'), m, NULL))}
  out
}

#' @rdname mmm
#' @export
mmm_funs <- function() {base::paste0("i", uj:::.mmms)}

#' @rdname mmm
#' @export
mmm_props <- function() {uj:::.mmms}

#' @rdname mmm
#' @export
is_mmm_spec <- function(spec) {spec <- uj:::.spec_vals(spec); uj::f0(base::length(spec) == 0, F, base::all(spec %in% .mmms))}

#' @rdname mmm
#' @export
immm <- function(x, spec, ...) {
  errs <- base::c(uj:::.meets_errs(x, ...), uj::f0(uj::is_mmm_spec(spec), NULL, '[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from mmm_props().'))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  if (!uj::meets(x, ...)) {return(F)}
  for (prop in uj:::.spec_vals(spec)) {if (uj::run('uj:::.i', prop, '(x)')) {return(T)}}
  F
}

#' @rdname mmm
#' @export
ich1 <- function(x, ...) {uj::immm(x, 'ch1', ...)}

#' @rdname mmm
#' @export
ich3 <- function(x, ...) {uj::immm(x, 'ch3', ...)}

#' @rdname mmm
#' @export
ichr <- function(x, ...) {uj::immm(x, 'chr', ...)}

#' @rdname mmm
#' @export
iclr <- function(x, ...) {uj::immm(x, 'clr', ...)}

#' @rdname mmm
#' @export
ievn <- function(x, ...) {uj::immm(x, 'evn', ...)}

#' @rdname mmm
#' @export
ifac <- function(x, ...) {uj::immm(x, 'fac', ...)}

#' @rdname mmm
#' @export
ifrc <- function(x, ...) {uj::immm(x, 'frc', ...)}

#' @rdname mmm
#' @export
iind <- function(x, ...) {uj::immm(x, 'ind', ...)}

#' @rdname mmm
#' @export
ilgl <- function(x, ...) {uj::immm(x, 'lgl', ...)}

#' @rdname mmm
#' @export
ineg <- function(x, ...) {uj::immm(x, 'neg', ...)}

#' @rdname mmm
#' @export
ingw <- function(x, ...) {uj::immm(x, 'ngw', ...)}

#' @rdname mmm
#' @export
inng <- function(x, ...) {uj::immm(x, 'nng', ...)}

#' @rdname mmm
#' @export
innw <- function(x, ...) {uj::immm(x, 'nnw', ...)}

#' @rdname mmm
#' @export
inps <- function(x, ...) {uj::immm(x, 'nps', ...)}

#' @rdname mmm
#' @export
inpw <- function(x, ...) {uj::immm(x, 'npw', ...)}

#' @rdname mmm
#' @export
inst <- function(x, ...) {uj::immm(x, 'nst', ...)}

#' @rdname mmm
#' @export
inum <- function(x, ...) {uj::immm(x, 'num', ...)}

#' @rdname mmm
#' @export
iodd <- function(x, ...) {uj::immm(x, 'odd', ...)}

#' @rdname mmm
#' @export
iord <- function(x, ...) {uj::immm(x, 'ord', ...)}

#' @rdname mmm
#' @export
ipct <- function(x, ...) {uj::immm(x, 'pct', ...)}

#' @rdname mmm
#' @export
ipos <- function(x, ...) {uj::immm(x, 'pos', ...)}

#' @rdname mmm
#' @export
ippn <- function(x, ...) {uj::immm(x, 'ppn', ...)}

#' @rdname mmm
#' @export
ipsw <- function(x, ...) {uj::immm(x, 'psw', ...)}

#' @rdname mmm
#' @export
isrt <- function(x, ...) {uj::immm(x, 'srt', ...)}

#' @rdname mmm
#' @export
istr <- function(x, ...) {uj::immm(x, 'str', ...)}

#' @rdname mmm
#' @export
iuno <- function(x, ...) {uj::immm(x, 'uno', ...)}

#' @rdname mmm
#' @export
iwhl <- function(x, ...) {uj::immm(x, 'whl', ...)}
