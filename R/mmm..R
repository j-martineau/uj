#' @family props
#' @title Extended mode properties
#' @description Extended modes are defined for non-empty atomic objects. For all other objects, the extended mode is `NULL`. These are not formally defined classes, but are evaluated dynamically based on the current characteristics of an object.
#' \cr\cr Atomic objects that contain only `NA` values are of every extended mode, as they can be coerced to any mode without introducing new `NA` values.
#' \cr\cr **Character extended modes** \tabular{rl}{
#'       `'chr'`   \tab character
#'   \cr `'ch1'`   \tab 1-char ( 1-character values)
#'   \cr `'ch3'`   \tab 3-char ( 3-character values)
#'   \cr `'clr'`   \tab color (valid color values)
#'   \cr `'str'`   \tab string (no blanks)
#' }
#' **Categorical extended modes** \tabular{rl}{
#'       `'fac'`   \tab factor
#'   \cr `'lgl'`   \tab logical
#'   \cr `'ord'`   \tab ordered factor
#'   \cr `'uno'`   \tab unordered factor
#' }
#' **Basic numeric extended modes** \tabular{rl}{
#'       `'num'`   \tab numeric
#'   \cr `'frc'`   \tab fractional
#'   \cr `'whl'`   \tab whole number
#' }
#' **Value-restricted numeric modes** \tabular{rl}{
#'       `'pct'`   \tab percentage (`0-100`)
#'   \cr `'ppn'`   \tab proportion (`0-1`)
#'   \cr `'pos'`   \tab positive
#'   \cr `'neg'`   \tab negative
#'   \cr `'nng'`   \tab non-negative
#'   \cr `'nps'`   \tab non-positive
#' }
#' **Whole-number value-restricted modes** \tabular{rl}{
#'       `'evn'`   \tab even
#'   \cr `'odd'`   \tab odd
#'   \cr `'ngw'`   \tab negative whole
#'   \cr `'psw'`   \tab positive whole
#'   \cr `'nnw'`   \tab non-negative whole
#'   \cr `'npw'`   \tab non-positive whole
#' }
#' **Combination extended modes** \tabular{rl}{
#'       `'ind'`   \tab indexer (`lgl`, `psw`)
#'   \cr `'srt'`   \tab sortable (`chr`, `num`, `ord`)
#'   \cr `'nst'`   \tab non-sortable atomic
#' }
#' **Extended mode functions** \tabular{rl}{
#'     `is_mmm_spec`   \tab Evaluates whether `spec` is a valid extended mode property specification.
#'   \cr               \tab  
#'   \cr `mmm_props`   \tab Gets a character vector of all possible extended mode property values.
#'   \cr               \tab  
#'   \cr      `immm`   \tab Evaluates whether `x` possesses one or more (possibly pipe-delimited) extended mode properties in `spec`, subject to any restrictions in `...`.
#'   \cr               \tab  
#'   \cr      `ixxx`   \tab Evaluates whether `x` matches extended mode property `xxx`\eqn{^1} (subject to any restrictions in `...`).
#'   \cr               \tab  
#'   \cr       `mmm`   \tab Gets a character vector containing all extended mode properties possessed by `x`.
#' }
#' \eqn{^{1.}} Represents an extended mode property.
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more extended mode properties from `mmm_props()`. Extended mode properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return \tabular{rl}{
#'     `is_mmm_spec`   \tab A logical scalar.
#'   \cr `mmm_props`   \tab A character vector.
#'   \cr      `immm`   \tab A logical scalar.
#'   \cr      `ixxx`   \tab A logical scalar\eqn{^2}.
#'   \cr       `mmm`   \tab A character scalar/vector.
#' }
#' \eqn{^{2.}} `xxx` represents an extended mode property.
#' @examples
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
  for (m in .mmms) {out <- c(out, f0(run('.i', m, '(x)'), m, NULL))}
  out
}

#' @rdname mmm
#' @export
mmm_props <- function() {.mmms}

#' @rdname mmm
#' @export
is_mmm_spec <- function(spec) {spec <- .spec_vals(spec); f0(length(spec) == 0, F, all(spec %in% .mmms))}

#' @rdname mmm
#' @export
immm <- function(x, spec, ...) {
  errs <- c(.meets_errs(x, ...), f0(is_mmm_spec(spec), NULL, '\n \u2022 [spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from mmm_props().'))
  if (!is.null(errs)) {stop(errs)}
  if (!meets(x, ...)) {return(F)}
  for (prop in .spec_vals(spec)) {if (run('.i', prop, '(x)')) {return(T)}}
  F
}

#' @rdname mmm
#' @export
ich1 <- function(x, ...) {immm(x, 'ch1', ...)}

#' @rdname mmm
#' @export
ich3 <- function(x, ...) {immm(x, 'ch3', ...)}

#' @rdname mmm
#' @export
ichr <- function(x, ...) {immm(x, 'chr', ...)}

#' @rdname mmm
#' @export
iclr <- function(x, ...) {immm(x, 'clr', ...)}

#' @rdname mmm
#' @export
ievn <- function(x, ...) {immm(x, 'evn', ...)}

#' @rdname mmm
#' @export
ifac <- function(x, ...) {immm(x, 'fac', ...)}

#' @rdname mmm
#' @export
ifrc <- function(x, ...) {immm(x, 'frc', ...)}

#' @rdname mmm
#' @export
iind <- function(x, ...) {immm(x, 'ind', ...)}

#' @rdname mmm
#' @export
ilgl <- function(x, ...) {immm(x, 'lgl', ...)}

#' @rdname mmm
#' @export
ineg <- function(x, ...) {immm(x, 'neg', ...)}

#' @rdname mmm
#' @export
ingw <- function(x, ...) {immm(x, 'ngw', ...)}

#' @rdname mmm
#' @export
inng <- function(x, ...) {immm(x, 'nng', ...)}

#' @rdname mmm
#' @export
innw <- function(x, ...) {immm(x, 'nnw', ...)}

#' @rdname mmm
#' @export
inps <- function(x, ...) {immm(x, 'nps', ...)}

#' @rdname mmm
#' @export
inpw <- function(x, ...) {immm(x, 'npw', ...)}

#' @rdname mmm
#' @export
inst <- function(x, ...) {immm(x, 'nst', ...)}

#' @rdname mmm
#' @export
inum <- function(x, ...) {immm(x, 'num', ...)}

#' @rdname mmm
#' @export
iodd <- function(x, ...) {immm(x, 'odd', ...)}

#' @rdname mmm
#' @export
iord <- function(x, ...) {immm(x, 'ord', ...)}

#' @rdname mmm
#' @export
ipct <- function(x, ...) {immm(x, 'pct', ...)}

#' @rdname mmm
#' @export
ipos <- function(x, ...) {immm(x, 'pos', ...)}

#' @rdname mmm
#' @export
ippn <- function(x, ...) {immm(x, 'ppn', ...)}

#' @rdname mmm
#' @export
ipsw <- function(x, ...) {immm(x, 'psw', ...)}

#' @rdname mmm
#' @export
isrt <- function(x, ...) {immm(x, 'srt', ...)}

#' @rdname mmm
#' @export
istr <- function(x, ...) {immm(x, 'str', ...)}

#' @rdname mmm
#' @export
iuno <- function(x, ...) {immm(x, 'uno', ...)}

#' @rdname mmm
#' @export
iwhl <- function(x, ...) {immm(x, 'whl', ...)}
