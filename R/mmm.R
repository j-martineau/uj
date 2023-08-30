#' @encoding UTF-8
#' @family properties
#' @title Extended mode (xmode) properties
#' @description xmodes are defined for non-empty atomic objects. For all other objects, the xmode is `NULL`. These are not formally defined modes, but are evaluated dynamically based on the current characteristics of an object.
#' \cr\cr Atomic objects that contain only `NA` values are of every xmode (they can be coerced to any mode without introducing new `NA` values).
#' \cr\cr **Generic xmode**
#' \tabular{ll}{  `'atm', 'ATM'`   \tab atomic (both a \code{\link[=bbb]{basic}} and an xmode property).}
#' **Character xmodes**
#' \tabular{ll}{  `'chr', 'CHR'`   \tab character                          \cr
#'                `'ch1', 'CH1'`   \tab onechar, or `all(nchar(x) == 1)`   \cr
#'                `'ch3', 'CH3'`   \tab threechar, or `all(nchar(x) == 3)` \cr
#'                `'clr', 'CLR'`   \tab color (valid color values)         \cr
#'                `'str', 'STR'`   \tab string (no blanks)                   }
#' **Categorical xmodes**
#' \tabular{ll}{  `'fac', 'FAC'`   \tab factor         \cr
#'                `'lgl', 'LGL'`   \tab logical        \cr
#'                `'ord', 'ORD'`   \tab ordered factor \cr
#'                `'uno', 'UNO'`   \tab unordered factor }
#' **Basic numeric xmodes**
#' \tabular{ll}{  `'num', 'NUM'`   \tab numeric    \cr
#'                `'frc', 'FRC'`   \tab fractional \cr
#'                `'whl', 'WHL'`   \tab whole number }
#' **Value-restricted numeric xmodes**
#' \tabular{ll}{  `'pct', 'PCT'`   \tab percentage (`0` to `100`) \cr
#'                `'ppn', 'PPN'`   \tab proportion (`0-1`)        \cr
#'                `'pos', 'POS'`   \tab positive                  \cr
#'                `'neg', 'NEG'`   \tab negative                  \cr
#'                `'nng', 'NNG'`   \tab non-negative              \cr
#'                `'nps', 'NPS'`   \tab non-positive                }
#' **Whole-number value-restricted xmodes**
#' \tabular{ll}{  `'evn', 'EVN'`   \tab even               \cr
#'                `'odd', 'ODD'`   \tab odd                \cr
#'                `'ngw', 'NGW'`   \tab negative whole     \cr
#'                `'psw', 'PSW'`   \tab positive whole     \cr
#'                `'npw', 'NPW'`   \tab non-positive whole \cr
#'                `'nnw', 'NNW'`   \tab non-negative whole   }
#' **Combination xmodes**
#' \tabular{ll}{  `'ind', 'IND'`   \tab indexer (`'lgl'`, `'psw'`)           \cr
#'                `'srt', 'SRT'`   \tab sortable (`'chr'`, `'num'`, `'ord'`) \cr
#'                `'nst', 'NST'`   \tab non-sortable atomic                    }
#' @details
#' \tabular{ll}{  `is_mmm_spec`    \tab Is `spec` an xmode specification?                                                                                  \cr   \tab   \cr
#'                `mmm_props`      \tab What xmode properties are there?                                                                                   \cr   \tab   \cr
#'                `mmm_funs`       \tab What xmode property functions are there?                                                                           \cr   \tab   \cr
#'                `{MMM}`          \tab Is `x` a match to the single xmode property `'{MMM}'` where `{MMM}` is a placeholder for any given xmode property? \cr   \tab   \cr
#'                `MMM`            \tab Is `x` a match to the xmode property spec in `spec`?                                                               \cr   \tab   \cr
#'                `mmm`            \tab What are `x`'s xmode properties?                                                                                                  }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more xmode properties from `mmm_props()`. xmode specs may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character vector** \cr\cr `mmm_props, mmm_funs, mmm`
#' \cr\cr  **A logical scalar**   \cr\cr `is_mmm_spec, {MMM}, MMM`
#' @examples
#' mmm_funs()
#' mmm_props()
#' mmm(letters)
#' mmm(1:10)
#' mmm(c(pi, log(10), exp(1)))
#' is_mmm_spec("invalid")
#' is_mmm_spec("psw|srt")
#' MMM(1:10, "psw|srt")
#' MMM(letters, "ch1")
#' MMM(letters, "srt")
#' MMM(1:10, "ch3")
#' CH1(letters)
#' SRT(letters)
#' CLR(1:10)
#' CLR("blue")
#' CLR("#1077ACFF")
#' @export
mmm <- function(x) {
  Y <- NULL
  for (MMM in uj::v(MMM)) {if (uj::run("uj:::.", MMM, "(x)")) {Y <- base::c(Y, base::tolower(MMM))}}
  Y
}

#' @rdname mmm
#' @export
mmm_funs <- function() {uj::v(MMM)}

#' @rdname mmm
#' @export
mmm_props <- function() {uj::v(mmm)}

#' @rdname mmm
#' @export
is_mmm_spec <- function(spec) {
  spec <- uj:::.spec2props(spec)
  if (base::length(spec) == 0) {F} else {base::all(spec %in% uj::v(mmm))}
}

#' @rdname mmm
#' @export
MMM <- function(x, spec, ...) {
  Errors <- uj:::.meets_errs(x, ...)
  if (!uj::is_mmm_spec(spec)) {Errors <- base::c(Errors, '[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from mmm_props().')}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  Props <- uj:::.spec2props(spec)
  for (Prop in base::toupper(Props)) {if (uj::run("uj:::.", Prop, "(x)")) {return(T)}}
  F
}

#' @rdname mmm
#' @export
CH1 <- function(x, ...) {uj::MMM(x, 'ch1', ...)}

#' @rdname mmm
#' @export
CH3 <- function(x, ...) {uj::MMM(x, 'ch3', ...)}

#' @rdname mmm
#' @export
CHR <- function(x, ...) {uj::MMM(x, 'chr', ...)}

#' @rdname mmm
#' @export
CLR <- function(x, ...) {uj::MMM(x, 'clr', ...)}

#' @rdname mmm
#' @export
EVN <- function(x, ...) {uj::MMM(x, 'evn', ...)}

#' @rdname mmm
#' @export
FAC <- function(x, ...) {uj::MMM(x, 'fac', ...)}

#' @rdname mmm
#' @export
FRC <- function(x, ...) {uj::MMM(x, 'frc', ...)}

#' @rdname mmm
#' @export
IND <- function(x, ...) {uj::MMM(x, 'ind', ...)}

#' @rdname mmm
#' @export
LGL <- function(x, ...) {uj::MMM(x, 'lgl', ...)}

#' @rdname mmm
#' @export
NEG <- function(x, ...) {uj::MMM(x, 'neg', ...)}

#' @rdname mmm
#' @export
NGW <- function(x, ...) {uj::MMM(x, 'ngw', ...)}

#' @rdname mmm
#' @export
NNG <- function(x, ...) {uj::MMM(x, 'nng', ...)}

#' @rdname mmm
#' @export
NNW <- function(x, ...) {uj::MMM(x, 'nnw', ...)}

#' @rdname mmm
#' @export
NPS <- function(x, ...) {uj::MMM(x, 'nps', ...)}

#' @rdname mmm
#' @export
NPW <- function(x, ...) {uj::MMM(x, 'npw', ...)}

#' @rdname mmm
#' @export
NST <- function(x, ...) {uj::MMM(x, 'nst', ...)}

#' @rdname mmm
#' @export
NUM <- function(x, ...) {uj::MMM(x, 'num', ...)}

#' @rdname mmm
#' @export
ODD <- function(x, ...) {uj::MMM(x, 'odd', ...)}

#' @rdname mmm
#' @export
ORD <- function(x, ...) {uj::MMM(x, 'ord', ...)}

#' @rdname mmm
#' @export
PCT <- function(x, ...) {uj::MMM(x, 'pct', ...)}

#' @rdname mmm
#' @export
POS <- function(x, ...) {uj::MMM(x, 'pos', ...)}

#' @rdname mmm
#' @export
PPN <- function(x, ...) {uj::MMM(x, 'ppn', ...)}

#' @rdname mmm
#' @export
PSW <- function(x, ...) {uj::MMM(x, 'psw', ...)}

#' @rdname mmm
#' @export
SRT <- function(x, ...) {uj::MMM(x, 'srt', ...)}

#' @rdname mmm
#' @export
STR <- function(x, ...) {uj::MMM(x, 'str', ...)}

#' @rdname mmm
#' @export
UNO <- function(x, ...) {uj::MMM(x, 'uno', ...)}

#' @rdname mmm
#' @export
WHL <- function(x, ...) {uj::MMM(x, 'whl', ...)}
