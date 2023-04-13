#' @encoding UTF-8
#' @family properties
#' @title Extended mode (xmode) properties
#' @description xmodes are defined for non-empty atomic objects. For all other objects, the xmode is `NULL`. These are not formally defined modes, but are evaluated dynamically based on the current characteristics of an object.
#' \cr\cr Atomic objects that contain only `NA` values are of every xmode (they can be coerced to any mode without introducing new `NA` values).
#' \cr\cr **Generic xmode**
#' \tabular{ll}{  `'atm', 'ATM'`   \tab atomic (both a \code{\link[=bbb]{basic}} and an xmode property).}
#' **Character xmodes**
#' \tabular{ll}{  `'chr', 'CHR'`   \tab character                          \cr
#'                `'ch1', 'CH1'`   \tab onechar, or `all(nchar(X) == 1)`   \cr
#'                `'ch3', 'CH3'`   \tab threechar, or `all(nchar(X) == 3)` \cr
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
#' \tabular{ll}{  `is_mmm_spec`    \tab Is `Spec` an xmode specification?                                                                                  \cr   \tab   \cr
#'                `mmm_props`      \tab What xmode properties are there?                                                                                   \cr   \tab   \cr
#'                `mmm_funs`       \tab What xmode property functions are there?                                                                           \cr   \tab   \cr
#'                `{MMM}`          \tab Is `X` a match to the single xmode property `'{MMM}'` where `{MMM}` is a placeholder for any given xmode property? \cr   \tab   \cr
#'                `MMM`            \tab Is `X` a match to the xmode property Spec in `Spec`?                                                               \cr   \tab   \cr
#'                `mmm`            \tab What are `X`'s xmode properties?                                                                                                  }
#' @param X An R object.
#' @param Spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more xmode properties from `mmm_props()`. xmode specs may be pipe-delimited. If there are multiple properties in `Spec`, `X` is inspected for a match to any of the specified properties.
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
mmm <- function(X) {
  Y <- NULL
  for (MMM in uj::v(MMM)) {if (uj::run("uj:::.", MMM, "(X)")) {Y <- base::c(Y, base::tolower(MMM))}}
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
is_mmm_spec <- function(Spec) {
  Spec <- uj:::.spec2props(Spec)
  if (base::length(Spec) == 0) {F} else {base::all(Spec %in% uj::v(mmm))}
}

#' @rdname mmm
#' @export
MMM <- function(X, Spec, ...) {
  Errors <- uj:::.meets_errs(X, ...)
  if (!uj::is_mmm_spec(Spec)) {Errors <- base::c(Errors, '[Spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from mmm_props().')}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  Props <- uj:::.spec2props(Spec)
  for (Prop in base::toupper(Props)) {if (uj::run("uj:::.", Prop, "(X)")) {return(T)}}
  F
}

#' @rdname mmm
#' @export
CH1 <- function(X, ...) {uj::MMM(X, 'ch1', ...)}

#' @rdname mmm
#' @export
CH3 <- function(X, ...) {uj::MMM(X, 'ch3', ...)}

#' @rdname mmm
#' @export
CHR <- function(X, ...) {uj::MMM(X, 'chr', ...)}

#' @rdname mmm
#' @export
CLR <- function(X, ...) {uj::MMM(X, 'clr', ...)}

#' @rdname mmm
#' @export
EVN <- function(X, ...) {uj::MMM(X, 'evn', ...)}

#' @rdname mmm
#' @export
FAC <- function(X, ...) {uj::MMM(X, 'fac', ...)}

#' @rdname mmm
#' @export
FRC <- function(X, ...) {uj::MMM(X, 'frc', ...)}

#' @rdname mmm
#' @export
IND <- function(X, ...) {uj::MMM(X, 'ind', ...)}

#' @rdname mmm
#' @export
LGL <- function(X, ...) {uj::MMM(X, 'lgl', ...)}

#' @rdname mmm
#' @export
NEG <- function(X, ...) {uj::MMM(X, 'neg', ...)}

#' @rdname mmm
#' @export
NGW <- function(X, ...) {uj::MMM(X, 'ngw', ...)}

#' @rdname mmm
#' @export
NNG <- function(X, ...) {uj::MMM(X, 'nng', ...)}

#' @rdname mmm
#' @export
NNW <- function(X, ...) {uj::MMM(X, 'nnw', ...)}

#' @rdname mmm
#' @export
NPS <- function(X, ...) {uj::MMM(X, 'nps', ...)}

#' @rdname mmm
#' @export
NPW <- function(X, ...) {uj::MMM(X, 'npw', ...)}

#' @rdname mmm
#' @export
NST <- function(X, ...) {uj::MMM(X, 'nst', ...)}

#' @rdname mmm
#' @export
NUM <- function(X, ...) {uj::MMM(X, 'num', ...)}

#' @rdname mmm
#' @export
ODD <- function(X, ...) {uj::MMM(X, 'odd', ...)}

#' @rdname mmm
#' @export
ORD <- function(X, ...) {uj::MMM(X, 'ord', ...)}

#' @rdname mmm
#' @export
PCT <- function(X, ...) {uj::MMM(X, 'pct', ...)}

#' @rdname mmm
#' @export
POS <- function(X, ...) {uj::MMM(X, 'pos', ...)}

#' @rdname mmm
#' @export
PPN <- function(X, ...) {uj::MMM(X, 'ppn', ...)}

#' @rdname mmm
#' @export
PSW <- function(X, ...) {uj::MMM(X, 'psw', ...)}

#' @rdname mmm
#' @export
SRT <- function(X, ...) {uj::MMM(X, 'srt', ...)}

#' @rdname mmm
#' @export
STR <- function(X, ...) {uj::MMM(X, 'str', ...)}

#' @rdname mmm
#' @export
UNO <- function(X, ...) {uj::MMM(X, 'uno', ...)}

#' @rdname mmm
#' @export
WHL <- function(X, ...) {uj::MMM(X, 'whl', ...)}
