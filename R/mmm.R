#' @name mmm.
#' @family props
#' @title Extended mode properties
#' @description Extended modes are defined for non-empty atomic objects. For all
#' other objects, the extended mode is \code{NULL}. These are not formally
#' defined classes, but are evaluated dynamically based on the current
#' characteristics of an object.
#' \cr\cr
#' Atomic objects that contain only \code{NA} values are of every extended
#' mode, as they can be coerced to any mode without introducing new \code{NA}
#' values. The following tables gives extended mode values, names, and
#' requirements
#' \cr\cr
#' \strong{Character Extended Modes}\tabular{lll}{
#' EXTENDED     \tab EXTENDED    \tab QUALIFYING                             \cr
#' MODE VALUE   \tab MODE NAME   \tab CHARACTERISTICS                        \cr
#' \code{'chr'} \tab Character   \tab Character.                             \cr
#' \code{'clr'} \tab Color       \tab Hex color value or color name.         \cr
#' \code{'ch1'} \tab Onechar     \tab Single character values.               \cr
#' \code{'str'} \tab String      \tab No blank ("") values.                    }
#' \strong{Categorical Extended Modes}\tabular{lll}{
#' EXTENDED     \tab EXTENDED    \tab QUALIFYING                             \cr
#' MODE VALUE   \tab MODE NAME   \tab CHARACTERISTICS                        \cr
#' \code{'fac'} \tab Factor      \tab Factor.                                \cr
#' \code{'lgl'} \tab Logical     \tab Logical.                               \cr
#' \code{'ord'} \tab Ordered     \tab Ordered factor.                        \cr
#' \code{'uno'} \tab Unordered   \tab Unordered factor.                        }
#' \strong{Combination Extended Modes}\tabular{lll}{
#' EXTENDED     \tab EXTENDED    \tab QUALIFYING                             \cr
#' MODE VALUE   \tab MODE NAME   \tab CHARACTERISTICS                        \cr
#' \code{'ind'} \tab Indexer     \tab Logical or positive whole number.      \cr
#' \code{'srt'} \tab Sortable    \tab Character, logical, numeric, or ordered
#'                                    factor.                                \cr
#' \code{'nst'} \tab non-sortable \tab Atomic, but not sortable.               }
#' \strong{Numeric Extended Modes}\tabular{lll}{
#' EXTENDED     \tab EXTENDED          \tab QUALIFYING                       \cr
#' MODE VALUE   \tab MODE NAME         \tab CHARACTERISTICS                  \cr
#' \code{'num'} \tab Numeric           \tab Numeric.                         \cr
#' \code{'frc'} \tab Fractional        \tab At least one non-\code{NA} value
#'                                          that is fractional (i.e., not a
#'                                          whole number).                   \cr
#' \code{'pct'} \tab Percent           \tab Percentage-valued numeric (in
#'                                          \code{[0, 100]}).                \cr
#' \code{'ppn'} \tab Proportion        \tab Proportion-valued numeric (in
#'                                          \code{[0, 1]}).                  \cr
#' \code{'pos'} \tab Positive          \tab Positive numeric.                \cr
#' \code{'nng'} \tab Non-negative      \tab Non-negative numeric.            \cr
#' \code{'nps'} \tab Non-positive      \tab Non-positive numeric.            \cr
#' \code{'neg'} \tab Negative          \tab Negative numeric.                \cr
#' \code{'whl'} \tab Whole             \tab Whole number.                    \cr
#' \code{'evn'} \tab Even              \tab Even (whole) number.             \cr
#' \code{'odd'} \tab Odd               \tab Odd (whole) number.              \cr
#' \code{'psw'} \tab Positive whole    \tab Positive whole-number.           \cr
#' \code{'nnw'} \tab Non-negative whole\tab Non-negative whole-number.       \cr
#' \code{'npw'} \tab Non-positive whole\tab Non-positive whole-number.       \cr
#' \code{'ngw'} \tab Negative whole    \tab Negative whole-number.             }
#' Functions related to extended mode are described in the following
#' table:\tabular{ll}{
#' FUNCTION          \tab WHAT THE                                           \cr
#' FORMAT            \tab FUNCTION DOES                                      \cr
#' \code{i•••}       \tab Evaluates whether an object is of the extended mode
#'                        represented by \code{•••}.                         \cr
#' \code{mmm}        \tab Gets a character vector containing all extended modes
#'                        of an object.                                      \cr
#' \code{immm}       \tab Evaluates an object for a specific extended mode and
#'                        any additional properties specified in \code{...}. \cr
#' \code{mmm_vals}   \tab Gets a character vector of all possible extended mode
#'                        property values.                                     }
#' @param x An ℝ object.
#' @param mmm \link[cmp_chr_scl]{Complete character scalar} containing one or
#'   more values from \code{mmm_vals()} separated by pipes and/or underscores.
#'   Combinations of extended modes can be specified by separating them with
#'   underscores. Separating extended modes or combinations of extended modes
#'   with pipes will result in a value of \code{TRUE} if any of them applies to
#'   \code{x}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @return \code{mmm_vals} and \code{mmm} returns a character vector or
#'  \code{NULL}. All others return either \code{TRUE} or \code{FALSE}. See
#'  details for more information.
#' @export
mmm. <- function() {help("mmm.", package = "uj")}

#' @describeIn mmm. Get a character vector containing every extended mode
#'   property from \code{mmm_vals()} that is applicable to \code{x}.
#' @export
mmm <- function(x) {
  f. <- function(x., v.) {if (x.) {v.} else {NULL}}
  if (!is.atomic(x) | length(x) == 0) {return(NULL)}
  if (all(is.na(x))) {return(c('ch1', 'chr', 'clr', 'evn', 'fac', 'frc', 'ind', 'lgl', 'neg', 'ngw', 'nng', 'nnw', 'nps', 'npw', 'nst', 'num', 'odd', 'ord', 'pct', 'pos', 'ppn', 'psw', 'srt', 'str', 'uno', 'whl'))}
  x <- x[!is.na(x)]
  chr <- is.character(x)
  fac <- is.factor(x)
  lgl <- is.logical(x)
  num <- is.numeric(x)
  ord <- is.ordered(x)
  uno <- fac & !ord
  srt <- chr | lgl | num | ord
  nst <- !nst
  out <- c(f.(chr, "chr"), f.(fac, "fac"), f.(lgl, "lgl"), f.(num, "num"), f.(ord, "ord"), f.(uno, "uno"), f.(srt, "srt"), f.(nst, "nst"))
  if (chr) {out <- c(out, f.(all(nchar(x) == 1), "ch1"), f.(all(iclr(x)), "clr"), f.(!any(x == ""), "str"))}
  if (num) {
    whl <- all(x == round(x))
    neg <- all(x <  0)
    pos <- all(x > 0)
    pct <- all(0 <= x & x <= 100)
    ppn <- all(0 <= x & x <= 1)
    nps <- all(x <= 0)
    nng <- all(x >= 0)
    pos <- all(x >  0)
    out <- c(out, f.(whl, "whl"), f.(!whl, "frc"), f.(neg, "neg"), f.(!neg, "nng"), f.(pos, "pos"), f.(!pos, "nps"), f.(pct, "pct"), f.(ppn, "ppn"))
    if (whl) {
      evn <- all(x / 2 == round(x / 2))
      odd <- all((x + 1) / 2 == round((x + 1) / 2))
      psw <- pos
      out <- c(out, f.(evn, "evn"), f.(odd, "odd"), f.(neg, "ngw"), f.(nps, "npw"), f.(nng, "nnw"), f.(psw, "psw"))
    }
  }
  out <- c(out, f.(lgl | psw, "ind"))
  sort(out)
}

#' @describeIn mmm. Is \code{x} of extended mode character?
#' @export
ichr <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else {is.character(x)}}

#' @describeIn mmm. Is \code{x} of extended mode onechar?
#' @export
ich1 <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else if (!is.character(x)) {F} else {all(nchar(x) %in% c(NA, 1))}}

#' @describeIn mmm. Is \code{x} of extended mode color?
#' @export
iclr <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else {iclr(x)}}

#' @describeIn mmm. Is \code{x} of extended mode even-numeric?
#' @export
ievn <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else if (!is.numeric(x)) {F} else {all(x / 2 == round(x / 2), na.rm = T)}}

#' @describeIn mmm. Is \code{x} of extended mode factor?
#' @export
ifac <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(av(x)))) {T} else (is.factor(x))}

#' @describeIn mmm. Is \code{x} of extended mode fractional-numeric?
#' @export
ifrc <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else if (!is.numeric(x)) {F} else {any(x != round(x), na.rm = T)}}

#' @describeIn mmm. Is \code{x} of extended mode indexer?
#' @export
iind <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else if (is.logical(x)) {T} else if (!is.numeric(x)) {F} else {!any(x <= 0 | round(x) != x, na.rm = T)}}

#' @describeIn mmm. Is \code{x} of extended mode logical?
#' @export
ilgl <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else {is.logical(x)} }

#' @describeIn mmm. Is \code{x} of extended mode negative-numeric?
#' @export
ineg <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else if (!is.numeric(x)) {F} else {!any(x >= 0, na.rm = T)}}

#' @describeIn mmm. Is \code{x} of extended mode negative-whole-numeric?
#' @export
ingw <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else if (!is.numeric(x)) {F} else {!any(x >= 0 | x != round(x), na.rm = T)}}

#' @describeIn mmm. Is \code{x} of extended mode non-negative-numeric?
#' @export
inng <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else if (!is.numeric(x)) {F} else {!any(x < 0, na.rm = T)}}

#' @describeIn mmm. Is \code{x} of extended mode non-negative-whole-numeric?
#' @export
innw <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else if (!is.numeric(x)) {F} else {!any(x < 0 | x != round(x), na.rm = T)}}

#' @describeIn mmm. Is \code{x} of extended mode non-positive-numeric?
#' @export
inps <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else if (!is.numeric(x)) {F} else {!any(x > 0, na.rm = T)}}

#' @describeIn mmm. Is \code{x} of extended mode non-positive-whole-numeric?
#' @export
inpw <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else if (!is.numeric(x)) {F} else {!any(x > 0 | x != round(x), na.rm = T)}}

#' @describeIn mmm. Is \code{x} of extended mode non-sortable?
#' @export
inst <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else {!any(is.character(x), is.logical(x), is.numeric(x), is.ordered(x))}}

#' @describeIn mmm. Is \code{x} of extended mode numeric?
#' @export
inum <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else {is.numeric(x)}}

#' @describeIn mmm. Is \code{x} of extended mode odd-numeric?
#' @export
iodd <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else if (!is.numeric(x)) {F} else {!any((x + 1) / 2 != round((x + 1) / 2), na.rm = T)}}

#' @describeIn mmm. Is \code{x} of extended mode ordered factor?
#' @export
iord <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else {is.ordered(x)}}

#' @describeIn mmm. Is \code{x} of extended mode percentage-numeric?
#' @export
ipct <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else if (!is.numeric(x)) {F} else {all(x >= 0 & x <= 100, na.rm = T)}}

#' @describeIn mmm. Is \code{x} of extended mode positive-numeric?
#' @export
ipos <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T}else if (!is.numeric(x)) {F} else {!any(x <= 0, na.rm = T)}}

#' @describeIn mmm. Is \code{x} of extended mode proportion-numeric?
#' @export
ippn <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else if (!is.numeric(x)) {F} else {all(x >= 0 & x <= 1, na.rm = T)}}

#' @describeIn mmm. Is \code{x} of extended mode positive-whole-numeric?
#' @export
ipsw <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else if (!is.numeric(x)) {F} else {!any(x <= 0 | x != round(x), na.rm = T)}}

#' @describeIn mmm. Is \code{x} of extended mode sortable?
#' @export
isrt <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else {any(is.character(x), is.logical(x), is.numeric(x), is.ordered(x))}}

#' @describeIn mmm. Is \code{x} of extended mode string?
#' @export
istr <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else if (!is.character(x)) {F} else {!any(x == "", na.rm = T)}}

#' @describeIn mmm. Is \code{x} of extended mode unordered factor?
#' @export
iuno <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else if (!is.factor(x)) {F} else {!is.ordered(x)}}

#' @describeIn mmm. Is \code{x} of extended mode whole-numeric?
#' @export
iwhl <- function(x) {if (length(x) == 0 | !is.atomic(x)) {F} else if (all(is.na(x))) {T} else if (!is.numeric(x)) {F} else {!any(x != round(x), na.rm = T)}}

#' @describeIn mmm. Get a character vector of all possible extended mode
#'   values.
#' @export
mmm_vals <- function() {c('ch1', 'chr', 'clr', 'evn', 'fac', 'frc', 'ind', 'lgl', 'neg', 'ngw', 'nng', 'nnw', 'nps', 'npw', 'nst', 'num', 'odd', 'ord', 'pct', 'pos', 'ppn', 'psw', 'srt', 'str', 'uno', 'whl')}

#' @describeIn mmm. Does \code{x} have the extended mode properties in
#'   \code{mmm}?
#' @export
immm <- function(x, mmm, ...) {
  if (!cmp_chr_scl(x)) {stop("\n • [mmm] must be a complete character scalar (?cmp_chr_scl).")}
  valid <- mmm_vals()
  combos <- strsplit(mmm, "|", fixed = T)[[1]]
  new <- strsplit(combos, ".", fixed = T)[[1]]
  valid <- all(new %in% valid)
  if (!valid) {stop("\n • [mmm] contains a value not in mmm_vals(), after splitting [mmm] on pipes and underscores.")}
  ippp(x, mmm, ...)
}
