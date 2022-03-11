#' @name mmm
#' @family props
#' @title Extended Mode (mmm) Property Family
#' @description Extended modes are defined for non-empty atomic objects. For all
#'   other objects, the extended mode is \code{NULL}. These are not formally
#'   defined classes, but are evaluated dynamically based on the current
#'   characteristics of an object. See details.
#' @param x An object.
#' @param mmm A character scalar containing one or more values from
#'   \code{mmm_vals()} separated by pipes and/or underscores. Combinations
#'   of mmms can be specified by separating them with underscores. Separating
#'   mmms or combinations of mmms with pipes will result in a value of
#'   \code{TRUE} if any of them applies to \code{x}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @details Extended modes \strong{apply only to non-empty atomic objects}.
#'  Atomic objects that contain only \code{NA} values are of every extended
#'  mode, as they can be coerced to any mode without introducing new \code{NA}
#'  values. The following tables gives mmm values, names, and requirements
#'  \strong{for non-\code{NA} values only}.
#'  \cr\cr
#'  \strong{Character Extended Modes}
#'  \tabular{lll}{
#'    \strong{Value}\tab\strong{Name}\tab\strong{Requirements}
#'    \cr\emph{\code{'chr'}}\tab\emph{character}\tab\emph{character}
#'    \cr \code{'clr'}\tab color\tab hex color value or color name
#'    \cr \code{'ch1'}\tab ch.one\tab single characters
#'    \cr \code{'str'}\tab string\tab no blank ("") values
#'  }
#'  \strong{Categorical Extended Modes}
#'  \tabular{lll}{
#'   \strong{Value}\tab\strong{Name}\tab\strong{Requirements}
#'   \cr\code{'fac'}\tab factor\tab factor
#'   \cr\code{'lgc'}\tab logical\tab logical
#'   \cr\code{'ord'}\tab ordered\tab ordered factor
#'   \cr\code{'uno'}\tab unordered\tab unordered factor
#'  }
#'  \strong{Combination Extended Modes}
#'  \tabular{lll}{
#'   \strong{Value}\tab\strong{Name}\tab\strong{Requirements}
#'   \cr\code{'ind'}\tab indexer\tab logical or positive whole number
#'   \cr\code{'srt'}\tab sortable\tab character, logical, numeric, or ordered
#'   \cr\code{'nst'}\tab nonsortable\tab atomic, but not sortable
#'  }
#'  \strong{Numeric Extended Modes}
#'  \tabular{lll}{
#'   \strong{Value}\tab\strong{Name}\tab\strong{Requirements}
#'   \cr\emph{\code{'num'}}\tab\emph{numeric}       \tab\emph{numeric}
#'   \cr      \code{'frc'} \tab fractional          \tab any fractional value
#'   \cr      \code{'pct'} \tab percent             \tab values in \[0, 100\]
#'   \cr      \code{'ppn'} \tab proportion          \tab values in \[0, 1\]
#'   \cr\emph{\code{'pos'}}\tab\emph{positive}      \tab\emph{values > 0}
#'   \cr\emph{\code{'nng'}}\tab\emph{non-negative}  \tab\emph{values ≥ 0}
#'   \cr\emph{\code{'nps'}}\tab\emph{non-positive}  \tab\emph{values ≤ 0}
#'   \cr\emph{\code{'neg'}}\tab\emph{negative}      \tab\emph{values < 0}
#'   \cr      \code{'whl'} \tab whole               \tab whole numbers
#'   \cr      \code{'evn'} \tab even                \tab even (whole) numbers
#'   \cr      \code{'odd'} \tab odd                 \tab odd (whole) numbers
#'   \cr\emph{\code{'psw'}}\tab\emph{positive whole}\tab\emph{whole numbers > 0}
#'   \cr\emph{\code{'nnw'}}\tab\emph{non-neg whole} \tab\emph{whole numbers ≥ 0}
#'   \cr\emph{\code{'npw'}}\tab\emph{non-pos whole} \tab\emph{whole numbers ≤ 0}
#'   \cr\emph{\code{'ngw'}}\tab\emph{negative whole}\tab\emph{whole numbers < 0}
#'  }
#'  \strong{\code{mmm_vals}}
#'  \cr Gets all valid extended mode properties.
#'  \cr\cr
#'  \strong{\code{mmm}}
#'  \cr Gets a character vector containing every extended mode property from
#'  \code{mmm_vals()} that is applicable to \code{x}.
#'  \cr\cr
#'  \strong{\code{is_mmm}}
#'  \cr Evaluates whether \code{xxx} contains an extended mode property
#'  applicable to \code{x}.
#'  \cr\cr
#'  \strong{\code{xmmm}}
#'  \cr Evaluates whether \code{x} has the extended mode property represented by
#'  \code{mmm}.
#'  \cr\cr
#'  \strong{Additional Arguments in \code{...}}
#'  \cr Submitting additional arguments to \code{is_mmm} via \code{...} allows
#'  for checking not just extended mode but whether length, number of rows,
#'  number of columns, and element values meet flexible criteria.
#' @return \code{mmm_vals} and \code{mmm} returns a character vector or
#'  \code{NULL}. All others return either \code{TRUE} or \code{FALSE}. See
#'  details for more information.
#' @export
mmm_vals <- function() {
  x <- c('ch1', 'chr', 'clr', 'evn', 'fac', 'frc', 'ind', 'lgc', 'neg',
         'ngw', 'nng', 'nnw', 'nps', 'npw', 'nst', 'num', 'odd', 'ord',
         'pct', 'pos', 'ppn', 'psw', 'srt', 'str', 'uno', 'whl')
  names(x) <- rep("mmm", length(x))
  x
}

#' @rdname mmm
#' @export
xchr <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else {is.character(x)}
}

#' @rdname mmm
#' @export
xch1 <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else if (!is.character(x)) {F}
  else {all(nchar(x) %in% c(NA, 1))}
}

#' @rdname mmm
#' @export
xclr <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else {is_color(x)}
}

#' @export
xevn <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else if (!is.numeric(x)) {F}
  else {all(x / 2 == round(x / 2), na.rm = T)}
  F
}

#' @rdname mmm
#' @export
xfac <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(av(x)))) {T}
  else (is.factor(x))
}

#' @rdname mmm
#' @export
xfrc <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else if (!is.numeric(x)) {F}
  else {any(x != round(x), na.rm = T)}
}

#' @rdname mmm
#' @export
xind <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else if (is.logical(x)) {T}
  else if (!is.numeric(x)) {F}
  else {!any(x <= 0 | round(x) != x, na.rm = T)}
  F
}

#' @rdname mmm
#' @export
xlgc <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else {is.logical(x)}
}

#' @rdname mmm
#' @export
xneg <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else if (!is.numeric(x)) {F}
  else {!any(x >= 0, na.rm = T)}
}

#' @rdname mmm
#' @export
xngw <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else if (!is.numeric(x)) {F}
  else {!any(x >= 0 | x != round(x), na.rm = T)}
}

#' @rdname mmm
#' @export
xnng <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else if (!is.numeric(x)) {F}
  else {!any(x < 0, na.rm = T)}
}

#' @rdname mmm
#' @export
xnnw <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else if (!is.numeric(x)) {F}
  else {!any(x < 0 | x != round(x), na.rm = T)}
}

#' @rdname mmm
#' @export
xnps <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else if (!is.numeric(x)) {F}
  else {!any(x > 0, na.rm = T)}
}

#' @rdname mmm
#' @export
xnpw <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else if (!is.numeric(x)) {F}
  else {!any(x > 0 | x != round(x), na.rm = T)}
}

#' @rdname mmm
#' @export
xnst <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else {!any(is.character(x), is.logical(x), is.numeric(x), is.ordered(x))}
}

#' @rdname mmm
#' @export
xnum <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else {is.numeric(x)}
}

#' @rdname mmm
#' @export
xodd <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else if (!is.numeric(x)) {F}
  else {!any((x + 1) / 2 != round((x + 1) / 2), na.rm = T)}
}

#' @rdname mmm
#' @export
xord <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else {is.ordered(x)}
}

#' @rdname mmm
#' @export
xpct <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else if (!is.numeric(x)) {F}
  else {all(x >= 0 & x <= 100, na.rm = T)}
}

#' @rdname mmm
#' @export
xpos <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else if (!is.numeric(x)) {F}
  else {!any(x <= 0, na.rm = T)}
}

#' @rdname mmm
#' @export
xppn <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else if (!is.numeric(x)) {F}
  else {all(x >= 0 & x <= 1, na.rm = T)}
}

#' @rdname mmm
#' @export
xpsw <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else if (!is.numeric(x)) {F}
  else {!any(x <= 0 | x != round(x), na.rm = T)}
}

#' @rdname mmm
#' @export
xsrt <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else {any(is.character(x), is.logical(x), is.numeric(x), is.ordered(x))}
}

#' @rdname mmm
#' @export
xstr <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else if (!is.character(x)) {F}
  else {!any(x == "", na.rm = T)}
}

#' @rdname mmm
#' @export
xuno <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else if (!is.factor(x)) {F}
  else {!is.ordered(x)}
}

#' @rdname mmm
#' @export
xwhl <- function(x) {
  if (length(x) == 0 | !is.atomic(x)) {F}
  else if (all(is.na(x))) {T}
  else if (!is.numeric(x)) {F}
  else {!any(x != round(x), na.rm = T)}
}

#' @rdname mmm
#' @export
mmm <- function(x) {
  f <- function(X, V) {if (X) {V} else {NULL}}
  if (!is.atomic(x) | length(x) == 0) {return(NULL)}
  if (all(is.na(x))) {return(mmm_vals())}
  x <- x[!is.na(x)]
  chr <- is.character(x)
  fac <- is.factor(x)
  lgc <- is.logical(x)
  num <- is.numeric(x)
  ord <- is.ordered(x)
  uno <- fac & !ord
  srt <- chr | lgc | num | ord
  nst <- !nst
  R <- c(f(chr, "chr"), f(fac, "fac"), f(lgc, "lgc"), f(num, "num"), f(ord, "ord"), f(uno, "uno"), f(srt, "srt"), f(nst, "nst"))
  if (chr) {R <- c(R, f(all(nchar(x) == 1), "ch1"), f(all(is_color(x)), "clr"), f(!any(x == ""), "str"))}
  if (num) {
    whl <- all(x == round(x))
    neg <- all(x <  0); pos <- all(x > 0)
    pct <- all(0 <= x & x <= 100); ppn <- all(0 <= x & x <= 1)
    nps <- all(x <= 0)           ; nng <- all(x >= 0); pos <- all(x >  0)
    R <- c(R, f(whl, "whl"), f(!whl, "frc"), f(neg, "neg"), f(!neg, "nng"), f(pos, "pos"), f(!pos, "nps"), f(pct, "pct"), f( ppn, "ppn"))
    if (whl) {
      evn <- all( x      / 2 == round( x      / 2))
      odd <- all((x + 1) / 2 == round((x + 1) / 2))
      psw <- pos
      R <- c(R, f(evn, "evn"), f(odd, "odd"), f(neg, "ngw"), f(nps, "npw"), f(nng, "nnw"), f(psw, "psw"))
    }
  }
  R <- c(R, f(lgc | psw, "ind"))
  sort(R)
}

#' @rdname mmm
#' @export
is_mmm <- function(x, xxx, ...) {
  if (!cmp_chr_scl(x)) {stop("\n  * [xxx] must be a non-NA character scalar.")}
  ValidVec <- mmm_vals()
  Combos   <- strsplit(xxx , "|", fixed = T)[[1]]
  XXX      <- strsplit(Combos, ".", fixed = T)[[1]]
  Valid    <- all(XXX %in% ValidVec)
  if (!Valid) {stop("\n  * [xxx] contains a value not in mmm_vals(), after splitting [xxx] on pipes and underscores.")}
  is_xxx(x, xxx, ...)
}
