#' @name mmm_uj
#' @family props
#' @title Extended Mode (mmm) Property Family
#' @description Extended modes are defined for non-empty atomic objects. For all
#'  other objects, the extended mode is \code{NULL}. These are not formally
#'  defined classes, but are evaluated dynamically based on the current
#'  characteristics of an object.
#'  \cr\cr
#'  Atomic objects that contain only \code{NA} values are of every extended
#'  mode, as they can be coerced to any mode without introducing new \code{NA}
#'  values. The following tables gives mmm values, names, and requirements
#'  \strong{for non-\code{NA} values only}.
#'  \cr\cr
#'  \strong{Character Extended Modes}\tabular{lll}{
#'    EXTENDED    \tab EXTENDED       \tab QUALIFYING                        \cr
#'    MODE VALUE  \tab MODE NAME      \tab CHARACTERISTICS                   \cr
#'    \code{'chr'}\tab character      \tab character                         \cr
#'    \code{'clr'}\tab color          \tab hex color value or color name     \cr
#'    \code{'ch1'}\tab onechar        \tab single characters                 \cr
#'    \code{'str'}\tab string         \tab no blank ("") values                }
#'  \strong{Categorical Extended Modes}\tabular{lll}{
#'    EXTENDED    \tab EXTENDED       \tab QUALIFYING                        \cr
#'    MODE VALUE  \tab MODE NAME      \tab CHARACTERISTICS                   \cr
#'    \code{'fac'}\tab factor         \tab factor                            \cr
#'    \code{'lgl'}\tab logical        \tab logical                           \cr
#'    \code{'ord'}\tab ordered        \tab ordered factor                    \cr
#'    \code{'uno'}\tab unordered      \tab unordered factor                    }
#'  \strong{Combination Extended Modes}\tabular{lll}{
#'    EXTENDED    \tab EXTENDED       \tab QUALIFYING                        \cr
#'    MODE VALUE  \tab MODE NAME      \tab CHARACTERISTICS                   \cr
#'    \code{'ind'}\tab indexer        \tab logical or positive whole number  \cr
#'    \code{'srt'}\tab sortable       \tab character, logical, numeric, or
#'                                        ordered factor                     \cr
#'    \code{'nst'}\tab nonsortable    \tab atomic, but not sortable            }
#'  \strong{Numeric Extended Modes}\tabular{lll}{
#'    EXTENDED    \tab EXTENDED       \tab QUALIFYING                        \cr
#'    MODE VALUE  \tab MODE NAME      \tab CHARACTERISTICS                   \cr
#'    \code{'num'}\tab numeric        \tab numeric                           \cr
#'    \code{'frc'}\tab fractional     \tab fractionally valued numeric       \cr
#'    \code{'pct'}\tab percent        \tab percentage valued (in \[0, 100\]) \cr
#'    \code{'ppn'}\tab proportion     \tab proportion valued (in \[0, 1\])   \cr
#'    \code{'pos'}\tab positive       \tab positive valued numeric           \cr
#'    \code{'nng'}\tab non-negative   \tab non-negative valued numeric       \cr
#'    \code{'nps'}\tab non-positive   \tab non-positive valued numeric       \cr
#'    \code{'neg'}\tab negative       \tab negative valued numeric           \cr
#'    \code{'whl'}\tab whole          \tab whole number valued               \cr
#'    \code{'evn'}\tab even           \tab even (whole) number-valued        \cr
#'    \code{'odd'}\tab odd            \tab odd (whole) number-valued         \cr
#'    \code{'psw'}\tab positive whole \tab positive whole-number valued      \cr
#'    \code{'nnw'}\tab non-neg whole  \tab non-negative whole-number valued  \cr
#'    \code{'npw'}\tab non-pos whole  \tab non-positive whole-number valued  \cr
#'    \code{'ngw'}\tab negative whole \tab negative whole-number valued        }
#'  \strong{Additional Arguments in \code{...}}
#'  \cr Submitting additional arguments to \code{is_mmm} via \code{...} allows
#'  for checking not just extended mode but whether length, number of rows,
#'  number of columns, and element values meet flexible criteria.
#' @param x. An object.
#' @param xxx. A character scalar containing one or more values from
#'   \code{mmm_vals()} separated by pipes and/or underscores. Combinations of
#'   extended modes can be specified by separating them with underscores.
#'   Separating extended modes or combinations of extended modes with pipes will
#'   result in a value of \code{TRUE} if any of them applies to \code{x.}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @return \code{mmm_vals} and \code{mmm} returns a character vector or
#'  \code{NULL}. All others return either \code{TRUE} or \code{FALSE}. See
#'  details for more information.
#' @export
mmm_uj <- function() {help("mmm_uj", package = "uj")}

#' @describeIn mmm_uj Get a character vector containing every extended mode
#'   property from \code{mmm_vals()} that is applicable to \code{x}.
#' @export
mmm <- function(x.) {
  f. <- function(X., V.) {if (X.) {V.} else {NULL}}
  if (!is.atomic(x.) | length(x.) == 0) {return(NULL)}
  if (all(is.na(x.))) {
    return(c('ch1', 'chr', 'clr', 'evn', 'fac', 'frc', 'ind', 'lgl', 'neg',
             'ngw', 'nng', 'nnw', 'nps', 'npw', 'nst', 'num', 'odd', 'ord',
             'pct', 'pos', 'ppn', 'psw', 'srt', 'str', 'uno', 'whl'       ))}
  x. <- x.[!is.na(x.)]
  chr. <- is.character(x.)
  fac. <- is.factor(x.)
  lgl. <- is.logical(x.)
  num. <- is.numeric(x.)
  ord. <- is.ordered(x.)
  uno. <- fac. & !ord.
  srt. <- chr. | lgl. | num. | ord.
  nst. <- !nst.
  out. <- c(f.(chr., "chr"), f.(fac., "fac"), f.(lgl., "lgl"), f.(num., "num"),
            f.(ord., "ord"), f.(uno., "uno"), f.(srt., "srt"), f.(nst., "nst"))
  if (chr.) {out. <- c(out., f.(all(nchar(x.) == 1), "ch1"),
                             f.(all(is_clr(x.)), "clr"),
                             f.(!any(x. == ""), "str"))}
  if (num.) {
    whl. <- all(x. == round(x.))
    neg. <- all(x. <  0)
    pos. <- all(x. > 0)
    pct. <- all(0 <= x. & x. <= 100)
    ppn. <- all(0 <= x. & x. <= 1)
    nps. <- all(x. <= 0)
    nng. <- all(x. >= 0)
    pos. <- all(x. >  0)
    out. <- c(out., f.( whl., "whl"), f.(!whl., "frc"), f.( neg., "neg"),
                    f.(!neg., "nng"), f.( pos., "pos"), f.(!pos., "nps"),
                    f.( pct., "pct"), f.( ppn., "ppn")                  )
    if (whl.) {
      evn. <- all( x.      / 2 == round( x.      / 2))
      odd. <- all((x. + 1) / 2 == round((x. + 1) / 2))
      psw. <- pos.
      out. <- c(out., f.(evn., "evn"), f.(odd., "odd"), f.(neg., "ngw"),
                      f.(nps., "npw"), f.(nng., "nnw"), f.(psw., "psw"))
    }
  }
  out. <- c(out., f.(lgl. | psw., "ind"))
  sort(out.)
}

#' @describeIn mmm_uj Is \code{x.} of extended mode character?
#' @export
ichr <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else {is.character(x.)}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode onechar?
#' @export
ich1 <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else if (!is.character(x.)) {F}
  else {all(nchar(x.) %in% c(NA, 1))}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode color?
#' @export
iclr <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else {is_clr(x.)}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode even-numeric?
#' @export
ievn <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else if (!is.numeric(x.)) {F}
  else {all(x. / 2 == round(x. / 2), na.rm = T)}
  F
}

#' @describeIn mmm_uj Is \code{x.} of extended mode factor?
#' @export
ifac <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(av(x.)))) {T}
  else (is.factor(x.))
}

#' @describeIn mmm_uj Is \code{x.} of extended mode fractional-numeric?
#' @export
ifrc <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else if (!is.numeric(x.)) {F}
  else {any(x. != round(x.), na.rm = T)}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode indexer?
#' @export
iind <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else if (is.logical(x.)) {T}
  else if (!is.numeric(x.)) {F}
  else {!any(x. <= 0 | round(x.) != x., na.rm = T)}
  F
}

#' @describeIn mmm_uj Is \code{x.} of extended mode logical?
#' @export
ilgl <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else {is.logical(x.)}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode negative-numeric?
#' @export
ineg <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else if (!is.numeric(x.)) {F}
  else {!any(x. >= 0, na.rm = T)}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode negative-whole-numeric?
#' @export
ingw <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else if (!is.numeric(x.)) {F}
  else {!any(x. >= 0 | x. != round(x.), na.rm = T)}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode non-negative-numeric?
#' @export
inng <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else if (!is.numeric(x.)) {F}
  else {!any(x. < 0, na.rm = T)}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode non-negative-whole-numeric?
#' @export
innw <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else if (!is.numeric(x.)) {F}
  else {!any(x. < 0 | x. != round(x.), na.rm = T)}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode non-positive-numeric?
#' @export
inps <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else if (!is.numeric(x.)) {F}
  else {!any(x. > 0, na.rm = T)}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode non-positive-whole-numeric?
#' @export
inpw <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else if (!is.numeric(x.)) {F}
  else {!any(x. > 0 | x. != round(x.), na.rm = T)}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode non-sortable?
#' @export
inst <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else {!any(is.character(x.), is.logical(x.), is.numeric(x.), is.ordered(x.))}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode numeric?
#' @export
inum <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else {is.numeric(x.)}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode odd-numeric?
#' @export
iodd <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else if (!is.numeric(x.)) {F}
  else {!any((x. + 1) / 2 != round((x. + 1) / 2), na.rm = T)}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode ordered factor?
#' @export
iord <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else {is.ordered(x.)}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode percentage-numeric?
#' @export
ipct <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else if (!is.numeric(x.)) {F}
  else {all(x. >= 0 & x. <= 100, na.rm = T)}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode positive-numeric?
#' @export
ipos <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else if (!is.numeric(x.)) {F}
  else {!any(x. <= 0, na.rm = T)}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode proportion-numeric?
#' @export
ippn <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else if (!is.numeric(x.)) {F}
  else {all(x. >= 0 & x. <= 1, na.rm = T)}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode positive-whole-numeric?
#' @export
ipsw <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else if (!is.numeric(x.)) {F}
  else {!any(x. <= 0 | x. != round(x.), na.rm = T)}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode sortable?
#' @export
isrt <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else {any(is.character(x.), is.logical(x.), is.numeric(x.), is.ordered(x.))}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode string?
#' @export
istr <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else if (!is.character(x.)) {F}
  else {!any(x. == "", na.rm = T)}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode unordered factor?
#' @export
iuno <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else if (!is.factor(x.)) {F}
  else {!is.ordered(x.)}
}

#' @describeIn mmm_uj Is \code{x.} of extended mode whole-numeric?
#' @export
iwhl <- function(x.) {
  if (length(x.) == 0 | !is.atomic(x.)) {F}
  else if (all(is.na(x.))) {T}
  else if (!is.numeric(x.)) {F}
  else {!any(x. != round(x.), na.rm = T)}
}

#' @describeIn mmm_uj Get a character vector of all possible extended mode
#'   values.
#' @export
mmm_vals <- function() {
  x. <- c('ch1', 'chr', 'clr', 'evn', 'fac', 'frc', 'ind', 'lgl', 'neg',
          'ngw', 'nng', 'nnw', 'nps', 'npw', 'nst', 'num', 'odd', 'ord',
          'pct', 'pos', 'ppn', 'psw', 'srt', 'str', 'uno', 'whl')
  names(x.) <- rep("mmm", length(x.))
  x.
}

#' @describeIn mmm_uj Does \code{x} have the extended mode properties in
#'   \code{xxx.}?
#' @export
immm <- function(x., xxx., ...) {
  if (!cmp_chr_scl(x.)) {stop("\n • [xxx.] must be a non-NA character scalar.")}
  valid. <- mmm_vals()
  combos. <- strsplit(xxx., "|", fixed = T)[[1]]
  new. <- strsplit(combos., ".", fixed = T)[[1]]
  valid. <- all(new. %in% valid.)
  if (!valid.) {stop("\n • [xxx.] contains a value not in mmm_vals(), after splitting [xxx.] on pipes and underscores.")}
  ixxx(x., xxx., ...)
}
