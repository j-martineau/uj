#' @name sss.
#' @family props
#' @title State of completeness properties
#' @description An object's state of completeness is defined for
#'   \link[=ipop]{populated} \link[=atm_dtf]{atomic data.frame}, populated
#'   \link[=atm_vls]{atomic vlists}, populated atomic vectors, and populated
#'   atomic arrays. For all others, state is undefined and returned as
#'   \code{NULL}. The following table summarizes valid state properties.
#' \tabular{lll}{
#' STATE       \tab STATE      \tab CHARACTERISTICS                          \cr
#' PROPERTY    \tab PROPERTY   \tab OF QUALIFYING                            \cr
#' VALUE       \tab NAME       \tab OBJECTS                                  \cr
#' \code{'cmp'}\tab Complete   \tab Non-empty, atomic, no \code{NA} values.  \cr
#' \code{'mss'}\tab Missing    \tab Non-empty, atomic, only \code{NA} values.\cr
#' \code{'prt'}\tab Partial    \tab Non-empty, atomic, both \code{NA} and
#'                                  non-\code{NA} values.                    \cr
#' \code{'nas'}\tab NA scalar  \tab Atomic \code{NA} scalar.                 \cr
#' \code{'oks'}\tab OK scalar  \tab Atomic non-\code{NA} scalar.               }
#' Functions related to state of completeness are described in the following
#' table:\tabular{ll}{
#' FUNCTION          \tab WHAT THE                                           \cr
#' FORMAT            \tab FUNCTION DOES                                      \cr
#' \code{i[sss]}     \tab Evaluates whether an object is of the state of
#'                        completeness represented by \code{[sss]}.          \cr
#' \code{sss}        \tab Gets a character vector containing all state of
#'                        completeness properties of an object.              \cr
#' \code{isss}       \tab Evaluates an object for a specific state of
#'                        completeness and any additional properties specified
#'                        in \code{...}.                                     \cr
#' \code{sss_vals}   \tab Gets a character vector of all possible state of
#'                        completeness property values.                        }
#' @param x An object.
#' @param sss \link[=cmp_chr_scl]{Complete character scalar} containing one or
#'   more values from \code{sss_vals()} separated by pipes.
#' @inheritDotParams meets.
#' @inheritSection meets. Specifying Additional Property Requirements
#' @return \tabular{lll}{
#'   \code{sss_vals()}            \tab   \tab A character vector.            \cr
#'   \code{sss(x)}                \tab   \tab A character scalar or
#'                                            \code{NULL}.                   \cr
#'   \code{isss} and \code{i[sss]}\tab   \tab A logical scalar.                }
#' @export
sss. <- function() {help("sss.", package = "uj")}

#' @rdname sss.
#' @export
sss <- function(x) {
  ok <- ipop(x) & iatm(x)
  av.x <- av(x)
  n.na <- length(which(na(av.x)))
  n.ok <- length(which(ok(av.x)))
  len <- length(x)
  if (!ok) {return(NULL)}
  c(f0(ok & n.na == 0 & n.ok >= 1, "cmp", NULL), f0(ok & n.na >= 1 & n.ok >= 1, "prt", NULL),
    f0(ok & n.na >= 1 & n.ok == 0, "mss", NULL), f0(ok & len == 1 & all(na(x)), "nas", NULL),
    f0(ok & len == 1 & all(ok(x)), "oks", NULL))
}

#' @rdname sss.
#' @export
icmp <- function(x) {ipop(x) & !any(is.na(av(x)))}

#' @rdname sss.
#' @export
imss <- function(x) {ipop(x) & any(is.na(av(x)))}

#' @rdname sss.
#' @export
inas <- function(x) {isNa(x)}

#' @rdname sss.
#' @export
ioks <- function(x) {isOk(x)}

#' @rdname sss.
#' @export
iprt <- function(x) {na <- is.na(av(x)); ipop(x) & any(na) & any(!na)}

#' @rdname sss.
#' @export
sss_vals <- function() {c('cmp', 'mss', 'nas', 'oks', 'prt')}

#' @rdname sss.
#' @export
isss <- function(x, sss, ...) {
  if (!cmp_chr_scl(sss)) {stop("\n \u2022 [sss] must be a non-NA character scalar.")}
  valid.sss <- ttt_vals()
  sss.combos <- strsplit(sss, "|", fixed = T)[[1]]
  new.sss <- unlist(strsplit(sss.combos, "_", fixed = T))
  if (!all(new.sss %in% valid.sss)) {stop("\n \u2022 [sss = '", sss, "'] specifies a 'state of completeness' property not in [uj::sss_vals() = c(", paste0(paste0("'", sss_vals(), "'"), collapse = ", "), ")].")}
  ippp(x, sss, ...)
}
