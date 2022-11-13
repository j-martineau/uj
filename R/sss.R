#' @name sss.
#' @family props
#' @title State of completeness properties
#' @description An object's state of completeness is defined for non-empty
#' atomic tibbles, non-empty atomic lists, non-empty atomic vectors, and
#' non-empty atomic arrays. For all others, state is undefined and returned as
#' \code{NULL}. The following table summarizes valid state properties.
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
#' \code{i•••}       \tab Evaluates whether an object is of the state of
#'                        completeness represented by \code{•••}.            \cr
#' \code{sss}        \tab Gets a character vector containing all state of
#'                        completeness properties of an object.              \cr
#' \code{isss}       \tab Evaluates an object for a specific state of
#'                        completeness and any additional properties specified
#'                        in \code{...}.                                     \cr
#' \code{sss_vals}   \tab Gets a character vector of all possible state of
#'                        completeness property values.                        }
#' @param x An object.
#' @param sss \link[cmp_chr_scl]{Complete character scalar} containing one or
#'   more values from \code{sss_vals()} separated by pipes.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @section Additional Arguments in \code{...}:
#'   Submitting additional arguments to \code{isss} via \code{...}
#'   allows for checking not just the state but whether length, number of rows,
#'   number of columns, and element values meet flexible criteria.
#' @return \code{sss_vals()} returns a character vector. \code{sss(x)}
#'   returns a character scalar or \code{NULL}. All others return either
#'   \code{TRUE} or \code{FALSE}.
#' @export
sss. <- function() {help("sss.", package = "uj")}

#' @describeIn sss. Get a character scalar containing all state properties
#'   from \code{sss_vals()} that is applicable to \code{x}, if defined. If not
#'   defined, return \code{NULL}.
#' @export
sss <- function(x) {
  ok <- ipop(x) & iatm(x)
  xav <- av(x)
  nna <- length(which(na(xav)))
  nok <- length(which(ok(xav)))
  len <- length(x)
  if (!ok) {return(NULL)}
  c(f0(ok & nna == 0 & nok >= 1, "cmp", NULL), f0(ok & nna >= 1 & nok >= 1, "prt", NULL), f0(ok & nna >= 1 & nok == 0, "mss", NULL), f0(ok & len == 1 & all(na(x)), "nas", NULL), f0(ok & len == 1 & all(ok(x)), "oks", NULL))
}

#' @describeIn sss. Is \code{x} complete (atomic, of length 1 or greater, and
#'   containing no \code{NA} values)?
#' @export
icmp <- function(x) {ipop(x) & !any(is.na(av(x)))}

#' @describeIn sss. Is \code{x} a missing object (atomic, of length 1 or
#'   greater, and containing only \code{NA} values)?
#' @export
imss <- function(x) {ipop(x) & any(is.na(av(x)))}

#' @describeIn sss. Is \code{x} an atomic scalar \code{NA} value?
#' @export
inas <- function(x) {isNa(x)}

#' @describeIn sss. Is \code{x} an atomic scalar non-\code{NA} value?
#' @export
ioks <- function(x) {isOk(x)}

#' @describeIn sss. Is \code{x} an atomic object with some \code{NA} and some
#'   non-\code{NA} values?
#' @export
iprt <- function(x) {na <- is.na(av(x)); ipop(x) & any(na) & any(!na)}

#' @describeIn sss. Get a character vector of all possible state property
#'   values.
#' @export
sss_vals <- function() {c('cmp', 'mss', 'nas', 'oks', 'prt')}

#' @describeIn sss. Evaluate whether \code{x} has a state property represented
#'   in \code{sss}.
#' @export
isss <- function(x, sss, ...) {
  if (!cmp_chr_scl(x)) {stop("\n • [sss] must be a complete character scalar.")}
  valid <- sss_vals()
  new <- av(strsplit(sss , "|", fixed = T))
  valid <- all(new %in% valid)
  if (!valid) {stop("\n • [sss] contains a value not in sss_vals(), after splitting [sss] on pipes.")}
  if (!meets(x, ...)) {return(FALSE)}
  obs <- sss(x)
  for (exp in new) {if (exp == obs) {return(TRUE)}}
  return(FALSE)
}
