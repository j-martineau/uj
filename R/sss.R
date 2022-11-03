#' @name sss
#' @family props
#' @title State of an object
#' @description Get a character scalar containing all state properties from
#'   \code{sss_vals()} that is applicable to \code{x.}, if defined. If not
#'   defined, return \code{NULL}.
#' @details An object's state (of completeness) is defined for non-empty
#'   atomic tibbles, non-empty atomic lists, non-empty atomic vectors, and
#'   non-empty atomic arrays. For all others, state is undefined and returned as
#'   \code{NULL}. The following table summarizes valid state properties.
#'   \tabular{lll}{
#'     STATE       \tab STATE           \tab CHARACTERISTICS                 \cr
#'     PROPERTY    \tab PROPERTY        \tab OF QUALIFYING                   \cr
#'     VALUE       \tab NAME            \tab OBJECTS                         \cr
#'     \code{'cmp'}\tab complete        \tab non-empty, atomic, no \code{NA}
#'                                           values                          \cr
#'     \code{'mss'}\tab missing         \tab non-empty, atomic, only \code{NA}
#'                                           values                          \cr
#'     \code{'prt'}\tab partial         \tab non-empty, atomic, both \code{NA}
#'                                           and non-\code{NA} values        \cr
#'     \code{'nas'}\tab\code{NA} scalar \tab atomic \code{NA} scalar         \cr
#'     \code{'oks'}\tab\code{OK} scalar \tab atomic non-\code{NA} scalar       }
#' @param x. An object.
#' @param xxx. A character scalar containing one or more values from
#'   \code{sss_vals()} separated by pipes.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @section Additional Arguments in \code{...}:
#'   Submitting additional arguments to \code{is_sss} via \code{...}
#'   allows for checking not just the state but whether length, number of rows,
#'   number of columns, and element values meet flexible criteria.
#' @return \code{sss_vals()} returns a character vector. \code{sss(x.)}
#'   returns a character scalar or \code{NULL}. All others return either
#'   \code{TRUE} or \code{FALSE}.
#' @export
sss <- function(x.) {
  ok. <- ipop(x.) & iatm(x.)
  xav. <- av(x.)
  nna. <- length(which(na(xav.)))
  nok. <- length(which(ok(xav.)))
  len. <- length(x.)
  if (!ok.) {return(NULL)}
  c(f0.(ok. & nna. == 0 & nok. >= 1  , "cmp", NULL),
    f0.(ok. & nna. >= 1 & nok. >= 1  , "prt", NULL),
    f0.(ok. & nna. >= 1 & nok. == 0  , "mss", NULL),
    f0.(ok. & len. == 1 & all(na(x.)), "nas", NULL),
    f0.(ok. & len. == 1 & all(ok(x.)), "oks", NULL))
}

#' @describeIn sss Is \code{x.} complete (atomic, of length 1 or greater, and
#'   containing no \code{NA} values)?
#' @export
icmp <- function(x.) {ipop(x.) & !any(is.na(av(x.)))}

#' @describeIn sss Is \code{x.} a missing object (atomic, of length 1 or
#'   greater, and containing only \code{NA} values)?
#' @export
imss <- function(x.) {ipop(x.) & any(is.na(av(x.)))}

#' @describeIn sss Is \code{x.} an atomic scalar \code{NA} value?
#' @export
inas <- function(x.) {isNa(x.)}

#' @describeIn sss Is \code{x.} an atomic scalar non-\code{NA} value?
#' @export
ioks <- function(x.) {isOk(x.)}

#' @describeIn sss Is \code{x.} an atomic object with some \code{NA} and some
#'   non-\code{NA} values?
#' @export
iprt <- function(x.) {na. <- is.na(av(x.)); ipop(x.) & any(na.) & any(!na.)}

#' @describeIn sss Get a character vector of all possible state property values.
#' @export
sss_vals <- function() {x. <- c('cmp', 'mss', 'nas', 'oks', 'prt'); names(x.) <- rep.int("sss", length(x.)); x.}

#' @describeIn sss Evaluate whether \code{x.} has a state property represented
#'   in \code{xxx.}.
#' @export
isss <- function(x., xxx., ...) {
  if (!cmp_chr_scl(x.)) {stop("\n • [xxx.] must be a complete character scalar.")}
  valid. <- sss_vals()
  new. <- strsplit(xxx. , "|", fixed = T)[[1]]
  valid. <- all(new. %in% valid.)
  if (!valid.) {stop("\n • [xxx.] contains a value not in sss_vals(), after splitting [xxx.] on pipes.")}
  if (!meets(x., ...)) {return(FALSE)}
  obs. <- xxx(x.)
  for (exp. in new.) {if (exp. == obs.) {return(TRUE)}}
  return(FALSE)
}
