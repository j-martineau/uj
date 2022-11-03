#' @name ttt
#' @family props
#' @title Fundamental Type (ttt) Property Family
#' @description Get a character vector of all fundamental type properties
#'   applicable to \code{x.}.
#' @details An object's fundamental type (ttt) is defined by its most
#'   basic structural properties as described in the following table:
#'   \tabular{lll}{
#'     FUNDAMENTAL \tab FUNDAMENTAL \tab CHARACTERISTICS OF                  \cr
#'     TYPE VALUE  \tab TYPE NAME   \tab QUALIFYING OBJECTS                  \cr
#'     \code{'nll'}\tab null        \tab \code{NULL}                         \cr
#'     \code{'def'}\tab defined     \tab not \code{NULL}                     \cr
#'     \code{'nil'}\tab nil-length  \tab length is 0 (includes \code{NULL})  \cr
#'     \code{'pop'}\tab populated   \tab length is > 0                       \cr
#'     \code{'fun'}\tab function ref\tab function object or character scalar
#'                                       containing a function name.         \cr
#'     \code{'atm'}\tab atomic      \tab atomic                              \cr
#'     \code{'rcr'}\tab recursive   \tab list-like (tibble, lists, or objects
#'                                       containing an element that is not an
#'                                       atomic scalar)                        }
#' @param x. An object.
#' @param xxx. A character scalar containing one or more values from
#'   \code{ttt_vals()} separated by pipes and/or underscores ("."). Combinations
#'   of fundamental types can be specified by separating them with underscores.
#'   Separating fundamental types or combinations of fundamental types with
#'   pipes will result in a value of \code{TRUE} if any of them applies to
#'   \code{x.}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @section Additional Arguments in \code{...}:
#'   Submitting additional arguments to \code{is_ttt} via \code{...} allows
#'   for checking not just the fundamental but whether length, number of rows,
#'   number of columns, and element values meet flexible criteria.
#' @return \code{ttt_vals} and \code{ttt} return a character scalar or
#'   vector. All others return either \code{TRUE} or \code{FALSE}.
#' @export
ttt <- function(x.) {
  c(f0( is.atomic( x.), 'atm', NULL ), f0(is.null(         x. ), 'nll', 'def'),
    f0(0 < length( x.), 'pop', 'nil'), f0(is.recursive(    x. ), 'rcr', NULL ),
    f0(is.function(x.), 'fun',         f0(!isERR(match.fun(x.)), 'fun', NULL)))
}

#' @describeIn ttt Is \code{x.} atomic (regardless of length)?
#' @export
iatm <- function(x.) {is.atomic(x.)}

#' @describeIn ttt Is \code{x.} defined (non-\code{NULL})?
#' @export
idef <- function(x.) {!is.null(x.)}

#' @describeIn ttt Is \code{x.} a function or a function name?
#' @export
ifun <- function(x.) {if (is.function(x.)) {T} else {!isERR(match.fun(x.))}}

#' @describeIn ttt Is \code{x.} nil (of length 0)?
#' @export
inil <- function(x.) {length(x.) == 0}

#' @describeIn ttt Is \code{x.} \code{NULL}
#' @export
inll <- function(x.) {is.null(x.)}

#' @describeIn ttt Is \code{x.} populated (of length > 0)?
#' @export
ipop <- function(x.) {length(x.) > 0}

#' @describeIn ttt Is \code{x.} recursive?
#' @export
ircr <- function(x.) {is.recursive(x.)}

#' @describeIn ttt Get a character vector of all possible fundamental type
#'   property values.
#' @export
ttt_vals <- function() {x. <- c('atm', 'nll', 'def', 'fun', 'nil', 'pop', 'rcr'); names(x.) <- rep.int("ttt", length(x.)); x.}

#' @describeIn ttt Determine whether \code{x.} is of the fundamental type(s)
#'   contained in \code{xxx.} subject to the additional specifications in
#'   \code{...}.
#' @export
ittt <- function(x., xxx., ...) {
  if (!cmp_chr_scl(xxx.)) {stop("\n • [xxx.] must be a non-NA character scalar.")}
  valid. <- ttt_vals()
  combos. <- strsplit(xxx., "|", fixed = T)[[1]]
  newxxx. <- unlist(strsplit(combos., ".", fixed = T))
  valid. <- all(newxxx. %in% valid.)
  if (!valid.) {stop("\n • [xxx.] contains a value not in ttt_vals() after splitting [xxx.] along pipes and underscores.")}
  is_xxx(x., xxx., ...)
}
