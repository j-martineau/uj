#' @name ttt
#' @family props
#' @title Fundamental Type (ttt) Property Family
#' @description An object's fundamental type (ttt) is defined by its most
#'   basic structural properties as described in the following table:
#'   \tabular{lll}{
#'   \emph{Value}   \tab\emph{Name}    \tab\emph{Requirements}
#'   \cr\code{'nll'}\tab null          \tab\code{NULL}.
#'   \cr\code{'def'}\tab defined       \tab not \code{NULL}.
#'   \cr\code{'nil'}\tab nil-length    \tab length is 0 (includes \code{NULL}).
#'   \cr\code{'pop'}\tab populated     \tab length is > 0.
#'   \cr\code{'fun'}\tab function ref  \tab function object or character scalar
#'                                          containing a function name.
#'   \cr\code{'atm'}\tab atomic        \tab atomic.
#'   \cr\code{'rcr'}\tab recursive     \tab list-like (tibble, lists, or objects
#'                                          containing an element that
#'                                          is not an atomic scalar).
#'   }
#' @param x An object.
#' @param xxx A character scalar containing one or more values from
#'   \code{ttt_vals()} separated by pipes and/or underscores ("."). Combinations
#'   of fundamental types can be specified by separating them with underscores.
#'   Separating fundamental types or combinations of fundamental types with
#'   pipes will result in a value of \code{TRUE} if any of them applies to
#'   \code{x}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @details \strong{\code{ttt_vals}}
#'   \cr Gets all valid fundamental type property values.
#'   \cr\cr
#'   \strong{\code{ttt}}
#'   \cr Gets a vector of fundamental properties from \code{ttt_vals()} that are
#'   applicable to \code{x}.
#'   \cr\cr
#'   \strong{\code{is_ttt}}
#'   \cr Evaluates whether \code{xxx} contains any fundamental type property (or
#'   combination of fundamental type properties) applicable to \code{x}.
#'   \cr\cr
#'   \strong{\code{xttt}}
#'   \cr All other functions evaluate whether \code{x} has the fundamental type
#'   property represented by \code{ttt}.
#'   \cr\cr
#'   \strong{Additional Arguments in \code{...}}
#'   \cr Submitting additional arguments to \code{is_ttt} via \code{...} allows
#'   for checking not just the fundamental but whether length, number of rows,
#'   number of columns, and element values meet flexible criteria.
#' @return \code{ttt_vals} and \code{ttt} return a character scalar or
#'   vector. All others return either \code{TRUE} or \code{FALSE}.
#' @export
ttt_vals <- function() {
  x <- sort(c('nll', 'def', 'nil', 'pop', 'fun', 'atm', 'rcr'))
  names(x) <- rep.int("ttt", length(x))
  x
}

#' @rdname ttt
#' @export
xnll <- function(x) {is.null(x)}

#' @rdname ttt
#' @export
xdef <- function(x) {!is.null(x)}

#' @rdname ttt
#' @export
xnil <- function(x) {length(x) == 0}

#' @rdname ttt
#' @export
xpop <- function(x) {length(x) > 0}

#' @rdname ttt
#' @export
xatm <- function(x) {is.atomic(x)}

#' @rdname ttt
#' @export
xfun <- function(x) {if (is.function(x)) {T} else {!isERR(match.fun(x))}}

#' @rdname ttt
#' @export
xrcr <- function(x) {is.recursive(x)}

#' @rdname ttt
#' @export
ttt <- function(x) {
  c(if (xatm(x)) {'atm'} else {NULL},
    if (xdef(x)) {'def'} else {NULL},
    if (xfun(x)) {'fun'} else {NULL},
    if (xnil(x)) {'nil'} else {NULL},
    if (xnll(x)) {'nll'} else {NULL},
    if (xpop(x)) {'pop'} else {NULL},
    if (xrcr(x)) {'rcr'} else {NULL})
}

#' @rdname ttt
#' @export
is_ttt <- function(x, xxx, ...) {
  if (!cmp_chr_scl(x)) {stop("\n  * [xxx] must be a non-NA character scalar.")
  }
  ValidVec <- ttt_vals()
  Combos   <- strsplit(xxx, "|", fixed = T)[[1]]
  XXX      <- unlist(strsplit(Combos, ".", fixed = T))
  Valid    <- all(XXX %in% ValidVec)
  if (!Valid) {stop("\n  * [xxx] contains a value not in ttt_vals() after splitting [xxx] along pipes and underscores.")}
  is_xxx(x, xxx, ...)
}
