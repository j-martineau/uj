#' @name ftype
#' @family props
#' @title Fundamental Type (ftype) Property Family
#' @description An object's fundamental type (ftype) is defined by its most
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
#' @param ftypes A character scalar containing one or more values from
#'   \code{props_ftype()} separated by pipes and/or underscores (".").
#'   Combinations of ftypes can be specified by separating them with
#'   underscores. Separating ftypes or combinations of ftypes with pipes will
#'   result in a value of \code{TRUE} if any of them applies to \code{x}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @details \strong{\code{ftype_vals}}
#'   \cr Gets all valid ftype property values.
#'   \cr\cr
#'   \strong{\code{ftypes}}
#'   \cr Gets a vector of ftype properties from \code{props_ftype()} that are
#'   applicable to \code{x}.
#'   \cr\cr
#'   \strong{\code{is_ftype}}
#'   \cr Evaluates whether \code{ftypes} contains any ftype property (or
#'   combination of ftype properties) applicable to \code{x}.
#'   \cr\cr
#'   \strong{\code{xfff}}
#'   \cr All other functions evaluate whether \code{x} has the ftype property
#'   represented by \code{fff}.
#'   \cr\cr
#'   \strong{Additional Arguments in \code{...}}
#'   \cr Submitting additional arguments to \code{is_ftype} via \code{...}
#'   allows for checking not just the ftype but whether length, number of rows,
#'   number of columns, and element values meet flexible criteria.
#' @return \code{ftype_vals} and \code{ftypes} returns a character scalar or
#'   vector. All others return either \code{TRUE} or \code{FALSE}.
#' @export
ftype_vals <- function() {
  x <- sort(c('nll', 'def', 'nil', 'pop', 'fun', 'atm', 'rcr'))
  names(x) <- rep.int("ftype", length(x))
  x
}

#' @rdname ftype
#' @export
xnll <- function(x) {is.null(x)}

#' @rdname ftype
#' @export
xdef <- function(x) {!is.null(x)}

#' @rdname ftype
#' @export
xnil <- function(x) {length(x) == 0}

#' @rdname ftype
#' @export
xpop <- function(x) {length(x) > 0}

#' @rdname ftype
#' @export
xatm <- function(x) {is.atomic(x)}

#' @rdname ftype
#' @export
xfun <- function(x) {if (is.function(x)) {T} else {!isERR(match.fun(x))}}

#' @rdname ftype
#' @export
xrcr <- function(x) {is.recursive(x)}

#' @rdname ftype
#' @export
ftypes <- function(x) {
  c(if (xatm(x)) {'atm'} else {NULL},
    if (xdef(x)) {'def'} else {NULL},
    if (xfun(x)) {'fun'} else {NULL},
    if (xnil(x)) {'nil'} else {NULL},
    if (xnll(x)) {'nll'} else {NULL},
    if (xpop(x)) {'pop'} else {NULL},
    if (xrcr(x)) {'rcr'} else {NULL})
}

#' @rdname ftype
#' @export
is_ftype <- function(x, ftypes, ...) {
  if (!cmp_chr_scl(x)) {stop("\n  * [ftypes] must be a non-NA character scalar.")
  }
  ValidVec <- ftype_vals()
  Combos   <- strsplit(ftypes , "|", fixed = T)[[1]]
  PropVec  <- unlist(strsplit(Combos, ".", fixed = T))
  Valid    <- all(PropVec %in% ValidVec)
  if (!Valid) {stop("\n  * [ftypes] contains a value not in ftype_vals() after splitting [ftypes] along pipes and underscores.")}
  is_prop(x, ftypes, ...)
}
