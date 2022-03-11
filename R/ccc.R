#' @name ccc
#' @family props
#' @title Extended Class (ccc) Property Family
#' @description Extended classes are not formally defined as a new classes, but
#'   are dynamically evaluated for characteristics through a call to
#'   \code{xccc(x)} where the function name (e.g., 'ccc') is the extended
#'   class name. \code{TRUE} or \code{FALSE} is always returned, and
#'   \code{FALSE} is always returned for the \code{NULL} object.
#'   \cr\cr
#'   See details.
#' @param x An object.
#' @param xxx A character scalar containing one or more values from
#'   \code{ccc_vals()} separated by pipes and/or underscores. Combinations of
#'   extended classes can be specified by separating them with underscores.
#'   Separating extended classes or combinations of extended classes with pipes
#'   will result in a value of \code{TRUE} if any of them applies to \code{x}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @details In this package, the following types of objects are new and are
#'   defined as given in the following table.
#'   \tabular{lll}{
#'     \emph{Type}\tab\code{is}\emph{ Function}\tab\emph{Definition}
#'     \cr\code{\link[is_vlist]{vlist}}\tab A vector list (i.e., a list that is
#'         not a tibble/tibble.
#'     \cr\code{\link[is_generic]{scalar}}\tab A vector, vlist, or array of any
#'         length.
#'     \cr\code{\link[is_scalar]{scalar}}\tab A vector, vlist, or array of
#'         length 1.
#'     \cr\code{\link[is_mvect]{mvect}}\tab A multiple-element vector, a
#'         multipe-element vlist, or an array with
#'         \link[=eee]{effective dimensionality} of 1.
#'     \cr\code{\link[is_vect]{vect}}\tab A scalar or mvect.
#'     \cr\code{\link[is_vtype]{vtype}}\tab A scalar, vlist, mvect, or an empty
#'         vector/vlist/array.
#'   }
#'   The following table defines extended class in the table below using
#'   existing ℝ concepts and the types just defined above. None of these
#'   newly defined types or extended classes are formally defined. They are
#'   evaluated dynamically for required properties. In the table, the
#'   first column indicates the structure of an object and column headings
#'   indicate restrictions on other properties. Table notes clarify some issues
#'   that may not be obvious.
#'   \tabular{ll}{
#'         \strong{Any}\tab\strong{Empty}
#'      \cr\code{'arr'}\tab atomic array
#'      \cr\code{'agn'}\tab atomic generic
#'      \cr\code{'atb'}\tab atomic tibble
#'      \cr\code{'avl'}\tab atomic vlist
#'      \cr\code{'avt'}\tab atomic vtype
#'      \cr\code{'mat'}\tab atomic matrix
#'      \cr\code{'mvc'}\tab atomic mvect
#'      \cr\code{'scl'}\tab atomic scalar
#'      \cr\code{'vec'}\tab atomic vect
#'   }
#'   Table Notes \itemize{
#'     \item an atomic tibble is a non-empty tibble whose columns are all
#'           atomic.
#'     \item an atomic vlist is a non-empty vlist all of whose elements are
#'           all non-empty and atomic.
#'   }
#'   \strong{\code{ccc_vals}}
#'   \cr Gets all valid extended class property values.
#'   \cr\cr
#'   \strong{\code{ccc}}
#'   \cr Gets a vector of properties from \code{ccc_vals()} that are
#'   applicable to \code{x}.
#'   \cr\cr
#'   \strong{\code{is_ccc}}
#'   \cr Evaluates whether any (combination) property in \code{xxx} is an
#'   extended class property applicable to \code{x}.
#'   \cr\cr
#'   \strong{\code{xccc}}
#'   \cr Evaluates whether \code{x} has the extended class property represented
#'   by \code{xxx}.
#'   \cr\cr
#'   \strong{Additional Arguments in \code{...}}
#'   \cr Submitting additional arguments to \code{is_ccc} via \code{...}
#'   allows for checking not just the ccc but whether length, number of rows,
#'   number of columns, and element values meet flexible criteria.
#' @return \code{ccc_vals} returns a character vector containing all valid
#'   extended class property values. \code{ccc} returns a character scalar
#'   or vector containing all extended class properties from
#'   \code{ccc_vals()} applicable to \code{x}. All others return either
#'   \code{TRUE} or \code{FALSE}.
#' @export
ccc_vals <- function() {
  x <- c('arr', 'agn', 'atb', 'avl', 'avt', 'mat', 'mvc', 'scl', 'vec')
  names(x) <- rep.int("ccc", length(x))
  x
}

#' @rdname ccc
#' @export
xarr <- function(x) {is_atm_array(x)}

#' @rdname ccc
#' @export
xagn <- function(x) {is_atm_generic(x)}

#' @rdname ccc
#' @export
xatb <- function(x) {is_atm_tibble(x)}

#' @rdname ccc
#' @export
xavl <- function(x) {is_atm_vlist(x)}

#' @rdname ccc
#' @export
xavt <- function(x) {is_atm_vtype(x)}

#' @rdname ccc
#' @export
xmat <- function(x) {is_atm_matrix(x)}

#' @rdname ccc
#' @export
xmvc <- function(x) {is_atm_mvect(x)}

#' @rdname ccc
#' @export
xscl <- function(x) {is_atm_scalar(x)}

#' @rdname ccc
#' @export
xvec <- function(x) {is_atm_vect(x)}

#' @rdname ccc
#' @export
ccc <- function(x) {
  c(if (is_atm_array(  x)) {'arr'} else {NULL},
    if (is_atm_generic(x)) {'agn'} else {NULL},
    if (is_atm_tibble( x)) {'atb'} else {NULL},
    if (is_atm_vlist(  x)) {'avl'} else {NULL},
    if (is_atm_vtype(  x)) {'avt'} else {NULL},
    if (is_atm_matrix( x)) {'mat'} else {NULL},
    if (is_atm_mvect(  x)) {'mvc'} else {NULL},
    if (is_atm_scalar( x)) {'scl'} else {NULL},
    if (is_atm_vect(   x)) {'vec'} else {NULL})
}

#' @rdname ccc
#' @export
is_ccc <- function(x, xxx, ...) {
  if (!cmp_chr_scl(x)) {stop("\n  * [xxx] must be a non-NA character scalar.")}
  ValidVec <- ccc_vals()
  Combos   <- av(strsplit(xxx , "|", fixed = T))
  XXX      <- av(strsplit(Combos, ".", fixed = T))
  Valid    <- all(XXX %in% ValidVec)
  if (!Valid) {stop("\n  * [xxx] contains a value not in ccc_vals(), after splitting [xxx] on pipes and underscores.")}
  is_xxx(x, xxx, ...)
}
