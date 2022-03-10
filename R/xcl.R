#' @name xcl
#' @family props
#' @title Atomic Extended Class (xcl) Property Family
#' @description Extended classes are not formally defined as a new classes, but
#'   are dynamically evaluated for characteristics through a call to
#'   \code{ccc(x)} where the function name (e.g., 'ccc') is the extended
#'   class value. \code{TRUE} or \code{FALSE} is always returned, and
#'   \code{FALSE} is always returned for the \code{NULL} object.
#'   \cr\cr
#'   See details.
#' @param x An object.
#' @param xcles A character scalar containing one or more values from
#'   \code{xcl_vals()} separated by pipes and/or underscores. Combinations of
#'   xcles can be specified by separating them with underscores. Separating
#'   xcles or combinations of xcles with pipes will result in a value of
#'   \code{TRUE} if any of them applies to \code{x}.
#' @param ... Additional arguments to \code{\link{meets}} containing value and
#'   element/row/column count restrictions.
#' @details In this package, the following types of ojects are new and are
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
#'         \link[=edim]{effective dimensionality} of 1.
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
#'   \strong{\code{xcl_vals}}
#'   \cr Gets all valid extended class property values.
#'   \cr\cr
#'   \strong{\code{xcles}}
#'   \cr Gets a vector of properties from \code{xcl_vals()} that are
#'   applicable to \code{x}.
#'   \cr\cr
#'   \strong{\code{is_xcl}}
#'   \cr Evaluates whether any (combination) property in \code{xcles} is an
#'   extended class property applicable to \code{x}.
#'   \cr\cr
#'   \strong{\code{xccc}}
#'   \cr Evaluates whether \code{x} has the extended class property represented
#'   by \code{ccc}.
#'   \cr\cr
#'   \strong{Additional Arguments in \code{...}}
#'   \cr Submitting additional arguments to \code{is_xcl} via \code{...}
#'   allows for checking not just the xcl but whether length, number of rows,
#'   number of columns, and element values meet flexible criteria.
#' @return \code{xcl_vals} returns a character vector containing all valid
#'   extended class property values. \code{xcles} returns a character scalar
#'   or vector containing all extended class properties from
#'   \code{xcl_vals()} applicable to \code{x}. All others return either
#'   \code{TRUE} or \code{FALSE}.
#' @export
xcl_vals <- function() {
  x <- c('arr', 'agn', 'atb', 'avl', 'avt', 'mat', 'mvc', 'scl', 'vec')
  names(x) <- rep.int("xcl", length(x))
  x
}

#' @rdname xcl
#' @export
xarr <- function(x) {is_atm_array(x)}

#' @rdname xcl
#' @export
xagn <- function(x) {is_atm_generic(x)}

#' @rdname xcl
#' @export
xatb <- function(x) {is_atm_tibble(x)}

#' @rdname xcl
#' @export
xavl <- function(x) {is_atm_vlist(x)}

#' @rdname xcl
#' @export
xavt <- function(x) {is_atm_vtype(x)}

#' @rdname xcl
#' @export
xmat <- function(x) {is_atm_matrix(x)}

#' @rdname xcl
#' @export
xmvc <- function(x) {is_atm_mvect(x)}

#' @rdname xcl
#' @export
xscl <- function(x) {is_atm_scalar(x)}

#' @rdname xcl
#' @export
xvec <- function(x) {is_atm_vect(x)}

#' @rdname xcl
#' @export
xcles <- function(x) {
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

#' @rdname xcl
#' @export
is_xcl <- function(x, xcles, ...) {
  if (!cmp_chr_scl(x)) {stop("\n  * [xcles] must be a non-NA character scalar.")}
  ValidVec <- xcl_vals()
  Combos   <- av(strsplit(xcles , "|", fixed = T))
  PropVec  <- av(strsplit(Combos, ".", fixed = T))
  Valid    <- all(PropVec %in% ValidVec)
  if (!Valid) {stop("\n  * [xcles] contains a value not in xcl_vals(), after splitting [xcles] on pipes and underscores.")}
  is_prop(x, xcles, ...)
}
