#' @name is_xxx_mvect
#' @family is_functions
#' @title Is an object an mvect?
#' @description An mvect is any non-data.frame object that is effectively
#'   linear/effectively 1D. This includes vectors and
#'   \link[=is_vlist]{vlists} of length ≥ 2, and effectively linear or
#'   \link[=edim]{effectively 1D} arrays (populated arrays with multiple
#'   index positions in exactly 1 dimension).
#'   \cr\cr
#'   Also checks for the following types of mvects:
#'   \tabular{lll}{
#'   \strong{Type}\tab\strong{Function}  \tab\strong{Definition}
#'   \cr Any      \tab\code{is_any_mvect}\tab Any mvect.
#'   \cr Atomic   \tab\code{is_atm_mvect}\tab An atomic mvect.
#'   \cr Recursive\tab\code{is_rcr_mvect}\tab A vlist of length ≥ 2 or an
#'                                            effectively linear array
#'                                            containing at least 1 element that
#'                                            is not an atomic scalar.
#'   \cr Complete \tab\code{is_cmp_mvect}\tab An atomic mvect containing no
#'                                            \code{NA} elements.
#'   }
#' @param x An object
#' @param xmd \code{NULL} or a character scalar from \code{xmd_vals}
#'   indicating an optional check for a specific xmd.
#' @return \code{TRUE} or \code{FALSE}.
#' @export
is_mvect <- function(x) {
  if (length(x) < 2) {F}
  else if (is.vector(x)) {T}
  else if (!is.array(x)) {F}
  else {length(which(dim(x) > 1) == 1)}
}

#' @name is_xxx_mvect
#' @export
is_any_mvect <- is_mvect

#' @name is_xxx_mvect
#' @export
is_atm_mvect <- function(x, xmd = NULL) {
  V <- ifelse(is.null(xmd), T, isIN(xmd, xmd_vals()))
  if (!V) {stop("\n  * [xmd] must be NULL or a character scalar value from xmd_vals().")}
  if (!is_mvect(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xmd)) {T}
  else {run("x", xmd, "(x)")}
}

#' @name is_xxx_mvect
#' @export
is_rcr_mvect <- function(x) {if (!is_mvect(x)) {F} else {is.recursive(x)}
}

#' @name is_xxx_mvect
#' @export
is_cmp_mvect <- function(x, xmd = NULL) {
  if (!is_atm_mvect(x, xmd)) {F}
  else {!any(is.na(x))}
}
