#' @name is_xxx_vtype
#' @family is_functions
#' @title Is an object a vtype?
#' @description A vtype is a non-data.frame object that is empty, effectively
#'   scalar, or effectively linear (having ≥ 2 elements). This includes any
#'   vector or vlist, empty arrays, scalar, and effectively
#'   linear/\link[=edim]{effectively 1D} arrays (populated arrays having
#'   multiple index positions in only 1 dimension).
#'   \cr\cr
#'   Also checks for the following types of vtypes:
#'   \tabular{lll}{
#'   \strong{Type}\tab\strong{Function}  \tab\strong{Definition}
#'   \cr Any      \tab\code{is_any_vtype}\tab Any vtype.
#'   \cr Populated\tab\code{is_pop_vtype}\tab A vtype of length ≥ 1.
#'   \cr Empty    \tab\code{is_emp_vtype}\tab An empty vtype.
#'   \cr Atomic   \tab\code{is_atm_vtype}\tab A populated atomic vtype.
#'   \cr Recursive\tab\code{is_rcr_vtype}\tab A populated vtype that is either a
#'                                            vlist or an effectively scalar or
#'                                            effectively linear array
#'                                            containing one or more recursive
#'                                            elements.
#'   \cr Complete \tab\code{is_cmp_vtype}\tab A populated atomic vtype
#'                                            containing no \code{NA} elements.
#'   }
#' @param x An object
#' @param xmd \code{NULL} or a character scalar from \code{xmd_vals}
#'   indicating an optional check for a specific xmd.
#' @return \code{TRUE} or \code{FALSE}.
#' @export
is_vtype <- function(x) {
  if (is.vector(x)) {T}
  else if (!is.array(x)) {F}
  else {edim(x) < 2}
}

#' @name is_xxx_vtype
#' @export
is_any_vtype <- is_vtype

#' @name is_xxx_vtype
#' @export
is_pop_vtype <- function(x) {if (!is_vtype(x)) {F} else {length(x) > 0}}

#' @name is_xxx_vtype
#' @export
is_emp_vtype <- function(x) {if (!is_vtype(x)) {F} else {length(x) == 0}}

#' @name is_xxx_vtype
#' @export
is_atm_vtype <- function(x, xmd = NULL) {
  V <- ifelse(is.null(xmd), T, isIN(xmd, xmd_vals()))
  if (!V) {stop("\n  * [xmd] must be NULL or a character scalar value from xmd_vals().")}
  if (!is_pop_vtype(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xmd)) {T}
  else {run("x", xmd, "(x)")}
}

#' @name is_xxx_vtype
#' @export
is_rcr_vtype <- function(x) {if (!is_pop_vtype(x)) {F} else {is.recursive(x)}}

#' @name is_xxx_vtype
#' @export
is_cmp_vtype <- function(x, xmd = NULL) {
  if (!is_atm_vtype(x, xmd)) {F}
  else {!any(is.na(x))}
}
