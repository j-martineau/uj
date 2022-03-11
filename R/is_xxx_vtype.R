#' @name is_xxx_vtype
#' @family is_functions
#' @title Is an object a vtype (maybe with another property as well)?
#' @description A vtype is a non-data.frame object that is empty, effectively
#'   scalar, or effectively linear (having ≥ 2 elements). This includes any
#'   vector or vlist, empty arrays, scalar, and effectively
#'   linear/\link[=eee]{effectively 1D} arrays (populated arrays having
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
#' @param xxx \code{NULL} or a character scalar from \code{mmm_vals}
#'   indicating an optional check for a specific extended mode.
#' @return \code{TRUE} or \code{FALSE}.
#' @export
is_vtype <- function(x) {
  if (is.vector(x)) {T}
  else if (!is.array(x)) {F}
  else {eee(x) < 2}
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
is_atm_vtype <- function(x, xxx = NULL) {
  V <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!V) {stop("\n  * [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!is_pop_vtype(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xxx)) {T}
  else {run("x", xxx, "(x)")}
}

#' @name is_xxx_vtype
#' @export
is_rcr_vtype <- function(x) {if (!is_pop_vtype(x)) {F} else {is.recursive(x)}}

#' @name is_xxx_vtype
#' @export
is_cmp_vtype <- function(x, xxx = NULL) {
  if (!is_atm_vtype(x, xxx)) {F}
  else {!any(is.na(x))}
}
