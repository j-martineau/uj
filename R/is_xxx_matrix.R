#' @family is_functions
#' @name is_xxx_matrix
#' @title Is an object a matrix?
#' @description Checks for the following types of matrices:
#'   \tabular{lll}{
#'   \strong{Type}\tab\strong{Function}  \tab\strong{Definition}
#'   \cr Any      \tab\code{is_any_matrix}\tab Any matrix.
#'   \cr Populated\tab\code{is_pop_matrix}\tab A non-empty matrix.
#'   \cr Empty    \tab\code{is_emp_matrix}\tab An empty matrix.
#'   \cr Atomic   \tab\code{is_atm_matrix}\tab A populated, atomic matrix.
#'   \cr Recursive\tab\code{is_rcr_matrix}\tab A populated matrix containing at
#'                                             least one element that is not an
#'                                             atomic scalar.
#'   \cr Complete \tab\code{is_cmp_matrix}\tab A populated atomic matrix
#'                                             containing no \code{NA} elements.
#'   }
#' @param x An object
#' @param xmd \code{NULL} or a character scalar from \code{xmd_vals}
#'   indicating an optional check for a specific xmd.
#' @return \code{TRUE} or \code{FALSE}.
#' @export
is_any_matrix <- function(x) {is.matrix(x)}

#' @rdname is_xxx_matrix
#' @export
is_pop_matrix <- function(x) {if (length(x) == 0) {F} else {is.matrix(x)}}

#' @rdname is_xxx_matrix
#' @export
is_emp_matrix <- function(x) {if (length(x) > 0) {F} else {is.matrix(x)}}

#' @rdname is_xxx_matrix
#' @export
is_atm_matrix <- function(x, xmd = NULL) {
  V <- ifelse(is.null(xmd), T, isIN(xmd, xmd_vals()))
  if (!V) {stop("\n  * [xmd] must be NULL or a character scalar value from xmd_vals().")}
  if (!is_pop_matrix(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xmd)) {T}
  else {run("x", xmd, "(x)")}
}

#' @rdname is_xxx_matrix
#' @export
is_rcr_matrix <- function(x) {
  if (!is_pop_matrix(x)) {F}
  else {is.recursive(x)}
}

#' @rdname is_xxx_matrix
#' @export
is_cmp_matrix <- function(x, xmd = NULL) {
  if (!is_atm_matrix(x, xmd)) {F}
  else {!any(is.na(x))}
}
