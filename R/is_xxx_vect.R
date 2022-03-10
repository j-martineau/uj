#' @name is_vect
#' @family is_functions
#' @title Is an object a vect?
#' @description A vect is any object that is effectively a vector of length 1 or
#'   greater. This includes \link[=is_scalar]{scalars}, vectors,
#'   \link[=is_vlist]{vlists}, length-1 arrays, and arrays of length ≥ 2 with
#'   multiple index positions in only 1 dimension (i.e.,
#'   \link[=edim]{effectively 1D}).
#'   \cr\cr
#'   Also checks for the following types of vects:
#'   \tabular{lll}{
#'   \strong{Type}\tab\strong{Function} \tab\strong{Definition}
#'   \cr Any      \tab\code{is_any_vect}\tab Any vect.
#'   \cr Atomic   \tab\code{is_atm_vect}\tab An atomic vect.
#'   \cr Recursive\tab\code{is_rcr_vect}\tab A \link[=is_vlist]{vlist} or an
#'                                           array vect containing one or more
#'                                           recursive element.
#'   \cr Complete \tab\code{is_cmp_vect}\tab An atomic vect containing no
#'                                           \code{NA} elements.
#'   }
#' @param x An object
#' @param xmd \code{NULL} or a character scalar from \code{xmd_vals}
#'   indicating an optional check for a specific xmd.
#' @return \code{TRUE} or \code{FALSE}
#' @export
is_vect <- function(x) {
  if (length(x) == 0) {F}
  else if (is.vector(x)) {T}
  else if (!is.array(x)) {F}
  else {length(which(dim(x) > 1)) <= 1}
}

#' @name is_vect
#' @export
is_any_vect <- is_vect

#' @name is_vect
#' @export
is_atm_vect <- function(x, xmd = NULL) {
  V <- ifelse(is.null(xmd), T, isIN(xmd, xmd_vals()))
  if (!V) {stop("\n  * [xmd] must be NULL or a character scalar value from xmd_vals().")}
  if (!is_vect(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xmd)) {T}
  else {run("x", xmd, "(x)")}
}

#' @name is_vect
#' @export
is_rcr_vect <- function(x) {if (!is_vect(x)) {F} else {is.recursive(x)}}

#' @name is_vect
#' @export
is_cmp_vect <- function(x, xmd = NULL) {
  if (!is_atm_vect(x, xmd)) {F}
  else {!any(is.na(x))}
}
