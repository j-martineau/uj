#' @name is_xxx_vect
#' @family is_functions
#' @title Is an object a vect (maybe with another property as well)?
#' @description A vect is any object that is effectively a vector of length 1 or
#'   greater. This includes \link[=is_scalar]{scalars}, vectors,
#'   \link[=is_vlist]{vlists}, length-1 arrays, and arrays of length ≥ 2 with
#'   multiple index positions in only 1 dimension (i.e.,
#'   \link[=eee]{effectively 1D}).
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
#' @param xxx \code{NULL} or a character scalar from \code{mmm_vals}
#'   indicating an optional check for a specific extended mode.
#' @return \code{TRUE} or \code{FALSE}
#' @export
is_vect <- function(x) {
  if (length(x) == 0) {F}
  else if (is.vector(x)) {T}
  else if (!is.array(x)) {F}
  else {length(which(dim(x) > 1)) <= 1}
}

#' @name is_xxx_vect
#' @export
is_any_vect <- is_vect

#' @name is_xxx_vect
#' @export
is_atm_vect <- function(x, xxx = NULL) {
  V <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!V) {stop("\n  * [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!is_vect(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xxx)) {T}
  else {run("x", xxx, "(x)")}
}

#' @name is_xxx_vect
#' @export
is_rcr_vect <- function(x) {if (!is_vect(x)) {F} else {is.recursive(x)}}

#' @name is_xxx_vect
#' @export
is_cmp_vect <- function(x, xxx = NULL) {
  if (!is_atm_vect(x, xxx)) {F}
  else {!any(is.na(x))}
}
