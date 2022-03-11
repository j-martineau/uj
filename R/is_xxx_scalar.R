#' @name is_xxx_scalar
#' @family is_functions
#' @title Is an object scalar?
#' @description A scalar is defined as a vector, array, or
#'   \link[is_vlist]{vlist} of length \code{1}.
#'   \cr\cr
#'   Also checks for the following types of scalars:
#'   \tabular{lll}{
#'   \strong{Type}\tab\strong{Function}   \tab\strong{Definition}
#'   \cr Any      \tab\code{is_any_scalar}\tab Any scalar.
#'   \cr Atomic   \tab\code{is_atm_scalar}\tab An atomic scalar.
#'   \cr Recursive\tab\code{is_rcr_scalar}\tab A scalar of recursive value.
#'   \cr Complete \tab\code{is_cmp_scalar}\tab A non-\code{NA} atomic scalar.
#'   }
#' @param x An object
#' @param xxx \code{NULL} or a character scalar from \code{mmm_vals}
#'   indicating an optional check for a specific extended mode
#' @return \code{TRUE} or \code{FALSE}
#' @export
is_scalar <- function(x) {
  if (length(x) != 1) {F}
  else {is.vector(x) | is.array(x)}
}

#' @name is_xxx_scalar
#' @export
is_any_scalar <- is_scalar

#' @name is_xxx_scalar
#' @export
is_atm_scalar <- function(x, xxx = NULL) {
  V <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!V) {stop("\n  * [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!is_scalar(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xxx)) {T}
  else {run("x", xxx, "(x)")}
}

#' @name is_xxx_scalar
#' @export
is_rcr_scalar <- function(x) {if (!is_scalar(x)) {F} else {is.recursive(x)}}

#' @name is_xxx_scalar
#' @export
is_cmp_scalar <- function(x, xxx = NULL) {
  if (!is_atm_scalar(x, xxx)) {F}
  else {!is.na(x)}
}
