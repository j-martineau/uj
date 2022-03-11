#' @name is_xxx_array
#' @family is_functions
#' @title Is an object an array (maybe with another property as well)?
#' @description Checks for the following types of arrays:
#'   \tabular{lll}{
#'   \strong{Type}\tab\strong{Function}  \tab\strong{Definition}
#'   \cr Any      \tab\code{is_any_array}\tab Any array.
#'   \cr Populated\tab\code{is_pop_array}\tab A non-empty array.
#'   \cr Empty    \tab\code{is_emp_array}\tab An empty array.
#'   \cr Atomic   \tab\code{is_atm_array}\tab An atomic array.
#'   \cr Recursive\tab\code{is_rcr_array}\tab A recursive array.
#'   \cr Complete \tab\code{is_cmp_array}\tab An atomic array containing no
#'                                            \code{NA} elements.
#'   }
#' @param x An object
#' @param xxx \code{NULL} or a character scalar from \code{mmm_vals}
#'   indicating an optional check for a specific extended mode.
#' @return \code{TRUE} or \code{FALSE}.
#' @export
is_any_array <- function(x) {is.array(x)}

#' @rdname is_xxx_array
#' @export
is_pop_array <- function(x) {if (length(x) == 0) {F} else {is.array(x)}}

#' @rdname is_xxx_array
#' @export
is_emp_array <- function(x) {if (length(x) > 0) {F} else {is.array(x)}}

#' @rdname is_xxx_array
#' @export
is_atm_array <- function(x, xxx = NULL) {
  V <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!V) {stop("\n  * [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!is_pop_array(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xxx)) {T}
  else {run("x", xxx, "(x)")}
}

#' @rdname is_xxx_array
#' @export
is_rcr_array <- function(x) {if (!is_pop_array(x)) {F} else {is.recursive(x)}}

#' @rdname is_xxx_array
#' @export
is_cmp_array <- function(x, xxx = NULL) {
  if (!is_atm_array(x, xxx)) {F}
  else {!any(is.na(x))}
}
