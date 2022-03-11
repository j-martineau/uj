#' @name is_xxx_generic
#' @family is_functions
#' @title Is an object a generic (maybe with another property as well)?
#' @description A generic is an array, \link[=is_vtype]{vtype},
#'   \link[tibble:is_tibble]{tibble}, or \link[=is_vlist]{vlist}.
#'   \cr\cr
#'   Also checks for the following types of generics:
#'   \tabular{lll}{
#'   \strong{Type}  \tab\strong{Function}\tab\strong{Definition}
#'     \cr Any      \tab\code{is_any_generic}\tab Any generic.
#'     \cr Populated\tab\code{is_pop_generic}\tab Length is ≥ 1.
#'     \cr Empty    \tab\code{is_emp_generic}\tab Length is 0.
#'     \cr Atomic   \tab\code{is_atm_generic}\tab A populated atomic array, a
#'                  populated \link[=is_atm_vtype]{atomic vtype}, a
#'                  populated \link[=is_atm_tibble]{atomic tibble}, or a
#'                  populated \link[=is_atm_vlist]{atomic vlist}).
#'     \cr Recursive\tab\code{is_rcr_generic}\tab A populated
#'                  \link[=is_rcr_array]{recursive array}, a proulated
#'                  \link[=is_rcr_vtype]{recursive vtype}, a populated
#'                  \link[=is_rcr_tibble]{recursive tibble}, or a
#'                  populated \link[=is_rcr_vlist]{recursive vlist}.
#'     \cr Complete \tab\code{is_cmp_generic}\tab An atomic generic containing
#'                  no \code{NA} elements.
#'   }
#' @param x An object
#' @param xxx \code{NULL} or a character scalar from \code{mmm_vals}
#'   indicating an optional check for a specific extended mode.
#' @return \code{TRUE} or \code{FALSE}
#' @export
is_generic <- function(x) {
  if (is.vector(x) | is.array(x)) {T}
  else {tibble::is_tibble(x)}
}

#' @rdname is_xxx_generic
#' @export
is_any_generic <- is_generic

#' @rdname is_xxx_generic
#' @export
is_pop_generic <- function(x) {if (length(x) == 0) {F} else {is_generic(x)}}

#' @rdname is_xxx_generic
#' @export
is_emp_generic <- function(x) {if (length(x) > 0) {F} else {is_generic(x)}}

#' @rdname is_xxx_generic
#' @export
is_atm_generic <- function(x, xxx = NULL) {
  V <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!V) {stop("\n  * [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!is_pop_generic(x)) {F}
  else if (!is.atomic(x)) {F}
  else if (is.null(xxx)) {T}
  else {run("x", xxx, "(x)")}
}

#' @rdname is_xxx_generic
#' @export
is_rcr_generic <- function(x) {
  if (!is_pop_generic(x)) {F} else {is.recursive(x)}
}

#' @rdname is_xxx_generic
#' @export
is_cmp_generic <- function(x, xxx = NULL) {
  if (!is_atm_generic(x, xxx)) {F}
  else {!any(is.na(av(x)))}
}
