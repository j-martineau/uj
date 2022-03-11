#' @name is_xxx_vlist
#' @family is_functions
#' @title Is an object a vlist?
#' @description A vlist (vector-list) is a non-data.frame list.
#'   \cr\cr Also checks for the following types of vlists:
#'   \tabular{lll}{
#'   \strong{Type}\tab\strong{Function}  \tab\strong{Definition}
#'   \cr Any      \tab\code{is_any_vlist}\tab Any vlist.
#'   \cr Populated\tab\code{is_pop_vlist}\tab A non-empty vlist.
#'   \cr Empty    \tab\code{is_emp_vlist}\tab A length-0 vlist.
#'   \cr Atomic   \tab\code{is_atm_vlist}\tab A populated vlist, all of whose
#'                                            elements are populated and atomic.
#'   \cr Recursive\tab\code{is_rcr_vlist}\tab A populated vlist, all of whose
#'                                            elements are populated and at
#'                                            least one of which is recursive.
#'   \cr Complete \tab\code{is_cmp_vlist}\tab A populated atomic vlist
#'                                            containing no \code{NA} values.
#'   }
#' @param x An object
#' @param xxx \code{NULL} or a character scalar from \code{mmm_vals}
#'   indicating an optional check for a specific mmm.
#' @return \code{TRUE} or \code{FALSE}
#' @export
is_vlist <- function(x) {if (is.data.frame(x)) {F} else {is.list(x)}}

#' @rdname is_xxx_vlist
#' @export
is_any_vlist <- is_vlist

#' @rdname is_xxx_vlist
#' @export
is_pop_vlist <- function(x) {
  if (!is_vlist(x)) {F}
  else if (length(x) == 0) {F}
  else if (any(lengths(x) == 0)) {F}
  else {T}
}

#' @rdname is_xxx_vlist
#' @export
is_emp_vlist <- function(x) {if (!is_vlist(x)) {F} else {length(x) == 0}}

#' @rdname is_xxx_vlist
#' @export
is_atm_vlist <- function(x, xxx = NULL) {
  V <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!V) {stop("\n  * [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!is_pop_vlist(x)) {F}
  else if (!all(sapply(x, is.atomic))) {F}
  else if (is.null(xxx)) {T}
  else {
    R <- paste0("x", xxx, "(x[[", 1:length(x), "]])")
    R <- paste0(R, collapse = ", ")
    R <- paste0("all(", R, ")")
    run(R)
  }
}

#' @rdname is_xxx_vlist
#' @export
is_rcr_vlist <- function(x) {
  if (!is_pop_vlist(x)) {F}
  else {!all(sapply(x, is.atomic))}
}

#' @rdname is_xxx_vlist
#' @export
is_cmp_vlist <- function(x, xxx = NULL) {
  if (!is_atm_vlist(x, xxx)) {F}
  else {!any(is.na(av(x)))}
}
