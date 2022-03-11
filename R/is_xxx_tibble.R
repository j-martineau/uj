#' @name is_xxx_tibble
#' @family is_functions
#' @title Is an object a type of tibble (maybe with another property as well)?
#' @description Checks for the following types of tibbles:
#'   \tabular{lll}{
#'   \strong{Type}\tab\strong{Function}   \tab\strong{Definition}
#'   \cr Any      \tab\code{is_any_tibble}\tab A
#'                                             \link[tibble:is_tibble]{tibble}.
#'   \cr Populated\tab\code{is_pop_tibble}\tab A tibble of length ≥ 1.
#'   \cr Empty    \tab\code{is_emp_tibble}\tab A tibble of length 0.
#'   \cr Atomic   \tab\code{is_atm_tibble}\tab A populated tibble whose cells
#'                                             all contain atomic scalars.
#'   \cr Recursive\tab\code{is_rcr_tibble}\tab A populated tibble with one or
#'                                             more cells containing a value
#'                                             that is not an atomic scalar.
#'   \cr Complete \tab\code{is_cmp_tibble}\tab A populated atomic tibble
#'                                             containing no \code{NA} elements.
#'   }
#' @param x An object
#' @param xxx \code{NULL} or a character scalar from \code{mmm_vals}
#'   indicating an optional check for a specific extended mode
#' @return \code{TRUE} or \code{FALSE}
is_any_tibble <- function(x) {tibble::is_tibble(x)}

#' @rdname is_xxx_tibble
#' @export
is_pop_tibble <- function(x) {if (!tibble::is_tibble(x)) {F} else {length(x) > 0}}

#' @rdname is_xxx_tibble
#' @export
is_emp_tibble <- function(x) {if (!tibble::is_tibble(x)) {F} else {length(x) == 0}}

#' @rdname is_xxx_tibble
#' @export
is_atm_tibble <- function(x, xxx = NULL) {
  V <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!V) {stop("\n  * [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!is_pop_tibble(x)) {F}
  else if (!all(apply(x, 2, is.atomic))) {F}
  else if (is.null(xxx)) {T}
  else {
    R <- paste0("x", xxx, "(x[ , ", 1:length(x), "])")
    R <- paste0(R, collapse = ", ")
    R <- paste0("all(", R, ")")
    run(R)
  }
}

#' @rdname is_xxx_tibble
#' @export
is_rcr_tibble <- function(x) {
  if (length(x) == 0) {F}
  else if (!tibble::is_tibble(x)) {F}
  else {!all(apply(x, 2, is.atomic))}
}

#' @rdname is_xxx_tibble
#' @export
is_cmp_tibble <- function(x, xxx = NULL) {
  if (is_atm_tibble(x, xxx)) {!any(is.na(x))}
  else {F}
}
