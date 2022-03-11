#' @name is_color
#' @family colors
#' @family is_functions
#' @title Is an object a vect of valid character color representations?
#' @param x An object
#' @param na \code{TRUE} or \code{FALSE}, indicating whether \code{NA} values
#'   qualify.
#' @return \code{TRUE} or \code{FALSE}.
is_color <- function(x, na = FALSE) {
  if (!isTF(na)) {stop("\n  * [na] must be TRUE or FALSE.")}
  else if (!xchr(x)) {F}
  else if (isF(na) & any(is.na(x))) {F}
  else {!isERR(as_color(x, na = na))}
}
