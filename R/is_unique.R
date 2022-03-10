#' @name is_unique
#' @family is_functions
#' @title Does an object have only unique atomic values?
#' @param x An atomic object.
#' @param a \code{TRUE} or \code{FALSE} indicating whether to reduce \code{x} to
#'   an atomic vector containing all of its atomic values. When \code{FALSE} and
#'   \code{x} is not atomic, throws an error.
#' @param na \code{TRUE} or \code{FALSE} indicating whether \code{NA} values are
#'   allowed.
#' @return \code{TRUE} or \code{FALSE}.
#' @export
is_unique <- function(x, a = T, na = F) {
  if (isTRUE(a)) {x <- unlist(x, T, F)}
  VX  <- is.atomic(x)
  VN  <- length(x) > 0
  VA  <- isTF(a)
  VNA <- isTF(na)
  VX2 <- f0(VX & isFALSE(na), !any(is.na(x)), T)
  E   <- NULL
  if (!VX ) {E <- c(E, "\n  * [x] must be atomic.")}
  if (!VN ) {E <- c(E, "\n  * [x] is of length 0.")}
  if (!VA ) {E <- c(E, "\n  * [a] must be TRUE or FALSE.")}
  if (!VNA) {E <- c(E, "\n  * [na] must be TRUE or FALSE.")}
  if (!VX2) {E <- c(E, "\n  * [x] contains NA but [na = FALSE].")}
  if (xdef(E)) {stop(E)}
  length(x) == length(unique(x))
}
