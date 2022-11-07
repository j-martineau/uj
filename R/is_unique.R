#' @name is_unq.
#' @family extensions
#' @title Does an object have only unique atomic values?
#' @param x An atomic object.
#' @param a \code{TRUE} or \code{FALSE} indicating whether to reduce \code{x} to
#'   an atomic vector containing all of its atomic values. When \code{FALSE} and
#'   \code{x} is not atomic, throws an error.
#' @param na \code{TRUE} or \code{FALSE} indicating whether \code{NA} values are
#'   allowed.
#' @return \code{TRUE} or \code{FALSE}.
#' @export
is_unq. <- function() {help("is_unq.", package = "uj")}

#' @describeIn is_unq. Evaluates whether \code{x} contains only only unique
#'   atomic values, with options for for atomizing before evaluation and for
#'   handling \code{NA} values.
#' @export
is_unq <- function(x, a = T, na = F) {
  if (isTRUE(a)) {x <- unlist(x, T, F)}
  vx  <- is.atomic(x)
  vn  <- length(x) > 0
  va  <- isTF(a)
  vna <- isTF(na)
  vx2 <- f0(vx & isFALSE(na), !any(is.na(x)), T)
  err <- NULL
  if (!vx ) {err <- c(err, "\n • [x] must be atomic.")}
  if (!vn ) {err <- c(err, "\n • [x] is of length 0.")}
  if (!va ) {err <- c(err, "\n • [a] must be TRUE or FALSE.")}
  if (!vna) {err <- c(err, "\n • [na] must be TRUE or FALSE.")}
  if (!vx2) {err <- c(err, "\n • [x] contains NA but [na = FALSE].")}
  if (idef(err)) {stop(err)}
  length(x) == length(unique(x))
}
