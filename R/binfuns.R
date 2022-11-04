#' @name bin_uj
#' @family meta
#' @title Binary functions returning scalar \code{TRUE} or \code{FALSE}.
#' @param x,y Any objects.
#' @export
bin_uj <- function() {help("bin_uj", package = "uj")}

#' @describeIn bin_uj Evaluates whether \code{x} and \code{y} are identical.
#' @export
`%IS%` <- function(x, y) {identical(x, y)}

#' @describeIn bin_uj Evaluates whether \code{x} and \code{y} are not identical.
#' @export
`%!IS%` <- function(x, y) {!identical(x, y)}

#' @describeIn bin_uj Evaluates whether \code{x} and \code{y} are setequal. If
#'   \code{x} and \code{y} are not compatible, returns \code{FALSE}.
#' @export
`%EQ%` <- function(x, y) {isEQ(x, y)}

#' @describeIn bin_uj Evaluates whether \code{x} and \code{y} are not setequal.
#'   If \code{x} and \code{y} are not compatible, returns \code{TRUE}.
#' @export
`%!EQ%` <- function(x, y) {!isEQ(x, y)}

#' @describeIn bin_uj Evaluates whether \code{x} and \code{y} are both
#'   \code{TRUE} (that is, a scalar \code{TRUE} value).
#' @export
`%AND%` <- function(x, y) {isT(x) & isT(y)}

#' @describeIn bin_uj Evaluates whether \code{x} and/or \code{y} is
#'   \code{TRUE}.
#' @export
`%OR%` <- function(x, y) {isT(x) | isT(y)}

#' @describeIn bin_uj Evaluates whether \code{x} and \code{y} are both
#'   \code{FALSE} (that is, a scalar \code{FALSE} value).
#' @export
`%!%` <- function(x, y) {isF(x) & isF(y)}

#' @describeIn bin_uj Evaluates whether one of \code{x} or \code{y} is
#'   \code{TRUE}.
#' @export
`%1%` <- function(x, y) {f0(isT(x), isF(y), isT(y))}

#' @describeIn bin_uj Evaluates whether atomic scalar \code{x} is contained in
#'   atomic object \code{y}. If \code{x} is not atomic scalar, \code{y} is not
#'   atomic, or \code{x} and \code{y} are not compatible, returns \code{FALSE}.
#' @export
`%IN%` <- function(x, y) {isIN(x, y)}

#' @describeIn bin_uj Evaluates whether atomic scalar \code{x} is contained in
#'   atomic object \code{y}. If \code{x} is not atomic scalar, \code{y} is not
#'   atomic, or \code{x} and \code{y} are not compatible, returns \code{FALSE}.
#' @export
`%!IN%` <- function(x, y) {notIN(x, y)}

#' @describeIn bin_uj Evaluates whether atomic object \code{x} contains atomic
#'   scalar \code{y}. If \code{x} is not atomic, \code{y} is not atomic scalar,
#'   or \code{x} and \code{y} are not compatible, returns \code{FALSE}.
#' @export
`%HAS%` <- function(x, y) {isIN(y, x)}

#' @describeIn bin_uj Evaluates whether atomic object \code{x} lacks (does not
#'   contain) atomic scalar \code{y}. If \code{x} is not atomic, \code{y} is not
#'   atomic scalar, or \code{x} and \code{y} are not compatible, returns
#'   \code{TRUE}.
#' @export
`%LACKS%` <- function(x, y) {notIN(y, x)}
