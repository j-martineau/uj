#' @name binfuns
#' @family meta
#' @title Binary Functions Returning Only a Scalar \code{TRUE} or \code{FALSE}.
#' @description Evaluates whether \code{x} and \code{y} are identical.
#' @param x,y Any objects.
#' @export
`%IS%` <- function(x, y) {identical(x, y)}

#' @describeIn binfuns Evaluates whether \code{x} and \code{y} are not
#'   identical.
#' @export
`%!IS%` <- function(x, y) {!identical(x, y)}

#' @describeIn binfuns Evaluates whether \code{x} and \code{y} are
#'   \code{\link{set.equal}}. If \code{x} and \code{y} are not
#'   \link[=compatible]{compatible}, returns \code{FALSE}.
#' @export
`%EQ%` <- function(x, y) {isEQ(x, y)}

#' @describeIn binfuns Evaluates whether \code{x} and \code{y} are not
#'   \code{\link{set.equal}}. If \code{x} and \code{y} are not
#'   \link[=compatible]{compatible}, returns \code{TRUE}.
#' @export
`%!EQ%` <- function(x, y) {!isEQ(x, y)}

#' @describeIn binfuns Evaluates whether \code{x} and \code{y} are both
#'   \code{TRUE} (that is, a scalar \code{TRUE} value).
#' @export
`%AND%` <- function(x, y) {isT(x) & isT(y)}

#' @describeIn binfuns Evaluates whether \code{x} and/or \code{y} is
#'   \code{TRUE}.
#' @export
`%OR%` <- function(x, y) {isT(x) | isT(y)}

#' @describeIn binfuns Evaluates whether \code{x} and \code{y} are both
#'   \code{FALSE} (that is, a scalar \code{FALSE} value).
#' @export
`%!%` <- function(x, y) {isF(x) & isF(y)}

#' @describeIn binfuns Evaluates whether one of \code{x} or \code{y} is
#'   \code{TRUE}.
#' @export
`%1%` <- function(x, y) {f0(isT(x), isF(y), isT(y))}

#' @describeIn binfuns Evaluates whether atomic scalar \code{x} is contained in
#'   atomic object \code{y}. If \code{x} is not atomic scalar, \code{y} is not
#'   atomic, or \code{x} and \code{y} are not compatible, returns \code{FALSE}.
#' @export
`%IN%` <- function(x, y) {isIN(x, y)}

#' @describeIn binfuns Evaluates whether atomic scalar \code{x} is contained in
#'   atomic object \code{y}. If \code{x} is not atomic scalar, \code{y} is not
#'   atomic, or \code{x} and \code{y} are not compatible, returns \code{FALSE}.
#' @export
`%!IN%` <- function(x, y) {notIN(x, y)}

#' @describeIn binfuns Evaluates whether atomic object \code{x} contains atomic
#'   scalar \code{y}. If \code{x} is not atomic, \code{y} is not atomic scalar,
#'   or \code{x} and \code{y} are not compatible, returns \code{FALSE}.
#' @export
`%HAS%` <- function(x, y) {isIN(y, x)}

#' @describeIn binfuns Evaluates whether atomic object \code{x} lacks (does not
#'   contain) atomic scalar \code{y}. If \code{x} is not atomic, \code{y} is not
#'   atomic scalar, or \code{x} and \code{y} are not compatible, returns
#'   \code{TRUE}.
#' @export
`%LACKS%` <- function(x, y) {notIN(y, x)}
