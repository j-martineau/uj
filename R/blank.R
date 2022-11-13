#' @name blank.
#' @title Blank strings
#' @family strings
#' @param x An object
#' @return Logical scalar
#' @export
blank. <- function() {help("blank.", package = "uj")}

#' @describeIn blank. Evaluates whether \code{x} is a blank string scalar.
#' @export
blank <- function(x) {f0(n1(x) & ichr(x), x == "", F)}
