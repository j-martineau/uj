#' @title Blank strings
#' @family strings
#' @description Evaluates whether \code{x} is a blank string scalar (i.e.,
#'   \code{""}).
#' @param x An object
#' @return Logical scalar
#' @export
blank <- function(x) {f0(n1(x) & ichr(x), x == "", F)}
