#' @name blank.
#' @title Blank strings
#' @description Evaluates whether \code{x} is a blank string scalar (i.e.,
#'   \code{""}).
#' @family strings
#' @param x An object
#' @return Logical scalar
#' @export
blank. <- function() {help("blank.", package = "uj")}

#' @rdname blank.
#' @export
blank <- function(x) {f0(n1(x) & ichr(x), x == "", F)}
