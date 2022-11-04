#' @name blank_uj
#' @title Blank strings
#' @family strings_uj
#' @param x An object
#' @return Logical scalar
#' @export
blank_uj <- function() {help("blank_uj", package = "uj")}

#' @describeIn blank_uj Evaluates whether \code{x} is a blank string scalar.
#' @export
blank <- function(x) {f0(n1(x) & ichr(x), x == v(blank), F)}
