#' @title Is an object a character scalar blank string.
#' @param x An object
#' @return Logical scalar
#' @export
blank <- function(x) {f0(n1(x) & ichr(x), x == v(blank), F)}
