#' @title Blank Strings
#' @family strings
#' @description Evaluates whether \code{x} is a blank string scalar (i.e.,
#'   \code{""}).
#' @param x An R object
#' @return Logical scalar
#' @export
blank <- function(x) {f0(length(x) == 1 & is.character(x), x == "", F)}
