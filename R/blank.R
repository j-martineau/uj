#' @title Blank Strings
#' @family strings
#' @description Evaluate whether `x` is a blank string scalar (i.e., `""`).
#' @param x An R object
#' @return A logical scalar
#' @export
blank <- function(x) {f0(length(x) == 1 & is.character(x), x == "", F)}
