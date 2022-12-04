#' @family strings
#' @title Blank strings
#' @description Evaluate whether `x` is a blank string scalar (i.e., `""`).
#' @param x An R object
#' @return A logical scalar
#' @examples
#' blank(NA)
#' blank(c("", ""))
#' blank("")
#' blank(v(blank))
#' blank(2)
#' blank(NULL)
#' @export
blank <- function(x) {f0(length(x) != 1 | !is.character(x), F, f0(is.na(x), F, x == ""))}
