#' @encoding UTF-8
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
blank <- function(x) {uj::f0(uj::notN1(x) | uj::notCHR(x), F, uj::f0(uj::na(x), F, x == ""))}
