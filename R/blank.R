#' @encoding UTF-8
#' @family strings
#' @title Is an Object a Scalar Blank String?
#' @description Evaluates whether `x` is a blank string scalar (i.e., `""`).
#' @param x An R object
#' @return A logical scalar
#' @examples
#' blank("")
#' blank(2)
#' blank(NA)
#' blank(NULL)
#' blank(v(blank))
#' blank(c("", ""))
#' @export
blank <- function(x) {
  if (base::length(x) == 1) {if (base::is.character(x)) {if (!base::is.na(x)) {return(x == "")}}}
  F
}
