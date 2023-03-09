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
blank <- function(x) {if (base::length(x) != 1) {F} else if (!base::is.character(x)) {F} else if (!base::is.na(x)) {F} else {x == ""}}
