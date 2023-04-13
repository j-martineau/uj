#' @encoding UTF-8
#' @family strings
#' @title Blank strings
#' @description Evaluate whether `X` is a blank string scalar (i.e., `""`).
#' @param X An R object
#' @return A logical scalar
#' @examples
#' blank(NA)
#' blank(c("", ""))
#' blank("")
#' blank(v(blank))
#' blank(2)
#' blank(NULL)
#' @export
blank <- function(X) {if (base::length(X) != 1) {F} else if (!base::is.character(X)) {F} else if (!base::is.na(X)) {F} else {X == ""}}
