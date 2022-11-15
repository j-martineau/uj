#' @name xpr.
#' @family strings
#' @family code
#' @title Build and/or Evaluate Expressions
#' @param ... An arbitrary number of objects atomized into a vector and then
#'   collapsed to create an expression.
#' @return Either a character scalar or the result of calling the expression.
#' @export
xpr. <- function() {help("xpr.", package = "uj")}

#' @describeIn xpr. Build an expression bolding the character scalar produced
#'   by collapsing \code{...}.
#' @export
xbold <- function(...) {paste0(av("expression(bold(", ..., "))"), collapse = "")}

#' @describeIn xpr. Build an expression from the character scalar produced by
#'   collapsing \code{...} and then execute the resulting command.
#' @export
xrun <- function(...) {run(paste0(av("expression(", ..., ")"), collapse = ""))}
