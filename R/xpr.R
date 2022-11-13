#' @name xpr.
#' @family strings
#' @family code
#' @title Build and/or evaluate expressions
#' @param ... An arbitrary number of objects atomized and glued to create the
#'   expression.
#' @return Either a character scalar or the result of calling the expression.
#' @export
xpr. <- function() {help("xpr.", package = "uj")}

#' @describeIn xpr. Build an expression containing bolded text.
#' @export
xbold <- function(...) {paste0(av("expression(bold(", ..., "))"), collapse = "")}

#' @describeIn xpr. Build an expression and execute the resulting command.
#' @export
xrun <- function(...) {run(paste0(av("expression(", ..., ")"), collapse = ""))}
