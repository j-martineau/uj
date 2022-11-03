#' @name xpr
#' @title Build expressions as character scalars and/or run resulting commands.
#' @description Build expression containing bold text.
#' @param ... An arbitrary number of objects atomized and glued to create the
#'   expression.
#' @return Either a character scalar or the result of calling the expression.
#' @export
xbold <- function(...) {daw00("expression(bold(", ..., "))")}

#' @describeIn xpr Build an expression and execute the resulting command.
#' @export
xrun <- function(...) {run(daw("expression(", ..., ")"))}
