#' @name failsafe
#' @encoding UTF-8
#' @family extensions
#' @family failsafe
#' @family errs
#' @title Evaluate objects and flag any errors
#' @description \tabular{rl}{
#'     `failsafe`   \tab Calls `identity(x)`. If an error is produced in doing so, returns an object of class `'error'` or of class `'simpleError'`. If an error is not generated, returns the safely evaluated value of `x`.
#'   \cr            \tab  
#'   \cr `notERR`   \tab Evaluates whether calling `identity(x)` *does not* produce an error.
#'   \cr            \tab  
#'   \cr `msgERR`   \tab Gets any error message associated with calling `identity(x)`. If there is none, returns `NULL`.
#'   \cr            \tab  
#'   \cr  `isERR`   \tab Evaluates whether calling `identity(x)` produced an error.
#' }
#' @param x An object or a call to evaluate in the environment of a parent function where the initial call was made.
#' @return *Either* `x`, *an object of class* `'error'` *or of class* `'simpleError'`
#'   \cr    `failsafe`
#'   \cr\cr `NULL` *or a character scalar*
#'   \cr    `msgERR`
#'   \cr\cr *A logical scalar*
#'   \cr    `notERR`
#'   \cr    `isERR`
#' @examples
#' failsafe(pi)
#' notERR(pi)
#' msgERR(pi)
#' isERR(pi)
#'
#' failsafe(non.existent.variable)
#' notERR(non.existent.variable)
#' msgERR(non.existent.variable)
#' isERR(non.existent.variable)
#' @export
failsafe <- function(x) {tryCatch(identity(x), error = function(e) e, finally = NULL)}

#' @rdname failsafe
#' @export
isERR <- function(x) {any(tryCatch(identity(x), error = function(e) e, finally = NULL) %in% c("error", "simpleError"))}

#' @rdname failsafe
#' @export
notERR <- function(x) {!any(tryCatch(identity(x), error = function(e) e, finally = NULL) %in% c("error", "simpleError"))}

#' @rdname failsafe
#' @export
msgERR <- function(x) {
  x <- tryCatch(identity(x), error = function(e) e, finally = NULL)
  if (any(class(x) %in% c("error", "simpleError"))) {x$message} else {NULL}
}
