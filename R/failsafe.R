#' @name failsafe
#' @family errs
#' @family failsafe
#' @family extensions
#' @title Evaluate objects and flag any errors
#' @description \itemize{
#'   \item **`isERR`**: Evaluate whether calling `identity(x)` produced an error.
#'   \item **`notERR`**: Evaluate whether calling `identity(x)` *did not* produce an error.
#'   \item **`msgERR`**: Get any error message associated with calling `identity(x)`. If there is none, returns `NULL`.
#'   \item **`failsafe`**: Call `identity(x)`. If an error is produced in doing so, returns an object of class `'error'` or of class `'simpleError'`. If an error is not generated, returns the safely evaluated value of `x`.
#' }
#' @param x An object or a call to evaluate in the environment of a parent function where the initial call was made.
#' @return \itemize{
#'   \item **`msgERR`**: either `NULL` or a character scalar.
#'   \item **`failsafe`**: either `x`, an object of class `'error'`, or an object of class `'simpleError'`.
#'   \item **`isERR, notERR`**: A logical scalar.
#' }
#' @export
failsafe <- function(x) {tryCatch(identity(x), error = function(e) e, finally = NULL)}

#' @rdname failsafe
#' @export
isERR <- function(x) {any(class(failsafe(x)) %in% c("error", "simpleError"))}

#' @rdname failsafe
#' @export
notERR <- function(x) {!any(class(failsafe(x)) %in% c("error", "simpleError"))}

#' @rdname failsafe
#' @export
msgERR <- function(x) {f0(isERR(x), x$message, NULL)}
