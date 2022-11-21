#' @name failsafe
#' @family extensions
#' @family errs
#' @family failsafe
#' @title Evaluate objects, flag any errors
#' @description \tabular{ll}{
#'   \code{failsafe}   \tab Attempts to call \code{identity(x)}. If an error is
#'                          produced in doing so, returns an object of class
#'                          \code{'error'} or of class \code{'simpleError'}. If
#'                          an error is not generated, returns the safely
#'                          evaluated value of \code{x}.                     \cr
#'   \code{isERR}      \tab Evaluates whether calling \code{identity(x)}
#'                          produced an error.                               \cr
#'   \code{notERR}     \tab Evaluates whether calling \code{identity(x)} did not
#'                          produce an error.                                \cr
#'   \code{msgERR}     \tab Gets any error message associated with calling
#'                          \code{identity(x)}. If there is none, returns
#'                          \code{NULL}.                                       }
#' @param x An object or a call to evaluate in the environment of a parent
#'   function where the initial call was made.
#' @return \tabular{lll}{
#'   \code{failsafe}            \tab   \tab Either \code{x}, an object of class
#'                                         \code{'error'}, or an object of class
#'                                         \code{'simpleError'}.             \cr
#'   \code{isERR}, \code{notERR}\tab   \tab A logical scalar.                \cr
#'   \code{msgERR}              \tab   \tab Either \code{NULL} or a character
#'                                          scalar.                            }
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
