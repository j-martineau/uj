#' @name failsafe
#' @family meta
#' @family errs
#' @family failsafe
#' @title Evaluate objects, flag any errors
#' @details \strong{\code{failsafe(x)}}
#'   \cr Attempts to call \code{identity(x)}. If an error is produced in doing
#'   so, returns an object of class \code{'error'} or of class
#'   \code{'simpleError'}. If an error is not generated, returns the safely
#'   evaluated value of \code{x}.
#'   \cr\cr
#'   \strong{\code{isERR(x)}}
#'   \cr Evaluates whether calling \code{identity(x)} produced an error.
#'   \cr\cr
#'   \strong{\code{notERR(x)}}
#'   Evaluates whether calling \code{identity(x)} did not produce an error.
#'   \cr\cr
#'   \strong{\code{msgERR(x)}}
#'   Gets any error message associated with calling \code{identity(x)}. If there
#'   is none, returns \code{NULL}.
#' @param x An object or a call to evaluate in the environment of a parent
#'   function where the initial call was made.
#' @return \code{failsafe} returns either \code{x} or an object of class
#'   \code{'simpleError'}. \code{isERR, notERR} returns a logical scalar.
#'   \code{msgERR} returns either \code{NULL} or a character scalar.
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
