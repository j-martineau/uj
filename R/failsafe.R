#' @name failsafe_uj
#' @family meta
#' @family errs
#' @family failsafe
#' @title Evaluate objects, flag any errors
#' @param x An object or a call to evaluate in the environment of a parent
#'   function where the initial call was made.
#' @return \code{failsafe} returns either \code{x} or an object of class
#'   \code{'simpleError'}. \code{isERR, notERR} returns a logical scalar.
#'   \code{msgERR} returns either \code{NULL} or a character scalar.
#' @export
failsafe_uj <- function() {help("failsafe_uj", package = "uj")}

#' @describeIn failsafe_uj Attempts to call \code{identity(x)}. If an error is
#'   produced in doing so, returns an object of class \code{'error'} or of class
#'   \code{'simpleError'}. If an error is not generated, returns the safely
#'   evaluated value of \code{x}.
#' @export
failsafe <- function(x) {tryCatch(identity(x), error = function(e) e, finally = NULL)}

#' @describeIn failsafe_uj Evaluates whether calling \code{identity(x)} produced
#'   an error.
#' @export
isERR <- function(x) {any(class(failsafe(x)) %in% c("error", "simpleError"))}

#' @describeIn failsafe_uj Evaluates whether calling \code{identity(x)} did not
#'   produce an error.
#' @export
notERR <- function(x) {!any(class(failsafe(x)) %in% c("error", "simpleError"))}

#' @describeIn failsafe_uj Gets any error message associated with calling
#'   \code{identity(x)}. If there is none, returns \code{NULL}.
#' @export
msgERR <- function(x) {f0(isERR(x), x$message, NULL)}
