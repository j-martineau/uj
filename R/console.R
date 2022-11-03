#' @name console
#' @title Interact with the user via the console
#' @description Say something to the user without asking for something.
#' @param ... An arbitrary number of atomic arguments to be atomized and glued
#'   together in a message.
#' @return \code{NULL} (called for side effects of updating user or pausing
#'   execution for user review).
say <- function(...) {cat(dw0(...))}

#' @describeIn console Prefix a newline and postfix a newline to message.
#' @export
cat0 <- function(...) {say("\n", ..., "\n")}

#' @describeIn console Pause execution until user hits \code{\[enter\]} or
#'   \code{\[return\]}.
#' @export
continue <- function() {readline("Hit [enter] or [return] to continue")}
