#' @name console.
#' @title Interact with the user via the console
#' @param ... An arbitrary number of atomic arguments to be atomized and glued
#'   together in a message.
#' @return \code{NULL} (called for side effects of updating user or pausing
#'   execution for user review).
#' @export
console. <- function() {help("console.", package = "uj")}

#' @describeIn console. Collapse \code{...} into a character scalar and print
#'   it to the console.
#' @export
say <- function(...) {cat(dw0(...))}

#' @describeIn console. Collapse \code{...} into a character scalar, prefix
#'   and post-fix a newline, and print the result to the console.
#' @export
cat0 <- function(...) {say("\n", ..., "\n")}

#' @describeIn console. Print \code{'Hit [enter] or [return] to continue'}
#'   to the console, and pause execution until user hits one of those keys.
#' @export
continue <- function() {readline("Hit [enter] or [return] to continue")}
