#' @name x.
#' @family extensions
#' @title Remove various objects or characteristics.
#' @param x. An object.
#' @param ... Character vectors containing attribute names or classes to remove.
#' @return An object (\code{xat} and \code{xcls}) or \code{NULL} (\code{xwarn}
#'   and \code{xcls}).
#' @export
x. <- function() {help("x.", package = "uj")}

#' @describeIn x. Remove from \code{x.} the attributes named in \code{...}.
#' @export
xat <- function(x., ...) {for (a. in av(...)) {attr(x., a.) <- NULL}; x.}

#' @describeIn x. Remove all attributes from \code{x.}.
#' @export
xat0 <- function(x.) {attributes(x.) <- NULL; x.}

#' @describeIn x. Remove from \code{x.} the classes named in \code{...}.
#' @export
xcls <- function(x., ...) {class(x.) <- class(x.)[!(class(x.) %in% av(...))]; x.}

#' @describeIn x. Purge all warnings.
#' @export
xwarn <- function() {assign("last.warning", NULL, envir = baseenv())}

#' @describeIn x. Purge all plots. Then if optional \code{x.} is provided, print
#'   it (\code{x.} is intended to be a plot object).
#' @export
xplot <- function(x. = NULL) {grDevices::graphics.off(); if (ipop(x.)) {print(x.)}}

#' @describeIn x. Clear the console.
#' @export
xcon <- function() {cat("\014")}
