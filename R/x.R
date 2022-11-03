#' @name removal
#' @title Remove various objects or characteristics.
#' @description Remove from \code{x} the attributes named in \code{...}.
#' @param x. An object.
#' @param ... Character vectors containing attribute names or classes to remove.
#' @return An object (\code{xat} and \code{xcls}) or \code{NULL} (\code{xwarn}
#'   and \code{xcls}).
#' @export
xat <- function(x., ...) {for (a. in av(...)) {attr(x., a.) <- NULL}; x.}

#' @describeIn removal Remove from \code{x.} the classes named in \code{...}.
#' @export
xcls <- function(x., ...) {class(x.) <- class(x.)[!(class(x.) %in% av(...))]; x.}

#' @describeIn removal Purge all warnings.
#' @export
xwarn <- function() {assign("last.warning", NULL, envir = baseenv())}

#' @describeIn removal Purge all plots and if optional \code{x.} is provided,
#'   print it (\code{x.} is intended to be a plot object).
#' @export
xplot <- function(x. = NULL) {grDevices::graphics.off(); if (ipop(x.)) {print(x.)}}

#' @describeIn removal Clear the console.
#' @export
xcon <- function() {cat("\014")}
