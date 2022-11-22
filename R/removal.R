#' @name removal
#' @family extensions
#' @title Remove Various Objects or Characteristics
#' @section Functions in This Family:
#'   \strong{\code{xat}}
#'   \cr Remove one or more attributes by name.
#'   \cr\cr
#'   \strong{\code{xat0}}
#'   \cr Remove all attributes.
#'   \cr\cr
#'   \strong{\code{xcls}}
#'   \cr Remove one or more class attributes by name.
#'   \cr\cr
#'   \strong{\code{xwarn}}
#'   \cr Purge all warnings.
#'   \cr\cr
#'   \strong{\code{xplot}}
#'   \cr Purge all plots (and if \code{x} is not \code{NULL}, print {x}, which
#'   is meant to be a plot object).
#'   \cr\cr
#'   \strong{\code{xcon}}
#'   \cr Clear the console.
#' @param x An R object.
#' @param ... One or more \link[=cmp_chr_vec]{complete character vecs}
#'   containing names of attributes or classes to remove.
#' @return \strong{\code{xat, xat0, xcls}}
#'   \cr An R object.
#'   \cr\cr
#'   \strong{\code{xwarn, splot, xcon}}
#'   \cr \code{NULL} (called for side effects).
#' @export
xat <- function(x, ...) {for (a in av(...)) {attr(x, a) <- NULL}; x}

#' @rdname removal
#' @export
xat0 <- function(x) {attributes(x) <- NULL; x}

#' @rdname removal
#' @export
xcls <- function(x, ...) {class(x) <- class(x)[!(class(x) %in% av(...))]; x}

#' @rdname removal
#' @export
xwarn <- function() {assign("last.warning", NULL, envir = baseenv())}

#' @rdname removal
#' @export
xplot <- function(x = NULL) {grDevices::graphics.off(); if (ipop(x)) {print(x)}}

#' @rdname removal
#' @export
xcon <- function() {cat("\014")}
