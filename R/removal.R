#' @name removal
#' @description \tabular{ll}{
#'   \code{xat}     \tab Remove one or more attributes by name.              \cr
#'   \code{xat0}    \tab Remove all attributes.                              \cr
#'   \code{xcls}    \tab Remove one or more class attributes by name.        \cr
#'   \code{xwarn}   \tab Purge all warnings.                                 \cr
#'   \code{xplot}   \tab Purge all plots (and if \code{x} is not \code{NULL},
#'                       print {x}, which is meant to be a plot object).     \cr
#'   \code{xcon}    \tab Clear the console.                                    }
#' @family extensions
#' @title Remove various objects or characteristics.
#' @param x An R object.
#' @param ... One or more \link[=cmp_chr_vec]{complete character vecs}
#'   containing names of attributes or classes to remove.
#' @return \tabular{lll}{
#'   \code{xat}, \code{xat0}, \code{xcls}
#'     \tab    \tab An R object.                                             \cr
#'     \tab    \tab                                                          \cr
#'   \code{xwarn}, \code{splot}, \code{xcon}
#'     \tab    \tab {NULL} (call for side effects).                            }
#' An object (\code{xat} and \code{xcls}) or \code{NULL} (\code{xwarn}
#'   and \code{xcls}).
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
