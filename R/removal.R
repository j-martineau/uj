#' @name removal
#' @family extensions
#' @title Remove Various Objects or Characteristics
#' @description \tabular{ll}{
#'   FUNCTION   \tab WHAT IT DOES \cr
#'   `xat`      \tab Remove one or more attributes by name.                  \cr
#'   `xat0`     \tab Remove all attributes.                                  \cr
#'   `xcls`     \tab Remove one or more class attributes by name.            \cr
#'   `xwarn`    \tab Purge all warnings.                                     \cr
#'   `xplot`    \tab Purge all plots (and if `x` is not `NULL`, print it).   \cr
#'   `xcon`     \tab Clear the console.                                        }
#' @param x An R object.
#' @param ... One or more \link[=cmp_chr_vec]{complete character vecs}
#'   containing names of attributes or classes to remove.
#' @return \tabular{ll}{
#'   FUNCTION                   \tab RETURN VALUE                            \cr
#'   `xat, xat0, xcls`          \tab An R object.                            \cr
#'   `xwarn`, `splot`, `xcon`   \tab `NULL`. Called for side effects.          }
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
