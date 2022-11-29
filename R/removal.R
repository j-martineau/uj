#' @name removal
#' @family extensions
#' @title Remove objects or object characteristics
#' @description \itemize{
#'   \item **`xat`**: remove one or more attributes by name.
#'   \item **`xat0`**: removes all attributes.
#'   \item **`xcls`**: removes one or more class attributes by name.
#'   \item **`xwarn`**: purges all warnings.
#'   \item **`xplot`**: purges all plots (and if `x` is not `NULL`, print it).
#'   \item **`xcon`**: clears the console.
#' }
#' @param x An R object.
#' @param ... One or more \link[=cmp_chr_vec]{complete character vecs} containing names of attributes or classes to remove.
#' @return \itemize{
#'   \item **`xat, xat0, xcls`**: An R object.
#'   \item **`xwarn, splot, xcon`**: `NULL` (called for side effects).
#' }
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
