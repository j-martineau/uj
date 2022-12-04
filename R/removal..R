#' @name removal
#' @encoding UTF-8
#' @title Remove objects or object characteristics
#' @description \tabular{rl}{
#'       `xplot`   \tab Purges plots + prints `x` if not `NULL`.
#'   \cr `xwarn`   \tab Purges warnings.
#'   \cr  `xcon`   \tab Purges the console.
#'   \cr  `xcls`   \tab Removes class(es) by name.
#'   \cr  `xat0`   \tab Removes all attributes.
#'   \cr   `xat`   \tab Removes attribute(s) by name.
#' }
#' @param x An R object.
#' @param ... One or more \link[=cmp_chr_vec]{complete character vecs} containing names of attributes or classes to remove.
#' @return \tabular{rl}{
#'    `xcon,xplot,xwarn`   \tab `NULL`.
#'   \cr `xat,xat0,xcls`   \tab An R object.
#'  }
#' @examples
#' x. <- data.frame(x = -5:5, y = sqrt(abs(-5:5)))
#' class(x.) <- c(class(x.), "ccc")
#' attr(x., "aaa") <- "aaa"
#'
#' ## Structure with custom class and attribute.
#' str(x.)
#'
#' ## After removing just the custom class.
#' str(xcls(x., "ccc"))
#'
#' ## After removing just the custom attribute.
#' str(xat(x., "aaa"))
#'
#' ## After removing all attributes.
#' str(xat0(x.))
#'
#' \dontrun{
#' ## Build a ggplot scatterplot object (y.)
#' library(ggplot2)
#' y. <- ggplot(x., aes(x, y)) + geom_point()
#'
#' ## Print to console, wait for user, purge console
#' cat("\n A message")
#' continue()
#' xcon()
#'
#' ## Display a scatterplot, wait for user
#' plot(x.$x, x.$y)
#' continue()
#'
#' ## Purge plots + print ggplot, wait for user, purge plots
#' xplot(y.)
#' continue()
#' xplot()
#'
#' ## Generate warning, print, purge, validate purge
#' sqrt(-1)
#' warnings()
#' xwarn()
#' warnings()
#' }
#' @export
xat <- function(x, ...) {
  rm.ats <- as.character(av(...))
  curr.ats <- names(attributes(x))
  for (rm.at in rm.ats) {if (rm.at %in% curr.ats) {attr(x, rm.at) <- NULL}}
  x
}

#' @rdname removal
#' @export
xat0 <- function(x) {attributes(x) <- NULL; x}

#' @rdname removal
#' @export
xcls <- function(x, ...) {class(x) <- class(x)[!(class(x) %in% av(...))]; x}

#' @rdname removal
#' @export
xwarn <- function() {
  x <- baseenv()$last.warning
  if (!is.null(x)) {
    locked <- bindingIsLocked("last.warning", baseenv())
    if (locked) {unlockBinding("last.warning", baseenv())}
    assign("last.warning", NULL, envir = baseenv())
    if (locked) {lockBinding("last.warning", baseenv())}
  }
  NULL
}

#' @rdname removal
#' @export
xplot <- function(x = NULL) {grDevices::graphics.off(); if (ipop(x)) {print(x)}}

#' @rdname removal
#' @export
xcon <- function() {cat("\014")}
