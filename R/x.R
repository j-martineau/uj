#' @encoding UTF-8
#' @family extensions
#' @family meta
#' @aliases removal
#' @title '`x`' out objects or their characteristics
#' @description All `x` (removal) functions are followed by garbage collection.
#' \tabular{rl}{
#'          `xall`   \tab **WARNING: Use with care!**
#'   \cr             \tab **Purges the global environment.**
#'   \cr             \tab  
#'   \cr   `xplot`   \tab Purges plots and prints `x`.
#'   \cr   `xwarn`   \tab Purges warnings.
#'   \cr    `xcon`   \tab Purges console.
#'   \cr    `xcls`   \tab Removes classes by name.
#'   \cr    `xat0`   \tab Removes all attributes.
#'   \cr     `xat`   \tab Removes attributes by name.
#'   \cr       `x`   \tab Deletes `...` args in the calling environment.
#' }
#' @param x An R object.
#' @param ... One or more \link[=cmp_chr_vec]{complete character vecs} containing names of attributes or classes to remove.
#' @return `NULL`
#'   \cr    `xplot
#'   \cr    `xwarn`
#'   \cr    `xcon`
#'   \cr    `xall`
#'   \cr    `x`
#'   \cr
#'   \cr *An* R *object*
#'   \cr    `xcls`
#'   \cr    `xat0`
#'   \cr    `xat`
#' @examples
#' obj. <- data.frame(x = -5:5, y = sqrt(abs(-5:5)))
#' class(obj.) <- c(class(obj.), "ccc")
#' attr(obj., "aaa") <- "aaa"
#'
#' ## structure with custom class and attribute.
#' str(obj.)
#'
#' ## structure after removing just the custom class.
#' str(xcls(obj., "ccc"))
#'
#' ## structure after removing just the custom attribute.
#' str(xat(obj., "aaa"))
#'
#' ## structure after removing all attributes.
#' str(xat0(obj.))
#'
#' \dontrun{
#' ## build a ggplot scatterplot object (y.)
#' library(ggplot2)
#' ggp. <- ggplot(obj., aes(x, y)) + geom_point()
#'
#' ## print to console, wait for user, purge console
#' say("\n A message")
#' continue()
#' xcon()
#'
#' ## display a scatterplot, wait for user
#' plot(obj.$x, obj.$y)
#' continue()
#'
#' ## purge plots and print ggplot
#' xplot(ggp.)
#'
#' ## wait for user, purge plots again
#' continue()
#' xplot()
#'
#' ## generate warning, print warnings
#' sqrt(-1)
#' warnings()
#'
#' ## purge warnings, validate that warnings are purged
#' xwarn()
#' warnings()
#' }
#' @export
x <- function(...) {
  x <- as.character(match.call())
  x <- x[2:length(x)]
  code <- paste0("rm(", paste0(x, collapse = ", "), ")")
  eval.parent(parse(text = code), n = 1)
  gc(verbose = FALSE)
}

#' @rdname x
#' @export
xall <- function() {
  rm(list = ls(envir = globalenv()), envir = globalenv())
  gc(verbose = FALSE)
}


#' @rdname x
#' @export
xat <- function(x, ...) {
  rm.ats <- as.character(av(...))
  curr.ats <- names(attributes(x))
  for (rm.at in rm.ats) {
    if (rm.at %in% curr.ats) {attr(x, rm.at) <- NULL}
  }
  gc(verbose = FALSE)
  x
}

#' @rdname x
#' @export
xat0 <- function(x) {
  attributes(x) <- NULL
  gc(verbose = FALSE)
  x
}

#' @rdname x
#' @export
xcls <- function(x, ...) {
  class(x) <- class(x)[!(class(x) %in% av(...))]
  gc(verbose = FALSE)
  x
}

#' @rdname x
#' @export
xwarn <- function() {
  x <- baseenv()$last.warning
  if (!is.null(x)) {
    locked <- bindingIsLocked("last.warning", baseenv())
    if (locked) {unlockBinding("last.warning", baseenv())}
    assign("last.warning", NULL, envir = baseenv())
    if (locked) {lockBinding("last.warning", baseenv())}
  }
  gc(verbose = FALSE)
}

#' @rdname x
#' @export
xplot <- function(x = NULL) {
  grDevices::graphics.off()
  if (ipop(x)) {print(x)}
  gc(verbose = FALSE)
}

#' @rdname x
#' @export
xcon <- function() {
  gc(verbose = FALSE)
  cat("\014")
}
