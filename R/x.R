#' @encoding UTF-8
#' @family extensions
#' @family meta
#' @aliases removal
#' @title '`x`' out objects
#' @description All `x` (removal) functions are followed by garbage collection.
#' \tabular{rl}{
#'          `xall`   \tab **WARNING: Use with care!**
#'   \cr             \tab **Purges the global environment.**
#'   \cr             \tab  
#'   \cr   `xplot`   \tab Purges plots and prints `x`.
#'   \cr   `xwarn`   \tab Purges warnings.
#'   \cr    `xcon`   \tab Purges console.
#'   \cr       `x`   \tab Deletes `...` args in the calling environment.
#' }
#' @param x An R object.
#' @param ... One or more \link[=cmp_chr_vec]{complete character vecs} containing names of attributes or classes to remove.
#' @return `NULL`
#'   \cr   `xplot`
#'   \cr   `xwarn`
#'   \cr   `xcon`
#'   \cr   `xall`
#'   \cr   `x`
#' @examples
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
  x <- base::as.character(base::match.call())
  x <- x[2:base::length(x)]
  code <- base::paste0("rm(", base::paste0(x, collapse = ", "), ")")
  base::eval.parent(base::parse(text = code), n = 1)
  base::gc(verbose = FALSE)
}

#' @rdname x
#' @export
xplot <- function(x = NULL) {
  grDevices::graphics.off()
  if (uj::ipop(x)) {base::print(x)}
  base::gc(verbose = FALSE)
}

#' @rdname x
#' @export
xall <- function() {
  base::rm(list = base::ls(envir = base::globalenv()), envir = base::globalenv())
  uj::xplot()
  base::gc(verbose = FALSE)
}

#' @rdname x
#' @export
xwarn <- function() {
  x <- base::baseenv()$last.warning
  if (!base::is.null(x)) {
    locked <- base::bindingIsLocked("last.warning", base::baseenv())
    if (locked) {unlockBinding("last.warning", base::baseenv())}
    base::assign("last.warning", NULL, envir = base::baseenv())
    if (locked) {base::lockBinding("last.warning", base::baseenv())}
  }
  base::gc(verbose = FALSE)
}

#' @rdname x
#' @export
xcon <- function() {
  base::gc(verbose = FALSE)
  base::cat("\014")
}
