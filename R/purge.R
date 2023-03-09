#' @encoding UTF-8
#' @name purge
#' @family extensions
#' @family meta
#' @title Purge objects and parts of objects
#' @details All purge (`x`) functions include garbage collection.
#' \tabular{ll}{  `purge_pref, rm__pref, xpref`   \tab Purge package prefixes\eqn{^{(1)}}    \cr
#'                `purge_plot, rm_plot, xplot`    \tab Purge plots\eqn{^{(2)}}               \cr
#'                `purge_warn, rm_warn, xwarn`    \tab Purge warnings                        \cr
#'                `purge_con, rm_con, xcon`       \tab Purge console                         \cr
#'                `purge, x`                      \tab Purge specific objects                  }
#'  \tabular{l}{  \eqn{^{(1)}} Converts `'base::paste'` to `'paste'`, for example.           \cr
#'                \eqn{^{(2)}} Followed by printing the object `x` if not `NULL`.  \cr   \tab  }
#' \tabular{ll}{  `purge_global, purge_all`      \tab **WARNING: Use with care!**            \cr
#'                `rm_global, rm_all`            \tab    *These functions purge *            \cr
#'                `xglobal, xall`                \tab    *the global environment*              }
#' @param x Anticipated to be a plot object, but any object can be handled.
#' @param funs \link[=cmp_chr_vec]{A complete character vec} of function names, possibly with package prefixes.
#' @param ... One or more \link[=cmp_chr_vec]{complete character vecs} containing names of objects to purge from the global environment.
#' @return **A character vector**    \cr\cr `purge_pref, rm_pref, xpref`
#' \cr\cr  **The** `NULL` **object** \cr\cr all others
#' @examples
#' egFunNames <- c("base::c", "uj::cmp_chr_scl", "run")
#' xpref(egFunNames)
#' \dontrun{
#'   ## build a ggplot scatterplot object (y.)
#'   library(ggplot2)
#'   egObj <- data.frame(x = 0:100, y = sqrt(0:100)
#'   egGgp <- ggplot(egObj, aes(x, y)) + geom_point()
#'
#'   ## print to console, wait for user, purge console
#'   say("\n A message")
#'   continue()
#'   xcon()
#'
#'   ## display a scatterplot, wait for user
#'   plot(egObj$x, egObj$y)
#'   continue()
#'
#'   ## purge plots and print ggplot
#'   xplot(egGgp)
#'
#'   ## wait for user, purge plots again
#'   continue()
#'   xplot()
#'
#'   ## generate warning, print warnings
#'   sqrt(-1)
#'   warnings()
#'
#'   ## purge warnings, validate that warnings are purged
#'   xwarn()
#'   warnings()
#' }
#' @export
purge <- function(...) {
  x <- base::as.character(base::match.call())
  x <- x[2:base::length(x)]
  code <- base::paste0("rm(", uj::g(", ", x), ")")
  base::eval.parent(base::parse(text = code), n = 1)
  base::gc(verbose = FALSE)
}

#' @rdname purge
#' @export
x <- purge

#' @rdname purge
#' @export
purge_plot <- function(x = NULL) {
  grDevices::graphics.off()
  if (!base::is.null(x)) {base::print(x)}
  base::gc(verbose = FALSE)
}

#' @rdname purge
#' @export
rm_plot <- purge_plot

#' @rdname purge
#' @export
xplot <- purge_plot

#' @rdname purge
#' @export
purge_warnings <- function() {
  base::gc(verbose = FALSE)
  x <- base::baseenv()$last.warning
  if (!base::is.null(x)) {
    locked <- base::bindingIsLocked("last.warning", base::baseenv())
    if (locked) {base::unlockBinding("last.warning", base::baseenv())}
    base::assign("last.warning", NULL, envir = base::baseenv())
    if (locked) {base::lockBinding("last.warning", base::baseenv())}
  }
}

#' @rdname purge
#' @export
purge_warn <- purge_warnings

#' @rdname purge
#' @export
xwarnings <- purge_warnings

#' @rdname purge
#' @export
xwarn <- purge_warnings

#' @rdname purge
#' @export
purge_console <- function() {
  base::gc(verbose = FALSE)
  base::cat("\014")
}

#' @rdname purge
#' @export
purge_con <- purge_console

#' @rdname purge
#' @export
xconsole <- purge_console

#' @rdname purge
#' @export
xcon <- purge_console

#' @rdname purge
#' @export
purge_pref <- function(funs) {
  base::gc(verbose = FALSE)
  if (!uj:::.cmp_chr_vec(funs)) {uj::stopperr("[funs] must be a complete character vec (?cmp_chr_vec).", PKG = "uj")}
  where <- base::regexpr(":::", funs, fixed = T)
  where[where == -1] <- -2
  funs <- base::substr(funs, where + 3, base::nchar(funs))
  base::substr(funs, base::regexpr("::", funs, fixed = T) + 2, base::nchar(funs))
}

#' @rdname purge
#' @export
rm_pref <- purge_pref

#' @rdname purge
#' @export
xpref <- purge_pref

#' @rdname purge
#' @export
purge_global <- function() {
  base::rm(list = base::ls(envir = base::.GlobalEnv), envir = base::.GlobalEnv)
  uj::xplot()
  uj::xwarn()
  uj::xcon()
  base::gc(verbose = FALSE)
}

#' @rdname purge
#' @export
purge_all <- purge_global

#' @rdname purge
#' @export
rm_global <- purge_global

#' @rdname purge
#' @export
rm_all <- purge_global

#' @rdname purge
#' @export
xglobal <- purge_global

#' @rdname purge
#' @export
xall <- purge_global
