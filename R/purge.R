#' @encoding UTF-8
#' @name purge
#' @family extensions
#' @family meta
#' @title Purge objects and parts of objects ('`x`' them out!)
#' @description All purge (`x`) functions include garbage collection.
#' \tabular{rl}{
#'     `purge_pkg_prefix`   \tab Strips package prefixes
#'   \cr `purge_pkg_pref`   \tab   (e.g., `'pkg::'`) from a
#'   \cr     `xpkgprefix`   \tab   vector of package names.
#'   \cr       `xpkgpref`   \tab
#'   \cr           `xpkg`   \tab
#'   \cr                    \tab  
#'   \cr `purge_warnings`   \tab Purges all cached warnings.
#'   \cr     `purge_warn`   \tab  
#'   \cr      `xwarnings`   \tab  
#'   \cr          `xwarn`   \tab  
#'   \cr                    \tab  
#'   \cr   `purge_global`   \tab **WARNING: Use with care!**
#'   \cr      `purge_all`   \tab   *These functions purge the*
#'   \cr        `xglobal`   \tab   *global environment.*
#'   \cr           `xall`   \tab  
#'   \cr                    \tab  
#'   \cr    `purge_plots`   \tab Purges plots and, if `x`
#'   \cr     `purge_plot`   \tab   is not `NULL`, prints `x`.
#'   \cr         `xplots`   \tab   (assumes that if x is not
#'   \cr          `xplot`   \tab   `NULL` it is a plot object.
#'   \cr                    \tab  
#'   \cr  `purge_console`   \tab Purges the console.
#'   \cr      `purge_con`   \tab  
#'   \cr       `xconsole`   \tab  
#'   \cr           `xcon`   \tab  
#'   \cr                    \tab  
#'   \cr          `purge`   \tab Purges specific objects.
#'   \cr              `x`   \tab  
#' }
#' @param funs \link[=cmp_chr_vec]{A complete character vec} of function names, possibly with package prefixes.
#' @param ... One or more \link[=cmp_chr_vec]{complete character vecs} containing names of objects to purge from the global environment.
#' @return *A character vector*
#'  \cr   `purge_pkg_prefix`
#'  \cr   `purge_pkg_pref`
#'  \cr   `xpkgprefix`
#'  \cr   `xpkgpref`
#'  \cr   `xpkg`
#'  \cr\cr `NULL`
#'  \cr   all others
#' @examples
#' fun.names <- c("base::c", "uj::cmp_chr_scl", "run")
#' xpkg(fun.names)
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
purge <- function(...) {
  x <- base::as.character(base::match.call())
  x <- x[2:base::length(x)]
  code <- base::paste0("rm(", base::paste0(x, collapse = ", "), ")")
  base::eval.parent(base::parse(text = code), n = 1)
  base::gc(verbose = FALSE)
}

#' @rdname purge
#' @export
x <- purge

#' @rdname purge
#' @export
purge_plots <- function(x = NULL) {
  grDevices::graphics.off()
  if (uj::ipop(x)) {base::print(x)}
  base::gc(verbose = FALSE)
}

#' @rdname purge
#' @export
purge_plot <- purge_plots

#' @rdname purge
#' @export
xplots <- purge_plots

#' @rdname purge
#' @export
xplot <- purge_plots

#' @rdname purge
#' @export
purge_global <- function() {
  base::rm(list = base::ls(envir = base::globalenv()), envir = base::globalenv())
  uj::xplot()
  uj::xconsole()
  base::gc(verbose = FALSE)
}

#' @rdname purge
#' @export
purge_all <- purge_global

#' @rdname purge
#' @export
xglobal <- purge_global

#' @rdname purge
#' @export
xall <- purge_global

#' @rdname purge
#' @export
purge_warnings <- function() {
  base::gc(verbose = FALSE)
  x <- base::baseenv()$last.warning
  if (!base::is.null(x)) {
    locked <- base::bindingIsLocked("last.warning", base::baseenv())
    if (locked) {unlockBinding("last.warning", base::baseenv())}
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
purge_pkg_prefix <- function(funs) {
  base::gc(verbose = FALSE)
  if (uj::cmp_chr_vec(funs)) {
    where <- base::regexpr(":::", funs, fixed = T)
    where[where == -1] <- -2
    funs <- base::substr(funs, where + 3, base::nchar(funs))
    base::substr(funs, base::regexpr("::", funs, fixed = T) + 2, base::nchar(funs))
  } else {stop(uj::format_errs(pkg = "uj", "[funs] must be a complete character vec (?cmp_chr_vec)."))}
}

#' @rdname purge
#' @export
purge_pkg_pref <- purge_pkg_prefix

#' @rdname purge
#' @export
xpkgprefix <- purge_pkg_prefix

#' @rdname purge
#' @export
xpkgpref <- purge_pkg_prefix

#' @rdname purge
#' @export
xpkg <- purge_pkg_prefix
