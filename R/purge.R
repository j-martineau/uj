#' @encoding UTF-8
#' @family meta
#' @family purge
#' @title Purge objects and parts of objects
#' @details All purge (`x`) functions include garbage collection.
#' \tabular{ll}{  `purge_plot, rm_plot, xplot`   \tab                \cr
#'                `purge_warn, rm_warn, xwarn`   \tab Purge warnings                        \cr
#'                `purge_con, rm_con, xcon`      \tab Purge console                         \cr
#'                `purge, x`                     \tab                   }
#'  \tabular{l}{  \eqn{^{(1)}} Converts `'base::paste'` to `'paste'`, for example.           \cr
#'                \eqn{^{(2)}} Followed by printing the object `x` if not `NULL`.            \cr   }
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
purge_help <- function() {utils::help("purge_help", package = "uj")}

#' @describeIn purge_help Purge specific objects as named in `...` (unquoted object names).
#' @export
purge <- function(...) {
  x <- base::as.character(base::match.call())
  x <- x[2:base::length(x)]
  Code <- base::paste0("rm(", uj::g(", ", x), ")")
  base::eval.parent(base::parse(text = Code), n = 1)
  base::gc(verbose = FALSE)
}

#' @describeIn purge_help An aliase for `purge`.
#' @export
x <- purge

#' @describeIn purge_help Closes all open plots/graphic devices, followed by printing `x` if not `NULL`.
#' @export
purge_plots <- function(x = NULL) {
  grDevices::graphics.off()
  if (!base::is.null(x)) {base::print(x)}
  base::gc(verbose = FALSE)
}

#' @describeIn purge_help An alias for `purge_plot`.
#' @export
rm_plot <- purge_plots

#' @describeIn purge_help An alias for `purge_plot`.
#' @export
xplot <- purge_plots

#' @describeIn purge_help An alias for `purge_plot`.
#' @export
purge_plot <- purge_plots

#' @describeIn purge_help An alias for `purge_plot`.
#' @export
rm_plots <- purge_plots

#' @describeIn purge_help An alias for `purge_plot`.
#' @export
xplots <- purge_plots

#' @describeIn purge_help Purges warning messages.
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

#' @describeIn purge_help An alias for `purge_warnings`.
#' @export
purge_warn <- purge_warnings

#' @describeIn purge_help An alias for `purge_warnings`.
#' @export
xwarnings <- purge_warnings

#' @describeIn purge_help An alias for `purge_warnings`.
#' @export
xwarn <- purge_warnings

#' @describeIn purge_help Purges the contents of the console.
#' @export
purge_console <- function() {
  base::gc(verbose = FALSE)
  base::cat("\014")
}

#' @describeIn purge_help An alias for `purge_console`.
#' @export
purge_con <- purge_console

#' @describeIn purge_help An alias for `purge_console`.
#' @export
xconsole <- purge_console

#' @describeIn purge_help An alias for `purge_console`.
#' @export
xcon <- purge_console

#' @describeIn purge_help Purges the global environment.
#' @export
purge_global <- function() {
  base::rm(list = base::ls(envir = base::.GlobalEnv), envir = base::.GlobalEnv)
  uj::xplot()
  uj::xwarn()
  uj::xcon()
  base::gc(verbose = FALSE)
}

#' @describeIn purge_help An alias for `purge_global`.
#' @export
purge_all <- purge_global

#' @describeIn purge_help An alias for `purge_global`.
#' @export
rm_global <- purge_global

#' @describeIn purge_help An alias for `purge_global`.
#' @export
rm_all <- purge_global

#' @describeIn purge_help An alias for `purge_global`.
#' @export
xglobal <- purge_global

#' @describeIn purge_help An alias for `purge_global`.
#' @export
xall <- purge_global
