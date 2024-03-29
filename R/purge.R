#' @encoding UTF-8
#' @name purge
#' @family extensions
#' @family meta
#' @family purge
#' @title Purge objects and parts of objects
#' @details All purge (`x`) functions include garbage collection.
#' \tabular{ll}{  `purge_pref, rm_pref, xpref`   \tab Purge package prefixes\eqn{^{(1)}}    \cr
#'                `purge_plot, rm_plot, xplot`   \tab Purge plots\eqn{^{(2)}}               \cr
#'                `purge_warn, rm_warn, xwarn`   \tab Purge warnings                        \cr
#'                `purge_con, rm_con, xcon`      \tab Purge console                         \cr
#'                `purge, x`                     \tab Purge specific objects                  }
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
purge <- function(...) {
  x <- base::as.character(base::match.call())
  x <- x[2:base::length(x)]
  Code <- base::paste0("rm(", uj::g(", ", x), ")")
  base::eval.parent(base::parse(text = Code), n = 1)
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
purge_plots <- purge_plot

#' @rdname purge
#' @export
rm_plots <- purge_plot

#' @rdname purge
#' @export
xplots <- purge_plot

#' @rdname purge
#' @export
purge_warnings <- function() {
  base::gc(verbose = FALSE)
  x <- base::baseenv()$last.warning
  if (!base::is.null(x)) {
    Locked <- base::bindingIsLocked("last.warning", base::baseenv())
    if (Locked) {base::unlockBinding("last.warning", base::baseenv())}
    base::assign("last.warning", NULL, envir = base::baseenv())
    if (Locked) {base::lockBinding("last.warning", base::baseenv())}
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
  if (!uj:::.cmp_chr_vec(funs)) {uj::stopperr("[funs] must be a complete character vec (?cmp_chr_vec).", .PKG = "uj")}
  Where <- base::regexpr(":::", funs, fixed = T)
  Where[Where == -1] <- -2
  funs <- base::substr(funs, Where + 3, base::nchar(funs))
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

#' @encoding UTF-8
#' @family function_form
#' @family purge
#' @title Manipulate positions, rows, and columns
#' @details
#' \tabular{ll}{  `purge_vals, rm_vals, xvals`   \tab Remove values in atomic vector `vals` from atomic vector `x`. \cr   \tab   \cr
#'                `purge_elts, rm_elts, xelts`   \tab Delete elements of `x` indexed in `elts`                      \cr   \tab   \cr
#'                `purge_rows, rm_rows, xrows`   \tab Delete rows of `x` indexed in `rows`.                         \cr   \tab   \cr
#'                `purge_cols, rm_cols, xcols`   \tab Delete columns of `x` indexed in `cols`                                      }
#' @param x For `*vals` and `*elts` an atomic vector. For `*cols` and `*rows`, a atomic data.frame (?uj::atm_dtf) or an atomic matrix.
#' @param vals A unique-valued atomic vector (?uj::unq_atm_vec) mode-compatible with `x`.
#' @param elts,rows,cols Atomic vector indexing elements, rows, or columns of `x` to be deleted, respectively. These are either
#'   \itemize{\item A uniquely-valued positive whole-number vector.
#'            \item A complete logical vector (?uj::cmp_lgl_vec) of length `length(x)`.
#'            \item A uniquely-valued character vector of element/column/row names.   }
#' @return An object of the same class as `x`.
#' @export
rm_vals <- function(x, vals) {
  Errs <- NULL
  if (!uj::.atm_vec(x)) {Errs <- base::c(Errs, "[x] must be an atomic vec (?uj::atm_vec).")}
  if (!uj::.unq_vec(vals)) {Errs <- base::c(Errs, "[vals] must be a unique-valued atomic vec (?uj::unq_vec).")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  if (!uj:::.compat(x, vals)) {uj::stopper("[x] and [vals] must be of compatible modes.", .PKG = "uj")}
  x <- x[!(x %in% vals)]
}

#' @rdname rm_vals
#' @export
rm_elts <- function(x, elts) {
  if (!uj::.atm_vec(x)) {uj::stopper("[x] must be an atomic vec (?uj::atm_vec).", .PKG = "uj")}
  N <- base::length(x)
  if (uj::unq_whl_vec(elts)) {
    if (base::all(elts != 0)) {
      elts[elts < 0] <- N + elts[elts < 0] - 1
      if (base::all(elts <= N)) {x[1:N[!(1:N %in% elts)], ]}
      else {uj::stopper("[elts] may not contain values larger than length(x).")}
    } else {uj::stopperr("[elts] may not contain 0.", .PKG = "uj")}
  } else if (uj::cmp_lgl_vec(elts)) {
    if (uj::N(elts) == N) {x[!elts]}
    else {uj::stopper("length(elts) must equal length(x) when [elts] is of mode logical.")}
  } else if (uj::unq_chr_scl(elts)) {
    if (base::all(elts %in% base::names(x))) {x[elts]}
    else {uj::stopperr("[elts] contains a name not in rownames(x).")}
  } else {uj::stopperr("[elts] must contain only unique positive integers ≤ length(x), a complete logical vector of length(x), or a unique character vector containing only names of elements of [x].")}
}

#' @rdname rm_vals
#' @export
rm_rows <- function(x, rows) {
  if (!(uj:::.atm_mat(x) | uj:::.atm_dtf(x)) | !uj:::.POP(x)) {uj::stopperr("[x] must be an atomic matrix or an atomic data frame (uj::?atm_dtf).", .PKG = "uj")}
  N <- base::nrow(x)
  if (uj::unq_whl_vec(rows)) {
    if (base::all(rows != 0)) {
      if (base::all(rows <= N)) {x[1:N[!(1:N %in% rows)], ]}
      else {uj::stopperr("[rows] may not contain values larger than nrow(x).")}
    } else {uj::stopperr()}
  } else if (uj::cmp_lgl_vec(rows)) {
    if (base::length(rows) == N) {x[!rows, ]}
    else {uj::stopper("length(rows) must equal nrow(x) when [rows] is of mode logical.")}
  } else if (uj::unq_chr_scl(rows)) {
    if (base::all(rows %in% base::rownames(x))) {x[rows, ]}
    else {uj::stopperr("[rows] contains a name not in rownames(x).")}
  } else {uj::stopperr("[rows] must contain only unique positive integers ≤ nrow(x), a complete logical vector of length nrow(x), or a unique character vector containing only rownames of [x].")}
}

#' @rdname rm_vals
#' @export
rm_cols <- function(x, cols) {
  if (!(uj:::.atm_mat(x) | uj:::.atm_dtf(x)) | !uj:::.POP(x)) {uj::stopperr("[x] must be an atomic matrix or an atomic data frame (uj::?atm_dtf).", .PKG = "uj")}
  N <- base::ncol(x)
  if (uj::unq_psw_vec(x)) {
    if (base::all(cols <= N)) {x[1:N[!(1:N %in% cols)], ]}
    else {uj::stopper("[cols] may not contain values larger than ncol(x).")}
  } else if (uj::cmp_lgl_vec(cols)) {
    if (base::length(cols) == N) {x[ , !cols]}
    else {uj::stopper("length(cols) must equal ncol(x) when [cols] is of mode logical.")}
  } else if (uj::unq_chr_scl(cols)) {
    if (base::all(C %in% base::colnames(x))) {x[ , C]}
    else {uj::stopper("[cols] contains a name not in colnames(x).")}
  } else {uj::stopper("[cols] must contain only unique positive integers ≤ ncol(x), a complete logical vector of length ncol(x), or a unique character vector containing only colnames of [x].")}
}

#' @rdname rm_vals
#' @export
xvals <- rm_vals

#' @rdname rm_vals
#' @export
xelts <- rm_elts

#' @rdname rm_vals
#' @export
xrows <- rm_rows

#' @rdname rm_vals
#' @export
xcols <- rm_cols
