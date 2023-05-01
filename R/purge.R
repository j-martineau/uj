#' @encoding UTF-8
#' @name purge
#' @family extensions
#' @family meta
#' @family purge
#' @title Purge objects and parts of objects
#' @details All purge (`X`) functions include garbage collection.
#' \tabular{ll}{  `purge_pref, rm__pref, xpref`   \tab Purge package prefixes\eqn{^{(1)}}    \cr
#'                `purge_plot, rm_plot, xplot`    \tab Purge plots\eqn{^{(2)}}               \cr
#'                `purge_warn, rm_warn, xwarn`    \tab Purge warnings                        \cr
#'                `purge_con, rm_con, xcon`       \tab Purge console                         \cr
#'                `purge, X`                      \tab Purge specific objects                  }
#'  \tabular{l}{  \eqn{^{(1)}} Converts `'base::paste'` to `'paste'`, for example.           \cr
#'                \eqn{^{(2)}} Followed by printing the object `X` if not `NULL`.            \cr   }
#' \tabular{ll}{  `purge_global, purge_all`      \tab **WARNING: Use with care!**            \cr
#'                `rm_global, rm_all`            \tab    *These functions purge *            \cr
#'                `xglobal, xall`                \tab    *the global environment*              }
#' @param X Anticipated to be a plot object, but any object can be handled.
#' @param Funs \link[=cmp_chr_vec]{A complete character vec} of function names, possibly with package prefixes.
#' @param ... One or more \link[=cmp_chr_vec]{complete character vecs} containing names of objects to purge from the global environment.
#' @return **A character vector**    \cr\cr `purge_pref, rm_pref, xpref`
#' \cr\cr  **The** `NULL` **object** \cr\cr all others
#' @examples
#' egFunNames <- c("base::c", "uj::cmp_chr_scl", "run")
#' xpref(egFunNames)
#' \dontrun{
#'   ## build a ggplot scatterplot object (y.)
#'   library(ggplot2)
#'   egObj <- data.frame(X = 0:100, y = sqrt(0:100)
#'   egGgp <- ggplot(egObj, aes(X, y)) + geom_point()
#'
#'   ## print to console, wait for user, purge console
#'   say("\n A message")
#'   continue()
#'   xcon()
#'
#'   ## display a scatterplot, wait for user
#'   plot(egObj$X, egObj$y)
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
  X <- base::as.character(base::match.call())
  X <- X[2:base::length(X)]
  Code <- base::paste0("rm(", uj::g(", ", X), ")")
  base::eval.parent(base::parse(text = Code), n = 1)
  base::gc(verbose = FALSE)
}

#' @rdname purge
#' @export
X <- purge

#' @rdname purge
#' @export
purge_plot <- function(X = NULL) {
  grDevices::graphics.off()
  if (!base::is.null(X)) {base::print(X)}
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
  X <- base::baseenv()$last.warning
  if (!base::is.null(X)) {
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
purge_pref <- function(Funs) {
  base::gc(verbose = FALSE)
  if (!uj:::.cmp_chr_vec(Funs)) {uj::stopperr("[Funs] must be a complete character vec (?cmp_chr_vec).", PKG = "uj")}
  Where <- base::regexpr(":::", Funs, fixed = T)
  Where[Where == -1] <- -2
  Funs <- base::substr(Funs, Where + 3, base::nchar(Funs))
  base::substr(Funs, base::regexpr("::", Funs, fixed = T) + 2, base::nchar(Funs))
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
#' \tabular{ll}{  `purge_vals, rm_vals, xvals`   \tab Remove values in atomic vector `Vals` from atomic vector `X`. \cr   \tab   \cr
#'                `purge_elts, rm_elts, xelts`   \tab Delete elements of `X` indexed in `Elts`                      \cr   \tab   \cr
#'                `purge_rows, rm_rows, xrows`   \tab Delete rows of `X` indexed in `Rows`.                         \cr   \tab   \cr
#'                `purge_cols, rm_cols, xcols`   \tab Delete columns of `X` indexed in `Cols`                                      }
#' @param X For `*vals` and `*elts` an atomic vector. For `*cols` and `*rows`, a atomic data.frame (?uj::atm_dtf) or an atomic matrix.
#' @param Vals A unique-valued atomic vector (?uj::unq_atm_vec) mode-compatible with `X`.
#' @param Elts,Rows,Cols Atomic vector indexing elements, rows, or columns of `X` to be deleted, respectively. These are either
#'   \itemize{\item A uniquely-valued positive whole-number vector.
#'            \item A complete logical vector (?uj::cmp_lgl_vec) of length `length(X)`.
#'            \item A uniquely-valued character vector of element/column/row names.   }
#' @return An object of the same class as `X`.
#' @export
rm_vals <- function(X, Vals) {
  Errs <- NULL
  if (!uj::.atm_vec(X)) {Errs <- base::c(Errs, "[X] must be an atomic vec (?uj::atm_vec).")}
  if (!uj::.unq_vec(Vals)) {Errs <- base::c(Errs, "[Vals] must be a unique-valued atomic vec (?uj::unq_vec).")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (!uj:::.compat(X, Vals)) {uj::stopper("[X] and [Vals] must be of compatible modes.", PKG = "uj")}
  X <- X[!(X %in% Vals)]
}

#' @rdname rm_vals
#' @export
rm_elts <- function(X, Elts) {
  if (!uj::.atm_vec(X)) {uj::stopper("[X] must be an atomic vec (?uj::atm_vec).", PKG = "uj")}
  N <- base::length(X)
  if (uj::unq_whl_vec(Elts)) {
    if (base::all(Elts != 0)) {
      Elts[Elts < 0] <- N + Elts[Elts < 0] - 1
      if (base::all(Elts <= N)) {X[1:N[!(1:N %in% Elts)], ]}
      else {uj::stopper("[Elts] may not contain values larger than length(X).")}
    } else {uj::stopperr("[Elts] may not contain 0.", PKG = "uj")}
  } else if (uj::cmp_lgl_vec(Elts)) {
    if (uj::N(Elts) == N) {X[!Elts]}
    else {uj::stopper("length(Elts) must equal length(X) when [Elts] is of mode logical.")}
  } else if (uj::unq_chr_scl(Elts)) {
    if (base::all(Elts %in% base::names(X))) {X[Elts]}
    else {uj::stopperr("[Elts] contains a name not in rownames(X).")}
  } else {uj::stopperr("[Elts] must contain only unique positive integers ≤ length(X), a complete logical vector of length(X), or a unique character vector containing only names of elements of [X].")}
}

#' @rdname rm_vals
#' @export
rm_rows <- function(X, Rows) {
  if (!(uj:::.atm_mat(X) | uj:::.atm_dtf(X)) | !uj:::.POP(X)) {uj::stopperr("[X] must be an atomic matrix or an atomic data frame (uj::?atm_dtf).", PKG = "uj")}
  N <- base::nrow(X)
  if (uj::unq_whl_vec(Rows)) {
    if (base::all(Rows != 0)) {
      if (base::all(Rows <= N)) {X[1:N[!(1:N %in% Rows)], ]}
      else {uj::stopperr("[Rows] may not contain values larger than nrow(X).")}
    } else {uj::stopperr()}
  } else if (uj::cmp_lgl_vec(Rows)) {
    if (base::length(Rows) == N) {X[!Rows, ]}
    else {uj::stopper("length(Rows) must equal nrow(X) when [Rows] is of mode logical.")}
  } else if (uj::unq_chr_scl(Rows)) {
    if (base::all(Rows %in% base::rownames(X))) {X[Rows, ]}
    else {uj::stopperr("[Rows] contains a name not in rownames(X).")}
  } else {uj::stopperr("[Rows] must contain only unique positive integers ≤ nrow(X), a complete logical vector of length nrow(X), or a unique character vector containing only rownames of [X].")}
}

#' @rdname rm_vals
#' @export
rm_cols <- function(X, Cols) {
  if (!(uj:::.atm_mat(X) | uj:::.atm_dtf(X)) | !uj:::.POP(X)) {uj::stopperr("[X] must be an atomic matrix or an atomic data frame (uj::?atm_dtf).", PKG = "uj")}
  N <- base::ncol(X)
  if (uj::unq_psw_vec(X)) {
    if (base::all(Cols <= N)) {X[1:N[!(1:N %in% Cols)], ]}
    else {uj::stopper("[Cols] may not contain values larger than ncol(X).")}
  } else if (uj::cmp_lgl_vec(Cols)) {
    if (base::length(Cols) == N) {X[ , !Cols]}
    else {uj::stopper("length(Cols) must equal ncol(X) when [Cols] is of mode logical.")}
  } else if (uj::unq_chr_scl(Cols)) {
    if (base::all(C %in% base::colnames(X))) {X[ , C]}
    else {uj::stopper("[Cols] contains a name not in colnames(X).")}
  } else {uj::stopper("[Cols] must contain only unique positive integers ≤ ncol(X), a complete logical vector of length ncol(X), or a unique character vector containing only colnames of [X].")}
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
