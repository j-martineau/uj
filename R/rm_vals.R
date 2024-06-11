#' @encoding UTF-8
#' @family purge
#' @title Manipulate positions, rows, and columns
#' @details
#' \tabular{ll}{  `purge_vals, rm_vals, xvals`   \tab  \cr   \tab   \cr
#'                `purge_elts, rm_elts, xelts`   \tab                       \cr   \tab   \cr
#'                `purge_rows, rm_rows, xrows`   \tab                          \cr   \tab   \cr
#'                `purge_cols, rm_cols, xcols`   \tab                                       }
#' @param x For `*vals` and `*elts` an atomic vector. For `*cols` and `*rows`, a atomic data.frame (?atm_dtf) or an atomic matrix.
#' @param vals A unique-valued atomic vector (?uj::unq_atm_vec) mode-compatible with `x`.
#' @param elts,rows,cols Atomic vector indexing elements, rows, or columns of `x` to be deleted, respectively. These are either
#'   \itemize{\item A uniquely-valued positive whole-number vector.
#'            \item A complete logical vector (?uj::cmp_lgl_vec) of length `length(x)`.
#'            \item A uniquely-valued character vector of element/column/row names.   }
#' @return An object of the same class as `x`.
#' @export
rm_vals_help <- function() {utils::help("rm_vals_help", package = "uj")}

#' @describeIn rm_vals_help Remove values in atomic vector `vals` from atomic vector `x`.
#' @export
rm_vals <- function(x, vals) {
  errs <- NULL
  if (!uj::.atm_vec(x)) {errs <- base::c(errs, "[x] must be an atomic vec (?atm_vec).")}
  if (!uj::.unq_vec(vals)) {errs <- base::c(errs, "[vals] must be a unique-valued atomic vec (?uj::unq_vec).")}
  if (uj::DEF(errs)) {uj::stopperr(errs)}
  if (!uj:::.compat(x, vals)) {uj::stopper("[x] and [vals] must be of compatible modes.")}
  x <- x[!(x %in% vals)]
}

#' @describeIn rm_vals_help Delete elements of `x` indexed in `elts`.
#' @export
rm_elts <- function(x, elts) {
  if (!uj::.atm_vec(x)) {uj::stopper("[x] must be an atomic vec (?atm_vec).")}
  n <- base::length(x)
  if (uj::unq_whl_vec(elts)) {
    if (base::all(elts != 0)) {
      elts[elts < 0] <- n + elts[elts < 0] - 1
      if (base::all(elts <= n)) {x[1:n[!(1:n %in% elts)], ]}
      else {uj::stopper("[elts] may not contain values larger than length(x).")}
    } else {uj::stopperr("[elts] may not contain 0.")}
  } else if (uj::cmp_lgl_vec(elts)) {
    if (uj::N(elts) == n) {x[!elts]}
    else {uj::stopper("length(elts) must equal length(x) when [elts] is of mode logical.")}
  } else if (uj::unq_chr_scl(elts)) {
    if (base::all(elts %in% base::names(x))) {x[elts]}
    else {uj::stopperr("[elts] contains a name not in rownames(x).")}
  } else {uj::stopperr("[elts] must contain only unique positive integers ≤ length(x), a complete logical vector of length(x), or a unique character vector containing only names of elements of [x].")}
}

#' @describeIn rm_vals_help Delete rows of `x` indexed in `rows`.
#' @export
rm_rows <- function(x, rows) {
  if (!(uj::.atm_mat(x) | uj::.atm_dtf(x)) | !uj::.POP(x)) {uj::stopperr("[x] must be an atomic matrix or an atomic data frame (?atm_dtf).")}
  n <- base::nrow(x)
  if (uj::unq_whl_vec(rows)) {
    if (base::all(rows != 0)) {
      if (base::all(rows <= n)) {x[1:n[!(1:n %in% rows)], ]}
      else {uj::stopperr("[rows] may not contain values larger than nrow(x).")}
    } else {uj::stopperr()}
  } else if (uj::cmp_lgl_vec(rows)) {
    if (base::length(rows) == n) {x[!rows, ]}
    else {uj::stopper("length(rows) must equal nrow(x) when [rows] is of mode logical.")}
  } else if (uj::unq_chr_scl(rows)) {
    if (base::all(rows %in% base::rownames(x))) {x[rows, ]}
    else {uj::stopperr("[rows] contains a name not in rownames(x).")}
  } else {uj::stopperr("[rows] must contain only unique positive integers ≤ nrow(x), a complete logical vector of length nrow(x), or a unique character vector containing only rownames of [x].")}
}

#' @describeIn rm_vals_help Delete columns of `x` indexed in `cols`.
#' @export
rm_cols <- function(x, cols) {
  if (!(uj::.atm_mat(x) | uj::.atm_dtf(x)) | !uj::.POP(x)) {uj::stopperr("[x] must be an atomic matrix or an atomic data frame (?atm_dtf).")}
  n <- base::ncol(x)
  if (uj::unq_psw_vec(x)) {
    if (base::all(cols <= n)) {x[1:n[!(1:n %in% cols)], ]}
    else {uj::stopper("[cols] may not contain values larger than ncol(x).")}
  } else if (uj::cmp_lgl_vec(cols)) {
    if (base::length(cols) == n) {x[ , !cols]}
    else {uj::stopper("length(cols) must equal ncol(x) when [cols] is of mode logical.")}
  } else if (uj::unq_chr_scl(cols)) {
    if (base::all(C %in% base::colnames(x))) {x[ , C]}
    else {uj::stopper("[cols] contains a name not in colnames(x).")}
  } else {uj::stopper("[cols] must contain only unique positive integers ≤ ncol(x), a complete logical vector of length ncol(x), or a unique character vector containing only colnames of [x].")}
}

#' @describeIn rm_vals_help An alias for `rm_vals`.
#' @export
xvals <- rm_vals

#' @describeIn rm_vals_help An alias for `rm_elts`.
#' @export
xelts <- rm_elts

#' @describeIn rm_vals_help An alias for `rm_rows`.
#' @export
xrows <- rm_rows

#' @describeIn rm_vals_help An alias for `rm_cols`.
#' @export
xcols <- rm_cols
