#' @encoding UTF-8
#' @title Manipulate positions, rows, and columns
#' @details `elt < 0`, `col < 0`, and `row < 0` index from the last position, row, or column rather than the first.
#' @param x For `insert_elt_before` and `insert_elt_after` a non-empty atomic vector. A non-empty atomic matrix or atomic data frame (?atm_dtf) for all others.
#' @param new.elts An atomic vector compatible with `x` (?uj::compatible).
#' @param new.cols A matrix or data.frame compatible witht `Y` for col binding.
#' @param new.rows A matrix or data.frame compatible with `x` for row binding.
#' @param elt A whole number scalar indexing a position of `x`. Negative values index from the last position.
#' @param col A whole number scalar indexing a column of `x`. Negative values index from the last column.
#' @param row A whole number scalar indexing a row of `x`. Negative values index from the last row.
#' @return An object of the same class as `x` with increased dimension.
#' @export
insert_help <- function() {utils::help("insert_help", package = "uj")}

#' @describeIn insert_help Insert `new.elts` into `x` before the `elt`-th positions of `x`.
#' @export
insert_elts_before <- function(x, new.elts, elt) {
  errs <- NULL
  if (!uj::.atm_vec(x)) {errs <- base::c(errs, "[x] must be an atomic vector.")}
  if (!uj::.atm_vec(new.elts)) {errs <- base::c(errs, "[new.elts] must be an atomic vector.")}
  if (!uj::.cmp_whl_scl(elt)) {errs <- base::c(errs, "[elt] must be a whole number scalar.")}
  if (uj::.DEF(errs)) {uj::stopperr(errs)}
  n <- uj::N(x)
  if (!uj:::.compat(x, new.elts)) {errs <- base::c(errs, "[new.elts] must be mode-compatible with [x] (?uj::compatible).")}
  if (base::abs(elt) > n) {errs <- base::c(errs, "abs(elt) > length(x).")}
  if (elt == 0) {errs <- base::c(errs, "[elt] may not be 0.")}
  if (uj::.DEF(errs)) {uj::stopperr(errs)}
  if (elt < 0) {elt <- n + elt + 1}
  before <- uj::f0(elt == 1, NULL, x[1:(elt - 1)])
  after <- x[elt:n]
  base::c(before, new.elts, after)
}

#' @describeIn insert_help Insert `new.cols` into `x` before the `col`-th column of `x`.
#' @export
insert_cols_before <- function(x, new.cols, col) {
  errs <- NULL
  if (!uj::.D2D(x) | uj::nv0(x)) {errs <- base::c(errs, "[x] must be a populated matrix or data.frame.")}
  if (!uj::.D2D(new.cols) | uj::nv0(new.cols)) {errs <- base::c(errs, "[new.cols] must be a populated matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(col)) {errs <- base::c(errs, "[col] must be a whole number scalar.")}
  if (uj::.DEF(errs)) {uj::stopperr(errs)}
  if      (uj::.atm_mat(x) & uj::.atm_mat(new.cols)) {if (!uj::compatible_mats(2, x, new.cols)) {uj::stopperr("[x] and [new.cols] are not compatible matrices for column binding (?uj::compatible_mats).")}}
  else if (uj::.atm_dtf(x) & uj::.atm_dtf(new.cols)) {if (!uj::compatible_dtfs(2, x, new.cols)) {uj::stopperr("[x] and [new.cols] are not compatible data.frames for column binding (?uj::compatible_dtfs).")}}
  else {uj::stopperr("[x] and [Y] must both be atomic matrices or atomic data.frames.")}
  n <- base::ncol(x)
  if (base::abs(col) > n) {errs <- base::c(errs, "abs(col) > ncol(x).")}
  if (col == 0) {errs <- base::c(errs, "[col] may not be 0.")}
  if (uj::.DEF(errs)) {uj::stopperr(errs)}
  if (col < 0) {col <- n + col + 1}
  before <- uj::f0(col == 1, NULL, 1:(col - 1))
  after <- col:n
  uj::f0(uj::.NLL(before), base::cbind(new.cols, x[ , after]), base::cbind(x[ , before], new.cols, x[ , after]))
}

#' @describeIn insert_help Insert `new.rows` into `x` before the `row`-th row of `x`.
#' @export
insert_rows_before <- function(x, new.rows, row) {
  errs <- NULL
  if (!uj::.D2D(x) | uj::nv0(x)) {errs <- base::c(errs, "[x] must be a populated matrix or data.frame.")}
  if (!uj::.D2D(new.rows) | uj::nv0(new.rows)) {errs <- base::c(errs, "[new.rows] must be a populated matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(row)) {errs <- base::c(errs, "[row] must be a whole number scalar.")}
  if (uj::.DEF(errs)) {uj::stopperr(errs)}
  if      (uj::.atm_mat(x) & uj::.atm_mat(new.rows)) {if (!uj::compatible_mats(1, x, new.rows)) {uj::stopperr("[x] and [new.rows] are not compatible matrices for row binding (?uj::compatible_mats).")}}
  else if (uj::.atm_dtf(x) & uj::.atm_dtf(new.rows)) {if (!uj::compatible_dtfs(1, x, new.rows)) {uj::stopperr("[x] and [new.rows] are not compatible data.frames for row binding (?uj::compatible_dtfs).")}}
  else {uj::stopperr("[x] and [new.rows] must both be atomic matrices or atomic data.frames (?atm_dtf).")}
  n <- base::nrow(x)
  if (base::abs(row) > n) {errs <- base::c(errs, "abs(row) > nrow(x).")}
  if (row == 0) {errs <- base::c(errs, "[row] may not be 0.")}
  if (uj::.DEF(errs)) {uj::stopperr(errs)}
  if (row < 0) {row <- n + row + 1}
  before <- uj::f0(row == 1, NULL, 1:(row - 1))
  after <- row:n
  uj::f0(uj::.NLL(before), base::rbind(new.rows, x[after, ]), base::rbind(x[before, ], new.rows, x[after, ]))
}

#' @describeIn insert_help Insert `new.elts` into `x` after the `elt`-the position of `x`.
#' @export
insert_elt_after <- function(x, new.elts, elt) {
  errs <- NULL
  if (!uj::.atm_vec(x)) {errs <- base::c(errs, "[x] must be an atomic vector.")}
  if (!uj::.atm_vec(new.elts)) {errs <- base::c(errs, "[new.elts] must be an atomic vector.")}
  if (!uj::.cmp_whl_scl(N)) {errs <- base::c(errs, "[N] must be a whole number scalar.")}
  if (uj::.DEF(errs)) {uj::stopperr(errs)}
  n <- base::length(x)
  if (!uj:::.compat(x, new.elts)) {errs <- base::c(errs, "[new.elts] must be mode-compatible with [x] (?uj::compatible).")}
  if (base::abs(elt) > n) {errs <- base::c(errs, "abs(elt) > length(x).")}
  if (elt == 0) {errs <- base::c(errs, "[N] may not be 0.")}
  if (uj::.DEF(errs)) {uj::stopperr(errs)}
  if (elt < 0) {elt <- n + elt + 1}
  before <- x[1:elt]
  after <- uj::f0(elt == n, NULL, x[(elt + 1):N])
  base::c(before, new.elts, after)
}

#' @describeIn insert_help Insert `new.cols` into `x` after the `col`-th column of `x`.
#' @export
insert_col_after <- function(x, new.cols, col) {
  errs <- NULL
  if (!uj::.D2D(x) | uj::nv0(x)) {errs <- base::c(errs, "[x] must be a populated matrix or data.frame.")}
  if (!uj::.D2D(new.cols) | uj::nv0(new.cols)) {errs <- base::c(errs, "[new.cols] must be a populated matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(new.cols)) {errs <- base::c(errs, "[col] must be a whole number scalar.")}
  if (uj::.DEF(errs)) {uj::stopperr(errs)}
  if (uj::.atm_mat(x) & uj::.atm_mat(new.cols)) {if (!uj::compatible_mats(2, x, new.cols)) {uj::stopperr("[x] and [new.cols] are not compatible matrices for column binding (?compatible_mats).")}}
  else if (uj::.atm_dtf(x) & uj::.atm_dtf(new.cols)) {if (!uj::compatible_dtfs(2, x, new.cols)) {uj::stopperr("[x] and [new.cols] are not compatible data.frames for column binding (?compatible_dtfs).")}}
  else {uj::stopperr("[x] and [new.cols] must both be atomic matrices or atomic data.frames.")}
  n <- base::ncol(x)
  if (base::abs(col) > n) {errs <- base::c(errs, "abs(col) > ncol(x).")}
  if (col == 0) {errs <- base::c(errs, "[col] may not be 0.")}
  if (uj::.DEF(errs)) {uj::stopperr(errs)}
  if (col < 0) {col <- n + col + 1}
  before <- 1:col
  after <- uj::f0(col == n, NULL, (col + 1):N)
  uj::f0(uj::.NLL(after), base::cbind(x[ , before], new.cols), base::cbind(x[ , before], new.cols, x[ , after]))
}

#' @describeIn insert_help Insert `new.rows` into `x` after the `row`-th row of `x`.
#' @export
insert_after_row <- function(x, new.rows, row) {
  errs <- NULL
  if (!uj::.D2D(x) | uj::nv0(x)) {errs <- base::c(errs, "[x] must be a populated matrix or data.frame.")}
  if (!uj::.D2D(new.rows) | uj::nv0(new.rows)) {errs <- base::c(errs, "[new.rows] must be a populated matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(row)) {errs <- base::c(errs, "[row] must be a whole number scalar.")}
  if (uj::.DEF(errs)) {uj::stopperr(errs)}
  if (uj::.atm_mat(x) & uj::.atm_mat(new.rows)) {if (!uj::compatible_mats(1, x, new.rows)) {uj::stopperr("[x] and [new.rows] are not compatible matrices (?compatible_mats) for row binding.")}}
  else if (uj::.atm_dtf(x) & uj::.atm_dtf(new.rows)) {if (!uj::compatible_dtfs(1, x, new.rows)) {uj::stopperr("[x] and [new.rows] are not compatible data frames (?compatible_dtfs) for row binding.")}}
  else {uj::stopperr("[x] and [new.rows] must be compatible atomic matrices or compatible atomic data.frames (?uj::compatible, ?atm_dtf).")}
  n <- base::ncol(x)
  if (base::abs(row) > n) {errs <- base::c(errs, "abs(row) > nrow(x).")}
  if (row == 0) {errs <- base::c(errs, "[row] may not be 0.")}
  if (uj::.DEF(errs)) {uj::stopperr(errs)}
  if (row < 0) {row <- n + row + 1}
  before <- 1:row
  after <- uj::f0(row == n, NULL, (row + 1):n)
  uj::f0(uj::.NLL(after), base::cbind(x[before, ], new.rows), base::cbind(x[before, ], new.rows, x[after, ]))
}
