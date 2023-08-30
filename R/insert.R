#' @encoding UTF-8
#' @family function_form
#' @title Manipulate positions, rows, and columns
#' @details
#' \tabular{ll}{  `insert_elts_before`   \tab Insert `Elts` into `x` before the `elt`-the positions of `x`. \cr   \tab   \cr
#'                `insert_cols_before`   \tab Insert `Cols` into `x` before the `col`-th column of `x`.     \cr   \tab   \cr
#'                `insert_rows_before`   \tab Insert `Rows` into `x` before the `row`-th row of `x`.        \cr   \tab   \cr
#'                `insert_elts_after`    \tab Insert `Elts` into `x` after the `elt`-the position of `x`.   \cr   \tab   \cr
#'                `insert_cols_after`    \tab Insert `Cols` into `x` after the `col`-th column of `x`.      \cr   \tab   \cr
#'                `insert_rows_after`    \tab Insert `Rows` into `x` after the `row`-th row of `x`.                        }
#' `elt < 0`, `col < 0`, and `row < 0` index from the last position, row, or column rather than the first.
#' @param x For `insert_elt_before` and `insert_elt_after` a non-empty atomic vector. A non-empty atomic matrix or atomic data frame (?uj::atm_dtf) for all others.
#' @param new.elts An atomic vector compatible with `x` (?uj::compatible).
#' @param new.cols A matrix or data.frame compatible witht `Y` for col binding.
#' @param new.rows A matrix or data.frame compatible with `x` for row binding.
#' @param elt A whole number scalar indexing a position of `x`. Negative values index from the last position.
#' @param col A whole number scalar indexing a column of `x`. Negative values index from the last column.
#' @param row A whole number scalar indexing a row of `x`. Negative values index from the last row.
#' @return An object of the same class as `x` with increased dimension.
#' @export
insert_elts_before <- function(x, new.elts, elt) {
  Errs <- NULL
  if (!uj::.atm_vec(x)) {Errs <- base::c(Errs, "[x] must be an atomic vector.")}
  if (!uj::.atm_vec(new.elts)) {Errs <- base::c(Errs, "[new.elts] must be an atomic vector.")}
  if (!uj::.cmp_whl_scl(elt)) {Errs <- base::c(Errs, "[elt] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  N <- uj::N(x)
  if (!uj:::.compat(x, new.elts)) {Errs <- base::c(Errs, "[new.elts] must be mode-compatible with [x] (?uj::compatible).")}
  if (base::abs(elt) > N) {Errs <- base::c(Errs, "abs(elt) > length(x).")}
  if (elt == 0) {Errs <- base::c(Errs, "[elt] may not be 0.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  if (elt < 0) {elt <- N + elt + 1}
  Before <- uj::f0(elt == 1, NULL, x[1:(elt - 1)])
  After <- x[elt:N]
  base::c(Before, new.elts, After)
}

#' @rdname insert_elts_before
#' @export
insert_cols_before <- function(x, new.cols, col) {
  Errs <- NULL
  if (!uj::.D2D(x) | uj::nv0(x)) {Errs <- base::c(Errs, "[x] must be a populated matrix or data.frame.")}
  if (!uj::.D2D(new.cols) | uj::nv0(new.cols)) {Errs <- base::c(Errs, "[new.cols] must be a populated matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(col)) {Errs <- base::c(Errs, "[col] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  if      (uj::.atm_mat(x) & uj::.atm_mat(new.cols)) {if (!uj::compatible_mats(2, x, new.cols)) {uj::stopperr("[x] and [new.cols] are not compatible matrices for column binding (?uj::compatible_mats).", .PKG = "uj")}}
  else if (uj::.atm_dtf(x) & uj::.atm_dtf(new.cols)) {if (!uj::compatible_dtfs(2, x, new.cols)) {uj::stopperr("[x] and [new.cols] are not compatible data.frames for column binding (?uj::compatible_dtfs).", .PKG = "uj")}}
  else {uj::stopperr("[x] and [Y] must both be atomic matrices or atomic data.frames.", .PKG = "uj")}
  N <- base::ncol(x)
  if (base::abs(col) > N) {Errs <- base::c(Errs, "abs(col) > ncol(x).")}
  if (col == 0) {Errs <- base::c(Errs, "[col] may not be 0.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  if (col < 0) {col <- N + col + 1}
  Before <- uj::f0(col == 1, NULL, 1:(col - 1))
  After <- col:N
  uj::f0(uj::NLL(Before), base::cbind(new.cols, x[ , After]), base::cbind(x[ , Before], new.cols, x[ , After]))
}

#' @rdname insert_elts_before
#' @export
insert_rows_before <- function(x, new.rows, row) {
  Errs <- NULL
  if (!uj::.D2D(x) | uj::nv0(x)) {Errs <- base::c(Errs, "[x] must be a populated matrix or data.frame.")}
  if (!uj::.D2D(new.rows) | uj::nv0(new.rows)) {Errs <- base::c(Errs, "[new.rows] must be a populated matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(row)) {Errs <- base::c(Errs, "[row] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  if      (uj::.atm_mat(x) & uj::.atm_mat(new.rows)) {if (!uj::compatible_mats(1, x, new.rows)) {uj::stopperr("[x] and [new.rows] are not compatible matrices for row binding (?uj::compatible_mats).", .PKG = "uj")}}
  else if (uj::.atm_dtf(x) & uj::.atm_dtf(new.rows)) {if (!uj::compatible_dtfs(1, x, new.rows)) {uj::stopperr("[x] and [new.rows] are not compatible data.frames for row binding (?uj::compatible_dtfs).", .PKG = "uj")}}
  else {uj::stopperr("[x] and [new.rows] must both be atomic matrices or atomic data.frames (?uj::atm_dtf).", .PKG = "uj")}
  N <- base::nrow(x)
  if (base::abs(row) > N) {Errs <- base::c(Errs, "abs(row) > nrow(x).")}
  if (row == 0) {Errs <- base::c(Errs, "[row] may not be 0.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  if (row < 0) {row <- N + row + 1}
  Before <- uj::f0(row == 1, NULL, 1:(row - 1))
  After <- row:N
  uj::f0(uj::NLL(Before), base::rbind(new.rows, x[After, ]), base::rbind(x[Before, ], new.rows, x[After, ]))
}

#' @rdname insert_elts_before
#' @export
insert_elt_after <- function(x, new.elts, elt) {
  Errs <- NULL
  if (!uj::.atm_vec(x)) {Errs <- base::c(Errs, "[x] must be an atomic vector.")}
  if (!uj::.atm_vec(new.elts)) {Errs <- base::c(Errs, "[new.elts] must be an atomic vector.")}
  if (!uj::.cmp_whl_scl(N)) {Errs <- base::c(Errs, "[N] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  N <- base::length(x)
  if (!uj:::.compat(x, new.elts)) {Errs <- base::c(Errs, "[new.elts] must be mode-compatible with [x] (?uj::compatible).")}
  if (base::abs(elt) > N) {Errs <- base::c(Errs, "abs(elt) > length(x).")}
  if (elt == 0) {Errs <- base::c(Errs, "[N] may not be 0.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  if (elt < 0) {elt <- N + elt + 1}
  Before <- x[1:elt]
  After <- uj::f0(elt == N, NULL, x[(elt + 1):N])
  base::c(Before, new.elts, After)
}

#' @rdname insert_elts_before
#' @export
insert_col_after <- function(x, new.cols, col) {
  Errs <- NULL
  if (!uj::.D2D(x) | uj::nv0(x)) {Errs <- base::c(Errs, "[x] must be a populated matrix or data.frame.")}
  if (!uj::.D2D(new.cols) | uj::nv0(new.cols)) {Errs <- base::c(Errs, "[new.cols] must be a populated matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(new.cols)) {Errs <- base::c(Errs, "[col] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  if (uj::.atm_mat(x) & uj::.atm_mat(new.cols)) {if (!uj::compatible_mats(2, x, new.cols)) {uj::stopperr("[x] and [new.cols] are not compatible matrices for column binding (?compatible_mats).", .PKG = "uj")}}
  else if (uj::.atm_dtf(x) & uj::.atm_dtf(new.cols)) {if (!uj::compatible_dtfs(2, x, new.cols)) {uj::stopperr("[x] and [new.cols] are not compatible data.frames for column binding (?compatible_dtfs).", .PKG = "uj")}}
  else {uj::stopperr("[x] and [new.cols] must both be atomic matrices or atomic data.frames.", .PKG = "uj")}
  N <- base::ncol(x)
  if (base::abs(col) > N) {Errs <- base::c(Errs, "abs(col) > ncol(x).")}
  if (col == 0) {Errs <- base::c(Errs, "[col] may not be 0.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  if (col < 0) {col <- N + col + 1}
  Before <- 1:col
  After <- uj::f0(col == N, NULL, (col + 1):N)
  uj::f0(uj::NLL(After), base::cbind(x[ , Before], new.cols), base::cbind(x[ , Before], new.cols, x[ , After]))
}

#' @rdname insert_elts_before
#' @export
insert_after_row <- function(x, new.rows, row) {
  Errs <- NULL
  if (!uj::.D2D(x) | uj::nv0(x)) {Errs <- base::c(Errs, "[x] must be a populated matrix or data.frame.")}
  if (!uj::.D2D(new.rows) | uj::nv0(new.rows)) {Errs <- base::c(Errs, "[new.rows] must be a populated matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(row)) {Errs <- base::c(Errs, "[row] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  if (uj::.atm_mat(x) & uj::.atm_mat(new.rows)) {if (!uj::compatible_mats(1, x, new.rows)) {uj::stopperr("[x] and [new.rows] are not compatible matrices (?compatible_mats) for row binding.", .PKG = "uj")}}
  else if (uj::.atm_dtf(x) & uj::.atm_dtf(new.rows)) {if (!uj::compatible_dtfs(1, x, new.rows)) {uj::stopperr("[x] and [new.rows] are not compatible data frames (?compatible_dtfs) for row binding.", .PKG = "uj")}}
  else {uj::stopperr("[x] and [new.rows] must be compatible atomic matrices or compatible atomic data.frames (?uj::compatible, ?uj::atm_dtf).", .PKG = "uj")}
  N <- base::ncol(x)
  if (base::abs(row) > N) {Errs <- base::c(Errs, "abs(row) > nrow(x).")}
  if (row == 0) {Errs <- base::c(Errs, "[row] may not be 0.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  if (row < 0) {row <- N + row + 1}
  Before <- 1:row
  After <- uj::f0(row == N, NULL, (row + 1):N)
  uj::f0(uj::NLL(After), base::cbind(x[Before, ], new.rows), base::cbind(x[Before, ], new.rows, x[After, ]))
}
