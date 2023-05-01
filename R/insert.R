#' @encoding UTF-8
#' @family function_form
#' @title Manipulate positions, rows, and columns
#' @details
#' \tabular{ll}{  `insert_elts_before`   \tab Insert `Elts` into `X` before the `Elt`-the positions of `X`. \cr   \tab   \cr
#'                `insert_cols_before`   \tab Insert `Cols` into `X` before the `Col`-th column of `X`.     \cr   \tab   \cr
#'                `insert_rows_before`   \tab Insert `Rows` into `X` before the `Row`-th row of `X`.        \cr   \tab   \cr
#'                `insert_elts_after`    \tab Insert `Elts` into `X` after the `Elt`-the position of `X`.   \cr   \tab   \cr
#'                `insert_cols_after`    \tab Insert `Cols` into `X` after the `Col`-th column of `X`.      \cr   \tab   \cr
#'                `insert_rows_after`    \tab Insert `Rows` into `X` after the `Row`-th row of `X`.                        }
#' `Elt < 0`, `Col < 0`, and `Row < 0` index from the last position, row, or column rather than the first.
#' @param X For `insert_elt_before` and `insert_elt_after` a non-empty atomic vector. A non-empty atomic matrix or atomic data frame (?uj::atm_dtf) for all others.
#' @param NewElts An atomic vector compatible with `X` (?uj::compatible).
#' @param NewCols A matrix or data.frame compatible witht `Y` for col binding.
#' @param NewRows A matrix or data.frame compatible with `X` for row binding.
#' @param Elt A whole number scalar indexing a position of `X`. Negative values index from the last position.
#' @param Col A whole number scalar indexing a column of `X`. Negative values index from the last column.
#' @param Row A whole number scalar indexing a row of `X`. Negative values index from the last row.
#' @return An object of the same class as `X` with increased dimension.
#' @export
insert_elts_before <- function(X, NewElts, Elt) {
  Errs <- NULL
  if (!uj::.atm_vec(X)) {Errs <- base::c(Errs, "[X] must be an atomic vector.")}
  if (!uj::.atm_vec(NewElts)) {Errs <- base::c(Errs, "[NewElts] must be an atomic vector.")}
  if (!uj::.cmp_whl_scl(Elt)) {Errs <- base::c(Errs, "[Elt] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  N <- uj::N(X)
  if (!uj:::.compat(X, NewElts)) {Errs <- base::c(Errs, "[NewElts] must be mode-compatible with [X] (?uj::compatible).")}
  if (base::abs(Elt) > N) {Errs <- base::c(Errs, "abs(Elt) > length(X).")}
  if (Elt == 0) {Errs <- base::c(Errs, "[Elt] may not be 0.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (Elt < 0) {Elt <- N + Elt + 1}
  Before <- uj::f0(Elt == 1, NULL, X[1:(Elt - 1)])
  After <- X[Elt:N]
  base::c(Before, NewElts, After)
}

#' @rdname insert_elts_before
#' @export
insert_cols_before <- function(X, NewCols, Col) {
  Errs <- NULL
  if (!uj::.D2D(X) | uj::nv0(X)) {Errs <- base::c(Errs, "[X] must be a populated matrix or data.frame.")}
  if (!uj::.D2D(NewCols) | uj::nv0(NewCols)) {Errs <- base::c(Errs, "[NewCols] must be a populated matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(Col)) {Errs <- base::c(Errs, "[Col] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if      (uj::.atm_mat(X) & uj::.atm_mat(NewCols)) {if (!uj::compatible_mats(2, X, NewCols)) {uj::stopperr("[X] and [NewCols] are not compatible matrices for column binding (?uj::compatible_mats).", PKG = "uj")}}
  else if (uj::.atm_dtf(X) & uj::.atm_dtf(NewCols)) {if (!uj::compatible_dtfs(2, X, NewCols)) {uj::stopperr("[X] and [NewCols] are not compatible data.frames for column binding (?uj::compatible_dtfs).", PKG = "uj")}}
  else {uj::stopperr("[X] and [Y] must both be atomic matrices or atomic data.frames.", PKG = "uj")}
  N <- base::ncol(X)
  if (base::abs(Col) > N) {Errs <- base::c(Errs, "abs(Col) > ncol(X).")}
  if (Col == 0) {Errs <- base::c(Errs, "[Col] may not be 0.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (Col < 0) {Col <- N + Col + 1}
  Before <- uj::f0(Col == 1, NULL, 1:(Col - 1))
  After <- Col:N
  uj::f0(uj::NLL(Before), base::cbind(NewCols, X[ , After]), base::cbind(X[ , Before], NewCols, X[ , After]))
}

#' @rdname insert_elts_before
#' @export
insert_rows_before <- function(X, NewRows, Row) {
  Errs <- NULL
  if (!uj::.D2D(X) | uj::nv0(X)) {Errs <- base::c(Errs, "[X] must be a populated matrix or data.frame.")}
  if (!uj::.D2D(NewRows) | uj::nv0(NewRows)) {Errs <- base::c(Errs, "[NewRows] must be a populated matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(Row)) {Errs <- base::c(Errs, "[Row] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if      (uj::.atm_mat(X) & uj::.atm_mat(NewRows)) {if (!uj::compatible_mats(1, X, NewRows)) {uj::stopperr("[X] and [NewRows] are not compatible matrices for row binding (?uj::compatible_mats).", PKG = "uj")}}
  else if (uj::.atm_dtf(X) & uj::.atm_dtf(NewRows)) {if (!uj::compatible_dtfs(1, X, NewRows)) {uj::stopperr("[X] and [NewRows] are not compatible data.frames for row binding (?uj::compatible_dtfs).", PKG = "uj")}}
  else {uj::stopperr("[X] and [NewRows] must both be atomic matrices or atomic data.frames (?uj::atm_dtf).", PKG = "uj")}
  N <- base::nrow(X)
  if (base::abs(Row) > N) {Errs <- base::c(Errs, "abs(Row) > nrow(X).")}
  if (Row == 0) {Errs <- base::c(Errs, "[Row] may not be 0.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (Row < 0) {Row <- N + Row + 1}
  Before <- uj::f0(Row == 1, NULL, 1:(Row - 1))
  After <- Row:N
  uj::f0(uj::NLL(Before), base::rbind(NewRows, X[After, ]), base::rbind(X[Before, ], NewRows, X[After, ]))
}

#' @rdname insert_elts_before
#' @export
insert_elt_after <- function(X, NewElts, Elt) {
  Errs <- NULL
  if (!uj::.atm_vec(X)) {Errs <- base::c(Errs, "[X] must be an atomic vector.")}
  if (!uj::.atm_vec(NewElts)) {Errs <- base::c(Errs, "[NewElts] must be an atomic vector.")}
  if (!uj::.cmp_whl_scl(N)) {Errs <- base::c(Errs, "[N] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  N <- base::length(X)
  if (!uj:::.compat(X, NewElts)) {Errs <- base::c(Errs, "[NewElts] must be mode-compatible with [X] (?uj::compatible).")}
  if (base::abs(Elt) > N) {Errs <- base::c(Errs, "abs(Elt) > length(X).")}
  if (Elt == 0) {Errs <- base::c(Errs, "[N] may not be 0.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (Elt < 0) {Elt <- N + Elt + 1}
  Before <- X[1:Elt]
  After <- uj::f0(Elt == N, NULL, X[(Elt + 1):N])
  base::c(Before, NewElts, After)
}

#' @rdname insert_elts_before
#' @export
insert_col_after <- function(X, NewCols, Col) {
  Errs <- NULL
  if (!uj::.D2D(X) | uj::nv0(X)) {Errs <- base::c(Errs, "[X] must be a populated matrix or data.frame.")}
  if (!uj::.D2D(NewCols) | uj::nv0(NewCols)) {Errs <- base::c(Errs, "[NewCols] must be a populated matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(NewCols)) {Errs <- base::c(Errs, "[Col] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (uj::.atm_mat(X) & uj::.atm_mat(NewCols)) {if (!uj::compatible_mats(2, X, NewCols)) {uj::stopperr("[X] and [NewCols] are not compatible matrices for column binding (?compatible_mats).", PKG = "uj")}}
  else if (uj::.atm_dtf(X) & uj::.atm_dtf(NewCols)) {if (!uj::compatible_dtfs(2, X, NewCols)) {uj::stopperr("[X] and [NewCols] are not compatible data.frames for column binding (?compatible_dtfs).", PKG = "uj")}}
  else {uj::stopperr("[X] and [NewCols] must both be atomic matrices or atomic data.frames.", PKG = "uj")}
  N <- base::ncol(X)
  if (base::abs(Col) > N) {Errs <- base::c(Errs, "abs(Col) > ncol(X).")}
  if (Col == 0) {Errs <- base::c(Errs, "[Col] may not be 0.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (Col < 0) {Col <- N + Col + 1}
  Before <- 1:Col
  After <- uj::f0(Col == N, NULL, (Col + 1):N)
  uj::f0(uj::NLL(After), base::cbind(X[ , Before], NewCols), base::cbind(X[ , Before], NewCols, X[ , After]))
}

#' @rdname insert_elts_before
#' @export
insert_after_row <- function(X, NewRows, Row) {
  Errs <- NULL
  if (!uj::.D2D(X) | uj::nv0(X)) {Errs <- base::c(Errs, "[X] must be a populated matrix or data.frame.")}
  if (!uj::.D2D(NewRows) | uj::nv0(NewRows)) {Errs <- base::c(Errs, "[NewRows] must be a populated matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(Row)) {Errs <- base::c(Errs, "[Row] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (uj::.atm_mat(X) & uj::.atm_mat(NewRows)) {if (!uj::compatible_mats(1, X, NewRows)) {uj::stopperr("[X] and [NewRows] are not compatible matrices (?compatible_mats) for row binding.", PKG = "uj")}}
  else if (uj::.atm_dtf(X) & uj::.atm_dtf(NewRows)) {if (!uj::compatible_dtfs(1, X, NewRows)) {uj::stopperr("[X] and [NewRows] are not compatible data frames (?compatible_dtfs) for row binding.", PKG = "uj")}}
  else {uj::stopperr("[X] and [NewRows] must be compatible atomic matrices or compatible atomic data.frames (?uj::compatible, ?uj::atm_dtf).", PKG = "uj")}
  N <- base::ncol(X)
  if (base::abs(Row) > N) {Errs <- base::c(Errs, "abs(Row) > nrow(X).")}
  if (Row == 0) {Errs <- base::c(Errs, "[Row] may not be 0.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (Row < 0) {Row <- N + Row + 1}
  Before <- 1:Row
  After <- uj::f0(Row == N, NULL, (Row + 1):N)
  uj::f0(uj::NLL(After), base::cbind(X[Before, ], NewRows), base::cbind(X[Before, ], NewRows, X[After, ]))
}
