#' @encoding UTF-8
#' @family function_form
#' @title Manipulate positions, rows, and columns
#' @details
#' \tabular{ll}{  `insert_after_pos`    \tab Insert value `V` after the `N`-the position of `X`.   \cr   \tab   \cr
#'                `insert_after_col`    \tab Insert `Y` into `X` after the `N`-th column of `X`    \cr   \tab   \cr
#'                `insert_after_row`    \tab Insert `Y` into `X` after the `N`-th row of `X`       \cr   \tab   \cr
#'                `insert_before_pos`   \tab Insert value `V` before the `N`-the positions of `X`. \cr   \tab   \cr
#'                `insert_before_col`   \tab Insert `Y` into `X` before the `N`-th column of `X`   \cr   \tab   \cr
#'                `insert_before_row`   \tab Insert `Y` into `X` before the `N`-th row of `X`                     }
#' `N < 0` index from the last position, row, or column rather than the first.
#' @param X For `move_pos` and `move_val` an atomic vector. For `move_row` and `move_col`, a data.frame or matrix.
#' @param V A non-`NA` atomic vector mode-compatible with `X`.
#' @param Y A matrix or data frame `cbind` or `rbind` compatible with `X`.
#' @param N A complete whole number scalar identifying a position, row, or column of `X`. Negative values index from the last position, row, or column rather than the first.
#' @return An object of the same class as `X` with increased dimension.
#' @export
insert_before_pos <- function(X, V, N) {
  Errs <- NULL
  if (!uj::.atm_vec(X)) {Errs <- base::c(Errs, "[X] must be an atomic vector.")}
  if (!uj::.atm_vec(V)) {Errs <- base::c(Errs, "[V] must be an atomic vector.")}
  if (!uj::.cmp_whl_scl(N)) {Errs <- base::c(Errs, "[N] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  NV <- uj::N(X)
  if (!uj:::.compat(X, V)) {Errs <- base::c(Errs, "[V] must be mode-compatible with [X].")}
  if (base::abs(N) > NV) {Errs <- base::c(Errs, "abs(N) > length(X).")}
  if (N == 0) {Errs <- base::c(Errs, "[N] may not be 0.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (N < 0) {N <- N + N + 1}
  Before <- uj::f0(N == 1, NULL, X[1:(NV - 1)])
  After  <- X[NV:N]
  base::c(Before, V, After)
}

#' @rdname insert_before_pos
#' @export
insert_before_col <- function(X, Y, N) {
  Errs <- NULL
  if (!uj::.D2D(X) | uj::nv0(X)) {Errs <- base::c(Errs, "[X] must be a populated matrix or data.frame.")}
  if (!uj::.D2D(Y) | uj::nv0(Y)) {Errs <- base::c(Errs, "[Y] must be a populated matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(N)) {Errs <- base::c(Errs, "[N] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (uj::.atm_mat(X) & uj::.atm_mat(Y)) {if (!uj::compatible_mats(2, X, Y)) {uj::stopperr("[X] and [Y] are not compatible for column binding.", PKG = "uj")}}
  else if (uj::.atm_dtf(X) & uj::.atm_dtf(Y)) {if (!uj::compatible_dtfs(2, X, Y)) {uj::stopperr("[X] and [Y] are not compatible for column binding.", PKG = "uj")}}
  else {uj::stopperr("[X] and [Y] must both be atomic matrices or atomic data.frames.", PKG = "uj")}
  NC <- uj::nc(X)
  if (base::abs(N) > NC) {Errs <- base::c(Errs, "abs(N) > ncol(X).")}
  if (N == 0) {Errs <- base::c(Errs, "[N] may not be 0.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (N < 0) {N <- N + N + 1}
  Before <- uj::f0(N == 1, NULL, 1:(N - 1))
  After  <- NC:N
  uj::f0(uj::NLL(Before), base::cbind(Y, X[ , After]), base::cbind(X[ , Before], Y, X[ , After]))
}

#' @rdname insert_before_pos
#' @export
insert_before_row <- function(X, Y, N) {
  Errs <- NULL
  if (!uj::.D2D(X) | uj::nv0(X)) {Errs <- base::c(Errs, "[X] must be a populated matrix or data.frame.")}
  if (!uj::.D2D(Y) | uj::nv0(Y)) {Errs <- base::c(Errs, "[Y] must be a populated matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(N)) {Errs <- base::c(Errs, "[N] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (uj::.atm_mat(X) & uj::.atm_mat(Y)) {if (!uj::compatible_mats(1, X, Y)) {uj::stopperr("[X] and [Y] are not compatible for row binding.", PKG = "uj")}}
  else if (uj::.atm_dtf(X) & uj::.atm_dtf(Y)) {if (!uj::compatible_dtfs(1, X, Y)) {uj::stopperr("[X] and [Y] are not compatible for row binding.", PKG = "uj")}}
  else {uj::stopperr("[X] and [Y] must both be atomic matrices or atomic data.frames.", PKG = "uj")}
  NR <- uj::nr(X)
  if (base::abs(N) > NR) {Errs <- base::c(Errs, "abs(N) > nrow(X).")}
  if (N == 0) {Errs <- base::c(Errs, "[N] may not be 0.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (N < 0) {N <- N + N + 1}
  Before <- uj::f0(N == 1, NULL, 1:(N - 1))
  After  <- NR:N
  uj::f0(uj::NLL(Before), base::rbind(Y, X[After, ]), base::rbind(X[Before, ], Y, X[After, ]))
}

#' @rdname insert_before_pos
#' @export
insert_after_pos <- function(X, V, N) {
  Errs <- NULL
  if (!uj::.atm_vec(X)) {Errs <- base::c(Errs, "[X] must be an atomic vector.")}
  if (!uj::.atm_vec(V)) {Errs <- base::c(Errs, "[V] must be an atomic vector.")}
  if (!uj::.cmp_whl_scl(N)) {Errs <- base::c(Errs, "[N] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  N <- uj::N(X)
  if (!uj:::.compat(X, V)) {Errs <- base::c(Errs, "[V] must be mode-compatible with [X].")}
  if (base::abs(N) > N) {Errs <- base::c(Errs, "abs(N) > length(X).")}
  if (N == 0) {Errs <- base::c(Errs, "[N] may not be 0.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (N < 0) {N <- N + N + 1}
  Before <- X[1:N]
  After <- uj::f0(N == N, NULL, X[(N + 1):N])
  base::c(Before, V, After)
}

#' @rdname insert_before_pos
#' @export
insert_after_col <- function(X, Y, N) {
  Errs <- NULL
  if (!uj::.D2D(X) | uj::nv0(X)) {Errs <- base::c(Errs, "[X] must be a populated matrix or data.frame.")}
  if (!uj::.D2D(Y) | uj::nv0(Y)) {Errs <- base::c(Errs, "[Y] must be a populated matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(N)) {Errs <- base::c(Errs, "[N] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (uj::.atm_mat(X) & uj::.atm_mat(Y)) {if (!uj::compatible_mats(2, X, Y)) {uj::stopperr("[X] and [Y] are not compatible for column binding.", PKG = "uj")}}
  else if (uj::.atm_dtf(X) & uj::.atm_dtf(Y)) {if (!uj::compatible_dtfs(2, X, Y)) {uj::stopperr("[X] and [Y] are not compatible for column binding.", PKG = "uj")}}
  else {uj::stopperr("[X] and [Y] must both be atomic matrices or atomic data.frames.", PKG = "uj")}
  NC <- uj::nc(X)
  if (base::abs(N) > NC) {Errs <- base::c(Errs, "abs(N) > ncol(X).")}
  if (N == 0) {Errs <- base::c(Errs, "[N] may not be 0.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (N < 0) {N <- N + N + 1}
  Before <- 1:N
  After  <- uj::f0(N == N, NULL, (N + 1):NC)
  uj::f0(uj::NLL(After), base::cbind(X[ , Before], Y), base::cbind(X[ , Before], Y, X[ , After]))
}

#' @rdname insert_before_pos
#' @export
insert_after_row <- function(X, Y, N) {
  Errs <- NULL
  if (!uj::.D2D(X) | uj::nv0(X)) {Errs <- base::c(Errs, "[X] must be a populated matrix or data.frame.")}
  if (!uj::.D2D(Y) | uj::nv0(Y)) {Errs <- base::c(Errs, "[Y] must be a populated matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(N)) {Errs <- base::c(Errs, "[N] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (uj::.atm_mat(X) & uj::.atm_mat(Y)) {if (!uj::compatible_mats(1, X, Y)) {uj::stopperr("[X] and [Y] are not compatible for row binding.", PKG = "uj")}}
  else if (uj::.atm_dtf(X) & uj::.atm_dtf(Y)) {if (!uj::compatible_dtfs(1, X, Y)) {uj::stopperr("[X] and [Y] are not compatible for row binding.", PKG = "uj")}}
  else {uj::stopperr("[X] and [Y] must both be atomic matrices or atomic data.frames.", PKG = "uj")}
  NR <- uj::nc(X)
  if (base::abs(N) > NR) {Errs <- base::c(Errs, "abs(N) > nrow(X).")}
  if (N == 0) {Errs <- base::c(Errs, "[N] may not be 0.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (N < 0) {N <- N + N + 1}
  Before <- 1:N
  After  <- uj::f0(N == N, NULL, (N + 1):NR)
  uj::f0(uj::NLL(After), base::cbind(X[Before, ], Y), base::cbind(X[Before, ], Y, X[After, ]))
}
