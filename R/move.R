#' @encoding UTF-8
#' @family function_form
#' @title Manipulate positions, rows, and columns
#' @details
#' \tabular{ll}{  `move_val`     \tab Move value `V` of `X` into the `I`-th element of `X`. Assumes `length(which(X == V)) == 1`. \cr   \tab   \cr
#'                `move_pos`     \tab Move the value in the `I`-th position of `X` to the `J`-th position of `X`.                 \cr   \tab   \cr
#'                `move_row`     \tab Move the `I`-th row of `X` to the `J`-th row of `X`.                                        \cr   \tab   \cr
#'                `move_col`     \tab Move the `I`-th column of `X` to the `J`-th column of `X`.                                  \cr   \tab   \cr
#'                `swap_pos`     \tab Move the `I`-th value of `X` to the `J`-th position and vice versa.                         \cr   \tab   \cr
#'                `swap_row`     \tab Move the `I`-th row of `X` to the `J`-th row and vice versa.                                \cr   \tab   \cr
#'                `swap_col`     \tab Move the `I`-th column of `X` to the `J`-th column and vice versa.                                         }
#' `I < 0` and `J < 0` index from the last position, row, or column rather than the first.
#' @param X For `move_val`, `move_pos`, and `swap_pos` an atomic vector. A data.frame or matrix for all others.
#' @param V A non-`NA` atomic scalar value found in `X`.
#' @param I,J Whole-number scalars indexing positions, row, or columns of `X`. Negatives index from the last position, row, or column.
#' @return An object of the same class and dimension as `X`.
#' @export
move_pos <- function(X, I, J) {
  Errs <- NULL
  if (!uj::.atm_vec(X)) {Errs <- base::c(Errs, "[X] must be an atomic vector.")}
  if (!uj::.cmp_whl_scl(I)) {Errs <- base::c(Errs, "[I] must be a whole number scalar.")}
  if (!uj::.cmp_whl_scl(J)) {Errs <- base::c(Errs, "[J] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  N <- uj::N(X)
  if (I == 0) {Errs <- base::c(Errs, "[I] may not be 0.")}
  if (J == 0) {Errs <- base::c(Errs, "[J] may not be 0.")}
  if (abs(I) > N) {Errs <- base::c(Errs, "abs(I) > length(X).")}
  if (abs(J) > N) {Errs <- base::c(Errs, "abs(J) > length(X).")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (I < 0) {I <- N + I + 1}
  if (J < 0) {J <- N + J + 1}
  Value  <- X[I]
  Before <- uj::f0(J == 1, NULL, X[1:(J - 1)])
  After  <- uj::f0(J == N, NULL, X[(J + 1):N])
  Before <- Before[Before != Value]
  After  <- After[ After  != Value]
  base::c(Before, Value, After)
}

#' @rdname move_pos
#' @export
move_val <- function(X, V, J) {
  Errs <- NULL
  if (!uj::.atm_vec(X)) {Errs <- base::c(Errs, "[X] must be an atomic vector.")}
  if (!uj::.cmp_scl(V)) {Errs <- base::c(Errs, "[V] must be a complete atomic scalar.")}
  if (!uj::.cmp_whl_scl(J)) {Errs <- base::c(Errs, "[J] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  Neg <- J < 0
  if (Neg) {J <- -J; X <- base::rev(X)}
  if (!uj:::.compat(X, V)) {Errs <- base::c(Errs, "[X] and [V] must be compatible.")}
  if (J == 0) {Errs <- base::c(Errs, "[J] may not be 0.")}
  if (J > uj::N(X)) {Errs <- base::c(Errs, "abs(J) > length(X).")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  I <- uj::wv(X == V)
  if (uj::n0(I)) {Errs <- base::c(Errs, "There are no values of [X] matching [V].")}
  if (uj::n2p(I)) {Errs <- base::c(Errs, "[V] matches multiple values of [X].")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  uj::move_pos(X, I, J)
}

#' @rdname move_pos
#' @export
move_row <- function(X, I, J) {
  Errs <- NULL
  if (!uj::.D2D(X)) {Errs <- base::c(Errs, "[X] must be a matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(I)) {Errs <- base::c(Errs, "[I] must be a whole number scalar.")}
  if (!uj::.cmp_whl_scl(J)) {Errs <- base::c(Errs, "[J] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  N <- uj::nr(X)
  if (I == 0) {Errs <- base::c(Errs, "[I] may not be 0.")}
  if (J == 0) {Errs <- base::c(Errs, "[J] may not be 0.")}
  if (abs(I) > N) {Errs <- base::c(Errs, "abs(I) > nrow(X).")}
  if (abs(J) > N) {Errs <- base::c(Errs, "abs(J) > nrow(X).")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (I < 0) {I <- N + I + 1}
  if (J < 0) {J <- N + J + 1}
  Before <- uj::f0(J == 1, NULL, 1:(J - 1))
  After  <- uj::f0(J == N, NULL, (J + 1):N)
  Before <- Before[Before != I]
  After  <- After[ After  != J]
  Order <- base::c(Before, I, After)
  X[Order, ]
}

#' @rdname move_pos
#' @export
move_col <- function(X, I, J) {
  Errs <- NULL
  if (!uj::.D2D(X)) {Errs <- base::c(Errs, "[X] must be a matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(I)) {Errs <- base::c(Errs, "[I] must be a whole number scalar.")}
  if (!uj::.cmp_whl_scl(J)) {Errs <- base::c(Errs, "[J] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  N <- uj::nc(X)
  if (I == 0) {Errs <- base::c(Errs, "[I] may not be 0.")}
  if (J == 0) {Errs <- base::c(Errs, "[J] may not be 0.")}
  if (abs(I) > N) {Errs <- base::c(Errs, "abs(I) > nrow(X).")}
  if (abs(J) > N) {Errs <- base::c(Errs, "abs(J) > nrow(X).")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (I < 0) {I <- N + I + 1}
  if (J < 0) {J <- N + J + 1}
  Before <- uj::f0(J == 1, NULL, 1:(J - 1))
  After  <- uj::f0(J == N, NULL, (J + 1):N)
  Before <- Before[Before != I]
  After  <- After[ After  != J]
  Order <- base::c(Before, I, After)
  X[ , Order]
}

#' @rdname move_pos
#' @export
swap_pos <- function(X, I, J) {
  Errs <- NULL
  if (!uj::.atm_vec(X)) {Errs <- base::c(Errs, "[X] must be an atomic vector.")}
  if (!uj::.cmp_whl_scl(I)) {Errs <- base::c(Errs, "[I] must be a whole number scalar.")}
  if (!uj::.cmp_whl_scl(J)) {Errs <- base::c(Errs, "[J] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  N <- uj::N(X)
  if (I == 0) {Errs <- base::c(Errs, "[I] may not be 0.")}
  if (J == 0) {Errs <- base::c(Errs, "[J] may not be 0.")}
  if (abs(I) > N) {Errs <- base::c(Errs, "abs(I) > length(X).")}
  if (abs(J) > N) {Errs <- base::c(Errs, "abs(J) > length(X).")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (I < 0) {I <- N + I + 1}
  if (J < 0) {J <- N + J + 1}
  ValI <- X[I]
  ValJ <- X[J]
  X[J] <- ValI
  X[I] <- ValJ
  X
}

#' @rdname move_pos
#' @export
swap_rows <- function(X, I, J) {
  Errs <- NULL
  if (!uj::.D2D(X)) {Errs <- base::c(Errs, "[X] must be a matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(I)) {Errs <- base::c(Errs, "[I] must be a whole number scalar.")}
  if (!uj::.cmp_whl_scl(J)) {Errs <- base::c(Errs, "[J] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  N <- uj::nr(X)
  if (I == 0) {Errs <- base::c(Errs, "[I] may not be 0.")}
  if (J == 0) {Errs <- base::c(Errs, "[J] may not be 0.")}
  if (abs(I) > N) {Errs <- base::c(Errs, "abs(I) > nrow(X).")}
  if (abs(J) > N) {Errs <- base::c(Errs, "abs(J) > nrow(X).")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (I < 0) {I <- N + I + 1}
  if (J < 0) {J <- N + J + 1}
  RowI <- X[I, ]
  RowJ <- X[J, ]
  X[I, ] <- RowJ
  X[J, ] <- RowI
  X
}

#' @rdname move_pos
#' @export
swap_cols <- function(X, I, J) {
  Errs <- NULL
  if (!uj::.D2D(X)) {Errs <- base::c(Errs, "[X] must be a matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(I)) {Errs <- base::c(Errs, "[I] must be a whole number scalar.")}
  if (!uj::.cmp_whl_scl(J)) {Errs <- base::c(Errs, "[J] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  N <- uj::nc(X)
  if (I == 0) {Errs <- base::c(Errs, "[I] may not be 0.")}
  if (J == 0) {Errs <- base::c(Errs, "[J] may not be 0.")}
  if (abs(I) > N) {Errs <- base::c(Errs, "abs(I) > ncol(X).")}
  if (abs(J) > N) {Errs <- base::c(Errs, "abs(J) > ncol(X).")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (I < 0) {I <- N + I + 1}
  if (J < 0) {J <- N + J + 1}
  ColI <- X[ , I]
  ColJ <- X[ , J]
  X[ , I] <- ColJ
  X[ , J] <- ColI
  X
}

