#' @encoding UTF-8
#' @family function_form
#' @title Manipulate positions, rows, and columns
#' @details
#' \tabular{ll}{  `move_val`     \tab Move value `Val` of `X` into the `From`-th element of `X`. Assumes `length(which(X == Val)) == 1`. \cr   \tab   \cr
#'                `move_elt`     \tab Move the value in the `From`-th position of `X` to the `To`-th position of `X`.                    \cr   \tab   \cr
#'                `move_row`     \tab Move the `From`-th row of `X` to the `To`-th row of `X`.                                           \cr   \tab   \cr
#'                `move_col`     \tab Move the `From`-th column of `X` to the `To`-th column of `X`.                                                    }
#' `From < 0` and `To < 0` index from the last position, row, or column rather than the first.
#' @param X For `move_elt` and `move_val` an atomic vector. A data.frame or matrix for all others.
#' @param Val A non-`NA` atomic scalar value found in `X`.
#' @param From Whole-number scalar indexing the element, row, or column to be moved. Negative values index from the last position, row or column.
#' @param To Whole-number scalar indexing the element, row, or column to be moved into. Negative values index from the last position, row or column.
#' @return An object of the same class and dimension as `X`.
#' @export
move_elt <- function(X, From, To) {
  Errs <- NULL
  if (!uj::.atm_vec(X)) {Errs <- base::c(Errs, "[X] must be an atomic vector.")}
  if (!uj::.cmp_whl_scl(From)) {Errs <- base::c(Errs, "[From] must be a whole number scalar.")}
  if (!uj::.cmp_whl_scl(To)) {Errs <- base::c(Errs, "[To] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  N <- base::length(X)
  if (From == 0) {Errs <- base::c(Errs, "[From] may not be 0.")}
  if (To == 0) {Errs <- base::c(Errs, "[To] may not be 0.")}
  if (base::abs(From) > N) {Errs <- base::c(Errs, "abs(From) > length(X).")}
  if (base::abs(To) > N) {Errs <- base::c(Errs, "abs(To) > length(X).")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (From < 0) {From <- N + From + 1}
  if (To < 0) {To <- N + To + 1}
  Value  <- X[From]
  Before <- uj::f0(To == 1, NULL, X[1:(To - 1)])
  After  <- uj::f0(To == N, NULL, X[(To + 1):N])
  Before <- Before[Before != Value]
  After  <- After[ After  != Value]
  base::c(Before, Value, After)
}

#' @rdname move_elt
#' @export
move_val <- function(X, Val, To) {
  Errs <- NULL
  if (!uj::.atm_vec(X)) {Errs <- base::c(Errs, "[X] must be an atomic vector.")}
  if (!uj::.cmp_scl(Val)) {Errs <- base::c(Errs, "[Val] must be a complete atomic scalar.")}
  if (!uj::.cmp_whl_scl(To)) {Errs <- base::c(Errs, "[To] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  Neg <- To < 0
  if (Neg) {To <- -To; X <- base::rev(X)}
  if (!uj:::.compat(X, Val)) {Errs <- base::c(Errs, "[X] and [Val] must be compatible.")}
  if (To == 0) {Errs <- base::c(Errs, "[To] may not be 0.")}
  if (To > base::length(X)) {Errs <- base::c(Errs, "abs(To) > length(X).")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  From <- uj::wv(X == Val)
  if (base::length(From) == 0) {Errs <- base::c(Errs, "There are no values of [X] matching [Val].")}
  if (base::length(From) > 1) {Errs <- base::c(Errs, "[Val] matches multiple values of [X].")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  uj::move_pos(X, From, To)
}

#' @rdname move_elt
#' @export
move_row <- function(X, From, To) {
  Errs <- NULL
  if (!uj::.D2D(X)) {Errs <- base::c(Errs, "[X] must be a matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(From)) {Errs <- base::c(Errs, "[From] must be a whole number scalar.")}
  if (!uj::.cmp_whl_scl(To)) {Errs <- base::c(Errs, "[To] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  N <- base::nrow(X)
  if (From == 0) {Errs <- base::c(Errs, "[From] may not be 0.")}
  if (To == 0) {Errs <- base::c(Errs, "[To] may not be 0.")}
  if (abs(From) > N) {Errs <- base::c(Errs, "abs(From) > nrow(X).")}
  if (abs(To) > N) {Errs <- base::c(Errs, "abs(To) > nrow(X).")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (From < 0) {From <- N + From + 1}
  if (To < 0) {To <- N + To + 1}
  Before <- uj::f0(To == 1, NULL, 1:(To - 1))
  After  <- uj::f0(To == N, NULL, (To + 1):N)
  Before <- Before[Before != From]
  After  <- After[ After  != To]
  Order <- base::c(Before, From, After)
  X[Order, ]
}

#' @rdname move_elt
#' @export
move_col <- function(X, From, To) {
  Errs <- NULL
  if (!uj::.D2D(X)) {Errs <- base::c(Errs, "[X] must be a matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(From)) {Errs <- base::c(Errs, "[From] must be a whole number scalar.")}
  if (!uj::.cmp_whl_scl(To)) {Errs <- base::c(Errs, "[To] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  N <- uj::nc(X)
  if (From == 0) {Errs <- base::c(Errs, "[From] may not be 0.")}
  if (To == 0) {Errs <- base::c(Errs, "[To] may not be 0.")}
  if (base::abs(From) > N) {Errs <- base::c(Errs, "abs(From) > nrow(X).")}
  if (base::abs(To) > N) {Errs <- base::c(Errs, "abs(To) > nrow(X).")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, PKG = "uj")}
  if (From < 0) {From <- N + From + 1}
  if (To < 0) {To <- N + To + 1}
  Before <- uj::f0(To == 1, NULL, 1:(To - 1))
  After  <- uj::f0(To == N, NULL, (To + 1):N)
  Before <- Before[Before != From]
  After  <- After[ After  != To]
  Order <- base::c(Before, From, After)
  X[ , Order]
}
