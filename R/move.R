#' @encoding UTF-8
#' @family function_form
#' @title Manipulate positions, rows, and columns
#' @details
#' \tabular{ll}{  `move_val`     \tab Move value `val` of `x` into the `from`-th element of `x`. Assumes `length(which(x == val)) == 1`. \cr   \tab   \cr
#'                `move_elt`     \tab Move the value in the `from`-th position of `x` to the `to`-th position of `x`.                    \cr   \tab   \cr
#'                `move_row`     \tab Move the `from`-th row of `x` to the `to`-th row of `x`.                                           \cr   \tab   \cr
#'                `move_col`     \tab Move the `from`-th column of `x` to the `to`-th column of `x`.                                                    }
#' `from < 0` and `to < 0` index from the last position, row, or column rather than the first.
#' @param x For `move_elt` and `move_val` an atomic vector. A data.frame or matrix for all others.
#' @param val A non-`NA` atomic scalar value found in `x`.
#' @param from Whole-number scalar indexing the element, row, or column to be moved. Negative values index from the last position, row or column.
#' @param to Whole-number scalar indexing the element, row, or column to be moved into. Negative values index from the last position, row or column.
#' @return An object of the same class and dimension as `x`.
#' @export
move_elt <- function(x, from, to) {
  Errs <- NULL
  if (!uj::.atm_vec(x)) {Errs <- base::c(Errs, "[x] must be an atomic vector.")}
  if (!uj::.cmp_whl_scl(from)) {Errs <- base::c(Errs, "[from] must be a whole number scalar.")}
  if (!uj::.cmp_whl_scl(to)) {Errs <- base::c(Errs, "[to] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  N <- base::length(x)
  if (from == 0) {Errs <- base::c(Errs, "[from] may not be 0.")}
  if (to == 0) {Errs <- base::c(Errs, "[to] may not be 0.")}
  if (base::abs(from) > N) {Errs <- base::c(Errs, "abs(from) > length(x).")}
  if (base::abs(to) > N) {Errs <- base::c(Errs, "abs(to) > length(x).")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  if (from < 0) {from <- N + from + 1}
  if (to < 0) {to <- N + to + 1}
  Value  <- x[from]
  Before <- uj::f0(to == 1, NULL, x[1:(to - 1)])
  After  <- uj::f0(to == N, NULL, x[(to + 1):N])
  Before <- Before[Before != Value]
  After  <- After[ After  != Value]
  base::c(Before, Value, After)
}

#' @rdname move_elt
#' @export
move_val <- function(x, val, to) {
  Errs <- NULL
  if (!uj::.atm_vec(x)) {Errs <- base::c(Errs, "[x] must be an atomic vector.")}
  if (!uj::.cmp_scl(val)) {Errs <- base::c(Errs, "[val] must be a complete atomic scalar.")}
  if (!uj::.cmp_whl_scl(to)) {Errs <- base::c(Errs, "[to] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  Neg <- to < 0
  if (Neg) {to <- -to; x <- base::rev(x)}
  if (!uj:::.compat(x, val)) {Errs <- base::c(Errs, "[x] and [val] must be compatible.")}
  if (to == 0) {Errs <- base::c(Errs, "[to] may not be 0.")}
  if (to > base::length(x)) {Errs <- base::c(Errs, "abs(to) > length(x).")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  from <- uj::wv(x == val)
  if (base::length(from) == 0) {Errs <- base::c(Errs, "There are no values of [x] matching [val].")}
  if (base::length(from) > 1) {Errs <- base::c(Errs, "[val] matches multiple values of [x].")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  uj::move_pos(x, from, to)
}

#' @rdname move_elt
#' @export
move_row <- function(x, from, to) {
  Errs <- NULL
  if (!uj::.D2D(x)) {Errs <- base::c(Errs, "[x] must be a matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(from)) {Errs <- base::c(Errs, "[from] must be a whole number scalar.")}
  if (!uj::.cmp_whl_scl(to)) {Errs <- base::c(Errs, "[to] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  N <- base::nrow(x)
  if (from == 0) {Errs <- base::c(Errs, "[from] may not be 0.")}
  if (to == 0) {Errs <- base::c(Errs, "[to] may not be 0.")}
  if (abs(from) > N) {Errs <- base::c(Errs, "abs(from) > nrow(x).")}
  if (abs(to) > N) {Errs <- base::c(Errs, "abs(to) > nrow(x).")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  if (from < 0) {from <- N + from + 1}
  if (to < 0) {to <- N + to + 1}
  Before <- uj::f0(to == 1, NULL, 1:(to - 1))
  After  <- uj::f0(to == N, NULL, (to + 1):N)
  Before <- Before[Before != from]
  After  <- After[ After  != to]
  Order <- base::c(Before, from, After)
  x[Order, ]
}

#' @rdname move_elt
#' @export
move_col <- function(x, from, to) {
  Errs <- NULL
  if (!uj::.D2D(x)) {Errs <- base::c(Errs, "[x] must be a matrix or data.frame.")}
  if (!uj::.cmp_whl_scl(from)) {Errs <- base::c(Errs, "[from] must be a whole number scalar.")}
  if (!uj::.cmp_whl_scl(to)) {Errs <- base::c(Errs, "[to] must be a whole number scalar.")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  N <- uj::nc(x)
  if (from == 0) {Errs <- base::c(Errs, "[from] may not be 0.")}
  if (to == 0) {Errs <- base::c(Errs, "[to] may not be 0.")}
  if (base::abs(from) > N) {Errs <- base::c(Errs, "abs(from) > nrow(x).")}
  if (base::abs(to) > N) {Errs <- base::c(Errs, "abs(to) > nrow(x).")}
  if (uj::DEF(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  if (from < 0) {from <- N + from + 1}
  if (to < 0) {to <- N + to + 1}
  Before <- uj::f0(to == 1, NULL, 1:(to - 1))
  After  <- uj::f0(to == N, NULL, (to + 1):N)
  Before <- Before[Before != from]
  After  <- After[ After  != to]
  Order <- base::c(Before, from, After)
  x[ , Order]
}
