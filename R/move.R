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
  errs <- NULL
  if (!ppp::.atm_vec(x)) {errs <- base::c(errs, "[x] must be an atomic vector.")}
  if (!ppp::.cmp_whl_scl(from)) {errs <- base::c(errs, "[from] must be a whole number scalar.")}
  if (!ppp::.cmp_whl_scl(to)) {errs <- base::c(errs, "[to] must be a whole number scalar.")}
  if (uj::DEF(errs)) {ppp::stopperr(errs, pkg = "uj")}
  n <- base::length(x)
  if (from == 0) {errs <- base::c(errs, "[from] may not be 0.")}
  if (to == 0) {errs <- base::c(errs, "[to] may not be 0.")}
  if (base::abs(from) > n) {errs <- base::c(errs, "abs(from) > length(x).")}
  if (base::abs(to) > n) {errs <- base::c(errs, "abs(to) > length(x).")}
  if (uj::DEF(errs)) {ppp::stopperr(errs, pkg = "uj")}
  if (from < 0) {from <- n + from + 1}
  if (to < 0) {to <- n + to + 1}
  value  <- x[from]
  before <- uj::f0(to == 1, NULL, x[1:(to - 1)])
  after  <- uj::f0(to == N, NULL, x[(to + 1):N])
  before <- before[before != value]
  after  <- after[ after  != value]
  base::c(before, value, after)
}

#' @rdname move_elt
#' @export
move_val <- function(x, val, to) {
  errs <- NULL
  if (!ppp::.atm_vec(x)) {errs <- base::c(errs, "[x] must be an atomic vector.")}
  if (!ppp::.cmp_scl(val)) {errs <- base::c(errs, "[val] must be a complete atomic scalar.")}
  if (!ppp::.cmp_whl_scl(to)) {errs <- base::c(errs, "[to] must be a whole number scalar.")}
  if (uj::DEF(errs)) {ppp::stopperr(errs, pkg = "uj")}
  neg <- to < 0
  if (neg) {to <- -to; x <- base::rev(x)}
  if (!uj:::.compat(x, val)) {errs <- base::c(errs, "[x] and [val] must be compatible.")}
  if (to == 0) {errs <- base::c(errs, "[to] may not be 0.")}
  if (to > base::length(x)) {errs <- base::c(errs, "abs(to) > length(x).")}
  if (uj::DEF(errs)) {ppp::stopperr(errs, pkg = "uj")}
  from <- uj::wv(x == val)
  if (base::length(from) == 0) {errs <- base::c(errs, "There are no values of [x] matching [val].")}
  if (base::length(from) > 1) {errs <- base::c(errs, "[val] matches multiple values of [x].")}
  if (uj::DEF(errs)) {ppp::stopperr(errs, pkg = "uj")}
  uj::move_pos(x, from, to)
}

#' @rdname move_elt
#' @export
move_row <- function(x, from, to) {
  errs <- NULL
  if (!uj::.D2D(x)) {errs <- base::c(errs, "[x] must be a matrix or data.frame.")}
  if (!ppp::.cmp_whl_scl(from)) {errs <- base::c(errs, "[from] must be a whole number scalar.")}
  if (!ppp::.cmp_whl_scl(to)) {errs <- base::c(errs, "[to] must be a whole number scalar.")}
  if (uj::DEF(errs)) {ppp::stopperr(errs, pkg = "uj")}
  N <- base::nrow(x)
  if (from == 0) {errs <- base::c(errs, "[from] may not be 0.")}
  if (to == 0) {errs <- base::c(errs, "[to] may not be 0.")}
  if (abs(from) > N) {errs <- base::c(errs, "abs(from) > nrow(x).")}
  if (abs(to) > N) {errs <- base::c(errs, "abs(to) > nrow(x).")}
  if (uj::DEF(errs)) {ppp::stopperr(errs, pkg = "uj")}
  if (from < 0) {from <- N + from + 1}
  if (to < 0) {to <- N + to + 1}
  before <- uj::f0(to == 1, NULL, 1:(to - 1))
  after  <- uj::f0(to == N, NULL, (to + 1):N)
  before <- before[before != from]
  after  <- after[ after  != to]
  order  <- base::c(before, from, after)
  x[order, ]
}

#' @rdname move_elt
#' @export
move_col <- function(x, from, to) {
  errs <- NULL
  if (!uj::.D2D(x)) {errs <- base::c(errs, "[x] must be a matrix or data.frame.")}
  if (!ppp::.cmp_whl_scl(from)) {errs <- base::c(errs, "[from] must be a whole number scalar.")}
  if (!ppp::.cmp_whl_scl(to)) {errs <- base::c(errs, "[to] must be a whole number scalar.")}
  if (uj::DEF(errs)) {ppp::stopperr(errs, pkg = "uj")}
  N <- uj::nc(x)
  if (from == 0) {errs <- base::c(errs, "[from] may not be 0.")}
  if (to == 0) {errs <- base::c(errs, "[to] may not be 0.")}
  if (base::abs(from) > N) {errs <- base::c(errs, "abs(from) > nrow(x).")}
  if (base::abs(to) > N) {errs <- base::c(errs, "abs(to) > nrow(x).")}
  if (uj::DEF(errs)) {ppp::stopperr(errs, pkg = "uj")}
  if (from < 0) {from <- N + from + 1}
  if (to < 0) {to <- N + to + 1}
  before <- uj::f0(to == 1, NULL, 1:(to - 1))
  after  <- uj::f0(to == N, NULL, (to + 1):N)
  before <- before[before != from]
  after  <- after[ after  != to]
  order <- base::c(before, from, after)
  x[ , order]
}
