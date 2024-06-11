# internals ####

.nth_errs <- function(x, n, scl.count, stack) {
  okN <- uj::f0(scl.count, uj::.cmp_psw_scl(n), uj::.cmp_psw_vec(n))
  fun <- uj::caller()
  errs <- NULL
  if (!uj::.pop_vec(x)   ) {errs <- base::c(errs, "[x] is not a populated vector (?pop_vec)."                          )}
  if ( scl.count & !okN   ) {errs <- base::c(errs, "[n] must be a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!scl.count & !okN   ) {errs <- base::c(errs, "[n] must be a complete positive whole-number vector (?cmp_psw_vec).")}
  if (!base::is.null(errs)) {uj::stopperr(errs, fun = fun, stack = stack)}
  if (base::any(n > base::length(x))) {uj::stopperr(base::paste0(uj::f0(scl.count, "", "The largest value in")," [n] is greater than the number of elements in [x]."), fun = fun, stack = stack)}
}

# exported ####

#' @encoding UTF-8
#' @title Extract elements by numeric position
#' @description Extract the `n`-th, `n`-th from last, first `n`, and last `n` elements of a vector of list.
#' @param x A \link[=pop_vec]{populated vector} to extract elements from.
#' @param n A \link[=cmp_psw_scl]{complete positive whole-number scalar}.
#' @return A scalar, vector, or \link[=VLS]{vlist}.
#' @examples
#' .nth_from_last(letters, 5)
#' .nth_plus(letters, 5)
#' .first_n(letters, 5)
#' .last_n(letters, 5)
#' .nth(letters, 5:7)
#' @export
nth_help <- function() {utils::help("nth_help", package = "uj")}

#' @describeIn nth_help Gets the `n`-th element(s) of `x`.
#' @export
.nth <- function(x, n) {
  uj:::.nth_errs(x, n, F, uj::callers())
  x[n]
}

#' @describeIn nth_help Gets the first `n` elements of `x`.
#' @export
.first_n <- function(x, n) {
  uj:::.nth_errs(x, n, T, uj::callers())
  x[1:n]
}

#' @describeIn nth_help Gets the last `n` elements of `x`.
#' @export
.last_n <- function(x, n) {
  uj:::.nth_errs(x, n, T, uj::callers())
  n <- 1:n
  x <- base::rev(x[base::rev(n)])
}

#' @describeIn nth_help Gets the `n`th from last element(s) of `x`.
#' @export
.nth_from_last <- function(x, n) {
  uj:::.nth_errs(x, n, F, uj::callers())
  base::rev(x)[n]
}

#' @describeIn nth_help Gets element `n` and onwards of `x`.
#' @export
.nth_plus <- function(x, n) {
  uj:::.nth_errs(x, n, F, uj::callers())
  x[n:base::length(x)]
}
