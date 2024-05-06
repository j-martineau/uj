# internals ####

.nth_errs <- function(x, n, scl.count, stack) {
  okN <- uj::f0(scl.count, uj::.cmp_psw_scl(n), uj::.cmp_psw_vec(n))
  fun <- uj::caller()
  errs <- NULL
  if (!uj::.pop_vec(x)   ) {errs <- base::c(errs, "[x] is not a populated vector (?pop_vec)."                          )}
  if ( scl.count & !okN   ) {errs <- base::c(errs, "[n] must be a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!scl.count & !okN   ) {errs <- base::c(errs, "[n] must be a complete positive whole-number vector (?cmp_psw_vec).")}
  if (!base::is.null(errs)) {uj::stopperr(errs, fun = fun, pkg = "uj", stack = stack)}
  if (base::any(n > base::length(x))) {uj::stopperr(base::paste0(uj::f0(scl.count, "", "The largest value in")," [n] is greater than the number of elements in [x]."), fun = fun, pkg = "uj", stack = stack)}
}

# exported ####

#' @encoding UTF-8
#' @name nth
#' @family to_std_fun_form
#' @family values
#' @title Extract elements by numeric position
#' @description Extract the `n`-th, `n`-th from last, first `n`, and last `n` elements of a vector of list.
#' \tabular{ll}{  `.nth_from_last`   \tab Gets the `n`th from last element(s). \cr
#'                `.nth_plus`        \tab Gets element `n` and onwards.        \cr
#'                `.first_n`          \tab Gets the first `n` elements.         \cr
#'                `.last_n`           \tab Gets the last `n` elements.          \cr
#'                `.nth`             \tab Gets the `n`-th element(s).            }
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
.nth <- function(x, n) {
  uj:::.nth_errs(x, n, F, uj::callers())
  x[n]
}

#' @rdname nth
#' @export
.first_n <- function(x, n) {
  uj:::.nth_errs(x, n, T, uj::callers())
  x[1:n]
}

#' @rdname nth
#' @export
.last_n <- function(x, n) {
  uj:::.nth_errs(x, n, T, uj::callers())
  n <- 1:n
  x <- base::rev(x[base::rev(n)])
}

#' @rdname nth
#' @export
.nth_from_last <- function(x, n) {
  uj:::.nth_errs(x, n, F, uj::callers())
  base::rev(x)[n]
}

#' @rdname nth
#' @export
.nth_plus <- function(x, n) {
  uj:::.nth_errs(x, n, F, uj::callers())
  x[n:base::length(x)]
}
