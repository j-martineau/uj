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
