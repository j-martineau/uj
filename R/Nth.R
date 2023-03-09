#' @encoding UTF-8
#' @family to_std_fun_form
#' @family values
#' @title Extract elements by numeric position
#' @description Extract the `N`-th, `n`-th from last, first `n`, and last `n` elements of a vector of list.
#' \tabular{ll}{  `Nth_from_last`   \tab Gets the `N`th from last element(s). \cr
#'                `Nth_plus`        \tab Gets element `N` and onwards.        \cr
#'                `firstN`          \tab Gets the first `N` elements.         \cr
#'                `lastN`           \tab Gets the last `N` elements.          \cr
#'                `Nth`             \tab Gets the `N`-th element(s).            }
#' @param x A \link[=pop_vec]{populated vector} to extract elements from.
#' @param n A \link[=cmp_psw_scl]{complete positive whole-number scalar}.
#' @return A scalar, vector, or \link[=VLS]{vlist}.
#' @examples
#' Nth_from_last(letters, 5)
#' Nth_plus(letters, 5)
#' firstN(letters, 5)
#' lastN(letters, 5)
#' Nth(letters, 5:7)
#' @export
Nth <- function(x, N) {
  uj:::.Nth_errs(x, N, F, uj::callers())
  x[N]
}

#' @rdname Nth
#' @export
firstN <- function(x, N) {
  uj:::.Nth_errs(x, N, T, uj::callers())
  x[1:N]
}

#' @rdname Nth
#' @export
lastN <- function(x, N) {
  uj:::.Nth_errs(x, N, T, uj::callers())
  N <- 1:N
  x <- base::rev(x[base::rev(N)])
}

#' @rdname Nth
#' @export
Nth_from_last <- function(x, N) {
  uj:::.Nth_errs(x, N, F, uj::callers())
  base::rev(x)[N]
}

#' @rdname Nth
#' @export
Nth_plus <- function(x, N) {
  uj:::.Nth_errs(x, N, F, uj::callers())
  x[N:base::length(x)]
}
