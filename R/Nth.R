#' @encoding UTF-8
#' @family to_std_fun_form
#' @family values
#' @title Extract elements by numeric position
#' @description Extract the `N`-th, `N`-th from last, first `N`, and last `N` elements of a vector of list.
#' \tabular{ll}{  `Nth_from_last`   \tab Gets the `N`th from last element(s). \cr
#'                `Nth_plus`        \tab Gets element `N` and onwards.        \cr
#'                `firstN`          \tab Gets the first `N` elements.         \cr
#'                `lastN`           \tab Gets the last `N` elements.          \cr
#'                `Nth`             \tab Gets the `N`-th element(s).            }
#' @param X A \link[=pop_vec]{populated vector} to extract elements from.
#' @param N A \link[=cmp_psw_scl]{complete positive whole-number scalar}.
#' @return A scalar, vector, or \link[=VLS]{vlist}.
#' @examples
#' Nth_from_last(letters, 5)
#' Nth_plus(letters, 5)
#' firstN(letters, 5)
#' lastN(letters, 5)
#' Nth(letters, 5:7)
#' @export
Nth <- function(X, N) {
  uj:::.Nth_errs(X, N, F, uj::callers())
  X[N]
}

#' @rdname Nth
#' @export
firstN <- function(X, N) {
  uj:::.Nth_errs(X, N, T, uj::callers())
  X[1:N]
}

#' @rdname Nth
#' @export
lastN <- function(X, N) {
  uj:::.Nth_errs(X, N, T, uj::callers())
  N <- 1:N
  X <- base::rev(X[base::rev(N)])
}

#' @rdname Nth
#' @export
Nth_from_last <- function(X, N) {
  uj:::.Nth_errs(X, N, F, uj::callers())
  base::rev(X)[N]
}

#' @rdname Nth
#' @export
Nth_plus <- function(X, N) {
  uj:::.Nth_errs(X, N, F, uj::callers())
  X[N:base::length(X)]
}
