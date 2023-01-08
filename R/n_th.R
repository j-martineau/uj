.n_th_errs <- function(x, n, n.scl) {
  ok.n <- uj::f0(n.scl, uj::cmp_psw_scl(n), uj::cmp_psw_vec(n))
  errs <- base::c(uj::f0(uj::pop_vec(x)         , NULL, "[x] is not a populated vector (?pop_vec)."),
                  uj::f0(uj::f0(!n.scl, T, ok.n), NULL, "[n] must be a complete positive whole-number scalar (?cmp_psw_scl)."),
                  uj::f0(uj::f0( n.scl, T, ok.n), NULL, "[n] must be a complete positive whole-number vector (?cmp_psw_vec)."))
  if (uj::idef(errs)) {uj:::.errs(errs)}
  else if (base::any(n > base::length(x))) {stop(uj:::.errs(uj::p0(uj::f0(n.scl, "", "The largest value in")," [n] is greater than the number of elements in [x].")))}
  else {NULL}
}

#' @encoding UTF-8
#' @family to_std_fun_form
#' @family values
#' @title Extract elements by numeric position
#' @description \tabular{rl}{
#'     `n_th_last`   \tab Gets the `n`-th-from-last element(s).
#'   \cr `first_n`   \tab Gets the first `n` elements.
#'   \cr  `last_n`   \tab Gets the last `n` elements.
#'   \cr    `n_th`   \tab Gets the `n`-th element(s) .
#' }
#' @param x A \link[=pop_vec]{populated vector} to extract elements from.
#' @param n A \link[=cmp_psw_scl]{complete positive whole-number scalar}.
#' @return A scalar, vector, or \link[=ivls]{vlist}.
#' @examples
#' n_th_last(letters, 5)
#' first_n(letters, 5)
#' last_n(letters, 5)
#' n_th(letters, 5:7)
#' @export
n_th <- function(x, n) {
  errs <- uj:::.n_th_errs(x, n, F)
  if (!base::is.null(errs)) {stop(errs)}
  x[n]
}

#' @rdname n_th
#' @export
n_th_last <- function(x, n) {
  errs <- uj:::.n_th_errs(x, n, F)
  if (!base::is.null(errs)) {stop(errs)}
  x[base::length(x) - n + 1]
}

#' @rdname n_th
#' @export
first_n <- function(x, n) {
  errs <- uj:::.n_th_errs(x, n, T)
  if (!base::is.null(errs)) {stop(errs)}
  x[1:n]
}

#' @rdname n_th
#' @export
last_n <- function(x, n) {
  errs <- uj:::.n_th_errs(x, n, T)
  if (!base::is.null(errs)) {stop(errs)}
  n <- 1:n
  base::rev(x[1 + base::length(x) - n])
}
