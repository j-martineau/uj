.n_th_errs <- function(x, n, n.scl) {
  ok.n <- f0(n.scl, cmp_psw_scl(n), cmp_psw_vec(n))
  errs <- c(f0(pop_vec(x)         , NULL, .errx("[x] is not a populated vector (?pop_vec).")),
            f0(f0(!n.scl, T, ok.n), NULL, .errx("[n] must be a complete positive whole-number scalar (?cmp_psw_scl).")),
            f0(f0( n.scl, T, ok.n), NULL, .errx("[n] must be a complete positive whole-number vector (?cmp_psw_vec).")))
  if (idef(errs)) {errs}
  else if (any(n > length(x))) {.errx(f0(n.scl, "", "The largest value in")," [n] is greater than the number of elements in [x].")}
  else {NULL}
}

#' @title Extract elements by numeric position
#' @description \tabular{rl}{
#'              `n_th`   \tab Gets the `n`-th element(s) .
#'   \cr   `n_th_last`   \tab Gets `n`-th-from-last element(s).
#'   \cr                 \tab  
#'   \cr     `first_n`   \tab Gets the first `n`` elements.
#'   \cr      `last_n`   \tab Gets the last `n` elements.
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
  errs <- .n_th_errs(x, n, F)
  if (!is.null(errs)) {stop(errs)}
  x[n]
}

#' @rdname n_th
#' @export
n_th_last <- function(x, n) {
  errs <- .n_th_errs(x, n, F)
  if (!is.null(errs)) {stop(errs)}
  x[length(x) - n + 1]
}

#' @rdname n_th
#' @export
first_n <- function(x, n) {
  errs <- .n_th_errs(x, n, T)
  if (!is.null(errs)) {stop(errs)}
  x[1:n]
}

#' @rdname n_th
#' @export
last_n <- function(x, n) {
  errs <- .n_th_errs(x, n, T)
  if (!is.null(errs)) {stop(errs)}
  n <- 1:n
  rev(x[1 + length(x) - n])
}
