#' @name n_th.
#' @family extensions
#' @title Extract elements by index position
#' @param x Vector to extract elements from.
#' @param n \link[cmp_psw_scl]{Complete positive whole-number scalar}.
#' @return Subset of \code{x}
#' @examples
#' first_n(letters, 5)
#' last_n(letters, 5)
#' enth(letters, 5)
#' enth_last(letters, )
#' @export
n_th. <- function() {help("n_th.", package = "uj")}

#' @describeIn n_th. Get the \code{n}-th element(s) of a vector (from the
#'   front).
#' @export
n_th <- function(x, n) {
  errs <- c(f0(pop_vec(x)    , NULL, "\n \u2022 [x] is not a non-empty vector."),
            f0(cmp_psw_vec(n), NULL, "\n \u2022 [n] must be a non-empty positive whole-number vector."))
  if (idef(errs)) {stop(errs)}
  if (any(n > length(x))) {stop("\n \u2022 The largest value in [n] is greater than the number of elements in [x].")}
  x[n]
}

#' @describeIn n_th. Get the first \code{n} elements of a vector.
#' @export
first_n <- function(x, n) {
  errs <- c(f0(pop_vec(x)    , NULL, "\n \u2022 [x] is not a non-empty vector."),
            f0(cmp_psw_scl(n), NULL, "\n \u2022 [n] must be a non-NA positive whole-number scalar."))
  if (idef(errs)) {stop(errs)}
  if (n > length(x)) {stop("\n \u2022 [n] is greater than the number of elements in [x].")}
  x[1:n]
}

#' @describeIn n_th. Get the last \code{n} elements of a vector.
#' @export
last_n <- function(x, n) {
  errs <- c(f0(pop_vec(x)    , NULL, "\n \u2022 [x] is not a non-empty vector."),
            f0(cmp_psw_scl(n), NULL, "\n \u2022 [n] must be a non-NA positive whole-number scalar."))
  if (idef(errs)) {stop(errs)}
  if (n > length(x)) {stop("\n \u2022 [n] is greater than the number of elements in [x].")}
  n <- 1:n; x[1 + length(x) - n]
}

#' @describeIn n_th. Get the \code{n}-th from last element(s).
#' @export
n_th_last <- function(x, n) {
  errs <- c(f0(pop_vec(x)    , NULL, "\n \u2022 [x] is not a non-empty vector."),
            f0(cmp_psw_vec(n), NULL, "\n \u2022 [n] must be a non-empty positive whole-number vector."))
  if (idef(errs)) {stop(errs)}
  if (any(n > length(x))) {stop("\n \u2022 The largest value in [n] is greater than the number of elements in [x].")}
  x[length(x) - n + 1]
}
