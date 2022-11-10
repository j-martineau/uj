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
n_th <- function(x, n) {x[n]}

#' @describeIn n_th. Get the first \code{n} elements of a vector.
#' @export
first_n <- function(x, n) {
  bank_funs()
  x[1:n]
}

#' @describeIn n_th. Get the last \code{n} elements of a vector.
#' @export
last_n <- function(x, n) {n <- 1:n; x[1 + length(x) - n]}

#' @describeIn n_th. Get the \code{n}-th from last element(s).
#' @export
n_th_last <- function(x, n) {x[length(x) - n + 1]}
