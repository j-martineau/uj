#' @name n_th
#' @title First \code{n}, last \code{n}, \code{n}th of, or \code{n}-th from
#'   last.
#' @description Get the \code{n}-th element(s) of a vector (from the front).
#' @param x Vector to extract elements from
#' @param n Positive integer vector of indexing values to extract.
#' @return Subset of \code{x}
#' @examples
#' first_n(letters, 5)
#' last_n(letters, 5)
#' n_th(letters, 5)
#' n_th_rev(letters, )
#' @export
n_th <- function(x, n) {x[n]}

#' @describeIn n_th Get the first \code{n} elements of a vector.
#' @export
first_n <- function(x, n) {
  bank_funs()
  x[1:n]
}

#' @describeIn n_th Get the last \code{n} elements of a vector.
#' @export
last_n <- function(x, n) {n <- 1:n; x[1 + length(x) - n]}

#' @describeIn n_th Get the \code{n}-th from last element(s).
#' @export
n_th_last <- function(x, n) {x[length(x) - n + 1]}
