#' @name inc
#' @family math
#' @title Increment or Decrement a Variable
#' @param x Any atomic numeric object.
#' @param i A numeric scalar giving the value by which to increment.
#' @param na A logical scalar indicating whether to throw an error if any values
#'   of \code{x} are \code{NA}.
#' @return \code{x} or \code{list(...)}, but with each element incremented or
#'   decremented.
#' @rdname increment
#' @export
inc <- function(x, i = 1, na = F) {
  VX <- f0(xnum(x), T, f0(num_atb(x), T, num_avl(x)))
  VI <- cmp_num_scl(i)
  VN <- isTF(na)
  VV <- f0(!VX | !isF(na), T, !any(is.na(av(x))))
  E <- NULL
  if (!VX) {E <- c(E, "\n  * [x] must be populated numeric object, a numeric tibble, or a numeric vlist.")}
  if (!VI) {E <- c(E, "\n  * [i] must be a non-NA numeric scalar.")}
  if (!VN) {E <- c(E, "\n  * [na] must be TRUE or FALSE.")}
  if (!VV) {E <- c(E, "\n  * [x] contains NA values but [na = FALSE].")}
  if (xdef(E)) {stop(E)}
  x + i
}
