#' @name inc
#' @family math
#' @title Increment or Decrement a Variable
#' @param x Any atomic numeric object.
#' @param i A numeric scalar giving the value by which to increment.
#' @param na A logical scalar indicating whether to throw an error if any values
#'   of \code{x} are \code{NA}.
#' @return \code{x} or \code{list(...)}, but with each element incremented or
#'   decremented.
#' @export
inc <- function(x, i = 1, na = F) {
  vx <- f0(inum(x), T, f0(num_atb(x), T, num_avl(x)))
  vi <- cmp_num_scl(i)
  vn <- isTF(na)
  vv <- f0(!vx | !isF(na), T, !any(is.na(av(x))))
  err <- NULL
  if (!vx) {err <- c(err, "\n • [x] must be populated numeric object, a numeric tibble, or a numeric vlist.")}
  if (!vi) {err <- c(err, "\n • [i] must be a non-NA numeric scalar.")}
  if (!vn) {err <- c(err, "\n • [na] must be TRUE or FALSE.")}
  if (!vv) {err <- c(err, "\n • [x] contains NA values but [na = FALSE].")}
  if (idef(err)) {stop(err)}
  x + i
}
