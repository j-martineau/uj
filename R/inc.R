#' @name inc.
#' @family math
#' @title Increment or Decrement a Variable
#' @param x Any atomic numeric object.
#' @param i A numeric scalar giving the value by which to increment.
#' @param d A numeric scalar giving the value by which to decrement.
#' @param na A logical scalar indicating whether to throw an error if any values
#'   of \code{x} are \code{NA}.
#' @return \code{x} or \code{list(...)}, but with each element incremented or
#'   decremented.
#' @export
inc. <- function() {help("inc.", package = "uj")}

#' @describeIn inc. Increments \code{x} by \code{i}.
#' @export
inc <- function(x, i = 1, na = F) {
  vx <- f0(inum(x), T, f0(num_dtf(x), T, num_vls(x)))
  vi <- cmp_num_scl(i)
  vn <- isTF(na)
  vv <- f0(!vx | !isF(na), T, !any(is.na(av(x))))
  err <- NULL
  if (!vx) {err <- c(err, "\n • [x] must be populated numeric object (?pop_num), a numeric dtf (?num_dtf), or a numeric vlist (?num_vls).")}
  if (!vi) {err <- c(err, "\n • [i] must be a complete numeric scalar (?cmp_num_scl).")}
  if (!vn) {err <- c(err, "\n • [na] must be TRUE or FALSE.")}
  if (!vv) {err <- c(err, "\n • [x] contains NA values but [na = FALSE].")}
  if (idef(err)) {stop(err)}
  x + i
}

#' @describeIn inc. Decrements \code{x} by \code{d}.
#' @export
dec <- function(x, d = 1, na = F) {
  vx <- f0(inum(x), T, f0(num_dtf(x), T, num_vls(x)))
  vd <- cmp_num_scl(d)
  vn <- isTF(na)
  vv <- f0(!vx | !isF(na), T, !any(is.na(av(x))))
  err <- NULL
  if (!vx) {err <- c(err, "\n • [x] must be populated numeric object (?pop_num), a numeric df (?num_dtf), or a numeric vlist (?num_vls).")}
  if (!vd) {err <- c(err, "\n • [d] must be a complete numeric scalar (?cmp_num_scl).")}
  if (!vn) {err <- c(err, "\n • [na] must be TRUE or FALSE.")}
  if (!vv) {err <- c(err, "\n • [x] contains NA values but [na = FALSE].")}
  if (idef(err)) {stop(err)}
  x - d
}
