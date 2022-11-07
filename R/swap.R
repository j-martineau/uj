#' @name swap.
#' @family extensions
#' @title Swap atomic values.
#' @param x An atomic object.
#' @param old Atomic vect of unique values to be replaced.
#' @param new Atomic vect of replacement values. \code{length(new)} must be in
#'   \code{c(1, length(old))}.
#' @param all \code{TRUE} or \code{FALSE} indicating whether all values of
#'   \code{x} must be contained in \code{old}.
#' @return An atomic object of the same dimensions as \code{x}.
#' @export
swap. <- function() {help("swap.", package = "uj")}

#' @describeIn swap. Swaps atomic values.
#' @export
swap <- function(x, old, new, all = F) {
  vx <- pop_atm(x)
  vo <- ivec(old)
  vn <- ivec(new)
  va <- isTF(all)
  err <- NULL
  if (!vx) {err <- c(err, "\n • [x] must be populated and atomic (?ipop).")}
  if (!vo) {err <- c(err, "\n • [old] must be populated and atomic (?ipop).")}
  if (!vn) {err <- c(err, "\n • [new] must be populated and atomic (?ipop).")}
  if (!va) {err <- c(err, "\n • [all] must be scalar TRUE or scalar FALSE")}
  if (idef(err)) {stop(err)}
  vc <- compatible(x, old, new)
  vo <- is_unq(old)
  vn <- length(new) %in% c(1, length(old))
  va <- f0(all, allIN(x, old), T)
  if (!vc) {err <- c(err, "\n • [x], [old], and [new] are of incompatible modes (?compatible).")}
  if (!vo) {err <- c(err, "\n • [old] must contain only unique elements.")}
  if (!vn) {err <- c(err, "\n • [length(new)] must be in [c(1, length(old))].")}
  if (!va) {err <- c(err, "\n • [all = TRUE] but not all elements of [x] are contained in [old].")}
  if (idef(err)) {stop(err)}
  if (length(new) == 1) {new <- rep.int(new, length(old))}
  if (any(is.na(old))) {x[is.na(x)] <- new[which(is.na(old))]}
  for (i in 1:length(old)) {x[x == old[i]] <- new[i]}
  x
}
