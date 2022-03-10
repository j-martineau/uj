#' @name swap
#' @family meta
#' @title Swap atomic values.
#' @description Swaps atomic values.
#' @param x An atomic object.
#' @param old Atomic vect of unique values to be replaced.
#' @param new Atomic vect of replacement values. \code{length(new)} must be in
#'   \code{c(1, length(old))}.
#' @param all \code{TRUE} or \code{FALSE} indicating whether all values of
#'   \code{x} must be contained in \code{old}.
#' @return An atomic object of the same dimensions as \code{x}.
#' @export
swap <- function(x, old, new, all = F) {
  VX <- pop_atm(x)
  VO <- xvec(old)
  VN <- xvec(new)
  VA <- isTF(all)
  E <- NULL
  if (!VX) {E <- c(E, "\n  * [x] must be populated and atomic.")}
  if (!VO) {E <- c(E, "\n  * [old] must be populated and atomic.")}
  if (!VN) {E <- c(E, "\n  * [new] must be populated and atomic.")}
  if (!VA) {E <- c(E, "\n  * [all] must be TRUE or FALSE")}
  if (xdef(E)) {stop(E)}
  VC <- compatible(x, old, new)
  VO <- is_unique(old)
  VN <- length(new) %in% c(1, length(old))
  VA <- f0(all, allIN(x, old), T)
  if (!VC) {E <- c(E, "\n  * [x], [old], and [new] are of incompatible modes.")}
  if (!VO) {E <- c(E, "\n  * [old] must contain only unique elements.")}
  if (!VN) {E <- c(E, "\n  * [length(new)] must be in [c(1, length(old))].")}
  if (!VA) {E <- c(E, "\n  * [all = T] but not all elements of [x] are contained in [old].")}
  if (xdef(E)) {stop(E)}
  if (length(new) == 1) {new <- rep.int(new, length(old))}
  if (any(is.na(old))) {x[is.na(x)] <- new[which(is.na(old))]}
  for (i in 1:length(old)) {x[x == old[i]] <- new[i]}
  x
}
