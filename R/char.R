#' @name charn.
#' @title Extract a single character
#' @family strings
#' @param x \link[ichr]{Atomic character object}.
#' @param n \link[cmp_psw_scl]{Complete positive whole-number scalar} or
#'   \link{cmp_psw}{complete positive whole-number atomic object} of the same
#'   dimensions as \code{x}.
#' @return \code{link[cmp_ch1]{Complete onechar}} atomic object of the same
#'   dimension as \code{x}.
#' @export
charn. <- function() {help("charn.", package = "uj")}

#' @describeIn charn. Get the \code{n}-th character of each element of
#'   \code{x}.
#' @export
charn <- function(x, n) {
  err <- NULL
  if (!ichr(x)) {err <- c(err, " • [x] must be an atomic object of mode 'character' (?ichr).")}
  if (!cmp_psw(n) | !(iscl(n) & identical(x, n))) {err <- c(err, " • [n] must be a complete positive whole-number valued scalar (?cmp_psw_scl) or of the same dimension as [x].")}
  if (idef(err)) {stop(paste0(err, collapse = "\n"))}
  substr(x, n, n)
}
