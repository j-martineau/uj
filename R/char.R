#' @name char_uj
#' @title Extract a single character
#' @family strings
#' @param x Atomic character object.
#' @param n Positive whole-number scalar or positive whole-number atomic object
#'   of the same dimensions as \code{x}.
#' @return \code{link[=ch1]{onechar}} atomic object of the same dimension as
#'   \code{x}.
#' @export
char_uj <- function() {help("char_uj", package = "uj")}

#' @describeIn char_uj Get the \code{n}-th character of each element of
#'   \code{x}.
#' @export
char <- function(x, n) {
  err <- NULL
  if (!ichr(x)) {err <- c(err, " • [x] must be an atomic object of mode 'character'.")}
  if (!ipsw(n) | !(iscl(n) & identical(x, n))) {err <- c(err, " • [n] must be a positive whole-number valued object of extended class 'scl' or of the same dimension as [x] (see ?iscl for information on extended class 'scl'.")}
  if (idef(err)) {stop(paste0(err, collapse = "\n"))}
  substr(x, n, n)
}
