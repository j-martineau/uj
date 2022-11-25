#' @title Length of Longest String
#' @description  Get the number of characters in the longest string in `x`.
#' @family strings
#' @param x An \link[=atm_chr]{atomic character object}.
#' @return An integer scalar.
#' @export
max_nch <- function(x) {
  if (!ichr(x)) {stop("\n \u2022 [x] must be an atomic character object (?ichr).")}
  max(nch(x))
}
