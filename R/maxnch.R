#' @family chars
#' @family strings
#' @title Length of longest string
#' @description Gets the number of characters in the longest string in `x`.
#' @param x An \link[=ichr]{character object}.
#' @return An integer scalar.
#' @export
max_nch <- function(x) {
  if (!ichr(x)) {stop("\n \u2022 [x] must be a character object (?ichr).")}
  max(nch(x))
}
