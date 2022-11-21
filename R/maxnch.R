#' @title Length of longest string
#' @description  Gets the number of characters in the longest string in
#'   \code{x}.
#' @family strings
#' @param x \link[=atm_chr]{Atomic character object}.
#' @return Integer scalar.
#' @export
max_nch <- function(x) {
  if (!ichr(x)) {stop("\n \u2022 [x] must be an atomic character object (?ichr).")}
  max(nch(x))
}
