#' @name max_nch.
#' @title Length of longest string
#' @family strings
#' @param x \link[ichr]{Atomic character object}.
#' @return Integer scalar.
#' @export
max_nch. <- function() {help("max_nch.", package = "uj")}

#' @describeIn max_nch. Gets the number of characters in the longest string in
#'   \code{x}.
#' @export
max_nch <- function(x) {
  if (!ichr(x)) {stop("\n \u2022 [x] must be an atomic character object (?ichr).")}
  max(nch(x))
}
