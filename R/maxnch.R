#' @name maxnch_uj
#' @title Length of longest string
#' @family strings
#' @param x Atomic character object.
#' @return Integer scalar.
#' @export
maxnch_uj <- function() {help("maxnch_uj", package = "uj")}

#' @describeIn maxnch_uj Gets the number of characters in the longest string in
#'   \code{x}.
#' @export
maxnch <- function(x) {
  if (!ichr(x)) {stop(" • [x] must be an atomic character object.")}
  max(nch(x))
}
