#' @name haspat_uj
#' @family strings
#' @title Whether a string has a substring.
#' @param x Character scalar or vector of strings to search.
#' @param pat Character scalar string to search for.
#' @return Logical scalar or vector.
#' @export
haspat_uj <- function() {help("haspat_uj", package = "uj")}

#' @describeIn haspat_uj Evaluate whether each element of \code{x} contains the
#'   string in \code{pat}.
#' @export
haspat <- function(x, pat) {nch(x) != nch(gsub(x, pat, v(blank), fixed = T))}
