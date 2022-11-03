#' @title Whether a string has a substring.
#' @param x Character scalar or vector of strings to search.
#' @param pat Character scalar string to search for.
#' @return Logical scalar or vector.
#' @export
haspat <- function(x, pat) {nch(x) != nch(gsub(x, pat, v(blank), fixed = T))}
