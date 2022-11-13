#' @name has_pat.
#' @family strings
#' @title Whether a string has (contains) a substring
#' @param x \link[chr_vec]{Character vec} of strings to search.
#' @param pat \link[cmp_chr_scl]{Complete character scalar} string to search
#'   for.
#' @return Logical scalar or vector.
#' @export
has_pat. <- function() {help("has_pat.", package = "uj")}

#' @describeIn has_pat. Evaluate whether each element of \code{x} contains the
#'   string in \code{pat}.
#' @export
has_pat <- function(x, pat) {
  err <- f0(cmp_chr_vec(x), NULL, " • [x] must be a complete character vec (?cmp_chr_vec).")
  if (!cmp_chr_scl(x)) {err <- c(err, " • [pat] must be a complete character scalar (?cmp_chr_vec).")}
  nchar(x) != nchar(gsub(x, pat, "", fixed = T))
}
