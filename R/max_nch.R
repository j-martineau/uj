#' @encoding UTF-8
#' @family base_extensions
#' @family strings
#' @family counts
#' @family chars
#' @title Length of longest string
#' @description Gets the number of characters in the longest string in `x`.
#' @param ... An arbitrary number of objects of mode character.
#' @return An integer scalar.
#' @examples
#' max_nch(letters)
#' max_nch("a", "collection", "of", "words")
#' @export
max_nch <- function(...) {
  x <- uj::av(...)
  x <- x[!base::is.na(x)]
  if (base::length(x) == 0) {stop(uj:::.errs("[...] does not contain any non-NA values."))}
  if (!uj::ichr(x))         {stop(uj:::.errs("[...] does not contain any character objects."))}
  base::max(base::nchar(x))
}
