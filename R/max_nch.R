#' @family chars
#' @family strings
#' @title Length of longest string
#' @description Gets the number of characters in the longest string in `x`.
#' @param ... An arbitrary number of objects of mode character.
#' @return An integer scalar.
#' @examples
#' max_nch(letters)
#' max_nch("a", "collection", "of", "words")
#' @export
max_nch <- function(...) {
  x <- av(...)
  x <- x[!is.na(x)]
  if (length(x) == 0) {stop(.errs("[...] does not contain any non-NA values."))}
  if (!ichr(x))       {stop(.errs("[...] does not contain any character objects."))}
  max(nchar(x))
}
