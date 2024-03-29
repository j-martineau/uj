#' @encoding UTF-8
#' @family base_extensions
#' @family strings
#' @family counts
#' @family chars
#' @title Length of longest string
#' @description Gets the number of characters in the longest string in `...`.
#' @param ... An arbitrary number of objects of mode character.
#' @return An integer scalar.
#' @examples
#' max_nch(letters)
#' maxlen("a", "collection", "of", "words")
#' @export
maxnch <- function(...) {
  x <- uj::av(...)
  x <- x[!base::is.na(x)]
  if (base::length(x) == 0) {uj::stopperr("[...] does not contain any non-NA values.", .PKG = "uj")}
  if (!base::is.character(x)) {uj::stopperr("[...] does not contain any character objects.", .PKG = "uj")}
  base::max(base::nchar(x))
}

#' @rdname maxnch
#' @export
maxlen <- maxnch
