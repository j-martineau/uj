#' @encoding UTF-8
#' @family base_extensions
#' @family strings
#' @family counts
#' @family chars
#' @title Length of longest string
#' @description Gets the number of characters in the longest string in `X`.
#' @param ... An arbitrary number of objects of mode character.
#' @return An integer scalar.
#' @examples
#' max_nch(letters)
#' maxlen("a", "collection", "of", "words")
#' @export
maxnch <- function(...) {
  X <- uj::av(...)
  X <- X[!base::is.na(X)]
  if (base::length(X) == 0) {uj::stopperr("[...] does not contain any non-NA values.", PKG = "uj")}
  if (!base::is.character(X)) {uj::stopperr("[...] does not contain any character objects.", PKG = "uj")}
  base::max(base::nchar(X))
}

#' @rdname maxnch
#' @export
maxlen <- maxnch
