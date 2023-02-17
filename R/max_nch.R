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
#' maxlen("a", "collection", "of", "words")
#' @export
max_nch <- function(...) {
  x <- uj::av(...)
  x <- x[uj::ok(x)]
  uj::err_if(uj::N0(x), "[...] does not contain any non-NA values.", PKG = "uj")
  uj::err_if_not(uj::isCHR(x), "[...] does not contain any character objects.", PKG = "uj")
  base::max(uj::LEN(x))
}

#' @rdname max_nch
#' @export
maxlen <- max_nch
