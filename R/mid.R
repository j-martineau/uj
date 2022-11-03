#' @title Substring extraction by position.
#' @param x Character scalar or vector of strings to search.
#' @param f,l Positive integer scalar first and last character positions.
#' @return Character scalar or vector.
#' @export
mid <- function(x, f, l) {substr(x, f, l)}
