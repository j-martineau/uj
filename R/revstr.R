#' @name rev_str.
#' @family strings
#' @title Reverse strings
#' @description Reverse the order of characters in each element of a character
#'   vector.
#' @param x A \link[=cmp_chr_vec]{complete character vec}.
#' @return Character vector.
#' @export
rev_str. <- function() {help("rev_str.", package = "uj")}

#' @rdname rev_str.
#' @export
rev_str <- function(x) {
  if (!cmp_chr_vec(x)) {stop("\n \u2022 [x] must be a complete character vec.")}
  vecply(x, function(xx) {paste0(rev(av(strsplit(xx, "", fixed = T))))})
}
