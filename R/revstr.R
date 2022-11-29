#' @name revstr
#' @family strings
#' @title Reverse strings
#' @description Reverse the order of characters in each element of a character vector.
#' @param x A \link[=cmp_chr_vec]{complete character vec}.
#' @return A character vector.
#' @export
revstr <- function(x) {
  if (!cmp_chr_vec(x)) {stop("\n \u2022 [x] must be a complete character vec.")}
  vecply(x, function(xx) {paste0(rev(av(strsplit(xx, "", fixed = T))))})
}
