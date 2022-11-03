#' @title Reverse the order of characters in each element of a character vector.
#' @param x Character vector.
#' @return Character vector.
#' @export
revstr <- function(x) {
  bank_xxx(chr_vec, x = x)
  err_check()
  rs. <- function(x.) {daw00(rev(ch(x.)))}
  mvcply(x, rs.)
}
