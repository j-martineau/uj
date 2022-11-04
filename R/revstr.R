#' @name revstr_uj
#' @family strings
#' @title Reverse strings
#' @param x Character vector.
#' @return Character vector.
#' @export
revstr_uj <- function() {help("revstr_uj", package = "uj")}

#' @describeIn revstr_uj Reverse the order of characters in each element of a
#'   character vector.
#' @export
revstr <- function(x) {
  bank_xxx(chr_vec, x = x)
  err_check()
  rs. <- function(x.) {daw00(rev(ch(x.)))}
  mvcply(x, rs.)
}
