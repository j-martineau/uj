#' @name rev_str.
#' @family strings
#' @title Reverse strings
#' @param x \link[chr_vec]{Character vec}.
#' @return Character vector.
#' @export
rev_str. <- function() {help("rev_str.", package = "uj")}

#' @describeIn rev_str. Reverse the order of characters in each element of a
#'   character vector.
#' @export
rev_str <- function(x) {
  bank_xxx(chr_vec, x = x)
  err_check()
  rs. <- function(x.) {daw00(rev(ch(x.)))}
  vecply(x, rs.)
}
