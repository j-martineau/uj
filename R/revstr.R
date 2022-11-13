#' @name rev_str.
#' @family strings
#' @title Reverse strings
#' @param x \link[cmp_chr_vec]{Complete character vec}.
#' @return Character vector.
#' @export
rev_str. <- function() {help("rev_str.", package = "uj")}

#' @describeIn rev_str. Reverse the order of characters in each element of a
#'   character vector.
#' @export
rev_str <- function(x) {
  if (!cmp_chr_vec(x)) {stop(" • [x] must be a complete character vec.")}
  rs <- function(xx) {paste0(rev(av(strsplit(xx, "", fixed = T))))}
  vecply(x, rs)
}
