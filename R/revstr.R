#' @encoding UTF-8
#' @family strings
#' @family chars
#' @title Reverse strings
#' @description Reverses the order of characters in each element of a non-empty \link[=atm_chr]{atomic character object}, \link[=chr_vls]{character vlist} or \link[=chr_dtf]{character data.frame}.
#' @param x A \link[=chr_vec]{character vec}.
#' @param nas `TRUE` or `FALSE` indicating whether `NA` values are acceptable.
#' @return An object of the same structure and dimension as `x`.
#' @examples
#' egBanana <- c(banana = "banana")
#' egCarrot <- c(carrot = "torrac")
#' egApple <- c(apple = "apple")
#' eg0 <- c(zero = "orez")
#'
#' egFront <- c(egApple, egBanana)
#' egBack <- c(eg0, egCarrot)
#'
#' egMat22 <- matrix(c(egFront, egBack.), nrow = 2)
#' egVls13 <- list(scl = av(egApple), vec = av(egBanana, egBack))
#' egDtf22 <- data.frame(front = av(egFront), back = av(egBack))
#' egDtfNA <- data.frame(front = av(NA, egFront), back = av(egBack, NA))
#'
#' egBack
#' revstr(egBack)
#'
#' egFront
#' revstr(egFront)
#'
#' egMat22
#' revstr(egMat22)
#'
#' egVLS13
#' revstr(egVLS13)
#'
#' egDtf22
#' revstr(egDtf22)
#'
#' egDtfNA
#' revstr(egDtfNA, na = TRUE)
#' @export
revstr <- function(x, nas = FALSE) {
  .rev <- function(xxx) {uj::g0(base::rev(uj::av(base::strsplit(xxx, "", fixed = T))))}
  .revs <- function(xx) {base::sapply(xx, .rev)}
  ok.x <- uj::f0(uj::chr_vec(x), T, uj::f0(uj::chr_arr(x), T, uj::f0(uj::N0(x), F, uj::f0(uj::chr_vls(x), T, uj::chr_dtf(x)))))
  ok.na <- uj::isTF1(nas)
  ok.nax <- uj::f0(!ok.na, T, uj::f0(nas, T, uj::noneNA(uj::av(x))))
  uj::errs_if_nots(ok.x  , "[x] must be an atomic character object (?atm_chr), character vlist (?chr_vls), or character data.frame (?chr_dtf).",
                 ok.na , "[nas] must be a TRUE or FALSE."                                                                                 ,
                 ok.nax, "[nas = FALSE] but [x] contains one or more NA values."                                                          , PKG = 'uj')
  nd <- uj::NDIM(x)
  uj::f0(uj::isDTF(x), base::apply(x, 2, .revs), uj::f0(uj::isLST(x), base::lapply(x, .revs), uj::f0(nd > 0, base::apply(x, 1:nd, .revs), .revs(x))))
}
