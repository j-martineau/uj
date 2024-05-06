#' @encoding UTF-8
#' @family strings
#' @family chars
#' @title Reverse strings
#' @description Reverses the order of characters in each element of a non-empty \link[=atm_chr]{atomic character object}, \link[=chr_vls]{character vlist} or \link[=chr_dtf]{character data.frame}.
#' @param x A \link[=chr_vec]{character vec}.
#' @param na `TRUE` or `FALSE` indicating whether `NA` values are acceptable.
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
revstr <- function(x, na = FALSE) {
  frev <- function(xx) {base::paste0(base::rev(uj::av(base::strsplit(xx, "", fixed = T))))}
  frevs <- function(x) {base::sapply(x, frev)}
  okX <- uj::f0(uj::.chr_vec(x), T, uj::f0(uj::.chr_arr(x), T, uj::f0(base::length(x) == 0, F, uj::f0(uj::.chr_vls(x), T, uj::.chr_dtf(x)))))
  okNAS <- uj::.cmp_lgl_scl(na)
  okNAX <- uj::f0(!okNAS, T, uj::f0(na, T, !base::any(base::isna(uj::av(x)))))
  errs <- NULL
  if (!okX  ) {base::c(errs, "[x] must be an atomic character object (?atm_chr), character vlist (?chr_vls), or character data.frame (?chr_dtf).")}
  if (!okNAS) {base::c(errs, "[na] must be a TRUE or FALSE.")}
  if (!okNAX) {base::c(errs, "[na = FALSE] but [x] contains one or more NA values.")}
  if (!base::is.null(errs)) {uj::stoppers(errs, pkg = "uj")}
  nDim <- base::length(base::dim(x))
  uj::f0(base::is.data.frame(x), base::apply(x, 2, frevs), uj::f0(base::is.list(x), base::lapply(x, frevs), uj::f0(nDim > 0, base::apply(x, 1:nDim, frevs), frevs(x))))
}
