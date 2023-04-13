#' @encoding UTF-8
#' @family strings
#' @family chars
#' @title Reverse strings
#' @description Reverses the order of characters in each element of a non-empty \link[=atm_chr]{atomic character object}, \link[=chr_vls]{character vlist} or \link[=chr_dtf]{character data.frame}.
#' @param X A \link[=chr_vec]{character vec}.
#' @param NAS `TRUE` or `FALSE` indicating whether `NA` values are acceptable.
#' @return An object of the same structure and dimension as `X`.
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
revstr <- function(X, NAS = FALSE) {
  .rev <- function(xx) {base::paste0(base::rev(uj::av(base::strsplit(xx, "", fixed = T))))}
  .revs <- function(x) {base::sapply(x, .rev)}
  OkX <- uj::f0(uj:::.chr_vec(X), T, uj::f0(uj:::.chr_arr(X), T, uj::f0(base::length(X) == 0, F, uj::f0(uj:::.chr_vls(X), T, uj:::.chr_dtf(X)))))
  OkNAS <- uj:::.cmp_lgl_scl(NAS)
  OkNAX <- uj::f0(!OkNAS, T, uj::f0(NAS, T, !base::any(base::is.na(uj::av(X)))))
  Errors <- NULL
  if (!OkX) {base::c(Errors, "[X] must be an atomic character object (?atm_chr), character vlist (?chr_vls), or character data.frame (?chr_dtf).")}
  if (!OkNAS) {base::c(Errors, "[NAS] must be a TRUE or FALSE.")}
  if (!OkNAX) {base::c(Errors, "[NAS = FALSE] but [X] contains one or more NA values.")}
  if (!base::is.null(Errors)) {uj::stoppers(Errors, PKG = "uj")}
  nDim <- base::length(base::dim(X))
  uj::f0(base::is.data.frame(X), base::apply(X, 2, .revs), uj::f0(base::is.list(X), base::lapply(X, .revs), uj::f0(nDim > 0, base::apply(X, 1:nDim, .revs), .revs(X))))
}
