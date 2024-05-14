#' @encoding UTF-8
#' @family strings
#' @family chars
#' @title Reverse strings
#' @description Reverses the order of characters in each element of a non-empty \link[=atm_chr]{atomic character object}, \link[=chr_vls]{character vlist} or \link[=chr_dtf]{character data.frame}.
#' @param x A \link[=chr_vec]{character vec}.
#' @param na `TRUE` or `FALSE` indicating whether `NA` values are acceptable.
#' @return An object of the same structure and dimension as `x`.
#' @examples
#' egRevStr <- function() {
#'   egBanana <- "banana"
#'   egApple  <- "apple"
#'   egCarrot <- "torrac"
#'   egZero   <- "orez"
#'   egFront  <- base::c(egApple, egBanana)
#'   egBack   <- base::c(egZero , egCarrot)
#'   egMat22  <- base::matrix(base::c(egFront, egBack), nrow = 2)
#'   egVls13  <- base::list(front = egFront, back = egBack)
#'   egDtf22  <- base::data.frame(front = egFront, back = egBack)
#'   base::list(
#'     egBanana = egBanana, revBanana = uj::revstr(egBanana),
#'     egCarrot = egCarrot, revCarrot = uj::revstr(egCarrot),
#'     egApple  = egApple , revApple  = uj::revstr(egApple ),
#'     egZero   = egZero  , revZero   = uj::revstr(egZero  ),
#'     egFront  = egFront , revFront  = uj::revstr(egFront ),
#'     egBack   = egBack  , revBack   = uj::revstr(egBack  ),
#'     egMat22  = egMat22 , revMat22  = uj::revstr(egMat22 ),
#'     egVls13  = egVls13 , revVls13  = uj::revstr(egVls13 ),
#'     egDtf22  = egDtf22 , revDtf22  = uj::revstr(egDtf22 ))
#' }
#' egRevStr <- egRevStr()
#' egRevStr
#' @export
revstr <- function(x) {
  frev <- function(z) {uj::av(base::paste0(base::rev(uj::av(base::strsplit(z, "", fixed = T))), collapse = ""))}
  frevs <- function(y) {uj::av(base::sapply(y, frev))}
  okX <- uj::f0(uj::.cmp_chr(x)     , T,
         uj::f0(base::length(x) == 0, F,
         uj::f0(uj::.cmp_chr_vls(x) , T, uj::.cmp_chr_dtf(x))))
  if (!okX) {uj::stopperr("[x] must be a complete atomic character object (?cmp_atm_chr), complete character vlist (?cmp_chr_vls), or complete character data.frame (?cmp_chr_dtf).")}
  ndim <- base::length(base::dim(x))
  if (ndim < 2) {
    if (base::is.list(x)) {base::lapply(x, frevs)}
    else {base::sapply(x, frev)}
  } else {base::apply(x, 1:ndim, frevs, simplify = F)}
}
