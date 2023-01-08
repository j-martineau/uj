#' @encoding UTF-8
#' @family strings
#' @family chars
#' @title Reverse strings
#' @description Reverses the order of characters in each element of a non-empty \link[=pop_chr]{atomic character object}, \link[=chr_vls]{character vlist} or \link[=chr_dtf]{character data.frame}.
#' @param x A \link[=cmp_chr_vec]{character vec}.
#' @param na A non-`NA` logical scalar indicating whether `NA` values are acceptable.
#' @return An object of the same structure and dimension as `x`.
#' @examples
#' zero. <- c(zero = "orez")
#' apple. <- c(apple = "apple")
#' banana. <- c(banana = "banana")
#' carrot. <- c(carrot = "torrac")
#' back. <- c(zero., carrot.)
#' front. <- c(apple., banana.)
#' mat22. <- matrix(c(front., back.), nrow = 2)
#' vls13. <- list(scl = av(apple.), vec = av(banana., back.))
#' dtf22. <- data.frame(front = av(front.), back = av(back.))
#' dtfNA. <- data.frame(front = av(NA, front.), back = av(back., NA))
#'
#' back.
#' revstr(back.)
#'
#' front.
#' revstr(front.)
#'
#' mat22.
#' revstr(mat22.)
#'
#' vls13.
#' revstr(vls13.)
#'
#' dtf22.
#' revstr(dtf22.)
#'
#' dtfNA.
#' revstr(dtfNA., na = TRUE)
#' @export
revstr <- function(x, na = FALSE) {
  .rev <- function(xxx) {base::paste0(base::rev(uj::av(base::strsplit(xxx, "", fixed = T))), collapse = "")}
  .revs <- function(xx) {base::sapply(xx, .rev)}
  ok.x <- uj::f0(uj::chr_vec(x), T, uj::f0(uj::chr_arr(x), T, uj::f0(base::length(x) == 0, F, uj::f0(uj::chr_vls(x), T, uj::chr_dtf(x)))))
  ok.na <- uj::isTF(na)
  ok.nax <- uj::f0(!ok.na, T, uj::f0(na, T, !base::any(base::is.na(uj::av(x)))))
  errs <- c(uj::f0(ok.x  , NULL, "[x] must be an atomic character object, character vlist (?chr_vls), or character data.frame (?chr_dtf)."),
            uj::f0(ok.na , NULL, "[na] must be a non-NA logical scalar."),
            uj::f0(ok.nax, NULL, "[na = FALSE] but [x] contains one or more NA values."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  nd <- base::length(base::dim(x))
  uj::f0(uj::idtf(x), base::apply(x, 2, .revs), uj::f0(uj::ivls(x), base::lapply(x, .revs), uj::f0(nd > 0, base::apply(x, 1:nd, .revs), .revs(x))))
}
