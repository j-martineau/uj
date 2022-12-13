#' @name revstr
#' @family chars
#' @family strings
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
  .rev <- function(xxx) {paste0(rev(av(strsplit(xxx, "", fixed = T))), collapse = "")}
  .revs <- function(xx) {sapply(xx, .rev)}
  ok.x <- f0(chr_vec(x), T, f0(chr_arr(x), T, f0(length(x) == 0, F, f0(chr_vls(x), T, chr_dtf(x)))))
  ok.na <- isTF(na)
  ok.nax <- f0(!ok.na, T, f0(na, T, !any(is.na(av(x)))))
  errs <- c(f0(ok.x  , NULL, "[x] must be an atomic character object, character vlist (?chr_vls), or character data.frame (?chr_dtf)."),
            f0(ok.na , NULL, "[na] must be a non-NA logical scalar."),
            f0(ok.nax, NULL, "[na = FALSE] but [x] contains one or more NA values."))
  if (!is.null(errs)) {stop(.errs(errs))}
  nd <- length(dim(x))
  f0(idtf(x), apply(x, 2, .revs), f0(ivls(x), lapply(x, .revs), f0(nd > 0, apply(x, 1:nd, .revs), .revs(x))))
}
