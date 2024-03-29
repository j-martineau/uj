#' @encoding UTF-8
#' @family strings
#' @family chars
#' @title String spacing functions
#' @description These functions create strings of spaces, pad strings with space, trim edge spaces, and squeeze out unnecessary spaces.
#' @details
#' \tabular{ll}{  `spaces`   \tab Creates a character scalar of `n` spaces.                                                                            \cr   \tab   \cr
#'                `pad`      \tab Pads strings with leading, trailing white space using \code{\link[stringr:str_pad]{stringr::str_pad}}.               \cr   \tab   \cr
#'                `sqz`      \tab Trims leading, trailing, and extra internal white space using \code{\link[stringr:str_squish]{stringr::str_squish}}. \cr   \tab   \cr
#'                `trm`      \tab Trims leading and trailing white space using \code{\link[stringr:str_trim]{stringr::str_trim}}.                                     }
#' @param ... An arbitrary number of atomic arguments to be processed.
#' @param n A \link[=cmp_psw_scl]{complete positive whole-number scalar} indicating the number of spaces or the number of pad characters.
#' @param s A \link[=cmp_ch1_vec]{complete onechar scalar} indicating side(s) to pad: `'l'`, `'r'`, or `'b'` for left, right, or both, respectively.
#' @param p A complete onechar scalar containing a single character used to pad strings.
#' @return **A character scalar** \cr\cr   `spaces`
#' \cr\cr  **A character object** \cr\cr   `pad, trm, sqz`
#' @examples
#' spaces(0)
#' spaces(4)
#' pad(0:9, n = 3)
#' pad(0:9, n = 3, s = "r")
#' pad(0:9, n = 3, s = "l")
#' pad(0:9, n = 3, s = "b")
#' pad(0:9, n = 3, s = "b", p = "*")
#' trm(c("a ", "b", "c", " ", "d  ", ""))
#' sqz(c("a ", "b", "c", " ", "d  ", ""))
#' sqz(trm(c("a ", "b", "c", " ", "d  ", "")))
#' @export
spaces <- function(n) {
  if (!uj:::.cmp_nnw_scl(n)) {uj::stopperr("[n] must be a complete positive whole-number scalar (?cmp_psw_scl).", .PKG = "uj")}
  base::paste0(base::rep.int(" ", n), collapse = "")
}

#' @rdname spaces
#' @export
pad <- function(..., n = 0, s = "r", p = " ") {
  OkND <- base::...length() > 0
  if (OkND) {
    Dots <- base::list(...)
    OkAtm <- base::all(base::sapply(Dots, uj:::.ATM))
    OkCmp <- base::all(base::sapply(Dots, uj:::.CMP))
  } else {OkAtm <- OkCmp <- T}
  if (uj:::.cmp_ch1_vec(s)) {
    if (base::is.na(s)) {OkS <- F}
    else {OkS <- s %in% base::c("l", "r", "b")}
  } else {OkS <- F}
  Errors <- NULL
  if (!OkS) {Errors <- base::c(Errors, "[s] must be a complete onechar scalar (?cmp_ch1_vec) in c('r', 'l', 'b').")}
  if (!OkND) {Errors <- base::c(Errors, "[...] must contain at least one argument.")}
  if (!OkAtm | !OkCmp) {Errors <- base::c(Errors, "[...] arguments must be complete and atomic (?cmp_atm).")}
  if (!uj:::.cmp_nnw_scl(n)) {Errors <- base::c(Errors, "[n] must be a complete non-negative whole-number scalar (?cmp_nnw_scl).")}
  if (!uj:::.cmp_ch1_vec(p)) {Errors <- base::c(Errors, "[p] must be a complete onechar scalar (?cmp_ch1_vec).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  base::sapply(uj::av(...), stringr::str_pad, width = n, side = s, pad = p)
}

#' @rdname spaces
#' @export
sqz <- function(...) {
  if (!base::all(base::sapply(base::list(...), uj:::.cmp_chr))) {uj::stopperr("[...] must contain at least one argument, and all must be complete character objects (?cmp_chr).", .PKG = "uj")}
  stringr::str_squish(uj::av(...))
}

#' @rdname spaces
#' @export
trm <- function(...) {
  if (!base::all(base::sapply(base::list(...), uj:::.cmp_chr))) {uj::stopperr("[...] must contain at least one argument, and all must be complete character objects (?cmp_chr)", .PKG = "uj")}
  stringr::str_trim(uj::av(...))
}
