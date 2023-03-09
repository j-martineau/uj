#' @name to_dec
#' @encoding UTF-8
#' @family conversions
#' @title Convert non-negative whole numbers between decimal and hexadecimal
#' @description \tabular{ll}{  `to_dec, todec`   \tab Converts hexadecimal \link[=cmp_chr]{complete character object} `x` from hexadecimal to decimal. `x` may be formatted as either `'hhh...'` or `'#hhh...'` where `h` is a placeholder for a hexidecimal digit in either upper or lower case. \cr   \tab   \cr
#'                             `to_hex, tohex`   \tab Converts \link[=cmp_psw]{complete positive whole-number} `x` to hexadecimal formatted as `'#HHH...'` where `H` is a placeholder for an uppercase hexadecimal digit.                                                                                        }
#' @param x A \link[=cmp_nnw]{complete non-negative whole-number object} or a \link[=cmp_chr]{complete character vec} containing only non-negative whole-number hexademical values.
#' @return **A non-negative whole number object**                                 \cr `to_dec, todec`
#' \cr\cr  **A character object of non-negative whole number hexadecimal values** \cr `to_hex, tohex`
#' @examples
#' egDEC <- sample(1:100, 5)
#' egHEX <- c("10", "#B25", "2bc4a", "#FF")
#' egDEC
#' egHEX
#' to_dec(egHEX)
#' to_hex(egDEC)
#' @export
to_dec <- function(x) {
  ok.x <- F
  ok.emp <- F
  ok.pref <- T
  if (uj:::.cmp_chr(x)) {
    ch <- uj::av(base::strsplit(uj::av(x), "", fixed = TRUE))
    valid <- base::c("#", 0:9, letters[1:6], LETTERS[1:6])
    ok.x <- base::all(ch %in% valid)
    if (ok.x) {
      pp <- uj::av(base::gregexpr("#", x, fixed = T))
      x  <- base::gsub("#", "", x, fixed = T)
      ok.pref <- base::length(base::which(pp > 1)) == 0
      ok.emp <- base::length(base::which(base::nchar(x) == 0)) == 0
    }}
  errs <- NULL
  if (!ok.x) {errs <- base::c(errs, "[x] must be of mode character, have at least one value, contain no NA values, and contain only the characters in '#0123456789ABCDEFabcdef'.")}
  if (!ok.emp) {errs <- base::c(errs, "After removing pound signs ('#'), one or more elements of [x] is a blank string.")}
  if (!ok.pref) {errs <- base::c(errs, "If pound signs ('#') are used, they must be the first character of any element of [x] in which they are used.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  conv <- function(xx) {
    cc <- base::strsplit(xx, "", T)[[1]]
    nn <- base::length(xx)
    xx[xx %in% base::c("a", "A")] <- "10"
    xx[xx %in% c("b", "B")] <- "11"
    xx[xx %in% c("c", "C")] <- "12"
    xx[xx %in% c("d", "D")] <- "13"
    xx[xx %in% c("e", "E")] <- "14"
    xx[xx %in% c("f", "F")] <- "15"
    xx <- base::as.integer(xx)
    pv <- base::rev(16 ^ (0:(nn - 1)))
    yy <- xx * pv
    base::sum(yy)
  }
  x <- base::as.list(x)
  base::sapply(base::lapply(x, uj::ch), conv)
}

#' @rdname to_dec
#' @export
to_hex <- function(x) {
  if (!uj:::.cmp_nnw_vec(x)) {uj::stopperr("[x] must be complete, non-negative whole-number vec (?cmp_nnw_vec).", PKG = "uj")}
  base::paste0("#", base::toupper(base::as.character.hexmode(x)))
}

#' @rdname to_dec
#' @export
todec <- to_dec

#' @rdname to_dec
#' @export
tohex <- to_hex
