#' @name to_dec
#' @encoding UTF-8
#' @family conversions
#' @title Convert non-negative whole numbers between decimal and hexadecimal
#' @description \tabular{ll}{  `to_dec`   \tab Converts hexadecimal \link[=cmp_chr]{complete character object} `x` from hexadecimal to decimal. `x` may be formatted as either `'hhh...'` or `'#hhh...'` where `h` is a placeholder for a hexidecimal digit in either upper or lower case. \cr   \tab   \cr
#'                             `to_hex`   \tab Converts \link[=cmp_psw]{complete positive whole-number} `x` to hexadecimal formatted as `'#HHH...'` where `H` is a placeholder for an uppercase hexadecimal digit.                                                                                        }
#' @param x A \link[=cmp_nnw]{complete non-negative whole-number object} or a \link[=cmp_chr]{complete character vec} containing only non-negative whole-number hexademical values.
#' @return **A non-negative whole number object**                                 \cr `to_dec`
#' \cr\cr  **A character object of non-negative whole number hexadecimal values** \cr `to_hex`
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
  if (uj::cmp_chr(x)) {
    ch <- uj::ch(uj::av(x))
    valid <- base::c("#", 0:9, letters[1:6], LETTERS[1:6])
    ok.x <- uj::allIN(ch, valid)
    if (ok.x) {
      pp <- uj::av(base::gregexpr("#", x, fixed = T))
      x  <- base::gsub("#", "", x, fixed = T)
      ok.pref <- uj::none(pp > 1)
      ok.emp <- uj::none(uj::LEN(x) == 0)
  }}
  uj::errs_if_nots(ok.x   , "[x] must be of mode character, have at least one value, contain no NA values, and contain only the characters in '#0123456789ABCDEFabcdef'.",
                   ok.emp , "After removing pound signs ('#'), one or more elements of [x] is a blank string."                                                           ,
                   ok.pref, "If pound signs ('#') are used, they must be the first character of any element of [x] in which they are used."                              , PKG = "uj")
  conv <- function(xx) {                                                         # sub-function to convert a single hex value to decimal.
    cc <- base::strsplit(xx, "", T)[[1]]                                         # split the string into characters
    nn <- uj::N(xx)                                                              # number of hex digits
    xx[xx %in% c("a", "A")] <- "10"                                              # convert alpha hex digits to decimal in character form
    xx[xx %in% c("b", "B")] <- "11"
    xx[xx %in% c("c", "C")] <- "12"
    xx[xx %in% c("d", "D")] <- "13"
    xx[xx %in% c("e", "E")] <- "14"
    xx[xx %in% c("f", "F")] <- "15"
    xx <- uj::asINT(xx)                                                          # convert digits to integer
    pv <- base::rev(16 ^ (0:(nn - 1)))                                           # get the hex place value of each position
    yy <- xx * pv                                                                # multiply digits by their place values
    base::sum(yy)                                                                # sum the result
  }
  x <- base::as.list(x)
  base::sapply(base::lapply(x, uj::ch), conv)
}

#' @rdname to_dec
#' @export
to_hex <- function(x) {
  uj::err_if_not(uj::cmp_nnw_vec(x), "[x] must be complete, non-negative whole-number vec (?cmp_nnw_vec).", PKG = "uj")
  uj::p0("#", base::toupper(base::as.character.hexmode(x)))
}
