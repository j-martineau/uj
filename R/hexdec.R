#' @name hexdec
#' @title Convert non-negative whole numbers between decimal and hexadecimal
#' @description \tabular{rl}{
#'       `todec`   \tab Converts hexadecimal \link[=cmp_chr]{complete character object} `x` from hexadecimal to decimal. `x` may be formatted as either `'hhh...'` or `'#hhh...'` where `h` is a placeholder for a hexidecimal digit in either upper or lower case.
#'   \cr `tohex`   \tab Converts \link[=cmp_psw]{complete positive whole-number} `x` to hexadecimal formatted as `'#HHH...'` where `H` is a placeholder for an uppercase hexadecimal digit.
#' }
#' @param x A \link[=cmp_nnw]{complete non-negative whole-number object} or a \link[=cmp_chr]{complete character vec} containing only non-negative whole-number hexademical values.
#' @return \tabular{rl}{
#'       `todec`   \tab A non-negative whole-number object.
#'   \cr `tohex`   \tab A character object of non-negative whole-number hexadecimal values.
#' }
#' @examples
#' dec. <- sample(1:100, 5)
#' hex. <- c("10", "#B25", "2bc4a", "#FF")
#'
#' dec.
#' tohex(dec.)
#'
#' hex.
#' todec(hex.)
#' @export
todec <- function(x) {
  ok.x <- F
  ok.emp <- F
  ok.pref <- T
  if (cmp_chr(x)) {
    valid <- c("#", 0:9, letters[1:6], LETTERS[1:6])
    ch <- ch(av(x))
    ok.x <- all(ch %in% valid)
    if (ok.x) {
      pp <- av(gregexpr("#", x, fixed = T))
      ok.pref <- !any(pp > 1)
      x  <- gsub("#", "", x, fixed = T)
      ok.emp <- !any(nchar(x) == 0)
  }}
  errs <- c(f0(ok.x   , NULL, "[x] must be of mode character, have at least one value, contain no NA values, and contain only the characters in '#0123456789ABCDEFabcdef'."),
            f0(ok.emp , NULL, "After removing pound signs ('#'), one or more elements of [x] is a blank string."),
            f0(ok.pref, NULL, "If pound signs ('#') are used, they must be the first character of any element of [x] in which they are used."))
  if (!is.null(errs)) {stop(.errs(errs))}
  conv <- function(xx) {                                                         # sub-function to convert a single hex value to decimal.
    cc <- strsplit(xx, "", T)[[1]]                                               # split the string into characters
    nn <- length(xx)                                                             # number of hex digits
    xx[xx %in% c("a", "A")] <- "10"                                              # convert alpha hex digits to decimal in character form
    xx[xx %in% c("b", "B")] <- "11"
    xx[xx %in% c("c", "C")] <- "12"
    xx[xx %in% c("d", "D")] <- "13"
    xx[xx %in% c("e", "E")] <- "14"
    xx[xx %in% c("f", "F")] <- "15"
    xx <- as.integer(xx)                                                         # convert digits to integer
    pv <- rev(16 ^ (0:(nn - 1)))                                                 # get the hex place value of each position
    yy <- xx * pv                                                                # multiply digits by their place values
    sum(yy)                                                                      # sum the result
  }
  x <- as.list(x)
  sapply(lapply(x, ch), conv)
}

#' @rdname hexdec
#' @export
tohex <- function(x) {
  if (!cmp_nnw_vec(x)) {stop(.errs("[x] must be complete, non-negative whole-number vec (?cmp_nnw_vec)."))}
  paste0("#", toupper(as.character.hexmode(x)))
}
