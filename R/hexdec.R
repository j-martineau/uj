#' @name hexdec.
#' @family math
#' @title Convert between integer and hexadecimal
#' @description Hexademical representation is of mode character and may begin
#'   with "#".
#' @param x \link[psw_vec]{Positive whole number vec} or
#'   \link[chr_vec]{character vec} of hexademical number representations.
#' @export
hexdec. <- function() {help("hexdec.", package = "uj")}

#' @describeIn hexdec. Convert from hexadecimal to decimal.
#' @export
todec <- function(x) {
  ok.x <- F
  if (cmp_chr(x)) {
    ch <- ch(av(x))
    if (!any(x != "")) {ok.x <- all(ch %in% ch("#0123456789ABCDEFabcdef"))}
    if (ok.x) {
      pp <- av(gregexpr("#", x, fixed = T))
      ok.pref <- !any(pp > 1)
      x  <- gsub("#", "", x, fixed = T)
      ok.emp <- !any(nchar(x) == 0)
  }}
  errs <- c(f0(ok.x   , NULL, "\n \u2022 [x] must be of mode character, have at least one value, contain no NA values, and contain only the characters in '#0123456789ABCDEFabcdef'."),
            f0(ok.pref, NULL, "\n \u2022 If pound signs ('#') are used, they must be the first character of any element of [x] in which they are used."),
            f0(ok.emp , NULL, "\n \u2022 After removing pound signs ('#'), one or more elements of [x] is a blank string."))
  if (idef(errs)) {stop(errs)}
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
    pv <- rev(16 ^ (0:(nch - 1)))                                                # get the hex place value of each position
    yy <- xx * pv                                                                # multiply digits by their place values
    sum(yy)                                                                      # sum the result
  }
  x <- as.list(x)
  sapply(lapply(x, ch), conv)
}

#' @describeIn hexdec. Convert from decimal to hexadecimal.
#' @export
tohex <- function(x) {
  if (!cmp_nnw_vec(x)) {stop("\n \u2022 [x] must be complete, non-negative whole-number vec (?cmp_nnw_vec).")}
  paste0("#", toupper(as.character.hexmode(x)))
}
