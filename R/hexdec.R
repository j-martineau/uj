#' @name hexdec.
#' @family math
#' @title Convert between integer and hexadecimal
#' @details Hexademical representation is of mode character and may begin with
#'   "#".
#' @param x Vector of positive whole numbers or strings with hexademical number
#'   representations.
#' @export
hexdec. <- function() {help("hexdec.", package = "uj")}

#' @describeIn hexdec. Convert from hexadecimal to decimal.
#' @export
todec <- function(x) {
  vx <- F
  if (cmp_chr(x)) {
    ch <- ch(av(x))
    if (!any(x != "")) {vx <- all(ch %in% ch("#0123456789ABCDEFabcdef"))}
    if (vx) {
      pp <- av(gregexpr("#", x, fixed = T))
      vp <- !any(pp > 1)
      x  <- gsub("#", "", x, fixed = T)
      ve <- !any(nchar(x) == 0)
    }
  }
  err <- NULL
  if (!vx) {err <- c(err, "\n • [x] must be of mode character, have at least one value, contain no NA values, and contain only the characters in '#0123456789ABCDEFabcdef'.")}
  if (!vp) {err <- c(err, "\n • If pound signs ('#') are used, they must be the first character of any element of [x] in which they are used.")}
  if (!ve) {err <- c(err, "\n • After removing pound signs ('#'), one or more elements of [x] is a blank string.")}
  if (idef(err)) {stop(err)}
  convert. <- function(v.) {                                                     # subfunction to convert a single hex value to decimal.
    ch. <- strsplit(v., "", T)[[1]]                                              # split the string into characters
    n. <- length(v.)                                                             # number of hex digits
    v.[v. %in% c("a", "A")] <- "10"                                              # convert alpha hex digits to decimal in character form
    v.[v. %in% c("b", "B")] <- "11"
    v.[v. %in% c("c", "C")] <- "12"
    v.[v. %in% c("d", "D")] <- "13"
    v.[v. %in% c("e", "E")] <- "14"
    v.[v. %in% c("f", "F")] <- "15"
    v. <- as.integer(v.)                                                         # convert digits to integer
    pv. <- rev(16 ^ (0:(n. - 1)))                                                # get the hex place value of each position
    out. <- v. * pv.                                                             # multiply digits by their place values
    out. <- sum(out.)                                                            # sum the result
    return(out.)
  }
  x <- as.list(x)
  sapply(lapply(x, ch), convert.)
}

#' @describeIn hexdec. Convert from decimal to hexadecimal.
#' @export
tohex <- function(x) {
  if (!cmp_nnw_vec(x)) {stop("\n • [x] must be complete, non-negative whole-number vec (?cmp_nnw_vec).")}
  paste0("#", toupper(as.character.hexmode(x)))
}
