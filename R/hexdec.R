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
  conv <- function(vv) {                                                         # subfunction to convert a single hex value to decimal.
    cc <- strsplit(vv, "", T)[[1]]                                               # split the string into characters
    nn <- length(vv)                                                             # number of hex digits
    vv[vv %in% c("a", "A")] <- "10"                                              # convert alpha hex digits to decimal in character form
    vv[vv %in% c("b", "B")] <- "11"
    vv[vv %in% c("c", "C")] <- "12"
    vv[vv %in% c("d", "D")] <- "13"
    vv[vv %in% c("e", "E")] <- "14"
    vv[vv %in% c("f", "F")] <- "15"
    vv <- as.integer(vv)                                                         # convert digits to integer
    pp <- rev(16 ^ (0:(nn - 1)))                                                 # get the hex place value of each position
    yy <- vv * pp                                                                # multiply digits by their place values
    sum(yy)                                                                      # sum the result
  }
  x <- as.list(x)
  sapply(lapply(x, ch), conv)
}

#' @describeIn hexdec. Convert from decimal to hexadecimal.
#' @export
tohex <- function(x) {
  if (!cmp_nnw_vec(x)) {stop("\n • [x] must be complete, non-negative whole-number vec (?cmp_nnw_vec).")}
  paste0("#", toupper(as.character.hexmode(x)))
}
