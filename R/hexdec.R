#' @name hexdec
#' @family math
#' @title Convert between integer and hexadecimal
#' @details Hexademical representation is of mode character and may begin with
#'   "#".
#' @param x Vector of positive whole numbers or strings with hexademical number
#'   representations.
#' @export
todec <- function(x) {
  VX <- F
  if (cmp_chr(x)) {
    CH <- ch(av(x))
    if (!any(x != "")) {VX <- all(CH %in% ch("#0123456789ABCDEFabcdef"))}
    if (VX) {
      PP <- av(gregexpr("#", x, fixed = T))
      VP <- !any(PP > 1)
      x  <- gsub("#", "", x, fixed = T)
      VE <- !any(nchar(x) == 0)
    }
  }
  E <- NULL
  if (!VX) {E <- c(E, "\n  * [x] must be of mode character, have at least one value, have no NA values, and contain only the characters in '#0123456789ABCDEFabcdef'.")}
  if (!VP) {E <- c(E, "\n  * If pound signs ('#') are used, they must be the first character of any element of [x] in which they are used.")}
  if (!VE) {E <- c(E, "\n  * After removing pound signs ('#'), one or more elements of [x] is a blank string.")}
  if (xdef(E)) {stop(E)}
  convert <- function(v) {                                                       # subfunction to convert a single hex value to decimal.
    V <- strsplit(v, "", T)[[1]]                                                 # split the string into characters
    N <- length(V)                                                               # number of hex digits
    V[V %in% c("a", "A")] <- "10"                                                # convert alpha hex digits to decimal in character form
    V[V %in% c("b", "B")] <- "11"
    V[V %in% c("c", "C")] <- "12"
    V[V %in% c("d", "D")] <- "13"
    V[V %in% c("e", "E")] <- "14"
    V[V %in% c("f", "F")] <- "15"
    V <- as.integer(V)                                                           # convert digits to integer
    P <- rev(16 ^ (0:(N - 1)))                                                   # get the hex place value of each position
    R <- V * P                                                                   # multiply digits by their place values
    R <- sum(R)                                                                  # sum the result
    return(R)
  }
  x <- as.list(x)
  sapply(lapply(x, ch), convert)
}

#' @rdname hexdec
#' @export
tohex <- function(x) {
  if (!cmp_nnw_vec(x)) {stop("\n  * [x] must be non-empty, atomic, and contain only non-negative whole numbers.")}
  paste0("#", toupper(as.character.hexmd(x)))
}
