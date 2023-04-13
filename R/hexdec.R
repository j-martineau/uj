#' @name to_dec
#' @encoding UTF-8
#' @family conversions
#' @title Convert non-negative whole numbers between decimal and hexadecimal
#' @description \tabular{ll}{  `to_dec, todec`   \tab Converts hexadecimal \link[=cmp_chr]{complete character object} `X` from hexadecimal to decimal. `X` may be formatted as either `'hhh...'` or `'#hhh...'` where `h` is a placeholder for a hexidecimal digit in either upper or lower case. \cr   \tab   \cr
#'                             `to_hex, tohex`   \tab Converts \link[=cmp_psw]{complete positive whole-number} `X` to hexadecimal formatted as `'#HHH...'` where `H` is a placeholder for an uppercase hexadecimal digit.                                                                                        }
#' @param X A \link[=cmp_nnw]{complete non-negative whole-number object} or a \link[=cmp_chr]{complete character vec} containing only non-negative whole-number hexademical values.
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
to_dec <- function(X) {
  OkX <- F
  OkEmp <- F
  OkPref <- T
  if (uj:::.cmp_chr(X)) {
    Ch <- uj::av(base::strsplit(uj::av(X), "", fixed = TRUE))
    Valid <- base::c("#", 0:9, letters[1:6], LETTERS[1:6])
    OkX <- base::all(Ch %in% Valid)
    if (OkX) {
      PP <- uj::av(base::gregexpr("#", X, fixed = T))
      X  <- base::gsub("#", "", X, fixed = T)
      OkPref <- base::length(base::which(PP > 1)) == 0
      OkEmp <- base::length(base::which(base::nchar(X) == 0)) == 0
    }}
  Errors <- NULL
  if (!OkX) {Errors <- base::c(Errors, "[X] must be of mode character, have at least one value, contain no NA values, and contain only the characters in '#0123456789ABCDEFabcdef'.")}
  if (!OkEmp) {Errors <- base::c(Errors, "After removing pound signs ('#'), one or more elements of [X] is a blank string.")}
  if (!OkPref) {Errors <- base::c(Errors, "If pound signs ('#') are used, they must be the first character of any element of [X] in which they are used.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  .conv <- function(X) {
    X <- base::strsplit(X, "", T)[[1]]
    n <- base::length(X)
    X[X %in% base::c("a", "A")] <- "10"
    X[X %in% c("b", "B")] <- "11"
    X[X %in% c("c", "C")] <- "12"
    X[X %in% c("d", "D")] <- "13"
    X[X %in% c("e", "E")] <- "14"
    X[X %in% c("f", "F")] <- "15"
    X <- base::as.integer(X)
    PlaceValue <- base::rev(16 ^ (0:(n - 1)))
    Y <- X * PlaceValue
    base::sum(Y)
  }
  X <- base::as.list(X)
  base::sapply(base::lapply(X, uj::ch), .conv)
}

#' @rdname to_dec
#' @export
to_hex <- function(X) {
  if (!uj:::.cmp_nnw_vec(X)) {uj::stopperr("[X] must be complete, non-negative whole-number vec (?cmp_nnw_vec).", PKG = "uj")}
  base::paste0("#", base::toupper(base::as.character.hexmode(X)))
}

#' @rdname to_dec
#' @export
todec <- to_dec

#' @rdname to_dec
#' @export
tohex <- to_hex
