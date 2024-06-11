#' @encoding UTF-8
#' @family conversions
#' @title Convert non-negative whole numbers between decimal and hexadecimal
#' @param x A \link[=cmp_nnw]{complete non-negative whole-number object} (`to_hex` and `tohex`) or a \link[=cmp_chr]{complete character vec} containing only non-negative whole-number hexademical values (`to_dec` or `todec`).
#' @examples
#' egHexDec <- function() {
#'   RGBA   <- col2rgb("orange", alpha = 0.5)
#'   HEX1   <- rgb(RGBA[1] / 255, RGBA[2] / 255, RGBA[3] / 255, RGBA[4] / 255)
#'   HEX1   <- c(substr(HEX1, 2, 3), substr(HEX1, 4, 5), substr(HEX1, 6, 7), substr(HEX1, 8, 9))
#'   DEC1   <- to_dec(HEX1)
#'   HEX2   <- to_hex(DEC1)
#'   HEX12A <- tohex(12)
#'   DEC12A <- todec(HEX12A)
#'   HEX12B <- tohex(DEC12A)
#'   list(RGBA = RGBA, HEX1 = HEX2, HEX12A = HEX12A, HEX12B = HEX12B, DEC1 = DEC1, DEC12A = DEC12A)
#' }
#' egHexDec <- egHexDec()
#' egHexDec
#' @export
hex_dec_help <- function() {help::utils("hex_dec_help", package = "uj")}

#' @describeIn hex_dec_help Convert hexadecimal numerals to decimal numerals. Returns a non-negative whole-number object.
#' @export
to_dec <- function(x) {
  if (uj::.cmp_chr(x)) {
    x <- base::toupper(base::gsub("#", "", x, fixed = T))
    if (base::any(x == "")) {uj::stopperr("After removing '#' prefixes, at least one elemnt of [x] is a blank string.")}
    ch <- uj::av(base::strsplit(x, "", fixed = TRUE))
    valid <- base::c(0:9, LETTERS[1:6])
    if (!base::all(ch %in% valid)) {uj::stopperr("At least one element of [x] contains invalid characters.")}
    base::strtoi(x, base = 16L)
  } else {uj::stopperr("[x] must be a complete character object (?cmp_chr).")}
}

#' @describeIn hex_dec_help Convert decimal numerals to hexadecimal numerals (*not* prefixed by `'#'`). Returns a character object of non-negative whole number hexadecimal values.
#' @export
to_hex <- function(x) {
  if (!uj::.cmp_nnw_vec(x)) {uj::stopperr("[x] must be complete, non-negative whole-number vec (?cmp_nnw_vec).")}
  base::toupper(base::as.character.hexmode(x))
}

#' @describeIn hex_dec_help An alias for `to_dec.`
#' @export
todec <- to_dec

#' @describeIn hex_dec_help An aliax for `to_hex.`
#' @export
tohex <- to_hex
