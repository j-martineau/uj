#' @name ipat
#' @family strings
#' @title Simplified string indexing for fixed patterns
#' @description \tabular{rl}{
#'     `has_pat`   \tab Evaluate whether each element of `x` contains the string in `pat`.
#'   \cr           \tab  
#'   \cr  `ipat`   \tab Logically indexes character vector elements containing a pattern as `TRUE` or `FALSE`.
#'   \cr           \tab  
#'   \cr  `wpat`   \tab Numerically indexes character vector elements containing a pattern as integer element numbers.
#'   \cr           \tab  
#'   \cr  `npat`   \tab Counts the number of times a pattern appears in each element of a character vector.
#'   \cr           \tab  
#'   \cr  `ppat`   \tab Identifies the position of *the first* character of the first instance of a pattern in each element of a character vector.
#'   \cr           \tab  
#'   \cr `ppats`   \tab Identifies the position of the first character of *every* instance of a pattern in each element of a character vector.
#' }
#' @details Always \link[=a]{atomizes} `...` to create a single character vector to search for a fixed pattern.
#' @param x A \link[=chr_vec]{character vector}.
#' @param pat A fixed value \link[=cmp_str_scl]{complete string scalar} pattern to search for in `x`.
#' @return \tabular{rl}{
#'     `has_pat`   \tab A logical
#'   \cr  `ipat`   \tab vector.
#'   \cr           \tab   
#'   \cr  `wpat`   \tab A positive
#'   \cr  `npat`   \tab whole-number
#'   \cr  `ppat`   \tab vector.
#'   \cr `ppats`   \tab   
#' }
#' @examples
#' words <- c("apple", "banana", "carrot")
#'
#' has_pat(words, "a")
#' ipat(words, "a")
#'
#' has_pat(words, "b")
#' ipat(words, "b")
#'
#' wpat(words, "a")
#' wpat(words, "b")
#'
#' npat(words, "a")
#' npat(words, "b")
#'
#' ppat(words, "a")
#' ppat(words, "b")
#'
#' ppats(words, "a")
#' ppats(words, "b")
#' @export
ipat <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, "[x] must be a complete character vec (?cmp_chr_vec)."     ),
            f0(cmp_str_scl(pat), NULL, "[pat] must be a complete string scalar (?cmp_str_scl)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  av(grepl(pat, x, fixed = T))
}

#' @rdname ipat
#' @export
has_pat <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, "[x] must be a complete character vec (?cmp_chr_vec)."     ),
            f0(cmp_str_scl(pat), NULL, "[pat] must be a complete string scalar (?cmp_str_scl)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  av(nchar(x) != nchar(gsub(pat, "", x, fixed = T)))
}

#' @rdname ipat
#' @export
wpat <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, "[x] must be a complete character vec (?cmp_chr_vec)."),
            f0(cmp_str_scl(pat), NULL, "[pat] must be a complete string scalar (?cmp_str_scl)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  av(grep(pat, x, fixed = T))
}

#' @rdname ipat
#' @export
npat <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, "[x] must be a complete character vec (?cmp_chr_vec)."),
            f0(cmp_str_scl(pat), NULL, "[pat] must be a complete string scalar (?cmp_str_scl)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  grgxpr <- function(xx) {
    yy <- av(gregexpr(pat, xx))
    nn <- length(yy)
    f0(nn > 1, nn, f0(yy == -1, 0, 1))
  }
  av(sapply(x, grgxpr))
}

#' @rdname ipat
#' @export
ppat <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, "[x] must be a complete character vec (?cmp_chr_vec)."),
            f0(cmp_str_scl(pat), NULL, "[pat] must be a complete string scalar (?cmp_chr_vec)."))
  if (!is.null(errs)) cmp_str_scl
  out <- regexpr(pat, x, fixed = T)
  out[out == -1] <- NA
  if (all(is.na(out))) {out <- NA}
  av(out)
}

#' @rdname ipat
#' @export
ppats <- function(x, pat) {
  errs <- c(f0(cmp_chr_vec(x)  , NULL, "[x] must be a complete character vec (?cmp_chr_vec)."),
            f0(cmp_str_scl(pat), NULL, "[pat] must be a complete string scalar (?cmp_str_scl)."))
  grgxpr <- function(xx) {yy <- av(gregexpr(pat, xx)); yy[yy == -1] <- NA; yy}
  if (!is.null(errs)) {stop(.errs(errs))}
  lapply(x, grgxpr)
}
