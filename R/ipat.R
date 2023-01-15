#' @encoding UTF-8
#' @family strings
#' @title Simplified string indexing for fixed patterns
#' @description \tabular{rl}{
#'     `has_pat`   \tab Evaluates whether each element of `x` contains the string in `pat`.
#'   \cr `ppats`   \tab Identifies the position of the first character of *every* instance of a pattern in each element of a character vector.
#'   \cr  `ppat`   \tab Identifies the position of *the first* character of the first instance of a pattern in each element of a character vector.
#'   \cr  `npat`   \tab Counts the number of times a pattern appears in each element of a character vector.
#'   \cr  `ipat`   \tab Logically indexes character vector elements containing a pattern as `TRUE` or `FALSE`.
#'   \cr  `wpat`   \tab Numerically indexes character vector elements containing a pattern as integer element numbers.
#' }
#' @details Always \link[=a]{atomizes} `...` to create a single character vector to search for a fixed pattern.
#' @param x A \link[=chr_vec]{character vector}.
#' @param pat A fixed value \link[=cmp_str_scl]{complete string scalar} pattern to search for in `x`.
#' @return *A positive whole number \link[=ivls]{vlist}* \cr   `ppats`
#'   \cr\cr *A positive whole number vector* \cr   `wpat, npat, ppat`
#'   \cr\cr *A logical scalar* \cr   `has_pat, ipat`
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
  errs <- base::c(uj::f0(uj::cmp_chr_vec(x)  , NULL, "[x] must be a complete character vec (?cmp_chr_vec)."     ),
                  uj::f0(uj::cmp_str_scl(pat), NULL, "[pat] must be a complete string scalar (?cmp_str_scl)."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  uj::av(base::grepl(pat, x, fixed = T))
}

#' @rdname ipat
#' @export
has_pat <- function(x, pat) {
  errs <- base::c(uj::f0(uj::cmp_chr_vec(x)  , NULL, "[x] must be a complete character vec (?cmp_chr_vec)."     ),
                  uj::f0(uj::cmp_str_scl(pat), NULL, "[pat] must be a complete string scalar (?cmp_str_scl)."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  uj::av(base::nchar(x) != base::nchar(base::gsub(pat, "", x, fixed = T)))
}

#' @rdname ipat
#' @export
wpat <- function(x, pat) {
  errs <- base::c(uj::f0(uj::cmp_chr_vec(x)  , NULL, "[x] must be a complete character vec (?cmp_chr_vec)."),
                  uj::f0(uj::cmp_str_scl(pat), NULL, "[pat] must be a complete string scalar (?cmp_str_scl)."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  uj::av(base::grep(pat, x, fixed = T))
}

#' @rdname ipat
#' @export
npat <- function(x, pat) {
  errs <- base::c(uj::f0(uj::cmp_chr_vec(x)  , NULL, "[x] must be a complete character vec (?cmp_chr_vec)."),
                  uj::f0(uj::cmp_str_scl(pat), NULL, "[pat] must be a complete string scalar (?cmp_str_scl)."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  grgxpr <- function(xx) {
    yy <- uj::av(base::gregexpr(pat, xx))
    nn <- base::length(yy)
    uj::f0(nn > 1, nn, uj::f0(yy == -1, 0, 1))
  }
  uj::av(base::sapply(x, grgxpr))
}

#' @rdname ipat
#' @export
ppat <- function(x, pat) {
  errs <- base::c(uj::f0(uj::cmp_chr_vec(x)  , NULL, "[x] must be a complete character vec (?cmp_chr_vec)."),
                  uj::f0(uj::cmp_str_scl(pat), NULL, "[pat] must be a complete string scalar (?cmp_chr_vec)."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  out <- base::regexpr(pat, x, fixed = T)
  out[out == -1] <- NA
  if (base::all(base::is.na(out))) {out <- NA}
  uj::av(out)
}

#' @rdname ipat
#' @export
ppats <- function(x, pat) {
  errs <- base::c(uj::f0(uj::cmp_chr_vec(x)  , NULL, "[x] must be a complete character vec (?cmp_chr_vec)."),
                  uj::f0(uj::cmp_str_scl(pat), NULL, "[pat] must be a complete string scalar (?cmp_str_scl)."))
  grgxpr <- function(xx) {yy <- av(base::gregexpr(pat, xx)); yy[yy == -1] <- NA; yy}
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  base::lapply(x, grgxpr)
}
