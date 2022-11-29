#' @name spacing
#' @family strings
#' @title Error-checked `stringr`-based spacing functions
#' @description \itemize{
#'   \item **`spaces`**: creates a character vector of `n` spaces.
#'   \item **`pad`**: pads a string with leading and/or trailing spaces with \code{\link[stringr:pad]{stringr::pad}}.
#'   \item **`sqz`**: trims leading, trailing, and extra internal spaces with \code{\link[stringr:str_squish]{stringr::str_squish}}.
#'   \item **`trm`**: trims leading and trailing spaces with \code{\link[stringr:str_trim]{stringr::str_trim}}.
#' }
#' @param ... An arbitrary number of atomic arguments to be processed.
#' @param n A link[=cmp_psw_scl]{complete positive whole-number scalar} indicating the number of spaces.
#' @param n. A complete positive whole-number scalar indicating the number of pad characters.
#' @param s. A \link[=cmp_ch1_scl]{complete onechar scalar} indicating side(s) to pad: `'l'`, `'r'`, or `'b'` for left, right, or both, respectively.
#' @param p. A complete onechar scalar containing a single character used to pad strings.
#' @return \itemize{
#'   \item **`spaces`**: a character scalar.
#'   \item **`pad`, `trm`, `sqz`**: a character object.
#' }
#' @examples
#' spaces(0)
#' spaces(4)
#' pad(0:9, n. = 3)
#' pad(0:9, n. = 3, s. = "r")
#' pad(0:9, n. = 3, s. = "l")
#' pad(0:9, n. = 3, s. = "b")
#' pad(0:9, n. = 3, s. = "b", p. = "*")
#' trm(c("a ", "b", "c", " ", "d  ", ""))
#' sqz(c("a ", "b", "c", " ", "d  ", ""))
#' sqz(trm(c("a ", "b", "c", " ", "d  ", "")))
#' @export
spaces <- function(n) {
  if (!cmp_nnw_scl(n)) {stop("\n \u2022 [n] must be a complete positive whole-number scalar (?cmp_psw_scl).")}
  paste0(rep(" ", n), collapse = "")
}

#' @rdname spacing
#' @export
pad <- function(..., n. = 0, s. = "r", p. = " ") {
  errs <- c(f0(all(sapply(list(...), cmp_chr)), NULL, "\n \u2022 [...] must contain at least one argument, and all must be complete character vecs (?cmp_chr_vec)."),
            f0(cmp_nnw_scl(n.)                , NULL, "\n \u2022 [n.] must be a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isIN(s., "l", "r", "b")        , NULL, "\n \u2022 [s.] must be a complete onechar scalar (?cmp_ch1_scl) in c('r', 'l', 'b')."),
            f0(cmp_ch1_scl(p.)                , NULL, "\n \u2022 [p.] must be a complete onechar scalar (?cmp_ch1_scl)."))
  if (!is.null(errs)) {stop(errs)}
  sapply(av(...), stringr::str_pad, width = n., side = s., pad = p.)
}

#' @rdname spacing
#' @export
sqz <- function(...) {
  if (!all(sapply(list(...), cmp_chr))) {stop("\n \u2022 [...] must contain at least one argument, and all must be complete character vecs (?cmp_chr_vec).")}
  stringr::str_squish(av(...))
}

#' @rdname spacing
#' @export
trm <- function(...) {
  if (!all(sapply(list(...), cmp_chr))) {stop("\n \u2022 [...] must contain at least one argument, and all must be complete character vecs (?cmp_chr_vec)")}
  stringr::str_trim(av(...))
}
