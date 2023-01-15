#' @encoding UTF-8
#' @family strings
#' @family chars
#' @title String spacing functions
#' @details \tabular{rl}{
#'     `spaces`   \tab Creates a character scalar of `n` spaces.
#'   \cr  `pad`   \tab Pads strings with leading, trailing white space using \code{\link[stringr:str_pad]{stringr::str_pad}}.
#'   \cr  `sqz`   \tab Trims leading, trailing, and extra internal white space using \code{\link[stringr:str_squish]{stringr::str_squish}}.
#'   \cr  `trm`   \tab Trims leading and trailing white space using \code{\link[stringr:str_trim]{stringr::str_trim}}.
#' }
#' @param ... An arbitrary number of atomic arguments to be processed.
#' @param n A link[=cmp_psw_scl]{complete positive whole-number scalar} indicating the number of spaces.
#' @param n. A complete positive whole-number scalar indicating the number of pad characters.
#' @param s. A \link[=cmp_ch1_scl]{complete onechar scalar} indicating side(s) to pad: `'l'`, `'r'`, or `'b'` for left, right, or both, respectively.
#' @param p. A complete onechar scalar containing a single character used to pad strings.
#' @return *A character scalar* \cr   `spaces`
#'  \cr\cr *A character object* \cr   `pad, trm, sqz`
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
  if (!uj::cmp_nnw_scl(n)) {stop(uj::format_errs(pkg = "uj", "[n] must be a complete positive whole-number scalar (?cmp_psw_scl)."))}
  base::paste0(base::rep(" ", n), collapse = "")
}

#' @rdname spaces
#' @export
pad <- function(..., n. = 0, s. = "r", p. = " ") {
  ok.nd <- base::...length() > 0
  ok.atm <- uj::f0(!ok.nd, T, base::all(base::sapply(base::list(...), iatm)))
  ok.cmp <- uj::f0(!ok.nd, T, base::all(base::sapply(base::list(...), icmp)))
  errs <- base::c(uj::f0(ok.nd                      , NULL, "[...] must contain at least one argument."),
                  uj::f0(ok.atm & ok.cmp            , NULL, "[...] arguments must be complete and atomic (?icmp)."),
                  uj::f0(uj::cmp_nnw_scl(n.)        , NULL, "[n.] must be a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
                  uj::f0(uj::isIN(s., "l", "r", "b"), NULL, "[s.] must be a complete onechar scalar (?cmp_ch1_scl) in c('r', 'l', 'b')."),
                  uj::f0(uj::cmp_ch1_scl(p.)        , NULL, "[p.] must be a complete onechar scalar (?cmp_ch1_scl)."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  base::sapply(uj::av(...), stringr::str_pad, width = n., side = s., pad = p.)
}

#' @rdname spaces
#' @export
sqz <- function(...) {
  if (!base::all(base::sapply(base::list(...), uj::cmp_chr))) {stop(uj::format_errs(pkg = "uj", "[...] must contain at least one argument, and all must be complete character vecs (?cmp_chr_vec)."))}
  stringr::str_squish(uj::av(...))
}

#' @rdname spaces
#' @export
trm <- function(...) {
  if (!base::all(base::sapply(base::list(...), uj::cmp_chr))) {stop(uj::format_errs(pkg = "uj", "[...] must contain at least one argument, and all must be complete character vecs (?cmp_chr_vec)"))}
  stringr::str_trim(uj::av(...))
}
