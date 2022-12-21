#' @encoding UTF-8
#' @family strings
#' @family chars
#' @title String spacing functions
#' @details \tabular{rl}{
#'     `spaces`   \tab Creates a character scalar of `n` spaces.
#'   \cr          \tab  
#'   \cr  `pad`   \tab Pads strings with leading, trailing white space\eqn{^a}.
#'   \cr          \tab  
#'   \cr  `sqz`   \tab Trims leading, trailing, and extra internal white/space\eqn{^b}.
#'   \cr          \tab  
#'   \cr  `trm`   \tab Trims leading and trailing white space\eqn{^b}.
#' }
#' \eqn{^{a.}} Uses \code{\link[stringr:str_squish]{stringr::str_squish}}.
#' \cr\eqn{^{b.}} Uses \code{\link[stringr:str_pad]{stringr::str_pad}}.
#' \cr\eqn{^{c.}} Uses \code{\link[stringr:str_trim]{stringr::str_trim}}.
#' @param ... An arbitrary number of atomic arguments to be processed.
#' @param n A link[=cmp_psw_scl]{complete positive whole-number scalar} indicating the number of spaces.
#' @param n. A complete positive whole-number scalar indicating the number of pad characters.
#' @param s. A \link[=cmp_ch1_scl]{complete onechar scalar} indicating side(s) to pad: `'l'`, `'r'`, or `'b'` for left, right, or both, respectively.
#' @param p. A complete onechar scalar containing a single character used to pad strings.
#' @return *A character scalar*
#'   \cr    `spaces`
#'   \cr
#'   \cr *A character object*
#'   \cr    `pad`
#'   \cr    `trm`
#'   \cr    `sqz`
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
  if (!cmp_nnw_scl(n)) {stop(.errs("[n] must be a complete positive whole-number scalar (?cmp_psw_scl)."))}
  paste0(rep(" ", n), collapse = "")
}

#' @rdname spaces
#' @export
pad <- function(..., n. = 0, s. = "r", p. = " ") {
  ok.nd <- ...length() > 0
  ok.atm <- f0(!ok.nd, T, all(sapply(list(...), iatm)))
  ok.cmp <- f0(!ok.nd, T, all(sapply(list(...), icmp)))
  errs <- c(f0(ok.nd                  , NULL, "[...] must contain at least one argument."),
            f0(ok.atm & ok.cmp        , NULL, "[...] arguments must be complete and atomic (?icmp)."),
            f0(cmp_nnw_scl(n.)        , NULL, "[n.] must be a complete non-negative whole-number scalar (?cmp_nnw_scl)."),
            f0(isIN(s., "l", "r", "b"), NULL, "[s.] must be a complete onechar scalar (?cmp_ch1_scl) in c('r', 'l', 'b')."),
            f0(cmp_ch1_scl(p.)        , NULL, "[p.] must be a complete onechar scalar (?cmp_ch1_scl)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  sapply(av(...), stringr::str_pad, width = n., side = s., pad = p.)
}

#' @rdname spaces
#' @export
sqz <- function(...) {
  if (!all(sapply(list(...), cmp_chr))) {stop(.errs("[...] must contain at least one argument, and all must be complete character vecs (?cmp_chr_vec)."))}
  stringr::str_squish(av(...))
}

#' @rdname spaces
#' @export
trm <- function(...) {
  if (!all(sapply(list(...), cmp_chr))) {stop(.errs("[...] must contain at least one argument, and all must be complete character vecs (?cmp_chr_vec)"))}
  stringr::str_trim(av(...))
}
