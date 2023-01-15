.delim_errs <- function(args, a) {
  n <- base::length(args$dots)
  ns <- base::lengths(args$dots)
  two <- "D" %in% base::names(args)
  dots <- n > 0
  pops <- uj::f0(!dots, T, base::all(ns > 0))
  ccsd <- uj::cmp_chr_scl(args$d)
  ccsD <- uj::f0(!two, T, uj::cmp_chr_scl(args$D))
  atms <- uj::f0(!ccsd, T, base::all(base::sapply(args$dots, uj::atm_vec)))
  recs <- uj::f0(a | !pops, T, uj::recyclable_n(base::max(ns) / ns))
  base::c(uj::f0(dots, NULL, "[...] is empty."),
          uj::f0(pops, NULL, "An argument in [...] is of length 0."),
          uj::f0(ccsd, NULL, "[d] must be a complete character scalar (?cmp_chr_scl)."),
          uj::f0(ccsD, NULL, "[D] must be a complete character scalar (?cmp_chr_scl)."),
          uj::f0(atms, NULL, "Arguments in [...] must be atomic vecs (?pop_vec)."),
          uj::f0(recs, NULL, "Arguments in [...] are not of recyclable lengths (?recyclable)."))
}

#' @name delim
#' @encoding UTF-8
#' @family strings
#' @title Error-checked string delimiting
#' @description Simplified and extended `base::paste` and `base::paste0`. There are both primary functions with user-specified delimiters and convenience functions for common delimiters.
#' \cr
#' \cr
#' **Primary functions**
#' \tabular{rl}{
#'       `dww`   \tab Delimit elements within each (atomic vector) `...` argument using delimiter `d`, then delimit elements within the resulting vector using delimiter `D`. Produces a character scalar of `...length()` substrings delimited by `D` where each substring contains sub-substrings delimited by `d`.
#'   \cr `daw`   \tab Delimit across corresponding elements of (recyclable atomic vector) `...` arguments using delimiter `d`, then delimit elements within the resulting character vector using delimiter `D`. Produces a character scalar of `...length()` substrings delimited by `D` where each substring contains `max(lengths(list(...)))` sub-substrings delimited by `d`.
#'   \cr  `dw`   \tab Delimits elements within each (atomic vector) `...` argument using delimiter `d`. Produces a character vector of `...length()` substrings delimited by `d`.
#'   \cr  `da`   \tab Delimits across corresponding elements of (recyclable atomic vector) `...` arguments using delimiter `d`. Produces a character vector of `max(lengths(list(...)))` substrings delimited by `d`.
#' }
#' \cr **Common-delimiter convenience functions**
#' \cr Convenience function names are constructed by append `1` or `2` codes for common delimiters to the function name as follows (where `X` and `Y` are placeholders for common-delimiter codes):
#' \tabular{rl}{
#'       `dawXY`   \tab Delimits across with `X` then within with `Y`.
#'   \cr `dwwXY`   \tab Delimits within with `X` then again with `Y`.
#'   \cr   `daX`   \tab Delimits across with `X`.
#'   \cr   `dwX`   \tab Delimits within with `X`.
#' }
#' \cr Common delimiters are encoded as: \tabular{cll}{
#'        *Code*   \tab *Name*                      \tab *Delimiter*
#'   \cr   `'0'`   \tab blank                       \tab `''`
#'   \cr `'1'`     \tab space                       \tab `' '`
#'   \cr `'B'`     \tab broken pipe                 \tab `'¦'`
#'   \cr `'C'`     \tab colon                       \tab `':'`
#'   \cr `'D'`     \tab dot                         \tab `'.'`
#'   \cr `'G'`     \tab grammatical comma\eqn{^1}   \tab `', '`
#'   \cr `'P'`     \tab pipe                        \tab `'|'`
#'   \cr `'Q'`     \tab back-tick                   \tab `'``'`
#'   \cr `'S'`     \tab simple comma\eqn{^1}        \tab `','`
#'   \cr `'T'`     \tab tilde                       \tab `'~'`
#' }
#' ` `\eqn{^{1.}} 'Grammatical comma' vs. 'simple comma' indicates whether the function produces grammatical comma-delimited lists vs. simple comma-delimited values (e.g., `'1, 2, 3'` vs. `'1,2,3'`).
#' @param ... An arbitrary number of atomic vector arguments to be delimited. Argument in `...` must be recyclable for functions that delimit across `...` arguments as the first or only step (i.e., functions with names beginning with `da`).
#' @param d,D \link[=chr_scl]{Character scalar} delimiters.
#' @return *A character vector* \cr   `daX, da, dw`
#'  \cr\cr *A character scalar* \cr   `dawXY, dwwXY, daw, dww`
#' @examples
#' # delimit across using delimiter '|'.
#' # aliases paste(1:3, 4:6, sep = '|').
#' da('|', 1:3, 4:6)
#'
#' # delimit within using delimiter ':'.
#' # aliases sapply(list(1:3, 4:6), paste0, collapse = ':').
#' dw(':', 1:3, 4:6)
#'
#' # delimit across using '|', then within using ':'
#' # aliases paste(..., sep = d, collapse = D)
#' daw('|', ':', 1:3, 4:6)
#'
#' # delimit within using '|' then again within using ':'.
#' # aliases paste0(sapply(list(...), paste0, collapse = d), collapse = D).
#' dww('|', ':', 1:3, 4:6)
#'
#' # delimit across using pipe (encoded by 'P' suffix in function name).
#' daP(1:3, 4:6)
#'
#' # delimit within using colon (encoded by 'C' suffix in function name).
#' dwC(1:3, 4:6)
#'
#' # delimit across using pipe, then within using colon.
#' dawPC(1:3, 4:6)
#'
#' # delimit within using colon, then again using pipe.
#' dwwCP(1:3, 4:6)
#' @export
da <- function(d, ...) {
  args <- base::list(d = d, dots =  base::list(...))
  errs <- uj:::.delim_errs(args, TRUE)
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  base::paste(..., sep = d)
}

#' @rdname delim
#' @export
dw <- function(d, ...) {
  args <- base::list(d = d, dots = base::list(...))
  errs <- uj:::.delim_errs(args, TRUE)
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  base::sapply(base::list(...), base::paste0, collapse = d)
}

#' @rdname delim
#' @export
daw <- function(d, D, ...) {
  args <- base::list(d = d, D = D, dots = base::list(...))
  errs <- uj:::.delim_errs(args, TRUE)
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  base::paste0(base::paste(..., sep = d), collapse = D)
}

#' @rdname delim
#' @export
dww <- function(d, D, ...) {
  args <- base::list(d = d, D = D, dots = base::list(...))
  errs <- uj:::.delim_errs(args, TRUE)
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  base::paste(base::sapply(base::list(...), paste0, collapse = d), collapse = D)
}

#' @rdname delim
#' @export
da0 <- function(...) {uj::da('', ...)}

#' @rdname delim
#' @export
da1 <- function(...) {uj::da(' ', ...)}

#' @rdname delim
#' @export
daB <- function(...) {uj::da('¦', ...)}

#' @rdname delim
#' @export
daC <- function(...) {uj::da(':', ...)}

#' @rdname delim
#' @export
daD <- function(...) {uj::da('.', ...)}

#' @rdname delim
#' @export
daG <- function(...) {uj::da(', ', ...)}

#' @rdname delim
#' @export
daP <- function(...) {uj::da('|', ...)}

#' @rdname delim
#' @export
daQ <- function(...) {uj::da('`', ...)}

#' @rdname delim
#' @export
daS <- function(...) {uj::da(',', ...)}

#' @rdname delim
#' @export
daT <- function(...) {uj::da('~', ...)}

#' @rdname delim
#' @export
dw0 <- function(...) {uj::dw('', ...)}

#' @rdname delim
#' @export
dw1 <- function(...) {uj::dw(' ', ...)}

#' @rdname delim
#' @export
dwB <- function(...) {uj::dw('¦', ...)}

#' @rdname delim
#' @export
dwC <- function(...) {uj::dw(':', ...)}

#' @rdname delim
#' @export
dwD <- function(...) {uj::dw('.', ...)}

#' @rdname delim
#' @export
dwG <- function(...) {uj::dw(', ', ...)}

#' @rdname delim
#' @export
dwP <- function(...) {uj::dw('|', ...)}

#' @rdname delim
#' @export
dwQ <- function(...) {uj::dw('`', ...)}

#' @rdname delim
#' @export
dwS <- function(...) {uj::dw(',', ...)}

#' @rdname delim
#' @export
dwT <- function(...) {uj::dw('~', ...)}

#' @rdname delim
#' @export
daw00 <- function(...) {uj::daw('', '', ...)}

#' @rdname delim
#' @export
daw01 <- function(...) {uj::daw('', ' ', ...)}

#' @rdname delim
#' @export
daw0B <- function(...) {uj::daw('', '¦', ...)}

#' @rdname delim
#' @export
daw0C <- function(...) {uj::daw('', ':', ...)}

#' @rdname delim
#' @export
daw0D <- function(...) {uj::daw('', '.', ...)}

#' @rdname delim
#' @export
daw0G <- function(...) {uj::daw('', ', ', ...)}

#' @rdname delim
#' @export
daw0P <- function(...) {uj::daw('', '|', ...)}

#' @rdname delim
#' @export
daw0Q <- function(...) {uj::daw('', '`', ...)}

#' @rdname delim
#' @export
daw0S <- function(...) {uj::daw('', ',', ...)}

#' @rdname delim
#' @export
daw0T <- function(...) {uj::daw('', '~', ...)}

#' @rdname delim
#' @export
daw10 <- function(...) {uj::daw(' ', '', ...)}

#' @rdname delim
#' @export
daw11 <- function(...) {uj::daw(' ', ' ', ...)}

#' @rdname delim
#' @export
daw1B <- function(...) {uj::daw(' ', '¦', ...)}

#' @rdname delim
#' @export
daw1C <- function(...) {uj::daw(' ', ':', ...)}

#' @rdname delim
#' @export
daw1D <- function(...) {uj::daw(' ', '.', ...)}

#' @rdname delim
#' @export
daw1G <- function(...) {uj::daw(' ', ', ', ...)}

#' @rdname delim
#' @export
daw1P <- function(...) {uj::daw(' ', '|', ...)}

#' @rdname delim
#' @export
daw1Q <- function(...) {uj::daw(' ', '`', ...)}

#' @rdname delim
#' @export
daw1S <- function(...) {uj::daw(' ', ',', ...)}

#' @rdname delim
#' @export
daw1T <- function(...) {uj::daw(' ', '~', ...)}

#' @rdname delim
#' @export
dawB0 <- function(...) {uj::daw('¦', '', ...)}

#' @rdname delim
#' @export
dawB1 <- function(...) {uj::daw('¦', ' ', ...)}

#' @rdname delim
#' @export
dawBB <- function(...) {uj::daw('¦', '¦', ...)}

#' @rdname delim
#' @export
dawBC <- function(...) {uj::daw('¦', ':', ...)}

#' @rdname delim
#' @export
dawBD <- function(...) {uj::daw('¦', '.', ...)}

#' @rdname delim
#' @export
dawBG <- function(...) {uj::daw('¦', ', ', ...)}

#' @rdname delim
#' @export
dawBP <- function(...) {uj::daw('¦', '|', ...)}

#' @rdname delim
#' @export
dawBQ <- function(...) {uj::daw('¦', '`', ...)}

#' @rdname delim
#' @export
dawBS <- function(...) {uj::daw('¦', ',', ...)}

#' @rdname delim
#' @export
dawBT <- function(...) {uj::daw('¦', '~', ...)}

#' @rdname delim
#' @export
dawC0 <- function(...) {uj::daw(':', '', ...)}

#' @rdname delim
#' @export
dawC1 <- function(...) {uj::daw(':', ' ', ...)}

#' @rdname delim
#' @export
dawCB <- function(...) {uj::daw(':', '¦', ...)}

#' @rdname delim
#' @export
dawCC <- function(...) {uj::daw(':', ':', ...)}

#' @rdname delim
#' @export
dawCD <- function(...) {uj::daw(':', '.', ...)}

#' @rdname delim
#' @export
dawCG <- function(...) {uj::daw(':', ', ', ...)}

#' @rdname delim
#' @export
dawCP <- function(...) {uj::daw(':', '|', ...)}

#' @rdname delim
#' @export
dawCQ <- function(...) {uj::daw(':', '`', ...)}

#' @rdname delim
#' @export
dawCS <- function(...) {uj::daw(':', ',', ...)}

#' @rdname delim
#' @export
dawCT <- function(...) {uj::daw(':', '~', ...)}

#' @rdname delim
#' @export
dawD0 <- function(...) {uj::daw('.', '', ...)}

#' @rdname delim
#' @export
dawD1 <- function(...) {uj::daw('.', ' ', ...)}

#' @rdname delim
#' @export
dawDB <- function(...) {uj::daw('.', '¦', ...)}

#' @rdname delim
#' @export
dawDC <- function(...) {uj::daw('.', ':', ...)}

#' @rdname delim
#' @export
dawDD <- function(...) {uj::daw('.', '.', ...)}

#' @rdname delim
#' @export
dawDG <- function(...) {uj::daw('.', ', ', ...)}

#' @rdname delim
#' @export
dawDP <- function(...) {uj::daw('.', '|', ...)}

#' @rdname delim
#' @export
dawDQ <- function(...) {uj::daw('.', '`', ...)}

#' @rdname delim
#' @export
dawDS <- function(...) {uj::daw('.', ',', ...)}

#' @rdname delim
#' @export
dawDT <- function(...) {uj::daw('.', '~', ...)}

#' @rdname delim
#' @export
dawG0 <- function(...) {uj::daw(', ', '', ...)}

#' @rdname delim
#' @export
dawG1 <- function(...) {uj::daw(', ', ' ', ...)}

#' @rdname delim
#' @export
dawGB <- function(...) {uj::daw(', ', '¦', ...)}

#' @rdname delim
#' @export
dawGC <- function(...) {uj::daw(', ', ':', ...)}

#' @rdname delim
#' @export
dawGD <- function(...) {uj::daw(', ', '.', ...)}

#' @rdname delim
#' @export
dawGG <- function(...) {uj::daw(', ', ', ', ...)}

#' @rdname delim
#' @export
dawGP <- function(...) {uj::daw(', ', '|', ...)}

#' @rdname delim
#' @export
dawGQ <- function(...) {uj::daw(', ', '`', ...)}

#' @rdname delim
#' @export
dawGS <- function(...) {uj::daw(', ', ',', ...)}

#' @rdname delim
#' @export
dawGT <- function(...) {uj::daw(', ', '~', ...)}

#' @rdname delim
#' @export
dawP0 <- function(...) {uj::daw('|', '', ...)}

#' @rdname delim
#' @export
dawP1 <- function(...) {uj::daw('|', ' ', ...)}

#' @rdname delim
#' @export
dawPB <- function(...) {uj::daw('|', '¦', ...)}

#' @rdname delim
#' @export
dawPC <- function(...) {uj::daw('|', ':', ...)}

#' @rdname delim
#' @export
dawPD <- function(...) {uj::daw('|', '.', ...)}

#' @rdname delim
#' @export
dawPG <- function(...) {uj::daw('|', ', ', ...)}

#' @rdname delim
#' @export
dawPP <- function(...) {uj::daw('|', '|', ...)}

#' @rdname delim
#' @export
dawPQ <- function(...) {uj::daw('|', '`', ...)}

#' @rdname delim
#' @export
dawPS <- function(...) {uj::daw('|', ',', ...)}

#' @rdname delim
#' @export
dawPT <- function(...) {uj::daw('|', '~', ...)}

#' @rdname delim
#' @export
dawQ0 <- function(...) {uj::daw('`', '', ...)}

#' @rdname delim
#' @export
dawQ1 <- function(...) {uj::daw('`', ' ', ...)}

#' @rdname delim
#' @export
dawQB <- function(...) {uj::daw('`', '¦', ...)}

#' @rdname delim
#' @export
dawQC <- function(...) {uj::daw('`', ':', ...)}

#' @rdname delim
#' @export
dawQD <- function(...) {uj::daw('`', '.', ...)}

#' @rdname delim
#' @export
dawQG <- function(...) {uj::daw('`', ', ', ...)}

#' @rdname delim
#' @export
dawQP <- function(...) {uj::daw('`', '|', ...)}

#' @rdname delim
#' @export
dawQQ <- function(...) {uj::daw('`', '`', ...)}

#' @rdname delim
#' @export
dawQS <- function(...) {uj::daw('`', ',', ...)}

#' @rdname delim
#' @export
dawQT <- function(...) {uj::daw('`', '~', ...)}

#' @rdname delim
#' @export
dawS0 <- function(...) {uj::daw(',', '', ...)}

#' @rdname delim
#' @export
dawS1 <- function(...) {uj::daw(',', ' ', ...)}

#' @rdname delim
#' @export
dawSB <- function(...) {uj::daw(',', '¦', ...)}

#' @rdname delim
#' @export
dawSC <- function(...) {uj::daw(',', ':', ...)}

#' @rdname delim
#' @export
dawSD <- function(...) {uj::daw(',', '.', ...)}

#' @rdname delim
#' @export
dawSG <- function(...) {uj::daw(',', ', ', ...)}

#' @rdname delim
#' @export
dawSP <- function(...) {uj::daw(',', '|', ...)}

#' @rdname delim
#' @export
dawSQ <- function(...) {uj::daw(',', '`', ...)}

#' @rdname delim
#' @export
dawSS <- function(...) {uj::daw(',', ',', ...)}

#' @rdname delim
#' @export
dawST <- function(...) {uj::daw(',', '~', ...)}

#' @rdname delim
#' @export
dawT0 <- function(...) {uj::daw('~', '', ...)}

#' @rdname delim
#' @export
dawT1 <- function(...) {uj::daw('~', ' ', ...)}

#' @rdname delim
#' @export
dawTB <- function(...) {uj::daw('~', '¦', ...)}

#' @rdname delim
#' @export
dawTC <- function(...) {uj::daw('~', ':', ...)}

#' @rdname delim
#' @export
dawTD <- function(...) {uj::daw('~', '.', ...)}

#' @rdname delim
#' @export
dawTG <- function(...) {uj::daw('~', ', ', ...)}

#' @rdname delim
#' @export
dawTP <- function(...) {uj::daw('~', '|', ...)}

#' @rdname delim
#' @export
dawTQ <- function(...) {uj::daw('~', '`', ...)}

#' @rdname delim
#' @export
dawTS <- function(...) {uj::daw('~', ',', ...)}

#' @rdname delim
#' @export
dawTT <- function(...) {uj::daw('~', '~', ...)}

#' @rdname delim
#' @export
dww00 <- function(...) {uj::dww('', '', ...)}

#' @rdname delim
#' @export
dww01 <- function(...) {uj::dww('', ' ', ...)}

#' @rdname delim
#' @export
dww0B <- function(...) {uj::dww('', '¦', ...)}

#' @rdname delim
#' @export
dww0C <- function(...) {uj::dww('', ':', ...)}

#' @rdname delim
#' @export
dww0D <- function(...) {uj::dww('', '.', ...)}

#' @rdname delim
#' @export
dww0G <- function(...) {uj::dww('', ', ', ...)}

#' @rdname delim
#' @export
dww0P <- function(...) {uj::dww('', '|', ...)}

#' @rdname delim
#' @export
dww0Q <- function(...) {uj::dww('', '`', ...)}

#' @rdname delim
#' @export
dww0S <- function(...) {uj::dww('', ',', ...)}

#' @rdname delim
#' @export
dww0T <- function(...) {uj::dww('', '~', ...)}

#' @rdname delim
#' @export
dww10 <- function(...) {uj::dww(' ', '', ...)}

#' @rdname delim
#' @export
dww11 <- function(...) {uj::dww(' ', ' ', ...)}

#' @rdname delim
#' @export
dww1B <- function(...) {uj::dww(' ', '¦', ...)}

#' @rdname delim
#' @export
dww1C <- function(...) {uj::dww(' ', ':', ...)}

#' @rdname delim
#' @export
dww1D <- function(...) {uj::dww(' ', '.', ...)}

#' @rdname delim
#' @export
dww1G <- function(...) {uj::dww(' ', ', ', ...)}

#' @rdname delim
#' @export
dww1P <- function(...) {uj::dww(' ', '|', ...)}

#' @rdname delim
#' @export
dww1Q <- function(...) {uj::dww(' ', '`', ...)}

#' @rdname delim
#' @export
dww1S <- function(...) {uj::dww(' ', ',', ...)}

#' @rdname delim
#' @export
dww1T <- function(...) {uj::dww(' ', '~', ...)}

#' @rdname delim
#' @export
dwwB0 <- function(...) {uj::dww('¦', '', ...)}

#' @rdname delim
#' @export
dwwB1 <- function(...) {uj::dww('¦', ' ', ...)}

#' @rdname delim
#' @export
dwwBB <- function(...) {uj::dww('¦', '¦', ...)}

#' @rdname delim
#' @export
dwwBC <- function(...) {uj::dww('¦', ':', ...)}

#' @rdname delim
#' @export
dwwBD <- function(...) {uj::dww('¦', '.', ...)}

#' @rdname delim
#' @export
dwwBG <- function(...) {uj::dww('¦', ', ', ...)}

#' @rdname delim
#' @export
dwwBP <- function(...) {uj::dww('¦', '|', ...)}

#' @rdname delim
#' @export
dwwBQ <- function(...) {uj::dww('¦', '`', ...)}

#' @rdname delim
#' @export
dwwBS <- function(...) {uj::dww('¦', ',', ...)}

#' @rdname delim
#' @export
dwwBT <- function(...) {uj::dww('¦', '~', ...)}

#' @rdname delim
#' @export
dwwC0 <- function(...) {uj::dww(':', '', ...)}

#' @rdname delim
#' @export
dwwC1 <- function(...) {uj::dww(':', ' ', ...)}

#' @rdname delim
#' @export
dwwCB <- function(...) {uj::dww(':', '¦', ...)}

#' @rdname delim
#' @export
dwwCC <- function(...) {uj::dww(':', ':', ...)}

#' @rdname delim
#' @export
dwwCD <- function(...) {uj::dww(':', '.', ...)}

#' @rdname delim
#' @export
dwwCG <- function(...) {uj::dww(':', ', ', ...)}

#' @rdname delim
#' @export
dwwCP <- function(...) {uj::dww(':', '|', ...)}

#' @rdname delim
#' @export
dwwCQ <- function(...) {uj::dww(':', '`', ...)}

#' @rdname delim
#' @export
dwwCS <- function(...) {uj::dww(':', ',', ...)}

#' @rdname delim
#' @export
dwwCT <- function(...) {uj::dww(':', '~', ...)}

#' @rdname delim
#' @export
dwwD0 <- function(...) {uj::dww('.', '', ...)}

#' @rdname delim
#' @export
dwwD1 <- function(...) {uj::dww('.', ' ', ...)}

#' @rdname delim
#' @export
dwwDB <- function(...) {uj::dww('.', '¦', ...)}

#' @rdname delim
#' @export
dwwDC <- function(...) {uj::dww('.', ':', ...)}

#' @rdname delim
#' @export
dwwDD <- function(...) {uj::dww('.', '.', ...)}

#' @rdname delim
#' @export
dwwDG <- function(...) {uj::dww('.', ', ', ...)}

#' @rdname delim
#' @export
dwwDP <- function(...) {uj::dww('.', '|', ...)}

#' @rdname delim
#' @export
dwwDQ <- function(...) {uj::dww('.', '`', ...)}

#' @rdname delim
#' @export
dwwDS <- function(...) {uj::dww('.', ',', ...)}

#' @rdname delim
#' @export
dwwDT <- function(...) {uj::dww('.', '~', ...)}

#' @rdname delim
#' @export
dwwG0 <- function(...) {uj::dww(', ', '', ...)}

#' @rdname delim
#' @export
dwwG1 <- function(...) {uj::dww(', ', ' ', ...)}

#' @rdname delim
#' @export
dwwGB <- function(...) {uj::dww(', ', '¦', ...)}

#' @rdname delim
#' @export
dwwGC <- function(...) {uj::dww(', ', ':', ...)}

#' @rdname delim
#' @export
dwwGD <- function(...) {uj::dww(', ', '.', ...)}

#' @rdname delim
#' @export
dwwGG <- function(...) {uj::dww(', ', ', ', ...)}

#' @rdname delim
#' @export
dwwGP <- function(...) {uj::dww(', ', '|', ...)}

#' @rdname delim
#' @export
dwwGQ <- function(...) {uj::dww(', ', '`', ...)}

#' @rdname delim
#' @export
dwwGS <- function(...) {uj::dww(', ', ',', ...)}

#' @rdname delim
#' @export
dwwGT <- function(...) {uj::dww(', ', '~', ...)}

#' @rdname delim
#' @export
dwwP0 <- function(...) {uj::dww('|', '', ...)}

#' @rdname delim
#' @export
dwwP1 <- function(...) {uj::dww('|', ' ', ...)}

#' @rdname delim
#' @export
dwwPB <- function(...) {uj::dww('|', '¦', ...)}

#' @rdname delim
#' @export
dwwPC <- function(...) {uj::dww('|', ':', ...)}

#' @rdname delim
#' @export
dwwPD <- function(...) {uj::dww('|', '.', ...)}

#' @rdname delim
#' @export
dwwPG <- function(...) {uj::dww('|', ', ', ...)}

#' @rdname delim
#' @export
dwwPP <- function(...) {uj::dww('|', '|', ...)}

#' @rdname delim
#' @export
dwwPQ <- function(...) {uj::dww('|', '`', ...)}

#' @rdname delim
#' @export
dwwPS <- function(...) {uj::dww('|', ',', ...)}

#' @rdname delim
#' @export
dwwPT <- function(...) {uj::dww('|', '~', ...)}

#' @rdname delim
#' @export
dwwQ0 <- function(...) {uj::dww('`', '', ...)}

#' @rdname delim
#' @export
dwwQ1 <- function(...) {uj::dww('`', ' ', ...)}

#' @rdname delim
#' @export
dwwQB <- function(...) {uj::dww('`', '¦', ...)}

#' @rdname delim
#' @export
dwwQC <- function(...) {uj::dww('`', ':', ...)}

#' @rdname delim
#' @export
dwwQD <- function(...) {uj::dww('`', '.', ...)}

#' @rdname delim
#' @export
dwwQG <- function(...) {uj::dww('`', ', ', ...)}

#' @rdname delim
#' @export
dwwQP <- function(...) {uj::dww('`', '|', ...)}

#' @rdname delim
#' @export
dwwQQ <- function(...) {uj::dww('`', '`', ...)}

#' @rdname delim
#' @export
dwwQS <- function(...) {uj::dww('`', ',', ...)}

#' @rdname delim
#' @export
dwwQT <- function(...) {uj::dww('`', '~', ...)}

#' @rdname delim
#' @export
dwwS0 <- function(...) {uj::dww(',', '', ...)}

#' @rdname delim
#' @export
dwwS1 <- function(...) {uj::dww(',', ' ', ...)}

#' @rdname delim
#' @export
dwwSB <- function(...) {uj::dww(',', '¦', ...)}

#' @rdname delim
#' @export
dwwSC <- function(...) {uj::dww(',', ':', ...)}

#' @rdname delim
#' @export
dwwSD <- function(...) {uj::dww(',', '.', ...)}

#' @rdname delim
#' @export
dwwSG <- function(...) {uj::dww(',', ', ', ...)}

#' @rdname delim
#' @export
dwwSP <- function(...) {uj::dww(',', '|', ...)}

#' @rdname delim
#' @export
dwwSQ <- function(...) {uj::dww(',', '`', ...)}

#' @rdname delim
#' @export
dwwSS <- function(...) {uj::dww(',', ',', ...)}

#' @rdname delim
#' @export
dwwST <- function(...) {uj::dww(',', '~', ...)}

#' @rdname delim
#' @export
dwwT0 <- function(...) {uj::dww('~', '', ...)}

#' @rdname delim
#' @export
dwwT1 <- function(...) {uj::dww('~', ' ', ...)}

#' @rdname delim
#' @export
dwwTB <- function(...) {uj::dww('~', '¦', ...)}

#' @rdname delim
#' @export
dwwTC <- function(...) {uj::dww('~', ':', ...)}

#' @rdname delim
#' @export
dwwTD <- function(...) {uj::dww('~', '.', ...)}

#' @rdname delim
#' @export
dwwTG <- function(...) {uj::dww('~', ', ', ...)}

#' @rdname delim
#' @export
dwwTP <- function(...) {uj::dww('~', '|', ...)}

#' @rdname delim
#' @export
dwwTQ <- function(...) {uj::dww('~', '`', ...)}

#' @rdname delim
#' @export
dwwTS <- function(...) {uj::dww('~', ',', ...)}

#' @rdname delim
#' @export
dwwTT <- function(...) {uj::dww('~', '~', ...)}
