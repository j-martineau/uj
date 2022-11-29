.delim_errs <- function(args, a) {
  ns <- lengths(args$dots)
  two <- "D" %in% names(args)
  dots <- ...length() > 0
  pops <- f0(!dots, T, all(ns > 0))
  ccsd <- cmp_chr_scl(args$d)
  ccsD <- f0(!two, T, cmp_chr_scl(args$D))
  atms <- f0(!ccsd, T, all(sapply(args$dots, atm_vec)))
  recs  <- f0(a | !pops, T, recyclable_n(max(ns) / ns))
  c(f0(dots, NULL, "\n \u2022 [...] is empty."),
    f0(pops, NULL, "\n \u2022 An argument in [...] is of length 0."),
    f0(ccsd, NULL, "\n \u2022 [d] must be a complete character scalar (?cmp_chr_scl)."),
    f0(ccsD, NULL, "\n \u2022 [D] must be a complete character scalar (?cmp_chr_scl)."),
    f0(atms, NULL, "\n \u2022 Arguments in [...] must be atomic vecs (?pop_vec)."),
    f0(recs, NULL, "\n \u2022 Arguments in [...] are not of recyclable lengths (?recyclable)."))
}

#' @name delim
#' @family strings
#' @title Error-checked string delimiting
#' @description Simplified and extended `base::paste` and `base::paste0`. There are both primary functions with user-specified delimiters and convenience functions for common delimiters.
#' \cr\cr
#' **Primary Functions**
#' \itemize{
#'   \item **`da`**: Delimits across corresponding elements of (recyclable atomic vector) `...` arguments using delimiter `d`. Produces a character vector of `max(lengths(list(...)))` substrings delimited by `d`.
#'   \item **`dw`**: Delimits elements within each (atomic vector) `...` argument using delimiter `d`. Produces a character vector of `...length()` substrings delimited by `d`.
#'   \item **`daw`**: Delimit across corresponding elements of (recyclable atomic vector) `...` arguments using delimiter `d`, then delimit elements within the resulting character vector using delimiter `D`. Produces a character scalar of `...length()` substrings delimited by `D` where each substring contains `max(lengths(list(...)))` sub-substrings delimited by `d`.
#'   \item **`dww`**: Delimit elements within each (atomic vector) `...` argument using delimiter `d`, then delimit elements within the resulting vector using delimiter `D`. Produces a character scalar of `...length()` substrings delimited by `D` where each substring contains sub-substrings delimited by `d`.
#' }
#' **Common-Delimiter Convenience Functions**
#' \cr Convenience function names are constructed by append `1` or `2` codes for common delimiters to the function name as follows (where `X` and `Y` are placeholders for common-delimiter codes):
#' \itemize{
#'   \item **`daX`**: Delimits across with `X`.
#'   \item **`dwX`**: Delimits within with `X`.
#'   \item **`dawXY`**: Delimits across with `X` then within with `Y`.
#'   \item **`dwwXY`**: Delimits within with `X` then again with `Y`.
#' }
#' Common delimiters are encoded as:
#' \tabular{lll}{
#'   CODE        \tab NAME           \tab DELIMITER INVOKED
#'   \cr `'0'`   \tab blank          \tab `''`
#'   \cr `'1'`   \tab space          \tab `' '`
#'   \cr `'B'`   \tab broken pipe    \tab `'¦'`
#'   \cr `'C'`   \tab colon          \tab `':'`
#'   \cr `'D'`   \tab dot            \tab `'.'`
#'   \cr `'G'`   \tab grammatical    \tab`', '`
#'   \cr         \tab comma          \tab  
#'   \cr `'P'`   \tab pipe           \tab `'|'`
#'   \cr `'Q'`   \tab back-tick      \tab `'``'`
#'   \cr `'S'`   \tab simple comma   \tab `','`
#'   \cr `'T'`   \tab tilde          \tab `'~'`
#' }
#' 'Grammatical comma' vs. 'simple comma' indicates whether the function produces grammatical comma-delimited lists vs. simple comma-delimited values (e.g., `'1, 2, 3'` vs. `'1,2,3'`).
#' @param ... An arbitrary number of atomic vector arguments to be delimited. Argument in `...` must be recyclable for functions that delimit across `...` arguments as the first or only step (i.e., functions with names beginning with `da`).
#' @param d,D \link[=chr_scl]{Character scalar} delimiters.
#' @return \itemize{
#'   \item **`da, dw, daX, daw, dww`**: a character scalar.
#'   \item **`dawXY, dwwXY`**: a character scalar.
#' }
#' @export
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
da <- function(d, ...) {
  errs <- .delim_errs(list(d = d, dots = list(...)), TRUE)
  if (!is.null(errs)) {stop(errs)}
  paste(..., sep = d)
}

#' @rdname delim
#' @export
dw <- function(d, ...) {
  errs <- .delim_errs(list(d = d, dots = list(...)), FALSE)
  if (!is.null(errs)) {stop(errs)}
  sapply(list(...), paste0, collapse = d)
}

#' @rdname delim
#' @export
daw <- function(d, D, ...) {
  errs <- .delim_errs(list(d = d, D = D, dots = list(...)), TRUE)
  if (!is.null(errs)) {stop(errs)}
  paste0(paste(..., sep = d), collapse = D)
}

#' @rdname delim
#' @export
dww <- function(d, D, ...) {
  errs <- .delim_errs(list(d = d, D = D, dots = list(...)), FALSE)
  if (!is.null(errs)) {stop(errs)}
  paste(sapply(list(...), paste0, collapse = d), sep = D)
}

#' @rdname delim
#' @export
da0 <- function(...) {da('', ...)}

#' @rdname delim
#' @export
da1 <- function(...) {da(' ', ...)}

#' @rdname delim
#' @export
daB <- function(...) {da('¦', ...)}

#' @rdname delim
#' @export
daC <- function(...) {da(':', ...)}

#' @rdname delim
#' @export
daD <- function(...) {da('.', ...)}

#' @rdname delim
#' @export
daG <- function(...) {da(', ', ...)}

#' @rdname delim
#' @export
daP <- function(...) {da('|', ...)}

#' @rdname delim
#' @export
daQ <- function(...) {da('`', ...)}

#' @rdname delim
#' @export
daS <- function(...) {da(',', ...)}

#' @rdname delim
#' @export
daT <- function(...) {da('~', ...)}

#' @rdname delim
#' @export
dw0 <- function(...) {dw('', ...)}

#' @rdname delim
#' @export
dw1 <- function(...) {dw(' ', ...)}

#' @rdname delim
#' @export
dwB <- function(...) {dw('¦', ...)}

#' @rdname delim
#' @export
dwC <- function(...) {dw(':', ...)}

#' @rdname delim
#' @export
dwD <- function(...) {dw('.', ...)}

#' @rdname delim
#' @export
dwG <- function(...) {dw(', ', ...)}

#' @rdname delim
#' @export
dwP <- function(...) {dw('|', ...)}

#' @rdname delim
#' @export
dwQ <- function(...) {dw('`', ...)}

#' @rdname delim
#' @export
dwS <- function(...) {dw(',', ...)}

#' @rdname delim
#' @export
dwT <- function(...) {dw('~', ...)}

#' @rdname delim
#' @export
daw00 <- function(...) {daw('', '', ...)}

#' @rdname delim
#' @export
daw01 <- function(...) {daw('', ' ', ...)}

#' @rdname delim
#' @export
daw0B <- function(...) {daw('', '¦', ...)}

#' @rdname delim
#' @export
daw0C <- function(...) {daw('', ':', ...)}

#' @rdname delim
#' @export
daw0D <- function(...) {daw('', '.', ...)}

#' @rdname delim
#' @export
daw0G <- function(...) {daw('', ', ', ...)}

#' @rdname delim
#' @export
daw0P <- function(...) {daw('', '|', ...)}

#' @rdname delim
#' @export
daw0Q <- function(...) {daw('', '`', ...)}

#' @rdname delim
#' @export
daw0S <- function(...) {daw('', ',', ...)}

#' @rdname delim
#' @export
daw0T <- function(...) {daw('', '~', ...)}

#' @rdname delim
#' @export
daw10 <- function(...) {daw(' ', '', ...)}

#' @rdname delim
#' @export
daw11 <- function(...) {daw(' ', ' ', ...)}

#' @rdname delim
#' @export
daw1B <- function(...) {daw(' ', '¦', ...)}

#' @rdname delim
#' @export
daw1C <- function(...) {daw(' ', ':', ...)}

#' @rdname delim
#' @export
daw1D <- function(...) {daw(' ', '.', ...)}

#' @rdname delim
#' @export
daw1G <- function(...) {daw(' ', ', ', ...)}

#' @rdname delim
#' @export
daw1P <- function(...) {daw(' ', '|', ...)}

#' @rdname delim
#' @export
daw1Q <- function(...) {daw(' ', '`', ...)}

#' @rdname delim
#' @export
daw1S <- function(...) {daw(' ', ',', ...)}

#' @rdname delim
#' @export
daw1T <- function(...) {daw(' ', '~', ...)}

#' @rdname delim
#' @export
dawB0 <- function(...) {daw('¦', '', ...)}

#' @rdname delim
#' @export
dawB1 <- function(...) {daw('¦', ' ', ...)}

#' @rdname delim
#' @export
dawBB <- function(...) {daw('¦', '¦', ...)}

#' @rdname delim
#' @export
dawBC <- function(...) {daw('¦', ':', ...)}

#' @rdname delim
#' @export
dawBD <- function(...) {daw('¦', '.', ...)}

#' @rdname delim
#' @export
dawBG <- function(...) {daw('¦', ', ', ...)}

#' @rdname delim
#' @export
dawBP <- function(...) {daw('¦', '|', ...)}

#' @rdname delim
#' @export
dawBQ <- function(...) {daw('¦', '`', ...)}

#' @rdname delim
#' @export
dawBS <- function(...) {daw('¦', ',', ...)}

#' @rdname delim
#' @export
dawBT <- function(...) {daw('¦', '~', ...)}

#' @rdname delim
#' @export
dawC0 <- function(...) {daw(':', '', ...)}

#' @rdname delim
#' @export
dawC1 <- function(...) {daw(':', ' ', ...)}

#' @rdname delim
#' @export
dawCB <- function(...) {daw(':', '¦', ...)}

#' @rdname delim
#' @export
dawCC <- function(...) {daw(':', ':', ...)}

#' @rdname delim
#' @export
dawCD <- function(...) {daw(':', '.', ...)}

#' @rdname delim
#' @export
dawCG <- function(...) {daw(':', ', ', ...)}

#' @rdname delim
#' @export
dawCP <- function(...) {daw(':', '|', ...)}

#' @rdname delim
#' @export
dawCQ <- function(...) {daw(':', '`', ...)}

#' @rdname delim
#' @export
dawCS <- function(...) {daw(':', ',', ...)}

#' @rdname delim
#' @export
dawCT <- function(...) {daw(':', '~', ...)}

#' @rdname delim
#' @export
dawD0 <- function(...) {daw('.', '', ...)}

#' @rdname delim
#' @export
dawD1 <- function(...) {daw('.', ' ', ...)}

#' @rdname delim
#' @export
dawDB <- function(...) {daw('.', '¦', ...)}

#' @rdname delim
#' @export
dawDC <- function(...) {daw('.', ':', ...)}

#' @rdname delim
#' @export
dawDD <- function(...) {daw('.', '.', ...)}

#' @rdname delim
#' @export
dawDG <- function(...) {daw('.', ', ', ...)}

#' @rdname delim
#' @export
dawDP <- function(...) {daw('.', '|', ...)}

#' @rdname delim
#' @export
dawDQ <- function(...) {daw('.', '`', ...)}

#' @rdname delim
#' @export
dawDS <- function(...) {daw('.', ',', ...)}

#' @rdname delim
#' @export
dawDT <- function(...) {daw('.', '~', ...)}

#' @rdname delim
#' @export
dawG0 <- function(...) {daw(', ', '', ...)}

#' @rdname delim
#' @export
dawG1 <- function(...) {daw(', ', ' ', ...)}

#' @rdname delim
#' @export
dawGB <- function(...) {daw(', ', '¦', ...)}

#' @rdname delim
#' @export
dawGC <- function(...) {daw(', ', ':', ...)}

#' @rdname delim
#' @export
dawGD <- function(...) {daw(', ', '.', ...)}

#' @rdname delim
#' @export
dawGG <- function(...) {daw(', ', ', ', ...)}

#' @rdname delim
#' @export
dawGP <- function(...) {daw(', ', '|', ...)}

#' @rdname delim
#' @export
dawGQ <- function(...) {daw(', ', '`', ...)}

#' @rdname delim
#' @export
dawGS <- function(...) {daw(', ', ',', ...)}

#' @rdname delim
#' @export
dawGT <- function(...) {daw(', ', '~', ...)}

#' @rdname delim
#' @export
dawP0 <- function(...) {daw('|', '', ...)}

#' @rdname delim
#' @export
dawP1 <- function(...) {daw('|', ' ', ...)}

#' @rdname delim
#' @export
dawPB <- function(...) {daw('|', '¦', ...)}

#' @rdname delim
#' @export
dawPC <- function(...) {daw('|', ':', ...)}

#' @rdname delim
#' @export
dawPD <- function(...) {daw('|', '.', ...)}

#' @rdname delim
#' @export
dawPG <- function(...) {daw('|', ', ', ...)}

#' @rdname delim
#' @export
dawPP <- function(...) {daw('|', '|', ...)}

#' @rdname delim
#' @export
dawPQ <- function(...) {daw('|', '`', ...)}

#' @rdname delim
#' @export
dawPS <- function(...) {daw('|', ',', ...)}

#' @rdname delim
#' @export
dawPT <- function(...) {daw('|', '~', ...)}

#' @rdname delim
#' @export
dawQ0 <- function(...) {daw('`', '', ...)}

#' @rdname delim
#' @export
dawQ1 <- function(...) {daw('`', ' ', ...)}

#' @rdname delim
#' @export
dawQB <- function(...) {daw('`', '¦', ...)}

#' @rdname delim
#' @export
dawQC <- function(...) {daw('`', ':', ...)}

#' @rdname delim
#' @export
dawQD <- function(...) {daw('`', '.', ...)}

#' @rdname delim
#' @export
dawQG <- function(...) {daw('`', ', ', ...)}

#' @rdname delim
#' @export
dawQP <- function(...) {daw('`', '|', ...)}

#' @rdname delim
#' @export
dawQQ <- function(...) {daw('`', '`', ...)}

#' @rdname delim
#' @export
dawQS <- function(...) {daw('`', ',', ...)}

#' @rdname delim
#' @export
dawQT <- function(...) {daw('`', '~', ...)}

#' @rdname delim
#' @export
dawS0 <- function(...) {daw(',', '', ...)}

#' @rdname delim
#' @export
dawS1 <- function(...) {daw(',', ' ', ...)}

#' @rdname delim
#' @export
dawSB <- function(...) {daw(',', '¦', ...)}

#' @rdname delim
#' @export
dawSC <- function(...) {daw(',', ':', ...)}

#' @rdname delim
#' @export
dawSD <- function(...) {daw(',', '.', ...)}

#' @rdname delim
#' @export
dawSG <- function(...) {daw(',', ', ', ...)}

#' @rdname delim
#' @export
dawSP <- function(...) {daw(',', '|', ...)}

#' @rdname delim
#' @export
dawSQ <- function(...) {daw(',', '`', ...)}

#' @rdname delim
#' @export
dawSS <- function(...) {daw(',', ',', ...)}

#' @rdname delim
#' @export
dawST <- function(...) {daw(',', '~', ...)}

#' @rdname delim
#' @export
dawT0 <- function(...) {daw('~', '', ...)}

#' @rdname delim
#' @export
dawT1 <- function(...) {daw('~', ' ', ...)}

#' @rdname delim
#' @export
dawTB <- function(...) {daw('~', '¦', ...)}

#' @rdname delim
#' @export
dawTC <- function(...) {daw('~', ':', ...)}

#' @rdname delim
#' @export
dawTD <- function(...) {daw('~', '.', ...)}

#' @rdname delim
#' @export
dawTG <- function(...) {daw('~', ', ', ...)}

#' @rdname delim
#' @export
dawTP <- function(...) {daw('~', '|', ...)}

#' @rdname delim
#' @export
dawTQ <- function(...) {daw('~', '`', ...)}

#' @rdname delim
#' @export
dawTS <- function(...) {daw('~', ',', ...)}

#' @rdname delim
#' @export
dawTT <- function(...) {daw('~', '~', ...)}

#' @rdname delim
#' @export
dww00 <- function(...) {dww('', '', ...)}

#' @rdname delim
#' @export
dww01 <- function(...) {dww('', ' ', ...)}

#' @rdname delim
#' @export
dww0B <- function(...) {dww('', '¦', ...)}

#' @rdname delim
#' @export
dww0C <- function(...) {dww('', ':', ...)}

#' @rdname delim
#' @export
dww0D <- function(...) {dww('', '.', ...)}

#' @rdname delim
#' @export
dww0G <- function(...) {dww('', ', ', ...)}

#' @rdname delim
#' @export
dww0P <- function(...) {dww('', '|', ...)}

#' @rdname delim
#' @export
dww0Q <- function(...) {dww('', '`', ...)}

#' @rdname delim
#' @export
dww0S <- function(...) {dww('', ',', ...)}

#' @rdname delim
#' @export
dww0T <- function(...) {dww('', '~', ...)}

#' @rdname delim
#' @export
dww10 <- function(...) {dww(' ', '', ...)}

#' @rdname delim
#' @export
dww11 <- function(...) {dww(' ', ' ', ...)}

#' @rdname delim
#' @export
dww1B <- function(...) {dww(' ', '¦', ...)}

#' @rdname delim
#' @export
dww1C <- function(...) {dww(' ', ':', ...)}

#' @rdname delim
#' @export
dww1D <- function(...) {dww(' ', '.', ...)}

#' @rdname delim
#' @export
dww1G <- function(...) {dww(' ', ', ', ...)}

#' @rdname delim
#' @export
dww1P <- function(...) {dww(' ', '|', ...)}

#' @rdname delim
#' @export
dww1Q <- function(...) {dww(' ', '`', ...)}

#' @rdname delim
#' @export
dww1S <- function(...) {dww(' ', ',', ...)}

#' @rdname delim
#' @export
dww1T <- function(...) {dww(' ', '~', ...)}

#' @rdname delim
#' @export
dwwB0 <- function(...) {dww('¦', '', ...)}

#' @rdname delim
#' @export
dwwB1 <- function(...) {dww('¦', ' ', ...)}

#' @rdname delim
#' @export
dwwBB <- function(...) {dww('¦', '¦', ...)}

#' @rdname delim
#' @export
dwwBC <- function(...) {dww('¦', ':', ...)}

#' @rdname delim
#' @export
dwwBD <- function(...) {dww('¦', '.', ...)}

#' @rdname delim
#' @export
dwwBG <- function(...) {dww('¦', ', ', ...)}

#' @rdname delim
#' @export
dwwBP <- function(...) {dww('¦', '|', ...)}

#' @rdname delim
#' @export
dwwBQ <- function(...) {dww('¦', '`', ...)}

#' @rdname delim
#' @export
dwwBS <- function(...) {dww('¦', ',', ...)}

#' @rdname delim
#' @export
dwwBT <- function(...) {dww('¦', '~', ...)}

#' @rdname delim
#' @export
dwwC0 <- function(...) {dww(':', '', ...)}

#' @rdname delim
#' @export
dwwC1 <- function(...) {dww(':', ' ', ...)}

#' @rdname delim
#' @export
dwwCB <- function(...) {dww(':', '¦', ...)}

#' @rdname delim
#' @export
dwwCC <- function(...) {dww(':', ':', ...)}

#' @rdname delim
#' @export
dwwCD <- function(...) {dww(':', '.', ...)}

#' @rdname delim
#' @export
dwwCG <- function(...) {dww(':', ', ', ...)}

#' @rdname delim
#' @export
dwwCP <- function(...) {dww(':', '|', ...)}

#' @rdname delim
#' @export
dwwCQ <- function(...) {dww(':', '`', ...)}

#' @rdname delim
#' @export
dwwCS <- function(...) {dww(':', ',', ...)}

#' @rdname delim
#' @export
dwwCT <- function(...) {dww(':', '~', ...)}

#' @rdname delim
#' @export
dwwD0 <- function(...) {dww('.', '', ...)}

#' @rdname delim
#' @export
dwwD1 <- function(...) {dww('.', ' ', ...)}

#' @rdname delim
#' @export
dwwDB <- function(...) {dww('.', '¦', ...)}

#' @rdname delim
#' @export
dwwDC <- function(...) {dww('.', ':', ...)}

#' @rdname delim
#' @export
dwwDD <- function(...) {dww('.', '.', ...)}

#' @rdname delim
#' @export
dwwDG <- function(...) {dww('.', ', ', ...)}

#' @rdname delim
#' @export
dwwDP <- function(...) {dww('.', '|', ...)}

#' @rdname delim
#' @export
dwwDQ <- function(...) {dww('.', '`', ...)}

#' @rdname delim
#' @export
dwwDS <- function(...) {dww('.', ',', ...)}

#' @rdname delim
#' @export
dwwDT <- function(...) {dww('.', '~', ...)}

#' @rdname delim
#' @export
dwwG0 <- function(...) {dww(', ', '', ...)}

#' @rdname delim
#' @export
dwwG1 <- function(...) {dww(', ', ' ', ...)}

#' @rdname delim
#' @export
dwwGB <- function(...) {dww(', ', '¦', ...)}

#' @rdname delim
#' @export
dwwGC <- function(...) {dww(', ', ':', ...)}

#' @rdname delim
#' @export
dwwGD <- function(...) {dww(', ', '.', ...)}

#' @rdname delim
#' @export
dwwGG <- function(...) {dww(', ', ', ', ...)}

#' @rdname delim
#' @export
dwwGP <- function(...) {dww(', ', '|', ...)}

#' @rdname delim
#' @export
dwwGQ <- function(...) {dww(', ', '`', ...)}

#' @rdname delim
#' @export
dwwGS <- function(...) {dww(', ', ',', ...)}

#' @rdname delim
#' @export
dwwGT <- function(...) {dww(', ', '~', ...)}

#' @rdname delim
#' @export
dwwP0 <- function(...) {dww('|', '', ...)}

#' @rdname delim
#' @export
dwwP1 <- function(...) {dww('|', ' ', ...)}

#' @rdname delim
#' @export
dwwPB <- function(...) {dww('|', '¦', ...)}

#' @rdname delim
#' @export
dwwPC <- function(...) {dww('|', ':', ...)}

#' @rdname delim
#' @export
dwwPD <- function(...) {dww('|', '.', ...)}

#' @rdname delim
#' @export
dwwPG <- function(...) {dww('|', ', ', ...)}

#' @rdname delim
#' @export
dwwPP <- function(...) {dww('|', '|', ...)}

#' @rdname delim
#' @export
dwwPQ <- function(...) {dww('|', '`', ...)}

#' @rdname delim
#' @export
dwwPS <- function(...) {dww('|', ',', ...)}

#' @rdname delim
#' @export
dwwPT <- function(...) {dww('|', '~', ...)}

#' @rdname delim
#' @export
dwwQ0 <- function(...) {dww('`', '', ...)}

#' @rdname delim
#' @export
dwwQ1 <- function(...) {dww('`', ' ', ...)}

#' @rdname delim
#' @export
dwwQB <- function(...) {dww('`', '¦', ...)}

#' @rdname delim
#' @export
dwwQC <- function(...) {dww('`', ':', ...)}

#' @rdname delim
#' @export
dwwQD <- function(...) {dww('`', '.', ...)}

#' @rdname delim
#' @export
dwwQG <- function(...) {dww('`', ', ', ...)}

#' @rdname delim
#' @export
dwwQP <- function(...) {dww('`', '|', ...)}

#' @rdname delim
#' @export
dwwQQ <- function(...) {dww('`', '`', ...)}

#' @rdname delim
#' @export
dwwQS <- function(...) {dww('`', ',', ...)}

#' @rdname delim
#' @export
dwwQT <- function(...) {dww('`', '~', ...)}

#' @rdname delim
#' @export
dwwS0 <- function(...) {dww(',', '', ...)}

#' @rdname delim
#' @export
dwwS1 <- function(...) {dww(',', ' ', ...)}

#' @rdname delim
#' @export
dwwSB <- function(...) {dww(',', '¦', ...)}

#' @rdname delim
#' @export
dwwSC <- function(...) {dww(',', ':', ...)}

#' @rdname delim
#' @export
dwwSD <- function(...) {dww(',', '.', ...)}

#' @rdname delim
#' @export
dwwSG <- function(...) {dww(',', ', ', ...)}

#' @rdname delim
#' @export
dwwSP <- function(...) {dww(',', '|', ...)}

#' @rdname delim
#' @export
dwwSQ <- function(...) {dww(',', '`', ...)}

#' @rdname delim
#' @export
dwwSS <- function(...) {dww(',', ',', ...)}

#' @rdname delim
#' @export
dwwST <- function(...) {dww(',', '~', ...)}

#' @rdname delim
#' @export
dwwT0 <- function(...) {dww('~', '', ...)}

#' @rdname delim
#' @export
dwwT1 <- function(...) {dww('~', ' ', ...)}

#' @rdname delim
#' @export
dwwTB <- function(...) {dww('~', '¦', ...)}

#' @rdname delim
#' @export
dwwTC <- function(...) {dww('~', ':', ...)}

#' @rdname delim
#' @export
dwwTD <- function(...) {dww('~', '.', ...)}

#' @rdname delim
#' @export
dwwTG <- function(...) {dww('~', ', ', ...)}

#' @rdname delim
#' @export
dwwTP <- function(...) {dww('~', '|', ...)}

#' @rdname delim
#' @export
dwwTQ <- function(...) {dww('~', '`', ...)}

#' @rdname delim
#' @export
dwwTS <- function(...) {dww('~', ',', ...)}

#' @rdname delim
#' @export
dwwTT <- function(...) {dww('~', '~', ...)}
