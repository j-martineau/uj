# @title Evaluate arguments of the [delim.] family of functions
# @param args A vlist containing the element [dots = list(...)], the element [d
#   = d], and possibly containing the element [D = D].
# @param a Whether the name of the calling function starts with [da].
# @return Either NULL or a character vector of argument property errors.
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

#' @name delim.
#' @family strings
#' @title Error-Checked String Delimiting (variations on a theme of
#'   \code{paste}).
#' @description Simplifies and extends \code{paste} and \code{paste0}, allowing
#'   for 'delimiting across' and 'delimiting within' arguments in \code{...},
#'   and a combination of delimiting within and across arguments, in any order.
#'   Also offers convenience functions for specific delimiters.
#'   \cr\cr
#'   \strong{\code{da(d, ...)}}
#'   \cr Delimits across corresponding elements of arguments in \code{...} using
#'   the delimiter \code{d}. Thus, \code{da("|", 1:3, 4:6)} produces
#'   \code{c('1|4', '2|5', '3|6')}, which is also the result of calling
#'   \code{paste(1:3, 4:6, sep = '|')}.
#'   \cr\cr
#'   \strong{\code{dw(d, ...)}}
#'   \cr Delimits within arguments in \code{...} using the delimiter \code{d}.
#'   Thus, \code{dw(":", 1:3, 4:6)} produces \code{c('1:2:3', '4:5:6')}, which
#'   is also the result of calling \code{sapply(list(1:3, 4:6), paste0, collapse
#'   = "|"}).
#'   \cr\cr
#'   \strong{\code{daw(d, D, ...)}}
#'   \cr Delimits across arguments in \code{...} using the first delimiter
#'   \code{d}, then delimits within the resulting vector using the delimiter
#'   \code{D}. Thus, \code{daw('|', ':', 1:3, 4:6)} produces
#'   \code{'1|4:2|5:3:6'}, which is also the result of calling \code{paste(1:3,
#'   4:6, sep = '|', collapse = ':')}.
#'   \cr\cr
#'   \strong{\code{dww(d, D, ...)}}
#'   \cr Delimits within arguments in \code{...} using the delimiter \code{d},
#'   then delimits within the resulting vector using the delimiter \code{D}.
#'   Thus, \code{dww(':', '|', 1:3, 4:6)} produces \code{'1:2:3|4:5:6'}, which
#'   is also the result of calling \code{paste0(sapply(list(1:3, 4:6), paste0,
#'   collapse = ":"), collapse = "|")}.
#'   \cr\cr
#'   \strong{Delimiter-Specific Extension Functions}
#'   \cr Delimiter-specific functions are available for convenience for common
#'   delimiters and for any combination of two common delimiters. Common
#'   delimiters and their extension characters are shown in the following
#'   table:\tabular{lll}{
#'     EXTENSION   \tab EXTENSION             \tab DELIMITER                 \cr
#'     CHARACTER   \tab DESCRIPTION           \tab INVOKED                   \cr
#'     \code{'0'}  \tab Blank                 \tab\code{""}                  \cr
#'     \code{'1'}  \tab Space                 \tab\code{" "}                 \cr
#'     \code{'B'}  \tab Broken pipe           \tab\code{"¦"}                 \cr
#'     \code{'C'}  \tab Colon                 \tab\code{":"}                 \cr
#'     \code{'D'}  \tab Dot                   \tab\code{"."}                 \cr
#'     \code{'G'}  \tab Grammatical comma*    \tab\code{", "}                \cr
#'     \code{'P'}  \tab Pipe                  \tab\code{"|"}                 \cr
#'     \code{'Q'}  \tab Back quote (back tick)\tab\code{"`"}                 \cr
#'     \code{'S'}  \tab Simple comma^         \tab\code{","}                 \cr
#'     \code{'T'}  \tab Tilde                 \tab\code{"~"}                   }
#'   NOTE: 'Grammatical comma' vs. 'simple comma' indicates whether the function
#'   produces grammatical comma-separated lists vs. simple comma-separated
#'   values (e.g., \code{'1, 2, 3'} vs. \code{'1,2,3'}).
#'   \cr\cr
#'   \strong{Extension Functions \code{da[d]}}
#'   \cr Delimits across arguments in \code{...} using the delimiter invoked by
#'   the extension character \code{[d]}. Thus, \code{daP(1:3, 4:6)} produces
#'   \code{c('1|4', '2|5', '3|6')}, which is also the result of calling
#'   \code{paste(1:3, 4:6, sep = '|')}.
#'   \cr\cr
#'   \strong{Extension Functions \code{dw[d]}}
#'   \cr Delimits within arguments in \code{...} using the delimiter invoked by
#'   the extension character \code{[d]}. Thus, \code{dwC(1:3, 4:6)} produces
#'   \code{c('1:2:3', '4:5:6')}, which is also the result of calling
#'   \code{sapply(list(1:3, 4:6), paste0, collapse = ":")}.
#'   \cr\cr
#'   \strong{Extension Functions \code{daw[d][D]}}
#'   \cr Delimits across arguments in \code{...} using the delimiter invoked by
#'   the extension character \code{[d]} and then delimits within the resulting
#'   vector using the delimiter invoked by the extension character \code{[D]}.
#'   Thus, \code{dawPC(1:3, 4:6)} produces \code{'1|4:2|5:3|6'}, which is also
#'   the result of calling \code{paste(1:3, 4:6, sep = "|", collapse = ":")}.
#'   \cr\cr
#'   \strong{Extension Functions \code{dww[d][D]}}
#'   \cr Delimits within using the delimiter invoked by the extension
#'   character \code{[d]} and then delimits within the resulting vector using
#'   the delimiter invoked by the extension character \code{[D]}. Thus,
#'   \code{dwwCP(1:3, 4:6)} produces \code{'1:2:3|4:5:6'}, which is also the
#'   result of calling \code{paste0(sapply(list(1:3, 4:6), paste0, collapse =
#'   ":"), collapse = "|")}.
#' @param ... An arbitrary number of atomic vector arguments to be delimited.
#'   Argument in \code{...} must be recyclable for functions that delimit across
#'   arguments in \code{...} as the first or only step (i.e., functions with
#'   names beginning with \code{da}).
#' @param d,D \link[=chr_scl]{Character scalar} delimiters.
#' @return \tabular{lll}{
#'   \code{da}, \code{dw}, \code{da[d]}, and \code{dw[d]}
#'        \tab    \tab A character vector.                                   \cr
#'   \code{daw}, \code{dww}, \code{daw[d][D]}, and code{dww[d][D]}
#'        \tab    \tab A character scalar.                                     }
#' @export
delim. <- function() {help("delim.", package = "uj")}

#' @rdname delim.
#' @export
da <- function(d, ...) {
  errs <- .delim_errs(list(d = d, dots = list(...)), TRUE)
  if (idef(errs)) {stop(errs)}
  paste(..., sep = d)
}

#' @rdname delim.
#' @export
dw <- function(d, ...) {
  errs <- .delim_errs(list(d = d, dots = list(...)), FALSE)
  if (idef(errs)) {stop(errs)}
  sapply(list(...), paste0, collapse = d)
}

#' @rdname delim.
#' @export
daw <- function(d, D, ...) {
  errs <- .delim_errs(list(d = d, D = D, dots = list(...)), TRUE)
  if (idef(errs)) {stop(errs)}
  paste0(paste(..., sep = d), collapse = D)
}

#' @rdname delim.
#' @export
dww <- function(d, D, ...) {
  errs <- .delim_errs(list(d = d, D = D, dots = list(...)), FALSE)
  if (idef(errs)) {stop(errs)}
  paste(sapply(list(...), paste0, collapse = d), sep = D)
}

#' @rdname delim.
#' @export
da0 <- function(...) {da('', ...)}

#' @rdname delim.
#' @export
da1 <- function(...) {da(' ', ...)}

#' @rdname delim.
#' @export
daB <- function(...) {da('¦', ...)}

#' @rdname delim.
#' @export
daC <- function(...) {da(':', ...)}

#' @rdname delim.
#' @export
daD <- function(...) {da('.', ...)}

#' @rdname delim.
#' @export
daG <- function(...) {da(', ', ...)}

#' @rdname delim.
#' @export
daP <- function(...) {da('|', ...)}

#' @rdname delim.
#' @export
daQ <- function(...) {da('`', ...)}

#' @rdname delim.
#' @export
daS <- function(...) {da(',', ...)}

#' @rdname delim.
#' @export
daT <- function(...) {da('~', ...)}

#' @rdname delim.
#' @export
dw0 <- function(...) {dw('', ...)}

#' @rdname delim.
#' @export
dw1 <- function(...) {dw(' ', ...)}

#' @rdname delim.
#' @export
dwB <- function(...) {dw('¦', ...)}

#' @rdname delim.
#' @export
dwC <- function(...) {dw(':', ...)}

#' @rdname delim.
#' @export
dwD <- function(...) {dw('.', ...)}

#' @rdname delim.
#' @export
dwG <- function(...) {dw(', ', ...)}

#' @rdname delim.
#' @export
dwP <- function(...) {dw('|', ...)}

#' @rdname delim.
#' @export
dwQ <- function(...) {dw('`', ...)}

#' @rdname delim.
#' @export
dwS <- function(...) {dw(',', ...)}

#' @rdname delim.
#' @export
dwT <- function(...) {dw('~', ...)}

#' @rdname delim.
#' @export
daw00 <- function(...) {daw('', '', ...)}

#' @rdname delim.
#' @export
daw01 <- function(...) {daw('', ' ', ...)}

#' @rdname delim.
#' @export
daw0B <- function(...) {daw('', '¦', ...)}

#' @rdname delim.
#' @export
daw0C <- function(...) {daw('', ':', ...)}

#' @rdname delim.
#' @export
daw0D <- function(...) {daw('', '.', ...)}

#' @rdname delim.
#' @export
daw0G <- function(...) {daw('', ', ', ...)}

#' @rdname delim.
#' @export
daw0P <- function(...) {daw('', '|', ...)}

#' @rdname delim.
#' @export
daw0Q <- function(...) {daw('', '`', ...)}

#' @rdname delim.
#' @export
daw0S <- function(...) {daw('', ',', ...)}

#' @rdname delim.
#' @export
daw0T <- function(...) {daw('', '~', ...)}

#' @rdname delim.
#' @export
daw10 <- function(...) {daw(' ', '', ...)}

#' @rdname delim.
#' @export
daw11 <- function(...) {daw(' ', ' ', ...)}

#' @rdname delim.
#' @export
daw1B <- function(...) {daw(' ', '¦', ...)}

#' @rdname delim.
#' @export
daw1C <- function(...) {daw(' ', ':', ...)}

#' @rdname delim.
#' @export
daw1D <- function(...) {daw(' ', '.', ...)}

#' @rdname delim.
#' @export
daw1G <- function(...) {daw(' ', ', ', ...)}

#' @rdname delim.
#' @export
daw1P <- function(...) {daw(' ', '|', ...)}

#' @rdname delim.
#' @export
daw1Q <- function(...) {daw(' ', '`', ...)}

#' @rdname delim.
#' @export
daw1S <- function(...) {daw(' ', ',', ...)}

#' @rdname delim.
#' @export
daw1T <- function(...) {daw(' ', '~', ...)}

#' @rdname delim.
#' @export
dawB0 <- function(...) {daw('¦', '', ...)}

#' @rdname delim.
#' @export
dawB1 <- function(...) {daw('¦', ' ', ...)}

#' @rdname delim.
#' @export
dawBB <- function(...) {daw('¦', '¦', ...)}

#' @rdname delim.
#' @export
dawBC <- function(...) {daw('¦', ':', ...)}

#' @rdname delim.
#' @export
dawBD <- function(...) {daw('¦', '.', ...)}

#' @rdname delim.
#' @export
dawBG <- function(...) {daw('¦', ', ', ...)}

#' @rdname delim.
#' @export
dawBP <- function(...) {daw('¦', '|', ...)}

#' @rdname delim.
#' @export
dawBQ <- function(...) {daw('¦', '`', ...)}

#' @rdname delim.
#' @export
dawBS <- function(...) {daw('¦', ',', ...)}

#' @rdname delim.
#' @export
dawBT <- function(...) {daw('¦', '~', ...)}

#' @rdname delim.
#' @export
dawC0 <- function(...) {daw(':', '', ...)}

#' @rdname delim.
#' @export
dawC1 <- function(...) {daw(':', ' ', ...)}

#' @rdname delim.
#' @export
dawCB <- function(...) {daw(':', '¦', ...)}

#' @rdname delim.
#' @export
dawCC <- function(...) {daw(':', ':', ...)}

#' @rdname delim.
#' @export
dawCD <- function(...) {daw(':', '.', ...)}

#' @rdname delim.
#' @export
dawCG <- function(...) {daw(':', ', ', ...)}

#' @rdname delim.
#' @export
dawCP <- function(...) {daw(':', '|', ...)}

#' @rdname delim.
#' @export
dawCQ <- function(...) {daw(':', '`', ...)}

#' @rdname delim.
#' @export
dawCS <- function(...) {daw(':', ',', ...)}

#' @rdname delim.
#' @export
dawCT <- function(...) {daw(':', '~', ...)}

#' @rdname delim.
#' @export
dawD0 <- function(...) {daw('.', '', ...)}

#' @rdname delim.
#' @export
dawD1 <- function(...) {daw('.', ' ', ...)}

#' @rdname delim.
#' @export
dawDB <- function(...) {daw('.', '¦', ...)}

#' @rdname delim.
#' @export
dawDC <- function(...) {daw('.', ':', ...)}

#' @rdname delim.
#' @export
dawDD <- function(...) {daw('.', '.', ...)}

#' @rdname delim.
#' @export
dawDG <- function(...) {daw('.', ', ', ...)}

#' @rdname delim.
#' @export
dawDP <- function(...) {daw('.', '|', ...)}

#' @rdname delim.
#' @export
dawDQ <- function(...) {daw('.', '`', ...)}

#' @rdname delim.
#' @export
dawDS <- function(...) {daw('.', ',', ...)}

#' @rdname delim.
#' @export
dawDT <- function(...) {daw('.', '~', ...)}

#' @rdname delim.
#' @export
dawG0 <- function(...) {daw(', ', '', ...)}

#' @rdname delim.
#' @export
dawG1 <- function(...) {daw(', ', ' ', ...)}

#' @rdname delim.
#' @export
dawGB <- function(...) {daw(', ', '¦', ...)}

#' @rdname delim.
#' @export
dawGC <- function(...) {daw(', ', ':', ...)}

#' @rdname delim.
#' @export
dawGD <- function(...) {daw(', ', '.', ...)}

#' @rdname delim.
#' @export
dawGG <- function(...) {daw(', ', ', ', ...)}

#' @rdname delim.
#' @export
dawGP <- function(...) {daw(', ', '|', ...)}

#' @rdname delim.
#' @export
dawGQ <- function(...) {daw(', ', '`', ...)}

#' @rdname delim.
#' @export
dawGS <- function(...) {daw(', ', ',', ...)}

#' @rdname delim.
#' @export
dawGT <- function(...) {daw(', ', '~', ...)}

#' @rdname delim.
#' @export
dawP0 <- function(...) {daw('|', '', ...)}

#' @rdname delim.
#' @export
dawP1 <- function(...) {daw('|', ' ', ...)}

#' @rdname delim.
#' @export
dawPB <- function(...) {daw('|', '¦', ...)}

#' @rdname delim.
#' @export
dawPC <- function(...) {daw('|', ':', ...)}

#' @rdname delim.
#' @export
dawPD <- function(...) {daw('|', '.', ...)}

#' @rdname delim.
#' @export
dawPG <- function(...) {daw('|', ', ', ...)}

#' @rdname delim.
#' @export
dawPP <- function(...) {daw('|', '|', ...)}

#' @rdname delim.
#' @export
dawPQ <- function(...) {daw('|', '`', ...)}

#' @rdname delim.
#' @export
dawPS <- function(...) {daw('|', ',', ...)}

#' @rdname delim.
#' @export
dawPT <- function(...) {daw('|', '~', ...)}

#' @rdname delim.
#' @export
dawQ0 <- function(...) {daw('`', '', ...)}

#' @rdname delim.
#' @export
dawQ1 <- function(...) {daw('`', ' ', ...)}

#' @rdname delim.
#' @export
dawQB <- function(...) {daw('`', '¦', ...)}

#' @rdname delim.
#' @export
dawQC <- function(...) {daw('`', ':', ...)}

#' @rdname delim.
#' @export
dawQD <- function(...) {daw('`', '.', ...)}

#' @rdname delim.
#' @export
dawQG <- function(...) {daw('`', ', ', ...)}

#' @rdname delim.
#' @export
dawQP <- function(...) {daw('`', '|', ...)}

#' @rdname delim.
#' @export
dawQQ <- function(...) {daw('`', '`', ...)}

#' @rdname delim.
#' @export
dawQS <- function(...) {daw('`', ',', ...)}

#' @rdname delim.
#' @export
dawQT <- function(...) {daw('`', '~', ...)}

#' @rdname delim.
#' @export
dawS0 <- function(...) {daw(',', '', ...)}

#' @rdname delim.
#' @export
dawS1 <- function(...) {daw(',', ' ', ...)}

#' @rdname delim.
#' @export
dawSB <- function(...) {daw(',', '¦', ...)}

#' @rdname delim.
#' @export
dawSC <- function(...) {daw(',', ':', ...)}

#' @rdname delim.
#' @export
dawSD <- function(...) {daw(',', '.', ...)}

#' @rdname delim.
#' @export
dawSG <- function(...) {daw(',', ', ', ...)}

#' @rdname delim.
#' @export
dawSP <- function(...) {daw(',', '|', ...)}

#' @rdname delim.
#' @export
dawSQ <- function(...) {daw(',', '`', ...)}

#' @rdname delim.
#' @export
dawSS <- function(...) {daw(',', ',', ...)}

#' @rdname delim.
#' @export
dawST <- function(...) {daw(',', '~', ...)}

#' @rdname delim.
#' @export
dawT0 <- function(...) {daw('~', '', ...)}

#' @rdname delim.
#' @export
dawT1 <- function(...) {daw('~', ' ', ...)}

#' @rdname delim.
#' @export
dawTB <- function(...) {daw('~', '¦', ...)}

#' @rdname delim.
#' @export
dawTC <- function(...) {daw('~', ':', ...)}

#' @rdname delim.
#' @export
dawTD <- function(...) {daw('~', '.', ...)}

#' @rdname delim.
#' @export
dawTG <- function(...) {daw('~', ', ', ...)}

#' @rdname delim.
#' @export
dawTP <- function(...) {daw('~', '|', ...)}

#' @rdname delim.
#' @export
dawTQ <- function(...) {daw('~', '`', ...)}

#' @rdname delim.
#' @export
dawTS <- function(...) {daw('~', ',', ...)}

#' @rdname delim.
#' @export
dawTT <- function(...) {daw('~', '~', ...)}

#' @rdname delim.
#' @export
dww00 <- function(...) {dww('', '', ...)}

#' @rdname delim.
#' @export
dww01 <- function(...) {dww('', ' ', ...)}

#' @rdname delim.
#' @export
dww0B <- function(...) {dww('', '¦', ...)}

#' @rdname delim.
#' @export
dww0C <- function(...) {dww('', ':', ...)}

#' @rdname delim.
#' @export
dww0D <- function(...) {dww('', '.', ...)}

#' @rdname delim.
#' @export
dww0G <- function(...) {dww('', ', ', ...)}

#' @rdname delim.
#' @export
dww0P <- function(...) {dww('', '|', ...)}

#' @rdname delim.
#' @export
dww0Q <- function(...) {dww('', '`', ...)}

#' @rdname delim.
#' @export
dww0S <- function(...) {dww('', ',', ...)}

#' @rdname delim.
#' @export
dww0T <- function(...) {dww('', '~', ...)}

#' @rdname delim.
#' @export
dww10 <- function(...) {dww(' ', '', ...)}

#' @rdname delim.
#' @export
dww11 <- function(...) {dww(' ', ' ', ...)}

#' @rdname delim.
#' @export
dww1B <- function(...) {dww(' ', '¦', ...)}

#' @rdname delim.
#' @export
dww1C <- function(...) {dww(' ', ':', ...)}

#' @rdname delim.
#' @export
dww1D <- function(...) {dww(' ', '.', ...)}

#' @rdname delim.
#' @export
dww1G <- function(...) {dww(' ', ', ', ...)}

#' @rdname delim.
#' @export
dww1P <- function(...) {dww(' ', '|', ...)}

#' @rdname delim.
#' @export
dww1Q <- function(...) {dww(' ', '`', ...)}

#' @rdname delim.
#' @export
dww1S <- function(...) {dww(' ', ',', ...)}

#' @rdname delim.
#' @export
dww1T <- function(...) {dww(' ', '~', ...)}

#' @rdname delim.
#' @export
dwwB0 <- function(...) {dww('¦', '', ...)}

#' @rdname delim.
#' @export
dwwB1 <- function(...) {dww('¦', ' ', ...)}

#' @rdname delim.
#' @export
dwwBB <- function(...) {dww('¦', '¦', ...)}

#' @rdname delim.
#' @export
dwwBC <- function(...) {dww('¦', ':', ...)}

#' @rdname delim.
#' @export
dwwBD <- function(...) {dww('¦', '.', ...)}

#' @rdname delim.
#' @export
dwwBG <- function(...) {dww('¦', ', ', ...)}

#' @rdname delim.
#' @export
dwwBP <- function(...) {dww('¦', '|', ...)}

#' @rdname delim.
#' @export
dwwBQ <- function(...) {dww('¦', '`', ...)}

#' @rdname delim.
#' @export
dwwBS <- function(...) {dww('¦', ',', ...)}

#' @rdname delim.
#' @export
dwwBT <- function(...) {dww('¦', '~', ...)}

#' @rdname delim.
#' @export
dwwC0 <- function(...) {dww(':', '', ...)}

#' @rdname delim.
#' @export
dwwC1 <- function(...) {dww(':', ' ', ...)}

#' @rdname delim.
#' @export
dwwCB <- function(...) {dww(':', '¦', ...)}

#' @rdname delim.
#' @export
dwwCC <- function(...) {dww(':', ':', ...)}

#' @rdname delim.
#' @export
dwwCD <- function(...) {dww(':', '.', ...)}

#' @rdname delim.
#' @export
dwwCG <- function(...) {dww(':', ', ', ...)}

#' @rdname delim.
#' @export
dwwCP <- function(...) {dww(':', '|', ...)}

#' @rdname delim.
#' @export
dwwCQ <- function(...) {dww(':', '`', ...)}

#' @rdname delim.
#' @export
dwwCS <- function(...) {dww(':', ',', ...)}

#' @rdname delim.
#' @export
dwwCT <- function(...) {dww(':', '~', ...)}

#' @rdname delim.
#' @export
dwwD0 <- function(...) {dww('.', '', ...)}

#' @rdname delim.
#' @export
dwwD1 <- function(...) {dww('.', ' ', ...)}

#' @rdname delim.
#' @export
dwwDB <- function(...) {dww('.', '¦', ...)}

#' @rdname delim.
#' @export
dwwDC <- function(...) {dww('.', ':', ...)}

#' @rdname delim.
#' @export
dwwDD <- function(...) {dww('.', '.', ...)}

#' @rdname delim.
#' @export
dwwDG <- function(...) {dww('.', ', ', ...)}

#' @rdname delim.
#' @export
dwwDP <- function(...) {dww('.', '|', ...)}

#' @rdname delim.
#' @export
dwwDQ <- function(...) {dww('.', '`', ...)}

#' @rdname delim.
#' @export
dwwDS <- function(...) {dww('.', ',', ...)}

#' @rdname delim.
#' @export
dwwDT <- function(...) {dww('.', '~', ...)}

#' @rdname delim.
#' @export
dwwG0 <- function(...) {dww(', ', '', ...)}

#' @rdname delim.
#' @export
dwwG1 <- function(...) {dww(', ', ' ', ...)}

#' @rdname delim.
#' @export
dwwGB <- function(...) {dww(', ', '¦', ...)}

#' @rdname delim.
#' @export
dwwGC <- function(...) {dww(', ', ':', ...)}

#' @rdname delim.
#' @export
dwwGD <- function(...) {dww(', ', '.', ...)}

#' @rdname delim.
#' @export
dwwGG <- function(...) {dww(', ', ', ', ...)}

#' @rdname delim.
#' @export
dwwGP <- function(...) {dww(', ', '|', ...)}

#' @rdname delim.
#' @export
dwwGQ <- function(...) {dww(', ', '`', ...)}

#' @rdname delim.
#' @export
dwwGS <- function(...) {dww(', ', ',', ...)}

#' @rdname delim.
#' @export
dwwGT <- function(...) {dww(', ', '~', ...)}

#' @rdname delim.
#' @export
dwwP0 <- function(...) {dww('|', '', ...)}

#' @rdname delim.
#' @export
dwwP1 <- function(...) {dww('|', ' ', ...)}

#' @rdname delim.
#' @export
dwwPB <- function(...) {dww('|', '¦', ...)}

#' @rdname delim.
#' @export
dwwPC <- function(...) {dww('|', ':', ...)}

#' @rdname delim.
#' @export
dwwPD <- function(...) {dww('|', '.', ...)}

#' @rdname delim.
#' @export
dwwPG <- function(...) {dww('|', ', ', ...)}

#' @rdname delim.
#' @export
dwwPP <- function(...) {dww('|', '|', ...)}

#' @rdname delim.
#' @export
dwwPQ <- function(...) {dww('|', '`', ...)}

#' @rdname delim.
#' @export
dwwPS <- function(...) {dww('|', ',', ...)}

#' @rdname delim.
#' @export
dwwPT <- function(...) {dww('|', '~', ...)}

#' @rdname delim.
#' @export
dwwQ0 <- function(...) {dww('`', '', ...)}

#' @rdname delim.
#' @export
dwwQ1 <- function(...) {dww('`', ' ', ...)}

#' @rdname delim.
#' @export
dwwQB <- function(...) {dww('`', '¦', ...)}

#' @rdname delim.
#' @export
dwwQC <- function(...) {dww('`', ':', ...)}

#' @rdname delim.
#' @export
dwwQD <- function(...) {dww('`', '.', ...)}

#' @rdname delim.
#' @export
dwwQG <- function(...) {dww('`', ', ', ...)}

#' @rdname delim.
#' @export
dwwQP <- function(...) {dww('`', '|', ...)}

#' @rdname delim.
#' @export
dwwQQ <- function(...) {dww('`', '`', ...)}

#' @rdname delim.
#' @export
dwwQS <- function(...) {dww('`', ',', ...)}

#' @rdname delim.
#' @export
dwwQT <- function(...) {dww('`', '~', ...)}

#' @rdname delim.
#' @export
dwwS0 <- function(...) {dww(',', '', ...)}

#' @rdname delim.
#' @export
dwwS1 <- function(...) {dww(',', ' ', ...)}

#' @rdname delim.
#' @export
dwwSB <- function(...) {dww(',', '¦', ...)}

#' @rdname delim.
#' @export
dwwSC <- function(...) {dww(',', ':', ...)}

#' @rdname delim.
#' @export
dwwSD <- function(...) {dww(',', '.', ...)}

#' @rdname delim.
#' @export
dwwSG <- function(...) {dww(',', ', ', ...)}

#' @rdname delim.
#' @export
dwwSP <- function(...) {dww(',', '|', ...)}

#' @rdname delim.
#' @export
dwwSQ <- function(...) {dww(',', '`', ...)}

#' @rdname delim.
#' @export
dwwSS <- function(...) {dww(',', ',', ...)}

#' @rdname delim.
#' @export
dwwST <- function(...) {dww(',', '~', ...)}

#' @rdname delim.
#' @export
dwwT0 <- function(...) {dww('~', '', ...)}

#' @rdname delim.
#' @export
dwwT1 <- function(...) {dww('~', ' ', ...)}

#' @rdname delim.
#' @export
dwwTB <- function(...) {dww('~', '¦', ...)}

#' @rdname delim.
#' @export
dwwTC <- function(...) {dww('~', ':', ...)}

#' @rdname delim.
#' @export
dwwTD <- function(...) {dww('~', '.', ...)}

#' @rdname delim.
#' @export
dwwTG <- function(...) {dww('~', ', ', ...)}

#' @rdname delim.
#' @export
dwwTP <- function(...) {dww('~', '|', ...)}

#' @rdname delim.
#' @export
dwwTQ <- function(...) {dww('~', '`', ...)}

#' @rdname delim.
#' @export
dwwTS <- function(...) {dww('~', ',', ...)}

#' @rdname delim.
#' @export
dwwTT <- function(...) {dww('~', '~', ...)}
