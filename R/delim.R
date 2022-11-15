#' @name delim.
#' @family strings
#' @title Delimit strings
#' @description Simplifies and extends \code{paste} and \code{paste0}, allowing
#'   for 'delimiting across' and 'delimiting within' arguments in \code{...},
#'   and a combination of delimiting within and across arguments, in any order.
#'   Also offers convenience functions for specific delimiters.
#'   \cr\cr
#'   \strong{\code{da(a, ...)}}
#'   \cr Delimits across corresponding elements of arguments in \code{...}.
#'   \code{da(a, ...)} is identical to \code{paste(..., sep = a)}.
#'   \cr\cr
#'   \strong{\code{dw(w, ...)}}
#'   \cr Delimits within arguments in \code{...}. \code{dw(w, ...)} is
#'   equivalent to calling \code{sapply(list(...), paste0, collapse = w)}.
#'   \cr\cr
#'   \strong{\code{daw(a, w, ...)}}
#'   \cr Delimits across then within. \code{daw(a, w, ...)} is
#'   identical to \code{paste(..., sep = a, collapse = w)}
#'   \cr\cr
#'   \strong{\code{dwa(w, a, ...)}}
#'   \cr Delimits within then across. \code{dwa(w, a, ...)} is
#'   equivalent to \code{paste(sapply(list(...), paste0, collapse = w),
#'   sep = a)}.
#'   \cr\cr
#'   \strong{Extensions}
#'   \cr Functions are extended for specific delimiters, signified by the
#'   following extension characters:\tabular{lll}{
#'   EXTENSION   \tab EXTENSION         \tab DELIMITER  \cr
#'   CHARACTER   \tab NAME              \tab INVOKED    \cr
#'   \code{'0'}  \tab Blank             \tab\code{""}   \cr
#'   \code{'1'}  \tab Space             \tab\code{" "}  \cr
#'   \code{'B'}  \tab Broken pipe       \tab\code{"¦"}  \cr
#'   \code{'C'}  \tab Colon             \tab\code{":"}  \cr
#'   \code{'D'}  \tab Dot               \tab\code{"."}  \cr
#'   \code{'G'}  \tab Grammatical comma \tab\code{", "} \cr
#'   \code{'P'}  \tab Pipe              \tab\code{"|"}  \cr
#'   \code{'Q'}  \tab (Back)-quote      \tab\code{"`"}  \cr
#'   \code{'S'}  \tab Simple comma      \tab\code{","}  \cr
#'   \code{'T'}  \tab Tilde             \tab\code{"~"}    }
#'   \strong{\code{daA}}
#'   \cr Delimits across using the extension character A.
#'   \cr\cr
#'   \strong{\code{dwW}}
#'   \cr Delimits within using the extension character W.
#'   \cr\cr
#'   \strong{\code{dawAW}}
#'   \cr Delimits across using the extension character A then delimits within
#'   using the extension character W.
#'   \cr\cr
#'   \strong{\code{dwaWA}}
#'   \cr Delimits within using the extension character W then delimits across
#'   using the extension character A.
#' @param ... An arbitrary number of atomic vector arguments to be delimited.
#'   Argument in \code{...} must be recyclable for \code{da}, \code{daw},
#'   \code{daA}, and \code{dawAW}.
#' @param a,w \link[chr_scl]{Character scalar} across delimiter vs. within
#'   delimiter, respectively. Defaults to a blank string (i.e., \code{""}).
#' @return a character-scalar, character vector, or list of character vectors.
delim. <- function() {help("delim.", package = "uj")}

#' @describeIn delim. Delimit across vectors in \code{...} using \code{a}
#'   resulting in a character vector of \code{max(lengths(list(...)))}
#'   \code{a}-delimited strings.
#' @export
da <- function(a, ...) {
  dots <- list(...)
  n.dots <- length(dots)
  dot.ns <- lengths(dots)
  ok.n <- n.dots > 0
  ok.ns <- f0(!ok.n, T, all(dot.ns > 0))
  ok.dots <- f0(!ok.n | !ok.ns, T, all(sapply(dots, atm_vec)))
  ok.reps <- f0(!ok.dots, T, all(round(max(dot.ns) / dot.ns) == max(dot.ns) / dot.ns))
  errs <- c(f0(cmp_chr_scl(a), NULL, "\n \u2022 [a] must be a complete character scalar (?cmp_chr_scl)."),
            f0(ok.n          , NULL, "\n \u2022 [...] is empty."),
            f0(ok.ns         , NULL, "\n \u2022 [...] contains an empty element."),
            f0(ok.dots       , NULL, "\n \u2022 Arguments in [...] must be populated atomic vecs (?pop_vec)."),
            f0(ok.reps       , NULL, "\n \u2022 Arguments in [...] are not recyclable (?recyclable)."))
  if (idef(errs)) {stop(errs)}
  paste(..., sep = a)
}

#' @describeIn delim. Delimit within vectors in \code{...} using \code{w},
#'   resulting in a character vector of \code{\link[base]{...length()}}
#'   \code{w}-separated strings.
#' @export
dw <- function(w, ...) {
  dots <- list(...)
  n.dots <- length(dots)
  dot.ns <- lengths(dots)
  ok.n <- n.dots > 0
  ok.ns <- f0(!ok.n, F, all(dot.ns > 0))
  ok.dots <- f0(!ok.n | !ok.ns, F, all(sapply(dots, atm_vec)))
  errs <- c(f0(cmp_chr_scl(w), NULL, "\n \u2022 [w] must be a complete character scalar (?cmp_chr_scl)."),
            f0(ok.n          , NULL, "\n \u2022 [...] is empty."),
            f0(ok.ns         , NULL, "\n \u2022 [...] contains an empty element."),
            f0(ok.dots       , NULL, "\n \u2022 Arguments in [...] must be populated atomic vecs (?pop_vec)."))
  if (idef(errs)) {stop(errs)}
  sapply(list(...), paste0, collapse = w)
}

#' @describeIn delim. Delimit across vectors in \code{...} using \code{a},
#'   then delimit within using \code{w}, resulting in a character-scalar string
#'   containing \code{max(lengths(list(...)))} \code{a}-delimited substrings,
#'   each of which contains \code{\link[base]{...length()}} \code{w}-delimited
#'   subsubstrings.
#' @export
daw <- function(a, w, ...) {
  dots <- list(...)
  n.dots <- length(dots)
  dot.ns <- lengths(dots)
  ok.n <- n.dots > 0
  ok.ns <- f0(!ok.n, T, all(dot.ns > 0))
  ok.dots <- f0(!ok.n | !ok.dots, T, all(sapply(dots, ivec)))
  ok.reps <- f0(!ok.dots, T, all(round(max(dot.ns) / dot.ns) == max(dot.ns) / dot.ns))
  errs <- c(f0(cmp_chr_scl(a), NULL, "\n \u2022 [a] must be a complete character scalar (?cmp_chr_scl)."),
            f0(cmp_chr_scl(w), NULL, "\n \u2022 [w] must be a complete character scalar (?cmp_chr_scl)."),
            f0(ok.n          , NULL, "\n \u2022 [...] is empty."),
            f0(ok.ns         , NULL, "\n \u2022 An argument in [...] is of length 0."),
            f0(ok.dots       , NULL, "\n \u2022 Arguments in [...] must be populated atomic vecs (?pop_vec)."),
            f0(ok.reps       , NULL, "\n \u2022 Arguments in [...] are not recyclable (?recyclable)."))
  if (idef(errs)) {stop(errs)}
  paste0(paste(..., sep = a), collapse = w)
}

#' @describeIn delim. Delimit within vectors in \code{...} using \code{w},
#'   then delimit across using \code{a}, resulting in a character-scalar string
#'   containing \code{\link[base]{...length()}} \code{w}-delimited substrings,
#'   each of which contains \code{max(length(list(...)))} \code{a}-delimited
#'   subsubstrings.
dwa <- function(w, a, ...) {
  dots  <- list(...)
  n.dots  <- length(dots)
  dot.ns  <- lengths(dots)
  ok.n <- n.dots > 0
  ok.ns <- f0(!ok.n, T, all(dot.ns > 0))
  ok.dots <- f0(!ok.n | !ok.ns, T, all(sapply(dots, ivec)))
  errs <- c(f0(cmp_chr_scl(w), NULL, "\n \u2022 [w] must be a complete character scalar (?cmp_chr_scl)."),
            f0(cmp_chr_scl(a), NULL, "\n \u2022 [a] must be a complete character scalar (?cmp_chr_scl)."),
            f0(ok.n          , NULL, "\n \u2022 [...] is empty."),
            f0(ok.ns         , NULL, "\n \u2022 [...] contains an empty element."),
            f0(ok.dots       , NULL, "\n \u2022 Arguments in [...] must be populated atomic vecs (?pop_vec)."))
  if (idef(errs)) {stop(errs)}
  paste(sapply(list(...), paste0, collapse = w), sep = a)
}

#' @describeIn delim. Delimit across vectors in \code{...} using blanks.
#' @export
da0 <- function(...) {da('', ...)}

#' @describeIn delim. Delimit across vectors in \code{...} using spaces.
#' @export
da1 <- function(...) {da(' ', ...)}

#' @describeIn delim. Delimit across vectors in \code{...} using broken-pipes.
#' @export
daB <- function(...) {da('¦', ...)}

#' @describeIn delim. Delimit across vectors in \code{...} using colons.
#' @export
daC <- function(...) {da(':', ...)}

#' @describeIn delim. Delimit across vectors in \code{...} using dots/periods.
#' @export
daD <- function(...) {da('.', ...)}

#' @describeIn delim. Delimit across vectors in \code{...} using grammatical
#'   commas.
#' @export
daG <- function(...) {da(', ', ...)}

#' @describeIn delim. Delimit across vectors in \code{...} using pipes.
#' @export
daP <- function(...) {da('|', ...)}

#' @describeIn delim. Delimit across vectors in \code{...} using
#'   (back)-quotes.
#' @export
daQ <- function(...) {da('`', ...)}

#' @describeIn delim. Delimit across vectors in \code{...} using simple
#'   commas.
#' @export
daS <- function(...) {da(',', ...)}

#' @describeIn delim. Delimit across vectors in \code{...} using tildes.
#' @export
daT <- function(...) {da('~', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using blanks.
#' @export
dw0 <- function(...) {dw('', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using spaces.
#' @export
dw1 <- function(...) {dw(' ', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using broken-pipes.
#' @export
dwB <- function(...) {dw('¦', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using colons.
#' @export
dwC <- function(...) {dw(':', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using dots/periods.
#' @export
dwD <- function(...) {dw('.', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using grammatical
#'   commas.
#' @export
dwG <- function(...) {dw(', ', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using pipes.
#' @export
dwP <- function(...) {dw('|', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using
#'   (back)-quotes.
#' @export
dwQ <- function(...) {dw('`', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using simple
#'   commas.
#' @export
dwS <- function(...) {dw(',', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using tildes.
#' @export
dwT <- function(...) {dw('~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks}.
#' @export
daw00 <- function(...) {daw('', '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks} then \emph{spaces}, respectively.
#' @export
daw01 <- function(...) {daw('', ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks} then \emph{broken-pipes}, respectively.
#' @export
daw0B <- function(...) {daw('', '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks} then \emph{colons}, respectively.
#' @export
daw0C <- function(...) {daw('', ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks} then \emph{dots/periods}, respectively.
#' @export
daw0D <- function(...) {daw('', '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks} then \emph{grammatical commas},
#'   respectively.
#' @export
daw0G <- function(...) {daw('', ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks} then \emph{pipes}, respectively.
#' @export
daw0P <- function(...) {daw('', '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks} then \emph{(back)-quotes}, respectively.
#' @export
daw0Q <- function(...) {daw('', '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks} then \emph{simple commas}, respectively.
#' @export
daw0S <- function(...) {daw('', ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks} then \emph{tildes}, respectively.
#' @export
daw0T <- function(...) {daw('', '~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces} then \emph{blanks}, respectively.
#' @export
daw10 <- function(...) {daw(' ', '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces}.
#' @export
daw11 <- function(...) {daw(' ', ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces} then \emph{broken-pipes}, respectively.
#' @export
daw1B <- function(...) {daw(' ', '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces} then \emph{colons}, respectively.
#' @export
daw1C <- function(...) {daw(' ', ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces} then \emph{dots/periods}, respectively.
#' @export
daw1D <- function(...) {daw(' ', '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces} then \emph{grammatical commas},
#'   respectively.
#' @export
daw1G <- function(...) {daw(' ', ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces} then \emph{pipes}, respectively.
#' @export
daw1P <- function(...) {daw(' ', '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces} then \emph{(back)-quotes}, respectively.
#' @export
daw1Q <- function(...) {daw(' ', '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces} then \emph{simple commas}, respectively.
#' @export
daw1S <- function(...) {daw(' ', ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces} then \emph{tildes}, respectively.
#' @export
daw1T <- function(...) {daw(' ', '~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{blanks}, respectively.
#' @export
dawB0 <- function(...) {daw('¦', '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{spaces}, respectively.
#' @export
dawB1 <- function(...) {daw('¦', ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes}.
#' @export
dawBB <- function(...) {daw('¦', '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{colons}, respectively.
#' @export
dawBC <- function(...) {daw('¦', ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{dots/periods},
#'   respectively.
#' @export
dawBD <- function(...) {daw('¦', '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{grammatical commas},
#'   respectively.
#' @export
dawBG <- function(...) {daw('¦', ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{pipes}, respectively.
#' @export
dawBP <- function(...) {daw('¦', '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{(back)-quotes},
#'   respectively.
#' @export
dawBQ <- function(...) {daw('¦', '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{simple commas},
#'   respectively.
#' @export
dawBS <- function(...) {daw('¦', ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{tildes}, respectively.
#' @export
dawBT <- function(...) {daw('¦', '~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons} then \emph{blanks}, respectively.
#' @export
dawC0 <- function(...) {daw(':', '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons} then \emph{spaces}, respectively.
#' @export
dawC1 <- function(...) {daw(':', ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons} then \emph{broken-pipes}, respectively.
#' @export
dawCB <- function(...) {daw(':', '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons}.
#' @export
dawCC <- function(...) {daw(':', ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons} then \emph{dots/periods}, respectively.
#' @export
dawCD <- function(...) {daw(':', '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons} then \emph{grammatical commas},
#'   respectively.
#' @export
dawCG <- function(...) {daw(':', ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons} then \emph{pipes}, respectively.
#' @export
dawCP <- function(...) {daw(':', '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons} then \emph{(back)-quotes}, respectively.
#' @export
dawCQ <- function(...) {daw(':', '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons} then \emph{simple commas}, respectively.
#' @export
dawCS <- function(...) {daw(':', ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons} then \emph{tildes}, respectively.
#' @export
dawCT <- function(...) {daw(':', '~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{blanks}, respectively.
#' @export
dawD0 <- function(...) {daw('.', '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{spaces}, respectively.
#' @export
dawD1 <- function(...) {daw('.', ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{broken-pipes},
#'   respectively.
#' @export
dawDB <- function(...) {daw('.', '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{colons}, respectively.
#' @export
dawDC <- function(...) {daw('.', ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods}.
#' @export
dawDD <- function(...) {daw('.', '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{grammatical commas},
#'   respectively.
#' @export
dawDG <- function(...) {daw('.', ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{pipes}, respectively.
#' @export
dawDP <- function(...) {daw('.', '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{(back)-quotes},
#'   respectively.
#' @export
dawDQ <- function(...) {daw('.', '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{simple commas},
#'   respectively.
#' @export
dawDS <- function(...) {daw('.', ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{tildes}, respectively.
#' @export
dawDT <- function(...) {daw('.', '~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{blanks},
#'   respectively.
#' @export
dawG0 <- function(...) {daw(', ', '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{spaces},
#'   respectively.
#' @export
dawG1 <- function(...) {daw(', ', ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{broken-pipes},
#'   respectively.
#' @export
dawGB <- function(...) {daw(', ', '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{colons},
#'   respectively.
#' @export
dawGC <- function(...) {daw(', ', ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{dots/periods},
#'   respectively.
#' @export
dawGD <- function(...) {daw(', ', '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas}.
#' @export
dawGG <- function(...) {daw(', ', ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{pipes}, respectively.
#' @export
dawGP <- function(...) {daw(', ', '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{(back)-quotes},
#'   respectively.
#' @export
dawGQ <- function(...) {daw(', ', '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{simple commas},
#'   respectively.
#' @export
dawGS <- function(...) {daw(', ', ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{tildes},
#'   respectively.
#' @export
dawGT <- function(...) {daw(', ', '~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes} then \emph{blanks}, respectively.
#' @export
dawP0 <- function(...) {daw('|', '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes} then \emph{spaces}, respectively.
#' @export
dawP1 <- function(...) {daw('|', ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes} then \emph{broken-pipes}, respectively.
#' @export
dawPB <- function(...) {daw('|', '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes} then \emph{colons}, respectively.
#' @export
dawPC <- function(...) {daw('|', ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes} then \emph{dots/periods}, respectively.
#' @export
dawPD <- function(...) {daw('|', '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes} then \emph{grammatical commas}, respectively.
#' @export
dawPG <- function(...) {daw('|', ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes}.
#' @export
dawPP <- function(...) {daw('|', '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes} then \emph{(back)-quotes}, respectively.
#' @export
dawPQ <- function(...) {daw('|', '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes} then \emph{simple commas}, respectively.
#' @export
dawPS <- function(...) {daw('|', ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes} then \emph{tildes}, respectively.
#' @export
dawPT <- function(...) {daw('|', '~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{blanks}, respectively.
#' @export
dawQ0 <- function(...) {daw('`', '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{spaces}, respectively.
#' @export
dawQ1 <- function(...) {daw('`', ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{broken-pipes},
#'   respectively.
#' @export
dawQB <- function(...) {daw('`', '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{colons}, respectively.
#' @export
dawQC <- function(...) {daw('`', ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{dots/periods},
#'   respectively.
#' @export
dawQD <- function(...) {daw('`', '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{grammatical commas},
#'   respectively.
#' @export
dawQG <- function(...) {daw('`', ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{pipes}, respectively.
#' @export
dawQP <- function(...) {daw('`', '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes}.
#' @export
dawQQ <- function(...) {daw('`', '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{simple commas},
#'   respectively.
#' @export
dawQS <- function(...) {daw('`', ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{tildes}, respectively.
#' @export
dawQT <- function(...) {daw('`', '~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas} then \emph{blanks}, respectively.
#' @export
dawS0 <- function(...) {daw(',', '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas} then \emph{spaces}, respectively.
#' @export
dawS1 <- function(...) {daw(',', ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas} then \emph{broken-pipes},
#'   respectively.
#' @export
dawSB <- function(...) {daw(',', '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas} then \emph{colons}, respectively.
#' @export
dawSC <- function(...) {daw(',', ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas} then \emph{dots/periods},
#'   respectively.
#' @export
dawSD <- function(...) {daw(',', '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas} then \emph{grammatical commas},
#'   respectively.
#' @export
dawSG <- function(...) {daw(',', ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas} then \emph{pipes}, respectively.
#' @export
dawSP <- function(...) {daw(',', '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas} then \emph{(back)-quotes},
#'   respectively.
#' @export
dawSQ <- function(...) {daw(',', '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas}.
#' @export
dawSS <- function(...) {daw(',', ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas} then \emph{tildes}, respectively.
#' @export
dawST <- function(...) {daw(',', '~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes} then \emph{blanks}, respectively.
#' @export
dawT0 <- function(...) {daw('~', '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes} then \emph{spaces}, respectively.
#' @export
dawT1 <- function(...) {daw('~', ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes} then \emph{broken-pipes}, respectively.
#' @export
dawTB <- function(...) {daw('~', '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes} then \emph{colons}, respectively.
#' @export
dawTC <- function(...) {daw('~', ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes} then \emph{dots/periods}, respectively.
#' @export
dawTD <- function(...) {daw('~', '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes} then \emph{grammatical commas},
#'   respectively.
#' @export
dawTG <- function(...) {daw('~', ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes} then \emph{pipes}, respectively.
#' @export
dawTP <- function(...) {daw('~', '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes} then \emph{(back)-quotes}, respectively.
#' @export
dawTQ <- function(...) {daw('~', '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes} then \emph{simple commas}, respectively.
#' @export
dawTS <- function(...) {daw('~', ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes}.
#' @export
dawTT <- function(...) {daw('~', '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks}.
#' @export
dwa00 <- function(...) {dwa('', '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks} then \emph{spaces}, respectively.
#' @export
dwa01 <- function(...) {dwa('', ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks} then \emph{broken-pipes}, respectively.
#' @export
dwa0B <- function(...) {dwa('', '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks} then \emph{colons}, respectively.
#' @export
dwa0C <- function(...) {dwa('', ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks} then \emph{dots/periods}, respectively.
#' @export
dwa0D <- function(...) {dwa('', '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks} then \emph{grammatical commas},
#'   respectively.
#' @export
dwa0G <- function(...) {dwa('', ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks} then \emph{pipes}, respectively.
#' @export
dwa0P <- function(...) {dwa('', '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks} then \emph{(back)-quotes}, respectively.
#' @export
dwa0Q <- function(...) {dwa('', '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks} then \emph{simple commas}, respectively.
#' @export
dwa0S <- function(...) {dwa('', ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks} then \emph{tildes}, respectively.
#' @export
dwa0T <- function(...) {dwa('', '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces} then \emph{blanks}, respectively.
#' @export
dwa10 <- function(...) {dwa(' ', '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces}.
#' @export
dwa11 <- function(...) {dwa(' ', ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces} then \emph{broken-pipes}, respectively.
#' @export
dwa1B <- function(...) {dwa(' ', '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces} then \emph{colons}, respectively.
#' @export
dwa1C <- function(...) {dwa(' ', ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces} then \emph{dots/periods}, respectively.
#' @export
dwa1D <- function(...) {dwa(' ', '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces} then \emph{grammatical commas},
#'   respectively.
#' @export
dwa1G <- function(...) {dwa(' ', ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces} then \emph{pipes}, respectively.
#' @export
dwa1P <- function(...) {dwa(' ', '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces} then \emph{(back)-quotes}, respectively.
#' @export
dwa1Q <- function(...) {dwa(' ', '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces} then \emph{simple commas}, respectively.
#' @export
dwa1S <- function(...) {dwa(' ', ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces} then \emph{tildes}, respectively.
#' @export
dwa1T <- function(...) {dwa(' ', '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{blanks}, respectively.
#' @export
dwaB0 <- function(...) {dwa('¦', '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{spaces}, respectively.
#' @export
dwaB1 <- function(...) {dwa('¦', ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes}.
#' @export
dwaBB <- function(...) {dwa('¦', '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{colons}, respectively.
#' @export
dwaBC <- function(...) {dwa('¦', ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{dots/periods},
#'   respectively.
#' @export
dwaBD <- function(...) {dwa('¦', '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{grammatical commas},
#'   respectively.
#' @export
dwaBG <- function(...) {dwa('¦', ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{pipes}, respectively.
#' @export
dwaBP <- function(...) {dwa('¦', '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{(back)-quotes},
#'   respectively.
#' @export
dwaBQ <- function(...) {dwa('¦', '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{simple commas},
#'   respectively.
#' @export
dwaBS <- function(...) {dwa('¦', ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{tildes}, respectively.
#' @export
dwaBT <- function(...) {dwa('¦', '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons} then \emph{blanks}, respectively.
#' @export
dwaC0 <- function(...) {dwa(':', '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons} then \emph{spaces}, respectively.
#' @export
dwaC1 <- function(...) {dwa(':', ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons} then \emph{broken-pipes}, respectively.
#' @export
dwaCB <- function(...) {dwa(':', '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons}.
#' @export
dwaCC <- function(...) {dwa(':', ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons} then \emph{dots/periods}, respectively.
#' @export
dwaCD <- function(...) {dwa(':', '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons} then \emph{grammatical commas},
#'   respectively.
#' @export
dwaCG <- function(...) {dwa(':', ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons} then \emph{pipes}, respectively.
#' @export
dwaCP <- function(...) {dwa(':', '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons} then \emph{(back)-quotes}, respectively.
#' @export
dwaCQ <- function(...) {dwa(':', '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons} then \emph{simple commas}, respectively.
#' @export
dwaCS <- function(...) {dwa(':', ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons} then \emph{tildes}, respectively.
#' @export
dwaCT <- function(...) {dwa(':', '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{blanks}, respectively.
#' @export
dwaD0 <- function(...) {dwa('.', '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{spaces}, respectively.
#' @export
dwaD1 <- function(...) {dwa('.', ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{broken-pipes},
#'   respectively.
#' @export
dwaDB <- function(...) {dwa('.', '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{colons}, respectively.
#' @export
dwaDC <- function(...) {dwa('.', ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods}.
#' @export
dwaDD <- function(...) {dwa('.', '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{grammatical commas},
#'   respectively.
#' @export
dwaDG <- function(...) {dwa('.', ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{pipes}, respectively.
#' @export
dwaDP <- function(...) {dwa('.', '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{(back)-quotes},
#'   respectively.
#' @export
dwaDQ <- function(...) {dwa('.', '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{simple commas},
#'   respectively.
#' @export
dwaDS <- function(...) {dwa('.', ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{tildes}, respectively.
#' @export
dwaDT <- function(...) {dwa('.', '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{blanks},
#'   respectively.
#' @export
dwaG0 <- function(...) {dwa(', ', '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{spaces},
#'   respectively.
#' @export
dwaG1 <- function(...) {dwa(', ', ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{broken-pipes},
#'   respectively.
#' @export
dwaGB <- function(...) {dwa(', ', '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{colons},
#'   respectively.
#' @export
dwaGC <- function(...) {dwa(', ', ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{dots/periods},
#'   respectively.
#' @export
dwaGD <- function(...) {dwa(', ', '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas}.
#' @export
dwaGG <- function(...) {dwa(', ', ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{pipes}, respectively.
#' @export
dwaGP <- function(...) {dwa(', ', '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{(back)-quotes},
#'   respectively.
#' @export
dwaGQ <- function(...) {dwa(', ', '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{simple commas},
#'   respectively.
#' @export
dwaGS <- function(...) {dwa(', ', ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{tildes},
#'   respectively.
#' @export
dwaGT <- function(...) {dwa(', ', '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes} then \emph{blanks}, respectively.
#' @export
dwaP0 <- function(...) {dwa('|', '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes} then \emph{spaces}, respectively.
#' @export
dwaP1 <- function(...) {dwa('|', ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes} then \emph{broken-pipes}, respectively.
#' @export
dwaPB <- function(...) {dwa('|', '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes} then \emph{colons}, respectively.
#' @export
dwaPC <- function(...) {dwa('|', ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes} then \emph{dots/periods}, respectively.
#' @export
dwaPD <- function(...) {dwa('|', '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes} then \emph{grammatical commas}, respectively.
#' @export
dwaPG <- function(...) {dwa('|', ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes}.
#' @export
dwaPP <- function(...) {dwa('|', '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes} then \emph{(back)-quotes}, respectively.
#' @export
dwaPQ <- function(...) {dwa('|', '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes} then \emph{simple commas}, respectively.
#' @export
dwaPS <- function(...) {dwa('|', ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes} then \emph{tildes}, respectively.
#' @export
dwaPT <- function(...) {dwa('|', '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{blanks}, respectively.
#' @export
dwaQ0 <- function(...) {dwa('`', '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{spaces}, respectively.
#' @export
dwaQ1 <- function(...) {dwa('`', ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{broken-pipes},
#'   respectively.
#' @export
dwaQB <- function(...) {dwa('`', '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{colons}, respectively.
#' @export
dwaQC <- function(...) {dwa('`', ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{dots/periods},
#'   respectively.
#' @export
dwaQD <- function(...) {dwa('`', '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{grammatical commas},
#'   respectively.
#' @export
dwaQG <- function(...) {dwa('`', ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{pipes}, respectively.
#' @export
dwaQP <- function(...) {dwa('`', '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes}.
#' @export
dwaQQ <- function(...) {dwa('`', '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{simple commas},
#'   respectively.
#' @export
dwaQS <- function(...) {dwa('`', ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{tildes}, respectively.
#' @export
dwaQT <- function(...) {dwa('`', '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas} then \emph{blanks}, respectively.
#' @export
dwaS0 <- function(...) {dwa(',', '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas} then \emph{spaces}, respectively.
#' @export
dwaS1 <- function(...) {dwa(',', ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas} then \emph{broken-pipes},
#'   respectively.
#' @export
dwaSB <- function(...) {dwa(',', '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas} then \emph{colons}, respectively.
#' @export
dwaSC <- function(...) {dwa(',', ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas} then \emph{dots/periods},
#'   respectively.
#' @export
dwaSD <- function(...) {dwa(',', '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas} then \emph{grammatical commas},
#'   respectively.
#' @export
dwaSG <- function(...) {dwa(',', ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas} then \emph{pipes}, respectively.
#' @export
dwaSP <- function(...) {dwa(',', '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas} then \emph{(back)-quotes},
#'   respectively.
#' @export
dwaSQ <- function(...) {dwa(',', '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas}.
#' @export
dwaSS <- function(...) {dwa(',', ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas} then \emph{tildes}, respectively.
#' @export
dwaST <- function(...) {dwa(',', '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes} then \emph{blanks}, respectively.
#' @export
dwaT0 <- function(...) {dwa('~', '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes} then \emph{spaces}, respectively.
#' @export
dwaT1 <- function(...) {dwa('~', ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes} then \emph{broken-pipes}, respectively.
#' @export
dwaTB <- function(...) {dwa('~', '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes} then \emph{colons}, respectively.
#' @export
dwaTC <- function(...) {dwa('~', ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes} then \emph{dots/periods}, respectively.
#' @export
dwaTD <- function(...) {dwa('~', '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes} then \emph{grammatical commas},
#'   respectively.
#' @export
dwaTG <- function(...) {dwa('~', ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes} then \emph{pipes}, respectively.
#' @export
dwaTP <- function(...) {dwa('~', '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes} then \emph{(back)-quotes}, respectively.
#' @export
dwaTQ <- function(...) {dwa('~', '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes} then \emph{simple commas}, respectively.
#' @export
dwaTS <- function(...) {dwa('~', ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes}.
#' @export
dwaTT <- function(...) {dwa('~', '~', ...)}
