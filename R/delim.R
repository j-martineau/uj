#' @name delim.
#' @family strings
#' @title Delimit strings
#' @description Simplifies and extends \code{paste} and \code{paste0}, allowing
#'   for 'delimiting across' and 'delimiting within' arguments in \code{...},
#'   and a combination of delimiting within and across arguments, in any order.
#'   Also offers convenience functions for specific delimiters.
#'   \cr\cr
#'   \strong{\code{da(a., ...)}}
#'   \cr Delimits across corresponding elements of arguments in \code{...}.
#'   \code{da(delim, ...)} is identical to \code{paste(..., sep = delim)}.
#'   \cr\cr
#'   \strong{\code{dw(w., ...)}}
#'   \cr Delimits within arguments in \code{...}. \code{dw(delim, ...)} is
#'   equivalent to calling \code{sapply(list(...), paste0, collapse = delim)}.
#'   \cr\cr
#'   \strong{\code{daw(a., w., ...)}}
#'   \cr Delimits across then within. \code{daw(delim.a, delimn.w, ...)} is
#'   identical to \code{paste(..., sep = delim.a, collapse = delim.w)}
#'   \cr\cr
#'   \strong{\code{dwa(w., a., ...)}}
#'   \cr Delimits within then across. \code{dwa(delim.w, delim.a, ...)} is
#'   equivalent to \code{paste(sapply(list(...), paste0, collapse = delim.w),
#'   sep = delim.a)}.
#'   \cr\cr
#'   \strong{Extensions}
#'   \cr Functions are extended for specific delimiters, signified by the
#'   following extension characters:\tabular{lll}{
#'     EXTENSION \tab EXTENSION         \tab DELIMITER  \cr
#'     CHARACTER \tab NAME              \tab INVOKED    \cr
#'     \code{'0'}\tab blank             \tab\code{""}   \cr
#'     \code{'1'}\tab space             \tab\code{" "}  \cr
#'     \code{'B'}\tab broken pipe       \tab\code{"¦"}  \cr
#'     \code{'C'}\tab colon             \tab\code{":"}  \cr
#'     \code{'D'}\tab dot               \tab\code{"."}  \cr
#'     \code{'G'}\tab grammatical comma \tab\code{", "} \cr
#'     \code{'P'}\tab pipe              \tab\code{"|"}  \cr
#'     \code{'Q'}\tab (back)-quote      \tab\code{"`"}  \cr
#'     \code{'S'}\tab simple comma      \tab\code{","}  \cr
#'     \code{'T'}\tab tilde             \tab\code{"~"}    }
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
#' @param a.,w. Character scalar across delimiter vs. within delimiter,
#'   respectively. Defaults to a blank string (i.e., \code{""}).
#' @return a character-scalar, character vector, or list of character vectors.
delim. <- function() {help("delim.", package = "uj")}

#' @describeIn delim. Delimit across vectors in \code{...} using \code{a.}
#'   resulting in a character vector of \code{max(lengths(list(...)))}
#'   \code{a.}-delimited strings.
#' @export
da <- function(a., ...) {
  x. <- list(...)
  n. <- length(x.)
  ns. <- lengths(x.)
  vn. <- n. > 0
  vns. <- f0(!vn., T, all(ns. > 0))
  vx. <- f0(!vn. | !vns., T, all(sapply(x., ivec)))
  vr. <- f0(!vx., T, all(round(max(ns.) / ns.) == max(ns.) / ns.))
  vd. <- chr_scl(a.)
  err.  <- NULL
  if (!vn. ) {err. <- c(err., "\n • [...] is empty.")}
  if (!vns.) {err. <- c(err., "\n • [...] contains an empty element.")}
  if (!vx. ) {err. <- c(err., "\n • Arguments in [...] must be populated atomic vecs (?pop_vec).")}
  if (!vr. ) {err. <- c(err., "\n • Arguments in [...] are not recyclable (?recyclable).")}
  if (!vd. ) {err. <- c(err., "\n • [a.] must be a complete character scalar (?cmp_chr_scl).")}
  if (idef(err.)) {stop(err.)}
  paste(..., sep = a.)
}

#' @describeIn delim. Delimit within vectors in \code{...} using \code{w.},
#'   resulting in a character vector of \code{\link[base]{...length()}}
#'   \code{w.}-separated strings.
#' @export
dw <- function(w., ...) {
  browser()
  x. <- list(...)
  n. <- length(x.)
  ns. <- lengths(x.)
  vn. <- n. > 0
  vns. <- f0(!vn., T, all(ns. > 0))
  vx. <- f0(!vn. | !vns., T, all(sapply(x., ivec)))
  vd. <- chr_scl(w.)
  err. <- NULL
  if (!vn. ) {err. <- c(err., "\n • [...] is empty.")}
  if (!vns.) {err. <- c(err., "\n • [...] contains an empty element.")}
  if (!vx. ) {err. <- c(err., "\n • Arguments in [...] must be populated atomic vecs (?pop_vec).")}
  if (!vd. ) {err. <- c(err., "\n • [w.] must be a complete character scalar (?cmp_chr_scl).")}
  if (idef(err.)) {stop(err.)}
  sapply(list(...), paste0, collapse = w.)
}

#' @describeIn delim. Delimit across vectors in \code{...} using \code{a.},
#'   then delimit within using \code{w.}, resulting in a character-scalar string
#'   containing \code{max(lengths(list(...)))} \code{a.}-delimited substrings,
#'   each of which contains \code{\link[base]{...length()}} \code{w.}-delimited
#'   subsubstrings.
#' @export
daw <- function(a., w., ...) {
  x. <- list(...)
  n. <- length(x.)
  ns. <- lengths(x.)
  vn. <- n. > 0
  vns. <- f0(!vn., T, all(ns. > 0))
  vx. <- f0(!vn. | !vns., T, all(sapply(x., ivec)))
  vr. <- f0(!vx., T, all(round(max(ns.) / ns.) == max(ns.) / ns.))
  va. <- chr_scl(a.)
  vw. <- chr_scl(w.)
  err. <- NULL
  if (!vn. ) {err. <- c(err., "\n • [...] is empty.")}
  if (!vns.) {err. <- c(err., "\n • [...] contains an empty element.")}
  if (!vx. ) {err. <- c(err., "\n • Arguments in [...] must be populated atomic vecs (?pop_vec).")}
  if (!vr. ) {err. <- c(err., "\n • Arguments in [...] are not recyclable (?recyclable).")}
  if (!va. ) {err. <- c(err., "\n • [a.] must be a complete character scalar (?cmp_chr_scl).")}
  if (!vw. ) {err. <- c(err., "\n • [w.] must be a complete character scalar (?cmp_chr_scl).")}
  if (idef(err.)) {stop(err.)}
  paste0(paste(..., sep = a.), collapse = w.)
}

#' @describeIn delim. Delimit within vectors in \code{...} using \code{w.},
#'   then delimit across using \code{a.}, resulting in a character-scalar string
#'   containing \code{\link[base]{...length()}} \code{w.}-delimited substrings,
#'   each of which contains \code{max(length(list(...)))} \code{a.}-delimited
#'   subsubstrings.
dwa <- function(w., a., ...) {
  x.  <- list(...)
  n.  <- length(x.)
  ns.  <- lengths(x.)
  vn. <- n. > 0
  vns. <- f0(!vn., T, all(ns. > 0))
  vx. <- f0(!vn. | !vns., T, all(sapply(x., ivec)))
  vw. <- chr_scl(w.)
  va. <- chr_scl(a.)
  err. <- NULL
  if (!vn. ) {err. <- c(err., "\n • [...] is empty.")}
  if (!vns.) {err. <- c(err., "\n • [...] contains an empty element.")}
  if (!vx. ) {err. <- c(err., "\n • Arguments in [...] must be populated atomic vecs (?pop_vec).")}
  if (!vw. ) {err. <- c(err., "\n • [w.] must be a complete character scalar (?cmp_chr_scl).")}
  if (!va. ) {err. <- c(err., "\n • [a.] must be a complete character scalar (?cmp_chr_scl).")}
  if (idef(err.)) {stop(err.)}
  paste(sapply(list(...), paste0, collapse = w.), sep = a.)
}

#' @describeIn delim. Delimit across vectors in \code{...} using blanks.
#' @export
da0 <- function(...) {da(a. = '', ...)}

#' @describeIn delim. Delimit across vectors in \code{...} using spaces.
#' @export
da1 <- function(...) {da(a. = ' ', ...)}

#' @describeIn delim. Delimit across vectors in \code{...} using broken-pipes.
#' @export
daB <- function(...) {da(a. = '¦', ...)}

#' @describeIn delim. Delimit across vectors in \code{...} using colons.
#' @export
daC <- function(...) {da(a. = ':', ...)}

#' @describeIn delim. Delimit across vectors in \code{...} using dots/periods.
#' @export
daD <- function(...) {da(a. = '.', ...)}

#' @describeIn delim. Delimit across vectors in \code{...} using grammatical
#'   commas.
#' @export
daG <- function(...) {da(a. = ', ', ...)}

#' @describeIn delim. Delimit across vectors in \code{...} using pipes.
#' @export
daP <- function(...) {da(a. = '|', ...)}

#' @describeIn delim. Delimit across vectors in \code{...} using
#'   (back)-quotes.
#' @export
daQ <- function(...) {da(a. = '`', ...)}

#' @describeIn delim. Delimit across vectors in \code{...} using simple
#'   commas.
#' @export
daS <- function(...) {da(a. = ',', ...)}

#' @describeIn delim. Delimit across vectors in \code{...} using tildes.
#' @export
daT <- function(...) {da(a. = '~', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using blanks.
#' @export
dw0 <- function(...) {dw(w. = '', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using spaces.
#' @export
dw1 <- function(...) {dw(w. = ' ', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using broken-pipes.
#' @export
dwB <- function(...) {dw(w. = '¦', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using colons.
#' @export
dwC <- function(...) {dw(w. = ':', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using dots/periods.
#' @export
dwD <- function(...) {dw(w. = '.', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using grammatical
#'   commas.
#' @export
dwG <- function(...) {dw(w. = ', ', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using pipes.
#' @export
dwP <- function(...) {dw(w. = '|', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using
#'   (back)-quotes.
#' @export
dwQ <- function(...) {dw(w. = '`', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using simple
#'   commas.
#' @export
dwS <- function(...) {dw(w. = ',', ...)}

#' @describeIn delim. Delimit within vectors in \code{...} using tildes.
#' @export
dwT <- function(...) {dw(w. = '~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks}.
#' @export
daw00 <- function(...) {daw(a. = '', w. = '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks} then \emph{spaces}, respectively.
#' @export
daw01 <- function(...) {daw(a. = '', w. = ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks} then \emph{broken-pipes}, respectively.
#' @export
daw0B <- function(...) {daw(a. = '', w. = '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks} then \emph{colons}, respectively.
#' @export
daw0C <- function(...) {daw(a. = '', w. = ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks} then \emph{dots/periods}, respectively.
#' @export
daw0D <- function(...) {daw(a. = '', w. = '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks} then \emph{grammatical commas},
#'   respectively.
#' @export
daw0G <- function(...) {daw(a. = '', w. = ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks} then \emph{pipes}, respectively.
#' @export
daw0P <- function(...) {daw(a. = '', w. = '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks} then \emph{(back)-quotes}, respectively.
#' @export
daw0Q <- function(...) {daw(a. = '', w. = '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks} then \emph{simple commas}, respectively.
#' @export
daw0S <- function(...) {daw(a. = '', w. = ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{blanks} then \emph{tildes}, respectively.
#' @export
daw0T <- function(...) {daw(a. = '', w. = '~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces} then \emph{blanks}, respectively.
#' @export
daw10 <- function(...) {daw(a. = ' ', w. = '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces}.
#' @export
daw11 <- function(...) {daw(a. = ' ', w. = ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces} then \emph{broken-pipes}, respectively.
#' @export
daw1B <- function(...) {daw(a. = ' ', w. = '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces} then \emph{colons}, respectively.
#' @export
daw1C <- function(...) {daw(a. = ' ', w. = ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces} then \emph{dots/periods}, respectively.
#' @export
daw1D <- function(...) {daw(a. = ' ', w. = '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces} then \emph{grammatical commas},
#'   respectively.
#' @export
daw1G <- function(...) {daw(a. = ' ', w. = ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces} then \emph{pipes}, respectively.
#' @export
daw1P <- function(...) {daw(a. = ' ', w. = '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces} then \emph{(back)-quotes}, respectively.
#' @export
daw1Q <- function(...) {daw(a. = ' ', w. = '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces} then \emph{simple commas}, respectively.
#' @export
daw1S <- function(...) {daw(a. = ' ', w. = ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{spaces} then \emph{tildes}, respectively.
#' @export
daw1T <- function(...) {daw(a. = ' ', w. = '~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{blanks}, respectively.
#' @export
dawB0 <- function(...) {daw(a. = '¦', w. = '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{spaces}, respectively.
#' @export
dawB1 <- function(...) {daw(a. = '¦', w. = ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes}.
#' @export
dawBB <- function(...) {daw(a. = '¦', w. = '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{colons}, respectively.
#' @export
dawBC <- function(...) {daw(a. = '¦', w. = ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{dots/periods},
#'   respectively.
#' @export
dawBD <- function(...) {daw(a. = '¦', w. = '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{grammatical commas},
#'   respectively.
#' @export
dawBG <- function(...) {daw(a. = '¦', w. = ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{pipes}, respectively.
#' @export
dawBP <- function(...) {daw(a. = '¦', w. = '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{(back)-quotes},
#'   respectively.
#' @export
dawBQ <- function(...) {daw(a. = '¦', w. = '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{simple commas},
#'   respectively.
#' @export
dawBS <- function(...) {daw(a. = '¦', w. = ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{tildes}, respectively.
#' @export
dawBT <- function(...) {daw(a. = '¦', w. = '~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons} then \emph{blanks}, respectively.
#' @export
dawC0 <- function(...) {daw(a. = ':', w. = '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons} then \emph{spaces}, respectively.
#' @export
dawC1 <- function(...) {daw(a. = ':', w. = ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons} then \emph{broken-pipes}, respectively.
#' @export
dawCB <- function(...) {daw(a. = ':', w. = '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons}.
#' @export
dawCC <- function(...) {daw(a. = ':', w. = ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons} then \emph{dots/periods}, respectively.
#' @export
dawCD <- function(...) {daw(a. = ':', w. = '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons} then \emph{grammatical commas},
#'   respectively.
#' @export
dawCG <- function(...) {daw(a. = ':', w. = ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons} then \emph{pipes}, respectively.
#' @export
dawCP <- function(...) {daw(a. = ':', w. = '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons} then \emph{(back)-quotes}, respectively.
#' @export
dawCQ <- function(...) {daw(a. = ':', w. = '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons} then \emph{simple commas}, respectively.
#' @export
dawCS <- function(...) {daw(a. = ':', w. = ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{colons} then \emph{tildes}, respectively.
#' @export
dawCT <- function(...) {daw(a. = ':', w. = '~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{blanks}, respectively.
#' @export
dawD0 <- function(...) {daw(a. = '.', w. = '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{spaces}, respectively.
#' @export
dawD1 <- function(...) {daw(a. = '.', w. = ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{broken-pipes},
#'   respectively.
#' @export
dawDB <- function(...) {daw(a. = '.', w. = '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{colons}, respectively.
#' @export
dawDC <- function(...) {daw(a. = '.', w. = ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods}.
#' @export
dawDD <- function(...) {daw(a. = '.', w. = '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{grammatical commas},
#'   respectively.
#' @export
dawDG <- function(...) {daw(a. = '.', w. = ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{pipes}, respectively.
#' @export
dawDP <- function(...) {daw(a. = '.', w. = '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{(back)-quotes},
#'   respectively.
#' @export
dawDQ <- function(...) {daw(a. = '.', w. = '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{simple commas},
#'   respectively.
#' @export
dawDS <- function(...) {daw(a. = '.', w. = ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{tildes}, respectively.
#' @export
dawDT <- function(...) {daw(a. = '.', w. = '~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{blanks},
#'   respectively.
#' @export
dawG0 <- function(...) {daw(a. = ', ', w. = '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{spaces},
#'   respectively.
#' @export
dawG1 <- function(...) {daw(a. = ', ', w. = ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{broken-pipes},
#'   respectively.
#' @export
dawGB <- function(...) {daw(a. = ', ', w. = '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{colons},
#'   respectively.
#' @export
dawGC <- function(...) {daw(a. = ', ', w. = ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{dots/periods},
#'   respectively.
#' @export
dawGD <- function(...) {daw(a. = ', ', w. = '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas}.
#' @export
dawGG <- function(...) {daw(a. = ', ', w. = ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{pipes}, respectively.
#' @export
dawGP <- function(...) {daw(a. = ', ', w. = '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{(back)-quotes},
#'   respectively.
#' @export
dawGQ <- function(...) {daw(a. = ', ', w. = '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{simple commas},
#'   respectively.
#' @export
dawGS <- function(...) {daw(a. = ', ', w. = ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{tildes},
#'   respectively.
#' @export
dawGT <- function(...) {daw(a. = ', ', w. = '~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes} then \emph{blanks}, respectively.
#' @export
dawP0 <- function(...) {daw(a. = '|', w. = '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes} then \emph{spaces}, respectively.
#' @export
dawP1 <- function(...) {daw(a. = '|', w. = ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes} then \emph{broken-pipes}, respectively.
#' @export
dawPB <- function(...) {daw(a. = '|', w. = '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes} then \emph{colons}, respectively.
#' @export
dawPC <- function(...) {daw(a. = '|', w. = ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes} then \emph{dots/periods}, respectively.
#' @export
dawPD <- function(...) {daw(a. = '|', w. = '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes} then \emph{grammatical commas}, respectively.
#' @export
dawPG <- function(...) {daw(a. = '|', w. = ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes}.
#' @export
dawPP <- function(...) {daw(a. = '|', w. = '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes} then \emph{(back)-quotes}, respectively.
#' @export
dawPQ <- function(...) {daw(a. = '|', w. = '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes} then \emph{simple commas}, respectively.
#' @export
dawPS <- function(...) {daw(a. = '|', w. = ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{pipes} then \emph{tildes}, respectively.
#' @export
dawPT <- function(...) {daw(a. = '|', w. = '~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{blanks}, respectively.
#' @export
dawQ0 <- function(...) {daw(a. = '`', w. = '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{spaces}, respectively.
#' @export
dawQ1 <- function(...) {daw(a. = '`', w. = ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{broken-pipes},
#'   respectively.
#' @export
dawQB <- function(...) {daw(a. = '`', w. = '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{colons}, respectively.
#' @export
dawQC <- function(...) {daw(a. = '`', w. = ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{dots/periods},
#'   respectively.
#' @export
dawQD <- function(...) {daw(a. = '`', w. = '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{grammatical commas},
#'   respectively.
#' @export
dawQG <- function(...) {daw(a. = '`', w. = ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{pipes}, respectively.
#' @export
dawQP <- function(...) {daw(a. = '`', w. = '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes}.
#' @export
dawQQ <- function(...) {daw(a. = '`', w. = '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{simple commas},
#'   respectively.
#' @export
dawQS <- function(...) {daw(a. = '`', w. = ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{tildes}, respectively.
#' @export
dawQT <- function(...) {daw(a. = '`', w. = '~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas} then \emph{blanks}, respectively.
#' @export
dawS0 <- function(...) {daw(a. = ',', w. = '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas} then \emph{spaces}, respectively.
#' @export
dawS1 <- function(...) {daw(a. = ',', w. = ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas} then \emph{broken-pipes},
#'   respectively.
#' @export
dawSB <- function(...) {daw(a. = ',', w. = '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas} then \emph{colons}, respectively.
#' @export
dawSC <- function(...) {daw(a. = ',', w. = ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas} then \emph{dots/periods},
#'   respectively.
#' @export
dawSD <- function(...) {daw(a. = ',', w. = '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas} then \emph{grammatical commas},
#'   respectively.
#' @export
dawSG <- function(...) {daw(a. = ',', w. = ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas} then \emph{pipes}, respectively.
#' @export
dawSP <- function(...) {daw(a. = ',', w. = '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas} then \emph{(back)-quotes},
#'   respectively.
#' @export
dawSQ <- function(...) {daw(a. = ',', w. = '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas}.
#' @export
dawSS <- function(...) {daw(a. = ',', w. = ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{simple commas} then \emph{tildes}, respectively.
#' @export
dawST <- function(...) {daw(a. = ',', w. = '~', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes} then \emph{blanks}, respectively.
#' @export
dawT0 <- function(...) {daw(a. = '~', w. = '', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes} then \emph{spaces}, respectively.
#' @export
dawT1 <- function(...) {daw(a. = '~', w. = ' ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes} then \emph{broken-pipes}, respectively.
#' @export
dawTB <- function(...) {daw(a. = '~', w. = '¦', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes} then \emph{colons}, respectively.
#' @export
dawTC <- function(...) {daw(a. = '~', w. = ':', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes} then \emph{dots/periods}, respectively.
#' @export
dawTD <- function(...) {daw(a. = '~', w. = '.', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes} then \emph{grammatical commas},
#'   respectively.
#' @export
dawTG <- function(...) {daw(a. = '~', w. = ', ', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes} then \emph{pipes}, respectively.
#' @export
dawTP <- function(...) {daw(a. = '~', w. = '|', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes} then \emph{(back)-quotes}, respectively.
#' @export
dawTQ <- function(...) {daw(a. = '~', w. = '`', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes} then \emph{simple commas}, respectively.
#' @export
dawTS <- function(...) {daw(a. = '~', w. = ',', ...)}

#' @describeIn delim. Delimit \emph{across} then \emph{within} vectors in
#'   \code{...} using \emph{tildes}.
#' @export
dawTT <- function(...) {daw(a. = '~', w. = '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks}.
#' @export
dwa00 <- function(...) {dwa(w. = '', a. = '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks} then \emph{spaces}, respectively.
#' @export
dwa01 <- function(...) {dwa(w. = '', a. = ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks} then \emph{broken-pipes}, respectively.
#' @export
dwa0B <- function(...) {dwa(w. = '', a. = '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks} then \emph{colons}, respectively.
#' @export
dwa0C <- function(...) {dwa(w. = '', a. = ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks} then \emph{dots/periods}, respectively.
#' @export
dwa0D <- function(...) {dwa(w. = '', a. = '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks} then \emph{grammatical commas},
#'   respectively.
#' @export
dwa0G <- function(...) {dwa(w. = '', a. = ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks} then \emph{pipes}, respectively.
#' @export
dwa0P <- function(...) {dwa(w. = '', a. = '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks} then \emph{(back)-quotes}, respectively.
#' @export
dwa0Q <- function(...) {dwa(w. = '', a. = '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks} then \emph{simple commas}, respectively.
#' @export
dwa0S <- function(...) {dwa(w. = '', a. = ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{blanks} then \emph{tildes}, respectively.
#' @export
dwa0T <- function(...) {dwa(w. = '', a. = '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces} then \emph{blanks}, respectively.
#' @export
dwa10 <- function(...) {dwa(w. = ' ', a. = '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces}.
#' @export
dwa11 <- function(...) {dwa(w. = ' ', a. = ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces} then \emph{broken-pipes}, respectively.
#' @export
dwa1B <- function(...) {dwa(w. = ' ', a. = '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces} then \emph{colons}, respectively.
#' @export
dwa1C <- function(...) {dwa(w. = ' ', a. = ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces} then \emph{dots/periods}, respectively.
#' @export
dwa1D <- function(...) {dwa(w. = ' ', a. = '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces} then \emph{grammatical commas},
#'   respectively.
#' @export
dwa1G <- function(...) {dwa(w. = ' ', a. = ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces} then \emph{pipes}, respectively.
#' @export
dwa1P <- function(...) {dwa(w. = ' ', a. = '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces} then \emph{(back)-quotes}, respectively.
#' @export
dwa1Q <- function(...) {dwa(w. = ' ', a. = '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces} then \emph{simple commas}, respectively.
#' @export
dwa1S <- function(...) {dwa(w. = ' ', a. = ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{spaces} then \emph{tildes}, respectively.
#' @export
dwa1T <- function(...) {dwa(w. = ' ', a. = '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{blanks}, respectively.
#' @export
dwaB0 <- function(...) {dwa(w. = '¦', a. = '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{spaces}, respectively.
#' @export
dwaB1 <- function(...) {dwa(w. = '¦', a. = ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes}.
#' @export
dwaBB <- function(...) {dwa(w. = '¦', a. = '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{colons}, respectively.
#' @export
dwaBC <- function(...) {dwa(w. = '¦', a. = ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{dots/periods},
#'   respectively.
#' @export
dwaBD <- function(...) {dwa(w. = '¦', a. = '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{grammatical commas},
#'   respectively.
#' @export
dwaBG <- function(...) {dwa(w. = '¦', a. = ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{pipes}, respectively.
#' @export
dwaBP <- function(...) {dwa(w. = '¦', a. = '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{(back)-quotes},
#'   respectively.
#' @export
dwaBQ <- function(...) {dwa(w. = '¦', a. = '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{simple commas},
#'   respectively.
#' @export
dwaBS <- function(...) {dwa(w. = '¦', a. = ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{broken-pipes} then \emph{tildes}, respectively.
#' @export
dwaBT <- function(...) {dwa(w. = '¦', a. = '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons} then \emph{blanks}, respectively.
#' @export
dwaC0 <- function(...) {dwa(w. = ':', a. = '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons} then \emph{spaces}, respectively.
#' @export
dwaC1 <- function(...) {dwa(w. = ':', a. = ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons} then \emph{broken-pipes}, respectively.
#' @export
dwaCB <- function(...) {dwa(w. = ':', a. = '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons}.
#' @export
dwaCC <- function(...) {dwa(w. = ':', a. = ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons} then \emph{dots/periods}, respectively.
#' @export
dwaCD <- function(...) {dwa(w. = ':', a. = '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons} then \emph{grammatical commas},
#'   respectively.
#' @export
dwaCG <- function(...) {dwa(w. = ':', a. = ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons} then \emph{pipes}, respectively.
#' @export
dwaCP <- function(...) {dwa(w. = ':', a. = '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons} then \emph{(back)-quotes}, respectively.
#' @export
dwaCQ <- function(...) {dwa(w. = ':', a. = '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons} then \emph{simple commas}, respectively.
#' @export
dwaCS <- function(...) {dwa(w. = ':', a. = ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{colons} then \emph{tildes}, respectively.
#' @export
dwaCT <- function(...) {dwa(w. = ':', a. = '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{blanks}, respectively.
#' @export
dwaD0 <- function(...) {dwa(w. = '.', a. = '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{spaces}, respectively.
#' @export
dwaD1 <- function(...) {dwa(w. = '.', a. = ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{broken-pipes},
#'   respectively.
#' @export
dwaDB <- function(...) {dwa(w. = '.', a. = '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{colons}, respectively.
#' @export
dwaDC <- function(...) {dwa(w. = '.', a. = ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods}.
#' @export
dwaDD <- function(...) {dwa(w. = '.', a. = '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{grammatical commas},
#'   respectively.
#' @export
dwaDG <- function(...) {dwa(w. = '.', a. = ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{pipes}, respectively.
#' @export
dwaDP <- function(...) {dwa(w. = '.', a. = '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{(back)-quotes},
#'   respectively.
#' @export
dwaDQ <- function(...) {dwa(w. = '.', a. = '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{simple commas},
#'   respectively.
#' @export
dwaDS <- function(...) {dwa(w. = '.', a. = ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{dots/periods} then \emph{tildes}, respectively.
#' @export
dwaDT <- function(...) {dwa(w. = '.', a. = '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{blanks},
#'   respectively.
#' @export
dwaG0 <- function(...) {dwa(w. = ', ', a. = '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{spaces},
#'   respectively.
#' @export
dwaG1 <- function(...) {dwa(w. = ', ', a. = ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{broken-pipes},
#'   respectively.
#' @export
dwaGB <- function(...) {dwa(w. = ', ', a. = '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{colons},
#'   respectively.
#' @export
dwaGC <- function(...) {dwa(w. = ', ', a. = ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{dots/periods},
#'   respectively.
#' @export
dwaGD <- function(...) {dwa(w. = ', ', a. = '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas}.
#' @export
dwaGG <- function(...) {dwa(w. = ', ', a. = ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{pipes}, respectively.
#' @export
dwaGP <- function(...) {dwa(w. = ', ', a. = '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{(back)-quotes},
#'   respectively.
#' @export
dwaGQ <- function(...) {dwa(w. = ', ', a. = '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{simple commas},
#'   respectively.
#' @export
dwaGS <- function(...) {dwa(w. = ', ', a. = ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{grammatical commas} then \emph{tildes},
#'   respectively.
#' @export
dwaGT <- function(...) {dwa(w. = ', ', a. = '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes} then \emph{blanks}, respectively.
#' @export
dwaP0 <- function(...) {dwa(w. = '|', a. = '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes} then \emph{spaces}, respectively.
#' @export
dwaP1 <- function(...) {dwa(w. = '|', a. = ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes} then \emph{broken-pipes}, respectively.
#' @export
dwaPB <- function(...) {dwa(w. = '|', a. = '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes} then \emph{colons}, respectively.
#' @export
dwaPC <- function(...) {dwa(w. = '|', a. = ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes} then \emph{dots/periods}, respectively.
#' @export
dwaPD <- function(...) {dwa(w. = '|', a. = '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes} then \emph{grammatical commas}, respectively.
#' @export
dwaPG <- function(...) {dwa(w. = '|', a. = ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes}.
#' @export
dwaPP <- function(...) {dwa(w. = '|', a. = '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes} then \emph{(back)-quotes}, respectively.
#' @export
dwaPQ <- function(...) {dwa(w. = '|', a. = '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes} then \emph{simple commas}, respectively.
#' @export
dwaPS <- function(...) {dwa(w. = '|', a. = ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{pipes} then \emph{tildes}, respectively.
#' @export
dwaPT <- function(...) {dwa(w. = '|', a. = '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{blanks}, respectively.
#' @export
dwaQ0 <- function(...) {dwa(w. = '`', a. = '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{spaces}, respectively.
#' @export
dwaQ1 <- function(...) {dwa(w. = '`', a. = ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{broken-pipes},
#'   respectively.
#' @export
dwaQB <- function(...) {dwa(w. = '`', a. = '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{colons}, respectively.
#' @export
dwaQC <- function(...) {dwa(w. = '`', a. = ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{dots/periods},
#'   respectively.
#' @export
dwaQD <- function(...) {dwa(w. = '`', a. = '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{grammatical commas},
#'   respectively.
#' @export
dwaQG <- function(...) {dwa(w. = '`', a. = ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{pipes}, respectively.
#' @export
dwaQP <- function(...) {dwa(w. = '`', a. = '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes}.
#' @export
dwaQQ <- function(...) {dwa(w. = '`', a. = '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{simple commas},
#'   respectively.
#' @export
dwaQS <- function(...) {dwa(w. = '`', a. = ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{(back)-quotes} then \emph{tildes}, respectively.
#' @export
dwaQT <- function(...) {dwa(w. = '`', a. = '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas} then \emph{blanks}, respectively.
#' @export
dwaS0 <- function(...) {dwa(w. = ',', a. = '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas} then \emph{spaces}, respectively.
#' @export
dwaS1 <- function(...) {dwa(w. = ',', a. = ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas} then \emph{broken-pipes},
#'   respectively.
#' @export
dwaSB <- function(...) {dwa(w. = ',', a. = '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas} then \emph{colons}, respectively.
#' @export
dwaSC <- function(...) {dwa(w. = ',', a. = ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas} then \emph{dots/periods},
#'   respectively.
#' @export
dwaSD <- function(...) {dwa(w. = ',', a. = '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas} then \emph{grammatical commas},
#'   respectively.
#' @export
dwaSG <- function(...) {dwa(w. = ',', a. = ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas} then \emph{pipes}, respectively.
#' @export
dwaSP <- function(...) {dwa(w. = ',', a. = '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas} then \emph{(back)-quotes},
#'   respectively.
#' @export
dwaSQ <- function(...) {dwa(w. = ',', a. = '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas}.
#' @export
dwaSS <- function(...) {dwa(w. = ',', a. = ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{simple commas} then \emph{tildes}, respectively.
#' @export
dwaST <- function(...) {dwa(w. = ',', a. = '~', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes} then \emph{blanks}, respectively.
#' @export
dwaT0 <- function(...) {dwa(w. = '~', a. = '', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes} then \emph{spaces}, respectively.
#' @export
dwaT1 <- function(...) {dwa(w. = '~', a. = ' ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes} then \emph{broken-pipes}, respectively.
#' @export
dwaTB <- function(...) {dwa(w. = '~', a. = '¦', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes} then \emph{colons}, respectively.
#' @export
dwaTC <- function(...) {dwa(w. = '~', a. = ':', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes} then \emph{dots/periods}, respectively.
#' @export
dwaTD <- function(...) {dwa(w. = '~', a. = '.', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes} then \emph{grammatical commas},
#'   respectively.
#' @export
dwaTG <- function(...) {dwa(w. = '~', a. = ', ', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes} then \emph{pipes}, respectively.
#' @export
dwaTP <- function(...) {dwa(w. = '~', a. = '|', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes} then \emph{(back)-quotes}, respectively.
#' @export
dwaTQ <- function(...) {dwa(w. = '~', a. = '`', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes} then \emph{simple commas}, respectively.
#' @export
dwaTS <- function(...) {dwa(w. = '~', a. = ',', ...)}

#' @describeIn delim. Delimit \emph{within} then \emph{across} vectors in
#'   \code{...} using \emph{tildes}.
#' @export
dwaTT <- function(...) {dwa(w. = '~', a. = '~', ...)}
