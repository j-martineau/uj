#' @name mkstr.
#' @family strings
#' @title Specialized string building functions
#' @param x. An object containing atomic values (atomized).
#' @param l.,r. Character scalars giving left and right side enclosures for
#'   \code{...} after atomization (see \code{\link{av.}}).
#' @param ... An arbitrary number of objects to be atomized into a single atomic
#'   vector.
#' @return Character vector
#' @export
mkstr. <- function() {help("mkstr.", package = "uj")}

#' @describeIn mkstr. Build a space-padded equality statements of atomized and
#'   glued values of \code{x.} and \code{...} (e.g., \code{geq(c('x', 'y', 'z'),
#'   0, 1, 2)} produces \code{'xyz = 012'}).
#' @export
geq <- function(x., ...) {paste0(av(x.), v(eq), av(...), collapse = v(blank))}

#' @describeIn mkstr. Build an unpadded equality statements of atomized and
#'   glued values of \code{.x} and \code{...} (e.g., \code{geq0(c('x', 'y',
#'   'z'), 0, 1, 2)} produces \code{'xyz=012'}).
#' @export
geq0 <- function(x., ...) {paste0(av(x.), v(eq0), av(...), collapse = v(blank))}

#' @describeIn mkstr. Build a vector concatenation statements of atomized and
#'   glued values of \code{...} (e.g., \code{gcat(c('x', 'y', 'z'), 0, 1, 2)}
#'   produces \code{'c(xyz012)'}).
#' @export
gcat <- function(...) {paste0("c", v(lparen), av(...), v(rparen), collapse = v(blank))}

#' @describeIn mkstr. Build an element extraction concatenation statement from
#'   atomized and glued values of \code{x.} and \code{...} (e.g.,
#'   \code{gelt(c('x', 'y', 'z'), 0, 1, 2)} produces \code{'xyz[012]'}).
#' @export
gelt <- function(x., ...) {paste0(av(x.), v(lbracket), av(...), v(rbracket), collapse = v(blank))}

#' @describeIn mkstr. Build a function call from atomized and glued values of
#'   \code{x.} and \code{...} (e.g., \code{gfun(c('x', 'y', 'z'), 0, 1, 2)}
#'   produces \code{'xyz(012)'}).
#' @export
gfun <- function(x., ...) {paste0(av(x.), v(lparen), av(...), v(rparen), collapse = v(blank))}

#' @describeIn mkstr. Build a comma-separated list from atomized and glued
#'   values of \code{...} (e.g., \code{glst(c('x', 'y', 'z'), 0, 1, 2)} produces
#'   \code{'x, y, z, 0, 1, 2'}).
#' @export
glst <- function(...) {paste0(av(...), collapse = v(comma))}

#' @describeIn mkstr. Build a model formula of additive terms from atomized and
#'   glued values of \code{x.} and \code{...} (e.g., \code{gform(c('x', 'y',
#'   'z'), 0, 1, 2)} produces \code{'xyz ~ 012'}).
#' @export
gform <- function(x., ...) {paste0(av(x.), v(tilde), dw(..., w. = v(plus)), collapse = v(blank))}

#' @describeIn mkstr. Build a backtick-quoted value from atomized and glued
#'   values of \code{...} (e.g., \code{gtick(c('x', 'y', 'z'), 0, 1, 2)}
#'   produces \code{'`xyz012`'}).
#' @export
gtick <- function(...) {paste0(v(tick) , av(...), v(tick), collapse = v(blank))}

#' @describeIn mkstr. Build left- and write-wrapped strings from atomized and
#'   glued values of \code{l.}, \code{r.}, and \code{...} (e.g.,
#'   \code{gwrap(c('a', 'b', 'c'), 'z', 0, 1, 2)} produces \code{'abc012z'}).
#' @export
gwrap <- function(l., r., ...) {paste0(av(l.), av(...), av(r.), collapse = v(blank))}

#' @describeIn mkstr. Build a curly-brace-enclosed value from atomized and glued
#'   values of \code{...} (e.g., \code{gbrace(c('x', 'y', 'z'), 0, 1, 2)}
#'   produces \code{'\{xyz012\}'}).
#' @export
gbrace <- function(...) {paste0(v(lbrace) , av(...), v(rbrace), collapse = v(blank))}

#' @describeIn mkstr. Build a colon-delimited list from atomized and glued
#'   values of \code{...} (e.g., \code{gcolon(c('x', 'y', 'z'), 0, 1, 2)}
#'   produces \code{'x:y:z:0:1:2'}).
#' @export
gcolon <- function(...) {paste0(av(...), collapse = v(colon0))}

#' @describeIn mkstr. Build a parentheses-enclosed value from atomized and glued
#'   values of \code{...} (e.g., \code{gparen(c('x', 'y', 'z'), 0, 1, 2)}
#'   produces \code{'(xyz012)'}).
#' @export
gparen <- function(...) {paste0(v(lparen) , av(...), v(rparen), collapse = v(blank))}

#' @describeIn mkstr. Build a single-straight-quote-enclosed value from atomized
#'   and glued values of \code{...} (e.g., \code{gquote(c('x', 'y', 'z'), 0, 1,
#'   2)} produces \code{"'xyz012'"}).
#' @export
gquote <- function(...) {paste0(v(quote) , av(...), v(quote), collapse = v(blank))}

#' @describeIn mkstr. Build a double-straight-quote-enclosed value from atomized
#'   and glued values of \code{...} (e.g., \code{gquote(c('x', 'y', 'z'), 0, 1,
#'   2)} produces \code{'"xyz012"'}).
#' @export
gquote2 <- function(...) {paste0(v(quote2), av(...), v(quote2), collapse = v(blank))}

#' @describeIn mkstr. Build a square-bracket-enclosed value from atomized and
#'   glued values of \code{...} (e.g., \code{gbracket(c('x', 'y', 'z'), 0, 1,
#'   2)} produces \code{'(xyz012)'}).
#' @export
gbracket <- function(...) {paste0(v(lbracket), av(...), v(rbracket), collapse = v(blank))}

#' @describeIn mkstr. Build vector concatenation statement from atomized values
#'   of \code{...} (e.g., \code{pcat(c('x', 'y', 'z'), 0, 1, 2)} produces
#'   \code{'c(x, y, z, 0, 1, 2)'}).
#' @export
pcat <- function(...) {paste0("c", v(lparen), paste(..., sep = v(comma)), v(rparen))}

#' @describeIn mkstr. Build string-padded equality statements (e.g.,
#'   \code{peq(c('x', 'y', 'z'), 0, 1, 2)} produces \code{c('x = 012', 'y =
#'   012', 'z = 012')}).
#' @export
peq <- function(x., ...) {paste0(x., v(eq), ...)}

#' @describeIn mkstr. Build unpadded equality statements (e.g.,
#'   \code{peq0(c('x', 'y', 'z'), 0, 1, 2)} produces \code{c('x=012', 'y=012',
#'   'z=012')}).
#' @export
peq0 <- function(x., ...) {paste0(x., v(eq0), ...)}

#' @describeIn mkstr. Build element extraction statements (e.g.,
#'   \code{pelt(c('x', 'y', 'z'), 0, 1, 2)} produces \code{c('x\[012\]',
#'   'y\[012\]', 'z\[012\]')}).
#' @export
pelt <- function(x., ...) {paste0(x., v(lbracket), ..., v(rbracket))}

#' @describeIn mkstr. Build function calls (e.g., \code{pfun(c('x', 'y', 'z'),
#'   0, 1, 2)} produces \code{c('x(012)', 'y(012)', 'z(012)')}).
#' @export
pfun <- function(x., ...) {paste0(x., v(lparen), ..., v(rparen))}

#' @describeIn mkstr. Build comma-separated lists (e.g., \code{plst(c('x', 'y',
#'   'z'), 0, 1, 2)} produces \code{c('x, 0, 1, 2', 'y, 0, 1, 2', 'z, 0, 1,
#'   2')}).
#' @export
plst <- function(...) {paste0(..., sep = ", ")}

#' @describeIn mkstr. Build model formula statements with additive terms only
#'   (e.g., \code{pform(c('x', 'y', 'z'), 0, 1, 2)} produces \code{c('x ~ 0 + 1
#'   + 2', 'y ~ 0 + 1 + 2', 'z ~ 0 + 1 + 2')}).
#' @export
pform <- function(x., ...) {paste0(x., v(tilde),  da(..., a. = v(plus)))}

#' @describeIn mkstr. Build tick enclosed vectors (e.g., \code{ptick(c('x', 'y',
#'   'z'), 0, 1, 2)} produces \code{c('`x012`', '`y012`', '`z012`')}).
#' @export
ptick <- function(...) {paste0(v(tick) , ..., v(tick))}

#' @describeIn mkstr. Build left/right wrapped vectors (e.g., \code{pwrap(c('l',
#'   'L', 'l'), 'r', 1, 2)} produces \code{c('l12r`', 'L12r`', 'l12r')}).
#' @export
pwrap <- function(l., r., ...) {paste0(l., ..., r.)}

#' @describeIn mkstr. Build curly-brace-enclosed vectors (e.g.,
#'   \code{pbrace(c('x', 'y', 'z'), 0, 1, 2)} produces \code{c('\{x012\}',
#'   '\{y012\}', '\{z012\}')}.
#' @export
pbrace <- function(...) {paste0(v(lbracket) , ..., v(rbracket))}

#' @describeIn mkstr. Build colon-delimited lists (e.g., \code{pcolon(c('x',
#'   'y', 'z'), 0, 1, 2)} produces \code{c('x:0:1:2', 'y:0:1:2', 'z:0:1:2')}).
#' @export
pcolon <- function(...) {paste0(..., sep = v(colon0))}

#' @describeIn mkstr. Build parentheses-enclosed vectors (e.g.,
#'   \code{pparen(c('x', 'y', 'z'), 0, 1, 2)} produces \code{c('(x012)',
#'   '(y012)', '(z012)')}.
#' @export
pparen <- function(...) {paste0(v(lparen) , ..., v(rparen))}

#' @describeIn mkstr. Build single-straight-quoted vectors (e.g.,
#'   \code{pquote(c('x', 'y', 'z'), 0, 1, 2)} produces \code{c("'x012'",
#'   "'y012'", "'z012'")}).
#' @export
pquote <- function(...) {paste0(v(quote) , ..., v(quote))}

#' @describeIn mkstr. Build single-straight-quoted vectors (e.g.,
#'   \code{pquote(c('x', 'y', 'z'), 0, 1, 2)} produces \code{c('"x012"',
#'   '"y012"', '"z012"')}).
#' @export
pquote2 <- function(...) {paste0(v(quote2), ..., v(quote2))}

#' @describeIn mkstr. Build square-bracket-enclosed vectors (e.g.,
#'   \code{pbrace(c('x', 'y', 'z'), 0, 1, 2)} produces \code{c('\[x012\]',
#'   '\[y012\]', '\[z012\]')}.
#' @export
pbracket <- function(...) {paste0(v(lbracket) , ..., v(rbracket))}
