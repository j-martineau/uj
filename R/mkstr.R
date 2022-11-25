#' @name mkstr
#' @family strings
#' @title Specialized String Building Functions
#' @description Build strings where function name components have the following
#'   meaning:\tabular{ll}{
#'     prefix = `g`         \tab Glue/collapse values within.           \cr
#'     prefix = `p`         \tab Paste values across.                   \cr
#'                          \tab                                        \cr
#'     suffix = `eq`        \tab Space-padded equality statement.       \cr
#'     suffix = `eq0`       \tab Non-padded equality statement.         \cr
#'     suffix = `cat`       \tab Vector concatenation statement.        \cr
#'     suffix = `elt`       \tab Element extraction statement.          \cr
#'     suffix = `fun`       \tab Function call statement.               \cr
#'     suffix = `lst`       \tab Comma-separated list.                  \cr
#'     suffix = `form`      \tab Formula statement.                     \cr
#'     suffix = `colon`     \tab Separate by colons.                    \cr
#'     suffix = `wrap`      \tab Enclose on left and right.             \cr
#'     suffix = `tick`      \tab Enclose in back-ticks.                 \cr
#'     suffix = `quote`     \tab Enclose in single quotes.              \cr
#'     suffix = `quote2`    \tab Enclose in double quotes.              \cr
#'     suffix = `brace`     \tab Enclose in curly braces.               \cr
#'     suffix = `paren`     \tab Enclose in parentheses.                \cr
#'     suffix = `bracket`   \tab Enclose in square brackets.              }
#' Each prefix is joined with each suffix to create a unique function. The
#' action indicated by the prefix happens first, followed by the action
#' indicated in the suffix. How each function works is illustrated in the
#' details.
#' @details \tabular{ll}{
#' FUNCTION CALL                   \tab RESULT                               \cr
#' `geq(c('x', 'y', 'z'), 0, 1, 2)`\tab `'xyz = 012'`                        \cr
#' `peq(c('x', 'y', 'z'), 0, 1, 2)`\tab `c('x = 012', 'y = 012', 'z = 012')` \cr
#'                                  \tab                                     \cr
#' `geq0(c('x', 'y', 'z'), 0, 1, 2)`\tab `'xyz=012'`                         \cr
#' `peq0(c('x', 'y', 'z'), 0, 1, 2)`\tab `c('x=012', 'y=012', 'z=012')`      \cr
#'                                  \tab                                     \cr
#' `gcat(c('x', 'y', 'z'), 0, 1, 2)`\tab `'c(xyz012)'`                       \cr
#' `pcat(c('x', 'y', 'z'), 0, 1, 2)`\tab `'c(x, y, z, 0, 1, 2)'`             \cr
#'                                  \tab                                     \cr
#' `gelt(c('x', 'y', 'z'), 0, 1, 2)`\tab `'xyz[012]'`                        \cr
#' `pelt(c('x', 'y', 'z'), 0, 1, 2)`\tab `c('x[0]', 'y[1]', 'z[2]')`         \cr
#'                                  \tab                                     \cr
#' `gfun(c('x', 'y', 'z'), 0, 1, 2)`\tab `'xyz(012)'`                        \cr
#' `pfun(c('x', 'y', 'z'), 0, 1, 2)`\tab `c('x(0)', 'y(1)', 'z(2)')`         \cr
#'                                  \tab                                     \cr
#' `glst(c('x', 'y', 'z'), 0, 1, 2)`\tab `'x, y, z, 0, 1, 2'`                \cr
#' `plst(c('x', 'y', 'z'), 0, 1, 2)`\tab \code{c('x, 0, 1, 2', 'y, 0, 1, 2',
#'                                               'z, 0, 1, 2')}              \cr
#'                                   \tab                                    \cr
#' `gform(c('x', 'y', 'z'), 0, 1, 2)`\tab `'xyz ~ 0 + 1 + 2'`                \cr
#' `pform(c('x', 'y', 'z'), 0, 1, 2)`\tab \code{c('x ~ 0 + 1 + 2',
#'                                                'y ~ 0 + 1 + 2',
#'                                                'z ~ 0 + 1 + 2')}          \cr
#'                                   \tab                                    \cr
#' `gtick(c('x', 'y', 'z'), 0, 1, 2)`\tab `'`xyz012`'`                       \cr
#' `ptick(c('x', 'y', 'z'), 0, 1, 2)`\tab `c('`x012`', '`y012`', '`z012`')`  \cr
#'                                    \tab                                   \cr
#' `gbrace(c('x', 'y', 'z'), 0, 1, 2)`\tab `'\{xyz012\}'`                    \cr
#' `pbrace(c('x', 'y', 'z'), 0, 1, 2)`\tab \code{c('\{x012\}', '\{y012\}',
#'                                                 '\{z012\`')}              \cr
#'                                    \tab                                   \cr
#' `gcolon(c('x', 'y', 'z'), 0, 1, 2)`\tab `'x:y:z:0:1:2'`                   \cr
#' `pcolon(c('x', 'y', 'z'), 0, 1, 2)`\tab \code{c('x:0:1:2', 'y:0:1:2',
#'                                                 'z:0:1:2')}               \cr
#'                                    \tab                                   \cr
#' `gparen(c('x', 'y', 'z'), 0, 1, 2)`\tab `'(xyz012)'`                      \cr
#' `pparen(c('x', 'y', 'z'), 0, 1, 2)`\tab `c('(x012)', '(y012)', '(z012)')` \cr
#'                                    \tab                                   \cr
#' `gquote(c('x', 'y', 'z'), 0, 1, 2)`\tab `"'xyz012'"`                      \cr
#' `pquote(c('x', 'y', 'z'), 0, 1, 2)`\tab `c('"x012"', '"y012"', '"z012"')` \cr
#'                                     \tab                                  \cr
#' `gquote2(c('x', 'y', 'z'), 0, 1, 2)`\tab `"'xyz012'"`                     \cr
#' `pquote2(c('x', 'y', 'z'), 0, 1, 2)`\tab `c('"x012"', '"y012"', '"z012"')`\cr
#'                                      \tab                                 \cr
#' `gbracket(c('x', 'y', 'z'), 0, 1, 2)`\tab `'[xyz012]'`                    \cr
#' `pbracket(c('x', 'y', 'z'), 0, 1, 2)`\tab \code{c('[x012]', '[y012]',
#'                                                  '[z012]')}               \cr
#'                                        \tab                               \cr
#' `gwrap(c('l', 'L', 'l'), 'r', 0, 1, 2)`\tab `'lLl012r'`                   \cr
#' `pwrap(c('l', 'L', 'l'), 'r', 0, 1, 2)`    \tab \code{c('l012r', 'L012r',
#'                                                        'l012r')}            }
#' @param x An object containing atomic values (atomized before processing).
#' @param l,r \link[=cmp_chr_scl]{Complete character scalars} giving left and
#'   right side enclosures for `...` after \link[=a]{atomization}.
#' @param ... An arbitrary number of objects to be atomized into a single atomic
#'   vector.
#' @return A character scalar or vector
#' @export
geq <- function(x, ...) {paste0(av(x), " = ", av(...), collapse = "")}

#' @rdname mkstr
#' @export
geq0 <- function(x, ...) {paste0(av(x), "=", av(...), collapse = "")}

#' @rdname mkstr
#' @export
gcat <- function(...) {paste0("c(", av(...), ")", collapse = "")}

#' @rdname mkstr
#' @export
gelt <- function(x, ...) {paste0(av(x), "[", av(...), "]", collapse = "")}

#' @rdname mkstr
#' @export
gfun <- function(x, ...) {paste0(av(x), "(", av(...), ")", collapse = "")}

#' @rdname mkstr
#' @export
glst <- function(...) {paste0(av(...), collapse = ", ")}

#' @rdname mkstr
#' @export
gform <- function(x, ...) {paste0(av(x), " ~ ", paste0(av(...), collapse = " + "), collapse = "")}

#' @rdname mkstr
#' @export
gtick <- function(...) {paste0("`" , av(...), "`", collapse = "")}

#' @rdname mkstr
#' @export
gwrap <- function(l, r, ...) {paste0(av(l), av(...), av(r), collapse = "")}

#' @rdname mkstr
#' @export
gbrace <- function(...) {paste0("{" , av(...), "}", collapse = "")}

#' @rdname mkstr
#' @export
gcolon <- function(...) {paste0(av(...), collapse = ":")}

#' @rdname mkstr
#' @export
gparen <- function(...) {paste0("(" , av(...), ")", collapse = "")}

#' @rdname mkstr
#' @export
gquote <- function(...) {paste0("'" , av(...), "'", collapse = "")}

#' @rdname mkstr
#' @export
gquote2 <- function(...) {paste0("\"", av(...), "\"", collapse = "")}

#' @rdname mkstr
#' @export
gbracket <- function(...) {paste0("[", av(...), "]", collapse = "")}

#' @rdname mkstr
#' @export
pcat <- function(...) {paste0("c(", paste(..., sep = ", "), ")")}

#' @rdname mkstr
#' @export
peq <- function(x, ...) {paste0(x, " = ", ...)}

#' @rdname mkstr
#' @export
peq0 <- function(x, ...) {paste0(x, "=", ...)}

#' @rdname mkstr
#' @export
pelt <- function(x, ...) {paste0(x, "[", ..., "]")}

#' @rdname mkstr
#' @export
pfun <- function(x, ...) {paste0(x, "(", ..., ")")}

#' @rdname mkstr
#' @export
plst <- function(...) {paste0(..., sep = ", ")}

#' @rdname mkstr
#' @export
pform <- function(x, ...) {paste0(x, " ~ ",  paste0(..., sep = " + "))}

#' @rdname mkstr
#' @export
ptick <- function(...) {paste0("`" , ..., "`")}

#' @rdname mkstr
#' @export
pwrap <- function(l, r, ...) {paste0(l, ..., r)}

#' @rdname mkstr
#' @export
pbrace <- function(...) {paste0("[" , ..., "]")}

#' @rdname mkstr
#' @export
pcolon <- function(...) {paste0(..., sep = ":")}

#' @rdname mkstr
#' @export
pparen <- function(...) {paste0("(" , ..., ")")}

#' @rdname mkstr
#' @export
pquote <- function(...) {paste0("'" , ..., "'")}

#' @rdname mkstr
#' @export
pquote2 <- function(...) {paste0("\"", ..., "\"")}

#' @rdname mkstr
#' @export
pbracket <- function(...) {paste0("[" , ..., "]")}
