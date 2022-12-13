#' @name mkstr
#' @family strings
#' @title Specialized string building functions
#' @description Build strings where function names are composed of a single-letter prefix and a longer suffix. Prefixes and their meanings are
#' \tabular{rl}{
#'       `g`   \tab Glue/collapse `...` args
#'   \cr `p`   \tab Paste across corresponding element of `...`.
#' }
#' Suffixes and their meanings are
#' \tabular{rl}{
#'      `bracket`   \tab Enclose in square brackets.
#'   \cr `quote2`   \tab   double quotes.
#'   \cr  `quote`   \tab   single quotes.
#'   \cr  `paren`   \tab   parentheses.
#'   \cr  `brace`   \tab   braces.
#'   \cr   `tick`   \tab   backticks.
#'   \cr   `wrap`   \tab   left & right.
#'   \cr            \tab   
#'   \cr  `colon`   \tab Delimit with colons.
#'   \cr    `lst`   \tab   comma + space.
#'   \cr            \tab   
#'   \cr   `form`   \tab Formula statement.
#'   \cr    `elt`   \tab   element extraction
#'   \cr    `fun`   \tab   function call
#'   \cr    `cat`   \tab   vector concatenation
#'   \cr    `eq0`   \tab   unpadded equality
#'   \cr     `eq`   \tab   space-padded equality
#' }
#' Each prefix is joined with each suffix to create a unique function. The action indicated by the prefix happens first, followed by the action indicated in the suffix. How each function works is illustrated in the details.
#' @param x An object containing atomic values (atomized before processing).
#' @param l,r \link[=cmp_chr_scl]{Complete character scalars} giving left and right side enclosures for `...` after \link[=a]{atomization}.
#' @param ... An arbitrary number of objects to be atomized into a single atomic vector.
#' @return A character scalar or vector
#' @examples
#' geq(c('x', 'y', 'z'), 0, 1, 2)
#' geq0(c('x', 'y', 'z'), 0, 1, 2)
#' gcat(c('x', 'y', 'z'), 0, 1, 2)
#' gelt(c('x', 'y', 'z'), 0, 1, 2)
#' gfun(c('x', 'y', 'z'), 0, 1, 2)
#' gform(c('x', 'y', 'z'), 0, 1, 2)
#'
#' peq(c('x', 'y', 'z'), 0, 1, 2)
#' peq0(c('x', 'y', 'z'), 0, 1, 2)
#' pcat(c('x', 'y', 'z'), 0, 1, 2)
#' pelt(c('x', 'y', 'z'), 0, 1, 2)
#' pfun(c('x', 'y', 'z'), 0, 1, 2)
#' pform(c('x', 'y', 'z'), 0, 1, 2)
#'
#' glst(c('x', 'y', 'z'), 0, 1, 2)
#' gtick(c('x', 'y', 'z'), 0, 1, 2)
#'
#' plst(c('x', 'y', 'z'), 0, 1, 2)
#' ptick(c('x', 'y', 'z'), 0, 1, 2)
#'
#' gbrace(c('x', 'y', 'z'), 0, 1, 2)
#' gcolon(c('x', 'y', 'z'), 0, 1, 2)
#' gparen(c('x', 'y', 'z'), 0, 1, 2)
#' gquote(c('x', 'y', 'z'), 0, 1, 2)
#' gquote2(c('x', 'y', 'z'), 0, 1, 2)
#' gbracket(c('x', 'y', 'z'), 0, 1, 2)
#' gwrap(c('l', 'L', 'l'), 'r', 0, 1, 2)
#'
#' pbrace(c('x', 'y', 'z'), 0, 1, 2)
#' pcolon(c('x', 'y', 'z'), 0, 1, 2)
#' pparen(c('x', 'y', 'z'), 0, 1, 2)
#' pquote(c('x', 'y', 'z'), 0, 1, 2)
#' pquote2(c('x', 'y', 'z'), 0, 1, 2)
#' pbracket(c('x', 'y', 'z'), 0, 1, 2)
#' pwrap(c('l', 'L', 'l'), 'r', 0, 1, 2)
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
