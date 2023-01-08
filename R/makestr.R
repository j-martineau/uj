#' @name makestr
#' @encoding UTF-8
#' @family strings
#' @title Specialized string building functions
#' @description Build strings where function names are composed of a single-letter prefix and a longer suffix. Prefixes and their meanings are
#' \tabular{rl}{
#'       `g`   \tab Glue/collapse `...` args
#'   \cr `p`   \tab Paste across corresponding element of `...`.
#' }
#' Suffixes and their meanings are
#' \tabular{rl}{
#'      `bracket`   \tab Enclose in square brackets
#'   \cr `quote2`   \tab Enclose in double quotes
#'   \cr  `quote`   \tab Enclose in single quotes
#'   \cr  `paren`   \tab Enclose in parentheses
#'   \cr  `brace`   \tab Enclose in braces
#'   \cr   `tick`   \tab Enclose in backticks
#'   \cr   `wrap`   \tab Enclose left and right
#'   \cr            \tab  
#'   \cr  `colon`   \tab Delimit with colons
#'   \cr    `lst`   \tab Delimit with comma + space
#'   \cr            \tab  
#'   \cr   `form`   \tab Formula statement
#'   \cr    `elt`   \tab Element extraction statement
#'   \cr    `fun`   \tab Function call statement
#'   \cr    `cat`   \tab Vector concatenation statement
#'   \cr    `eq0`   \tab Unpadded equality statement
#'   \cr     `eq`   \tab Space-padded equality statement
#' }
#' Each prefix is joined with each suffix to create a unique function. The action indicated by the prefix happens first, followed by the action indicated in the suffix. How each function works is illustrated in the details.
#' @param x An object containing atomic values (atomized before processing).
#' @param l,r \link[=cmp_chr_scl]{Complete character scalars} giving left and right side enclosures for `...` after \link[=a]{atomization}.
#' @param ... An arbitrary number of objects to be atomized into a single atomic vector.
#' @return A character scalar or vector
#' @examples
#' geq(c('x', 'y', 'z'), 0, 1, 2)
#' peq(c('x', 'y', 'z'), 0, 1, 2)
#'
#' geq0(c('x', 'y', 'z'), 0, 1, 2)
#' peq0(c('x', 'y', 'z'), 0, 1, 2)
#'
#' gcat(c('x', 'y', 'z'), 0, 1, 2)
#' pcat(c('x', 'y', 'z'), 0, 1, 2)
#'
#' gelt(c('x', 'y', 'z'), 0, 1, 2)
#' pelt(c('x', 'y', 'z'), 0, 1, 2)
#'
#' gfun(c('x', 'y', 'z'), 0, 1, 2)
#' pfun(c('x', 'y', 'z'), 0, 1, 2)
#'
#' gform(c('x', 'y', 'z'), 0, 1, 2)
#' pform(c('x', 'y', 'z'), 0, 1, 2)
#'
#' glst(c('x', 'y', 'z'), 0, 1, 2)
#' plst(c('x', 'y', 'z'), 0, 1, 2)
#'
#' gtick(c('x', 'y', 'z'), 0, 1, 2)
#' ptick(c('x', 'y', 'z'), 0, 1, 2)
#'
#' gbrace(c('x', 'y', 'z'), 0, 1, 2)
#' pbrace(c('x', 'y', 'z'), 0, 1, 2)
#'
#' gcolon(c('x', 'y', 'z'), 0, 1, 2)
#' pcolon(c('x', 'y', 'z'), 0, 1, 2)
#'
#' gparen(c('x', 'y', 'z'), 0, 1, 2)
#' pparen(c('x', 'y', 'z'), 0, 1, 2)
#'
#' gquote(c('x', 'y', 'z'), 0, 1, 2)
#' pquote(c('x', 'y', 'z'), 0, 1, 2)
#'
#' gquote2(c('x', 'y', 'z'), 0, 1, 2)
#' pquote2(c('x', 'y', 'z'), 0, 1, 2)
#'
#' gbracket(c('x', 'y', 'z'), 0, 1, 2)
#' pbracket(c('x', 'y', 'z'), 0, 1, 2)
#'
#' gwrap(c('l', 'L', 'l'), 'r', 0, 1, 2)
#' pwrap(c('l', 'L', 'l'), 'r', 0, 1, 2)
#' @export
geq <- function(x, ...) {base::paste0(base::c(uj::av(x), " = ", uj::av(...)), collapse = "")}

#' @rdname makestr
#' @export
geq0 <- function(x, ...) {base::paste0(base::c(uj::av(x), "=", uj::av(...)), collapse = "")}

#' @rdname makestr
#' @export
gcat <- function(...) {base::paste0(base::c("c(", uj::av(...), ")"), collapse = "")}

#' @rdname makestr
#' @export
gelt <- function(x, ...) {base::paste0(base::c(uj::av(x), "[", uj::av(...), "]"), collapse = "")}

#' @rdname makestr
#' @export
gfun <- function(x, ...) {base::paste0(base::c(uj::av(x), "(", uj::av(...), ")"), collapse = "")}

#' @rdname makestr
#' @export
glst <- function(...) {base::paste0(uj::av(...), collapse = ", ")}

#' @rdname makestr
#' @export
gform <- function(x, ...) {base::paste0(base::c(uj::av(x), " ~ ", base::paste0(uj::av(...), collapse = " + ")), collapse = "")}

#' @rdname makestr
#' @export
gtick <- function(...) {base::paste0(base::c("`" , uj::av(...), "`"), collapse = "")}

#' @rdname makestr
#' @export
gwrap <- function(l, r, ...) {base::paste0(base::c(uj::av(l), uj::av(...), uj::av(r)), collapse = "")}

#' @rdname makestr
#' @export
gbrace <- function(...) {base::paste0(base::c("{" , uj::av(...), "}"), collapse = "")}

#' @rdname makestr
#' @export
gcolon <- function(...) {base::paste0(uj::av(...), collapse = ":")}

#' @rdname makestr
#' @export
gparen <- function(...) {base::paste0(base::c("(" , uj::av(...), ")"), collapse = "")}

#' @rdname makestr
#' @export
gquote <- function(...) {base::paste0(base::c("'" , uj::av(...), "'"), collapse = "")}

#' @rdname makestr
#' @export
gquote2 <- function(...) {base::paste0(base::c("\"", uj::av(...), "\""), collapse = "")}

#' @rdname makestr
#' @export
gbracket <- function(...) {base::paste0(base::c("[", uj::av(...), "]"), collapse = "")}

#' @rdname makestr
#' @export
pcat <- function(...) {base::paste0("c(", base::paste(..., sep = ", "), ")")}

#' @rdname makestr
#' @export
peq <- function(x, ...) {base::paste(x, " = ", ..., sep = "")}

#' @rdname makestr
#' @export
peq0 <- function(x, ...) {base::paste(x, "=", ..., sep = "")}

#' @rdname makestr
#' @export
pelt <- function(x, ...) {base::paste(x, "[", ..., "]", sep = "")}

#' @rdname makestr
#' @export
pfun <- function(x, ...) {base::paste(x, "(", ..., ")", sep = "")}

#' @rdname makestr
#' @export
plst <- function(...) {base::paste(..., sep = ", ")}

#' @rdname makestr
#' @export
pform <- function(x, ...) {base::paste0(x, " ~ ",  base::paste(..., sep = " + "))}

#' @rdname makestr
#' @export
ptick <- function(...) {base::paste("`" , ..., "`", sep = "")}

#' @rdname makestr
#' @export
pwrap <- function(l, r, ...) {base::paste(l, ..., r, sep = "")}

#' @rdname makestr
#' @export
pbrace <- function(...) {base::paste("[" , ..., "]", sep = "")}

#' @rdname makestr
#' @export
pcolon <- function(...) {base::paste(..., sep = ":")}

#' @rdname makestr
#' @export
pparen <- function(...) {base::paste("(" , ..., ")", sep = "")}

#' @rdname makestr
#' @export
pquote <- function(...) {base::paste("'" , ..., "'", sep = "")}

#' @rdname makestr
#' @export
pquote2 <- function(...) {base::paste("\"", ..., "\"", sep = "")}

#' @rdname makestr
#' @export
pbracket <- function(...) {base::paste("[" , ..., "]", sep = "")}
