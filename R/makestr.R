#' @name makestr
#' @encoding UTF-8
#' @family strings
#' @title Specialized string building functions
#' @description Build strings where function names are composed of a single-letter prefix and a longer suffix. See *details*.
#' @details Prefixes and their meanings are:
#' \tabular{ll}{  `g`         \tab Glue/collapse `...` args                     \cr
#'                `p`         \tab Paste across corresponding element of `...`.   }
#' \cr\cr Suffixes and their meanings are:
#' \tabular{ll}{  `bracket`   \tab Enclose in square brackets                    \cr
#'                `paren`     \tab Enclose in parentheses                        \cr
#'                `brace`     \tab Enclose in braces                \cr   \tab   \cr
#'                `quote2`    \tab Enclose in double quotes                      \cr
#'                `quote`     \tab Enclose in single quotes                      \cr
#'                `tick`      \tab Enclose in backticks             \cr   \tab   \cr
#'                `wrap`      \tab Enclose left and right           \cr   \tab   \cr
#'                `colon`     \tab Delimit with colons                           \cr
#'                `list`      \tab Delimit with comma + space       \cr   \tab   \cr
#'                `form`      \tab formula statement                             \cr
#'                `elt`       \tab Element extraction statement                  \cr
#'                `fun`       \tab function call statement                       \cr
#'                `cat`       \tab Vector concatenation statement   \cr   \tab   \cr
#'                `eq0`       \tab Unpadded equality statement                   \cr
#'                `eq`        \tab Space-padded equality statement                 }
#' \cr\ Each prefix is joined with each suffix to create a unique function. The action indicated by the prefix happens first, followed by the action indicated in the suffix.
#' How each function works is illustrated in the details.
#' @param X An object containing atomic values (atomized before processing).
#' @param L,R \link[=cmp_chr_scl]{Complete character scalars} giving left and right side enclosures for `...` after \link[=av]{atomization}.
#' @param ... An arbitrary number of objects to be atomized into a single atomic vector.
#' @return A character scalar or vector
#' @examples
#' geq(c('X', 'Y', 'Z'), 0, 1, 2)
#' peq(c('X', 'Y', 'Z'), 0, 1, 2)
#' geq0(c('X', 'Y', 'Z'), 0, 1, 2)
#' peq0(c('X', 'Y', 'Z'), 0, 1, 2)
#' gcat(c('X', 'Y', 'Z'), 0, 1, 2)
#' pcat(c('X', 'Y', 'Z'), 0, 1, 2)
#' gelt(c('X', 'Y', 'Z'), 0, 1, 2)
#' pelt(c('X', 'Y', 'Z'), 0, 1, 2)
#' gfun(c('X', 'Y', 'Z'), 0, 1, 2)
#' pfun(c('X', 'Y', 'Z'), 0, 1, 2)
#' gform(c('X', 'Y', 'Z'), 0, 1, 2)
#' pform(c('X', 'Y', 'Z'), 0, 1, 2)
#' glist(c('X', 'Y', 'Z'), 0, 1, 2)
#' plist(c('X', 'Y', 'Z'), 0, 1, 2)
#' gtick(c('X', 'Y', 'Z'), 0, 1, 2)
#' ptick(c('X', 'Y', 'Z'), 0, 1, 2)
#' gwrap(c('L', 'L', 'L'), 'R', 0, 1, 2)
#' pwrap(c('L', 'L', 'L'), 'R', 0, 1, 2)
#' gbrace(c('X', 'Y', 'Z'), 0, 1, 2)
#' pbrace(c('X', 'Y', 'Z'), 0, 1, 2)
#' gcolon(c('X', 'Y', 'Z'), 0, 1, 2)
#' pcolon(c('X', 'Y', 'Z'), 0, 1, 2)
#' gparen(c('X', 'Y', 'Z'), 0, 1, 2)
#' pparen(c('X', 'Y', 'Z'), 0, 1, 2)
#' gquote(c('X', 'Y', 'Z'), 0, 1, 2)
#' pquote(c('X', 'Y', 'Z'), 0, 1, 2)
#' gquote2(c('X', 'Y', 'Z'), 0, 1, 2)
#' pquote2(c('X', 'Y', 'Z'), 0, 1, 2)
#' gbracket(c('X', 'Y', 'Z'), 0, 1, 2)
#' pbracket(c('X', 'Y', 'Z'), 0, 1, 2)
#' @export
makestr <- function() {utils::help("makestr", package = "uj")}

#' @rdname makestr
#' @export
geq <- function(X, ...) {base::paste0(base::c(uj::av(X), uj::v(equal1), uj::av(...)), collapse = uj::v(blank))}

#' @rdname makestr
#' @export
geq0 <- function(X, ...) {base::paste0(base::c(uj::av(X), uj::v(equal), uj::av(...)), collapse = uj::v(blank))}

#' @rdname makestr
#' @export
gcat <- function(...) {base::paste0(base::c("c", uj::v(lparen), uj::av(...), uj::v(rparen)), collapse = uj::v(blank))}

#' @rdname makestr
#' @export
gelt <- function(X, ...) {base::paste0(base::c(uj::av(X), uj::v(lbracket), uj::av(...), uj::v(rbracket)))}

#' @rdname makestr
#' @export
gfun <- function(X, ...) {base::paste0(base::c(uj::av(X), uj::v(lparen), uj::av(...), uj::v(rparen)), collapse = uj::v(blank))}

#' @rdname makestr
#' @export
glist <- function(...) {base::paste0(uj::av(...), collapse = uj::v(comma1))}

#' @rdname makestr
#' @export
gform <- function(X, ...) {base::paste0(base::c(uj::av(X), uj::v(tilde1), base::paste0(uj::av(...), collapse = uj::v(plus1))), collapse = uj::v(blank))}

#' @rdname makestr
#' @export
gtick <- function(...) {base::paste0(base::c(uj::v(tick) , uj::av(...), uj::v(tick)), collapse = uj::v(blank))}

#' @rdname makestr
#' @export
gwrap <- function(L, R, ...) {base::paste0(base::c(uj::av(L), uj::av(...), uj::av(R)), collapse = uj::v(blank))}

#' @rdname makestr
#' @export
gbrace <- function(...) {base::paste0(base::c(uj::v(lbrace) , uj::av(...), uj::v(rbraace)), collapse = uj::v(blank))}

#' @rdname makestr
#' @export
gcolon <- function(...) {base::paste0(uj::av(...), collapse = uj::v(colon))}

#' @rdname makestr
#' @export
gparen <- function(...) {base::paste0(base::c(uj::v(lparen) , uj::av(...), uj::v(rparen)), collapse = uj::v(blank))}

#' @rdname makestr
#' @export
gquote <- function(...) {base::paste0(base::c(uj::v(lquote) , uj::av(...), uj::v(rquote)), collapse = uj::v(blank))}

#' @rdname makestr
#' @export
gquote2 <- function(...) {base::paste0(base::c(uj::v(lquote2), uj::av(...), uj::v(rquote2), collapse = uj::v(blank)))}

#' @rdname makestr
#' @export
gbracket <- function(...) {base::paste0(base::c(uj::v(lbracket), uj::av(...), uj::v(rbracket)), collapse = uj::v(blank))}

#' @rdname makestr
#' @export
pcat <- function(...) {base::paste0("c", uj::v(lparen), base::paste(uj::v(comma1), ...), uj::v(rparen))}

#' @rdname makestr
#' @export
peq <- function(X, ...) {base::paste0(X, uj::v(equal1), ...)}

#' @rdname makestr
#' @export
peq0 <- function(X, ...) {base::paste0(X, uj::v(equal), ...)}

#' @rdname makestr
#' @export
pelt <- function(X, ...) {base::paste0(X, uj::v(lbracket), ..., uj::v(rbracket))}

#' @rdname makestr
#' @export
pfun <- function(X, ...) {base::paste0(X, uj::v(lparen), ..., uj::v(rparen))}

#' @rdname makestr
#' @export
plist <- function(...) {base::paste(..., sep = uj::v(comma1))}

#' @rdname makestr
#' @export
pform <- function(X, ...) {base::paste0(X, uj::v(tilde1),  base::paste(uj::v(plus1), ...))}

#' @rdname makestr
#' @export
ptick <- function(...) {base::paste0(uj::v(tick) , ..., uj::v(tick))}

#' @rdname makestr
#' @export
pwrap <- function(L, R, ...) {base::paste0(L, ..., R)}

#' @rdname makestr
#' @export
pbrace <- function(...) {base::paste0(uj::v(lbrace) , ..., uj::v(rbrace))}

#' @rdname makestr
#' @export
pcolon <- function(...) {base::paste(..., sep = uj::v(colon))}

#' @rdname makestr
#' @export
pparen <- function(...) {base::paste0(uj::v(lparen), ..., uj::v(rparen))}

#' @rdname makestr
#' @export
pquote <- function(...) {base::paste0(uj::v(quote) , ..., uj::v(quote))}

#' @rdname makestr
#' @export
pquote2 <- function(...) {base::paste0(uj::v(quote2), ..., uj::v(quote2))}

#' @rdname makestr
#' @export
pbracket <- function(...) {base::paste0(uj::v(lbracket), ..., uj::v(rbracklet))}
