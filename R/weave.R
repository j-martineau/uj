#' @encoding UTF-8
#' @family strings
#' @title Weave inlay values into a string
#' @description A reformulation of \code{\link[base]{sprintf}} relying on a template containing escape sequences which in turn contain formatting switches. Escape sequences specify where to insert inlay arguments. Switches specify how to format arguments to be inlaid, and come in three families as shown in the table below
#' \tabular{llll}{
#'     **Switch** \tab   **Switch Type**                  \tab   **Sample**  \tab   **Text of Result**    \cr
#'     **Value**  \tab   **(and Formatting)**             \tab   **Arg**     \tab   **with Sample Arg**   \cr
#'     `'0'`      \tab   `decimal `(`0` decimal points)   \tab   `arg = pi`  \tab   `3`                   \cr
#'     `'1'`      \tab   `decimal `(`1` decimal point)    \tab   `arg = pi`  \tab   `3.1`                 \cr
#'     `'2'`      \tab   `decimal `(`2` decimal points)   \tab   `arg = pi`  \tab   `3.14`                \cr
#'     `'3'`      \tab   `decimal `(`3` decimal points)   \tab   `arg = pi`  \tab   `3.141`               \cr
#'     `'4'`      \tab   `decimal `(`4` decimal points)   \tab   `arg = pi`  \tab   `3.1415`              \cr
#'     `'5'`      \tab   `decimal `(`5` decimal points)   \tab   `arg = pi`  \tab   `3.14159`             \cr
#'     `'6'`      \tab   `decimal `(`6` decimal points)   \tab   `arg = pi`  \tab   `3.141592`            \cr
#'     `'7'`      \tab   `decimal `(`7` decimal points)   \tab   `arg = pi`  \tab   `3.1415926`           \cr
#'     `'8'`      \tab   `decimal `(`8` decimal points)   \tab   `arg = pi`  \tab   `3.14159265`          \cr
#'     `'9'`      \tab   `decimal `(`9` decimal points)   \tab   `arg = pi`  \tab   `3.141592653`         \cr
#'     `'q'`      \tab   `quote   `(single straight)      \tab   `arg = 'x'` \tab   `'x'`                 \cr
#'     `'Q'`      \tab   `quote   `(double straight)      \tab   `arg = 'x'` \tab   `"x"`                 \cr
#'     `'t'`      \tab   `quote   `(single typeset)       \tab   `arg = 'x'` \tab   `‘x’`                 \cr
#'     `'T'`      \tab   `quote   `(double typeset)       \tab   `arg = 'x'` \tab   `“x”`                 \cr
#'     `'l'`      \tab   `list    `(simple)               \tab   `arg = 1:3` \tab   `1, 2, 3`             \cr
#'     `'|'`      \tab   `list    `(Oxford 'or')          \tab   `arg = 1:3` \tab   `1, 2, or 3`          \cr
#'     `'&'`      \tab   `list    `(Oxford 'and')         \tab   `arg = 1:3` \tab   `1, 2, and 3`         \cr
#'     `'a'`      \tab   `list    `(Oxford 'all of')      \tab   `arg = 1:3` \tab   `any of 1, 2, or 3`   \cr
#'     `'A'`      \tab   `list    `(Oxford 'any of')      \tab   `arg = 1:3` \tab   `all of 1, 2, and 3`  \cr
#'     `'e'`      \tab   `list    `(Oxford 'either/or')   \tab   `arg = 1:3` \tab   `either 1, 2, or 3`   \cr
#'     `'n'`      \tab   `list    `(Oxford 'neither/nor') \tab   `arg = 1:3` \tab   `neither 1, 2, nor 3` \cr
#'     `'b'`      \tab   `list    `(brace-enclosed)       \tab   `arg = 1:3` \tab   `'{1, 2, 3}'`         \cr
#'     `'c'`      \tab   `list    `(`c(.)` statement)     \tab   `arg = 1:3` \tab   `'c(1, 2, 3)'`        \cr
#'     `'p'`      \tab   `list    `(paren-enclosed)       \tab   `arg = 1:3` \tab   `'(1, 2, 3)'`         \cr
#'     `'s'`      \tab   `list    `(bracket-enclosed)     \tab   `arg = 1:3` \tab   `'[1, 2, 3]'`           }
#' \cr\cr **Escape sequences** are formatted as `'{@@}'`, `'{@@x}'`, `'{@@xy}'`, and `'{@@xyz}` where `x`, `y`, and `z` are formatting switches and where there may only be `1` formatting switch per family.
#' \cr\cr Escape sequences in format `'{@@}'` mean 'insert \link[=atm_scl]{atomic scalar} arg *as is*'.
#' \cr\cr Escape sequences in formats `'{@@x}', '{@@xy}',` and `'{@@xyz}'` mean 'insert \link[=atm_scl]{atomic vec} arg *after applying*, respectively, switch `x`; switches `x` and `y`; and switches `x`, `y`, and `z`.'
#' \cr\cr **Ordering of switches in escape sequences** is arbitrary. Regardless of order in escape sequences, any `decimal` switches are applied first, followed by any `quote` switches, followed by any `list` switches.
#' @param x A \link[=cmp_chr_scl]{complete character scalar} with embedded escape sequences. See details.
#' @param ... Arbitrary number of atomic scalar/vector arguments to be inserted into `x` with formatting specified in inlay escape sequences. The `N`-th argument in `...` corresponds to the `N`-th inlay escape sequence in `x`. The number of inlay escape sequences in `x` must be equal to the number of `...` arguments. See details.
#' @return A character scalar.
#' @examples
#' cat(weave('{@@}', FALSE))
#' cat(weave('{@@}', 42))
#' cat(weave('{@@b}', 4:7))
#' cat(weave('{@@q}', 'foo::bar'))
#' cat(weave('{@@0}', pi))
#' cat(weave('{@@c2}', c(pi, exp(1), 42)))
#' cat(weave('{@@Q6}', pi))
#' cat(weave('{@@et3}', c(pi, exp(1))))
#' cat(weave('{@@aq}', c('me', 'myself', 'I')))
#' @export
weave <- function(x, ...) {
  n.dots <- base::...length()
  ok.x <- uj:::.cmp_chr_scl(x)
  ok.n <- n.dots > 0
  ok.ppp <- base::ifelse(ok.n, base::all(base::sapply(base::list(...), uj::cmp_chr)), T)
  errs <- NULL
  if (!ok.n) {errs <- base::c(errs, "[...] is empty.")}
  if (!ok.x) {errs <- base::c(errs, "[x] must be a complete character scalar (?cmp_chr_scl).")}
  if (!ok.ppp) {errs <- base::c(errs, "Arguments in [...] must be complete character objects (?cmp_chr).")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  dots <- base::list(...)                                                        # The arguments to weave into {x}
  non.scl <- " is not scalar, but the "                                          # argument is not scalar
  non.num <- " is not numeric, but the "                                         # argument is not numeric
  switches <- "[&|aAbcelnpsqQtT0123456789]"                                      # gregexpr pattern for all valid switches
  has.mult <- " contains more than 1 "                                           # multiple switches from a choose-max-of-1 set
  assume.scl <- " assumes a scalar argument."                                    # switch code assumes a scalar argument
  assume.num <- " assumes a numeric argument."                                   # switch code assumes a numeric argument
  code.prefix <- "[{][@]"                                                        # gregexpr pattern for switch code prefix, i.e., '{@'
  code.suffix <- "[}]"                                                           # gregexpr pattern for switch code suffix, i.e., '}'
  ls.switches <- " list type switch (i.e., from characters in \"|&aAbcelnps\")." # choose-max-of-1 set of list-type switches
  qt.switches <- " quote type switch (i.e., from characters in \"qtQT\")."       # choose-max-of-1 set of quote-type switches
  dc.switches <- " decimal places switch (i.e., from characters in \"0123456789\")." # choose-max-of-1 set of decimal-type switches
  all.switches <- base::c("&", "|", "a", "A", "b", "c", "e", "l", "n", "p", "s", "q", "Q", "t", "T", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  inlay.sequence <- " valid inlay escape sequence in x "                         # infix for describing inlay escape sequences
  count.mismatch <- base::paste0("The number of arguments in [...] (", n.dots, ") and number of valid inlay escape sequences in [x.] ")
  pat0 <- base::paste0(code.prefix, code.suffix)                                 # 0-switch pattern '{@}'
  pat1 <- base::paste0(code.prefix, switches, code.suffix)                       # 1-switch pattern '{@s}'
  pat2 <- base::paste0(code.prefix, switches, switches, code.suffix)             # 2-switch pattern '{@ss}'
  pat3 <- base::paste0(code.prefix, switches, switches, switches, code.suffix)   # 3-switch pattern '{@sss}'
  switches0 <- base::gregexpr(pat0, x)                                           # results of searching {x} for 0-switch patterns
  switches1 <- base::gregexpr(pat1, x)                                           # results of searching {x} for 1-switch patterns
  switches2 <- base::gregexpr(pat2, x)                                           # results of searching {x} for 2-switch patterns
  switches3 <- base::gregexpr(pat3, x)                                           # results of searching {x} for 3-switch patterns
  switches0 <- uj::f0(base::setequal(uj::av(switches0), -1), NULL, uj::tb(type = 0, pos = uj::av(switches0), len = 3)) # tibble of 0-switch pattern positions and lengths
  switches1 <- uj::f0(base::setequal(uj::av(switches1), -1), NULL, uj::tb(type = 1, pos = uj::av(switches1), len = 4)) # tibble of 1-switch pattern positions and lengths
  switches2 <- uj::f0(base::setequal(uj::av(switches2), -1), NULL, uj::tb(type = 2, pos = uj::av(switches2), len = 5)) # tibble of 2-switch pattern positions and lengths
  switches3 <- uj::f0(base::setequal(uj::av(switches3), -1), NULL, uj::tb(type = 3, pos = uj::av(switches3), len = 6)) # tibble of 3-switch pattern positions and lengths
  switches <- base::rbind(switches0, switches1, switches2, switches3)            # tibble of all switch pattern positions and lengths
  switches <- switches[base::order(switches$pos, decreasing = T), ]              # sort by decreasing position of first letter of pattern in {x}
  n.switches <- uj::nr(switches)                                                 # number of switch patterns found in {x}
  if (n.switches != n.dots) {uj::stopperr(base::paste0(count.mismatch, "(", n.switches, ") don't match."), PKG = "uj")}
  dots <- base::rev(dots)
  errs <- NULL
  for (i in 1:n.switches) {
    prev0 <- 1
    next1 <- base::nchar(x)
    code0 <- switches$pos[i]
    code1 <- code0 + switches$len[i] - 1
    prev1 <- code0 - 1
    next0 <- code1 + 1
    code <- base::substr(x, code0, code1)
    sequence <- base::paste0(i, "-th", inlay.sequence, "('", code, "')")
    prev.text <- uj::f0(prev1 < prev0, "", base::substr(x, prev0, prev1))
    next.text <- uj::f0(next0 > next1, "", base::substr(x, next0, next1))
    code <- uj::av(base::strsplit(code, "", fixed = T))
    code <- code[code %in% all.switches]
    ok.code <- base::length(switches) == base::length(base::unique(switches))
    if (!ok.code) {errs <- base::c(errs, base::paste0("The ", sequence, "contains duplicate switches."))}
    else {
      io <- "|" %in% code; iv <- "&" %in% code; ia <- "a" %in% code; iA <- "A" %in% code; ib <- "b" %in% code
      ic <- "c" %in% code; ie <- "e" %in% code; il <- "l" %in% code; iN <- "n" %in% code; io <- "o" %in% code
      ip <- "p" %in% code; iq <- "q" %in% code; iQ <- "Q" %in% code; is <- "s" %in% code; it <- "t" %in% code
      iT <- "T" %in% code; i0 <- "0" %in% code; i1 <- "1" %in% code; i2 <- "2" %in% code; i3 <- "3" %in% code
      i4 <- "4" %in% code; i5 <- "5" %in% code; i6 <- "6" %in% code; i7 <- "7" %in% code; i8 <- "8" %in% code
      i9 <- "9" %in% code
      nq <- base::length(base::which(base::c(iq, iQ, it, iT)))
      nl <- base::length(base::which(base::c(io, iv, ia, iA, ib, ic, ie, il, iN, ip, is)))
      nd <- base::length(base::which(base::c(i0, i2, i3, i4, i5, i6, i7, i8, i9)))
      ok.qt <- nq < 2
      ok.ls <- ls < 2
      ok.dc <- nd < 2
      errs <- base::c(errs, uj::f0(ok.qt, NULL, base::paste0("The ", sequence, has.mult, qt.switches)),
                            uj::f0(ok.ls, NULL, base::paste0("The ", sequence, has.mult, ls.switches)),
                            uj::f0(ok.dc, NULL, base::paste0("The ", sequence, has.mult, dc.switches)))
      if (ok.ls & ok.qt & ok.dc) {
        dot <- dots[[i]]
        dot.n <- base::length(dot)
        ok.ls <- nl == 1 | dot.n == 1
        ok.dot <- nd == 0 | base::is.numeric(dot)
        errs <- base::c(errs,
                        uj::f0(ok.ls , NULL, base::paste0("..", i, non.scl, sequence, assume.scl)),
                        uj::f0(ok.dot, NULL, base::paste0("..", i, non.num, sequence, assume.num)))
        if (ok.ls & ok.dot) {
          dot <- uj::f0(i0, base::sprintf("%0.0f", dot),
                        uj::f0(i1, base::sprintf("%0.1f", dot),
                               uj::f0(i2, base::sprintf("%0.2f", dot),
                                      uj::f0(i3, base::sprintf("%0.3f", dot),
                                             uj::f0(i4, base::sprintf("%0.4f", dot),
                                                    uj::f0(i5, base::sprintf("%0.5f", dot),
                                                           uj::f0(i6, base::sprintf("%0.6f", dot),
                                                                  uj::f0(i7, base::sprintf("%0.7f", dot),
                                                                         uj::f0(i8, base::sprintf("%0.8f", dot),
                                                                                uj::f0(i9, base::sprintf("%0.9f", dot), dot))))))))))

          dot <- uj::f0(iq, base::paste0("'", dot, "'"),
                        uj::f0(iQ, base::paste0("\"", dot, "\""),
                               uj::f0(it, base::paste0('\u2018', dot, '\u2019'),
                                      uj::f0(iT, base::paste0('\u201C', dot, '\u201D'), dot))))

          dot <- uj::f0(io, uj::ox_or(dot),
                        uj::f0(iv, uj::ox_and(dot),
                               uj::f0(ia, uj::ox_any(dot),
                                      uj::f0(iA, uj::ox_all(dot),
                                             uj::f0(ie, uj::ox_either(dot),
                                                    uj::f0(iN, uj::ox_neither(dot),
                                                           uj::f0(il, base::paste0(dot, collapse = ", "),
                                                                  uj::f0(ic, base::paste0("c(", base::paste0(dot, collapse = ", "), ")"),
                                                                         uj::f0(ip, base::paste0( "(", base::paste0(dot, collapse = ", "), ")"),
                                                                                uj::f0(ib, base::paste0( "{", base::paste0(dot, collapse = ", "), "}"),
                                                                                       uj::f0(is, base::paste0("[", uj::g(", ", dot), dot, "]"), dot)))))))))))

          x <- base::paste0(prev.text, dot, next.text)
  }}}}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  x
}
