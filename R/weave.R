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
  n.dots <- uj::ND()                                                             # The number of dot arguments
  ok.dots <- n.dots > 0
  uj::errs_if_nots(uj::cmp_chr_scl(x)                                                   , "[x] must be a complete character scalar (?cmp_chr_scl)."          ,
                   ok.dots                                                              , "[...] is empty."                                                  ,
                   uj::f0(ok.dots, base::all(base::sapply(base::list(...), cmp_vec)), F), "Arguments in [...] must be complete character objects (?cmp_chr).",
                   uj::LGL(x)                                                           , "[...] does not resolve to a logical object."                      , PKG = "uj")
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
  count.mismatch <- uj::p0("The number of arguments in [...] (", n.dots, ") and number of valid inlay escape sequences in [x.] ")
  pat0 <- uj::p0(code.prefix, code.suffix)                                       # 0-switch pattern '{@}'
  pat1 <- uj::p0(code.prefix, switches, code.suffix)                             # 1-switch pattern '{@s}'
  pat2 <- uj::p0(code.prefix, switches, switches, code.suffix)                   # 2-switch pattern '{@ss}'
  pat3 <- uj::p0(code.prefix, switches, switches, switches, code.suffix)         # 3-switch pattern '{@sss}'
  switches0 <- base::gregexpr(pat0, x)                                           # results of searching {x} for 0-switch patterns
  switches1 <- base::gregexpr(pat1, x)                                           # results of searching {x} for 1-switch patterns
  switches2 <- base::gregexpr(pat2, x)                                           # results of searching {x} for 2-switch patterns
  switches3 <- base::gregexpr(pat3, x)                                           # results of searching {x} for 3-switch patterns
  switches0 <- uj::f0(uj::isSEQ(uj::av(switches0), -1), NULL, uj::tb(type = 0, pos = uj::av(switches0), len = 3)) # tibble of 0-switch pattern positions and lengths
  switches1 <- uj::f0(uj::isSEQ(uj::av(switches1), -1), NULL, uj::tb(type = 1, pos = uj::av(switches1), len = 4)) # tibble of 1-switch pattern positions and lengths
  switches2 <- uj::f0(uj::isSEQ(uj::av(switches2), -1), NULL, uj::tb(type = 2, pos = uj::av(switches2), len = 5)) # tibble of 2-switch pattern positions and lengths
  switches3 <- uj::f0(uj::isSEQ(uj::av(switches3), -1), NULL, uj::tb(type = 3, pos = uj::av(switches3), len = 6)) # tibble of 3-switch pattern positions and lengths
  switches <- base::rbind(switches0, switches1, switches2, switches3)            # tibble of all switch pattern positions and lengths
  switches <- switches[base::order(switches$pos, decreasing = T), ]              # sort by decreasing position of first letter of pattern in {x}
  n.switches <- uj::NR(switches)                                                 # number of switch patterns found in {x}
  uj::err_if(n.switches != n.dots, count.mismatch, "(", n.switches, ") don't match.", PKG = "uj")
  dots <- base::rev(dots)
  errs <- NULL
  for (i in 1:n.switches) {                                                      # for each switch code found in {x}
    prev0 <- 1
    next1 <- uj::LEN(x)
    code0 <- switches$pos[i]
    code1 <- code0 + switches$len[i] - 1
    prev1 <- code0 - 1
    next0 <- code1 + 1
    code <- uj::MID(x, code0, code1)
    sequence <- uj::p0(i, "-th", inlay.sequence, "('", code, "')")               # : string for communicating about the current switch code
    prev.text <- uj::f0(prev1 < prev0, "", uj::MID(x, prev0, prev1))
    next.text <- uj::f0(next0 > next1, "", uj::MID(x, next0, next1))
    code <- uj::ch(code)                                                         # : all of the characters in the switch code
    code <- uj::xIN(code, all.switches)                                          # : get just the switches (removing '{@', and '}')
    ok.code <- uj::UNQ(switches)                                                 # : are all switches unique?
    if (!ok.code) {errs <- base::c(errs, uj::p0("The ", sequence, "contains duplicate switches."))}
    else {                                                                       # : if no duplicate switches error
      io <- uj::isIN1("|", code); iv <- uj::isIN1("&", code);
      ia <- uj::isIN1("a", code); iA <- uj::isIN1("A", code)
      ib <- uj::isIN1("b", code); ic <- uj::isIN1("c", code)
      ie <- uj::isIN1("e", code); il <- uj::isIN1("l", code)
      iN <- uj::isIN1("n", code); io <- uj::isIN1("o", code)
      ip <- uj::isIN1("p", code); iq <- uj::isIN1("q", code)
      iQ <- uj::isIN1("Q", code); is <- uj::isIN1("s", code)
      it <- uj::isIN1("t", code); iT <- uj::isIN1("T", code)
      i0 <- uj::isIN1("0", code); i1 <- uj::isIN1("1", code)
      i2 <- uj::isIN1("2", code); i3 <- uj::isIN1("3", code)
      i4 <- uj::isIN1("4", code); i5 <- uj::isIN1("5", code)
      i6 <- uj::isIN1("6", code); i7 <- uj::isIN1("7", code)
      i8 <- uj::isIN1("8", code); i9 <- uj::isIN1("9", code)
      nq <- uj::NW(base::c(iq, iQ, it, iT))                                      # : : 0 to 1 switch from choose-max-of-1 quote type set?
      nl <- uj::NW(base::c(io, iv, ia, iA, ib, ic, ie, il, iN, ip, is))          # : : 0 to 1 switch from choose-max-of-1 list type set?
      nd <- uj::NW(base::c(i0, i2, i3, i4, i5, i6, i7, i8, i9))                  # : : 0 to 1 switch from choose-max-of-1 digits of precision set?
      ok.qt <- nq < 2
      ok.ls <- ls < 2
      ok.dc <- nd < 2
      errs <- base::c(errs, uj::f0(ok.qt, NULL, uj::p0("The ", sequence, has.mult, qt.switches)),
                            uj::f0(ok.ls, NULL, uj::p0("The ", sequence, has.mult, ls.switches)),
                            uj::f0(ok.dc, NULL, uj::p0("The ", sequence, has.mult, dc.switches)))
      if (ok.ls & ok.qt & ok.dc) {
        dot <- dots[[i]]                                                         # : : : get the i-th arg from {...}
        dot.n <- uj::N(dot)                                                      # : : : get its length
        ok.ls <- nl == 1 | dot.n == 1
        ok.dot <- nd == 0 | uj::isNUM(dot)
        errs <- base::c(errs,
                        uj::f0(ok.ls , NULL, uj::p0("..", i, non.scl, sequence, assume.scl)),
                        uj::f0(ok.dot, NULL, uj::p0("..", i, non.num, sequence, assume.num)))
        if (ok.ls & ok.dot) {                                                    # : : : if no such errors
          dot <- uj::f0(i0, uj::SPF("%0.0f", dot)                  , uj::f0(i1, uj::SPF("%0.1f", dot),
                 uj::f0(i2, uj::SPF("%0.2f", dot)                  , uj::f0(i3, uj::SPF("%0.3f", dot),
                 uj::f0(i4, uj::SPF("%0.4f", dot)                  , uj::f0(i5, uj::SPF("%0.5f", dot),
                 uj::f0(i6, uj::SPF("%0.6f", dot)                  , uj::f0(i7, uj::SPF("%0.7f", dot),
                 uj::f0(i8, uj::SPF("%0.8f", dot)                  , uj::f0(i9, uj::SPF("%0.9f", dot), dot))))))))))
          dot <- uj::f0(iq, uj::p0("'"     , dot, "'"     )        , uj::f0(iQ, uj::p0('"'     , dot, '"'     ),
                 uj::f0(it, uj::p0('\u2018', dot, '\u2019')        , uj::f0(iT, uj::p0('\u201C', dot, '\u201D'), dot))))
          dot <- uj::f0(io, uj::ox_or(dot)                         , uj::f0(iv, uj::ox_and(dot)                    ,
                 uj::f0(ia, uj::ox_any(dot)                        , uj::f0(iA, uj::ox_all(dot)                    ,
                 uj::f0(ie, uj::ox_either(dot)                     , uj::f0(iN, uj::ox_neither(dot)                ,
                 uj::f0(il, uj::g(", ", dot)                       , uj::f0(ic, uj::p0("c(", uj::g(", ", dot), ")"),
                 uj::f0(ip, uj::p0( "(", uj::g(", ", dot), ")")    , uj::f0(ib, uj::p0( "{", uj::g(", ", dot), "}"),
                 uj::f0(is, uj::p0("[", uj::g(", ", dot), dot, "]"), dot)))))))))))
          x <- uj::p0(prev.text, dot, next.text)
  }}}}
  if (uj::DEF(errs)) {uj::stopper(errs, PKG = "uj")}
  x
}
