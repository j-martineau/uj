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
#' @param tmp A \link[=cmp_chr_scl]{complete character scalar} template with embedded escape sequences. See details.
#' @param ... Arbitrary number of unnamed atomic scalar/vector arguments to be inserted into `tmp` with formatting specified in inlay escape sequences. The `n`-th argument in `...` corresponds to the `n`-th inlay escape sequence in `tmp`. The number of inlay escape sequences in `tmp` must be equal to the number of `...` arguments. See details.
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
weave <- function(tmp, ...) {
  nDots <- base::...length()
  OkTmp <- uj:::.cmp_chr_scl(tmp)
  OkN <- nDots > 0
  OkPPP <- base::ifelse(OkN, base::all(base::sapply(base::list(...), uj::cmp_chr)), T)
  Errors <- NULL
  if (!OkN) {Errors <- base::c(Errors, "[...] is empty.")}
  if (!OkTmp) {Errors <- base::c(Errors, "[tmp] must be a complete character scalar (?cmp_chr_scl).")}
  if (!OkPPP) {Errors <- base::c(Errors, "Arguments in [...] must be complete character objects (?cmp_chr).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  Dots <- base::list(...)                                                                                                                             # The arguments to weave into X
  NonScl <- " is not scalar, but the "                                                                                                                # argument is not scalar
  NonNum <- " is not numeric, but the "                                                                                                               # argument is not numeric
  Switches <- "[&|aAbcelnpsqQtT0123456789]"                                                                                                           # gregexpr pattern for all valid switches
  HasMult <- " contains more than 1 "                                                                                                                 # multiple switches from a choose-max-of-1 set
  AssumeScl <- " assumes a scalar argument."                                                                                                          # switch code assumes a scalar argument
  AssumeNum <- " assumes a numeric argument."                                                                                                         # switch code assumes a numeric argument
  CodePrefix <- "[{][@]"                                                                                                                              # gregexpr pattern for switch code prefix, i.e., '{@'
  CodeSuffix <- "[}]"                                                                                                                                 # gregexpr pattern for switch code suffix, i.e., '}'
  ListSwitches <- " list type switch (i.e., from characters in \"|&aAbcelnps\")."                                                                     # choose-max-of-1 set of list-type switches
  QuoteSwitches <- " quote type switch (i.e., from characters in \"qtQT\")."                                                                          # choose-max-of-1 set of quote-type switches
  DecimalSwitches <- " decimal places switch (i.e., from characters in \"0123456789\")."                                                              # choose-max-of-1 set of decimal-type switches
  AllSwitches <- base::c("&", "|", "a", "A", "b", "c", "e", "l", "n", "p", "s", "q", "Q", "t", "T", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9") # all possible switch values
  InlaySequence <- " valid inlay escape sequence in [tmp] "                                                                                           # infix for describing inlay escape sequences
  CountMismatch <- base::paste0("The number of arguments in [...] (", nDots, ") and number of valid inlay escape sequences in [X] ")                  # count mismatch error message value
  Pattern0 <- base::paste0(CodePrefix, CodeSuffix)                                                                                                    # 0-switch pattern '{@}'
  Pattern1 <- base::paste0(CodePrefix, Switches, CodeSuffix)                                                                                          # 1-switch pattern '{@s}'
  Pattern2 <- base::paste0(CodePrefix, Switches, Switches, CodeSuffix)                                                                                # 2-switch pattern '{@ss}'
  Pattern3 <- base::paste0(CodePrefix, Switches, Switches, Switches, CodeSuffix)                                                                      # 3-switch pattern '{@sss}'
  Switches0 <- base::gregexpr(Pattern0, tmp)                                                                                                          # results of searching [tmp] for 0-switch patterns
  Switches1 <- base::gregexpr(Pattern1, tmp)                                                                                                          # results of searching [tmp] for 1-switch patterns
  Switches2 <- base::gregexpr(Pattern2, tmp)                                                                                                          # results of searching [tmp] for 2-switch patterns
  Switches3 <- base::gregexpr(Pattern3, tmp)                                                                                                          # results of searching [tmp] for 3-switch patterns
  Switches0 <- uj::f0(base::setequal(uj::av(Switches0), -1), NULL, uj::tb(Type = 0, Position = uj::av(Switches0), Length = 3))                        # tibble of 0-switch pattern positions and lengths
  Switches1 <- uj::f0(base::setequal(uj::av(Switches1), -1), NULL, uj::tb(Type = 1, Position = uj::av(Switches1), Length = 4))                        # tibble of 1-switch pattern positions and lengths
  Switches2 <- uj::f0(base::setequal(uj::av(Switches2), -1), NULL, uj::tb(Type = 2, Position = uj::av(Switches2), Length = 5))                        # tibble of 2-switch pattern positions and lengths
  Switches3 <- uj::f0(base::setequal(uj::av(Switches3), -1), NULL, uj::tb(Type = 3, Position = uj::av(Switches3), Length = 6))                        # tibble of 3-switch pattern positions and lengths
  Switches <- base::rbind(Switches0, Switches1, Switches2, Switches3)                                                                                 # tibble of all switch pattern positions and lengths
  Switches <- Switches[base::order(Switches$Position, decreasing = T), ]                                                                              # sort by decreasing position of first letter of pattern in [tmp]
  nSwitches <- uj::nr(Switches)                                                                                                                       # number of switch patterns found in [tmp]
  if (nSwitches != nDots) {uj::stopperr(base::paste0(CountMismatch, "(", nSwitches, ") don't match."), .PKG = "uj")}
  Dots <- base::rev(Dots)
  Errors <- NULL
  for (i in 1:nSwitches) {
    Prev0 <- 1
    Next1 <- base::nchar(tmp)
    Code0 <- Switches$pos[i]
    Code1 <- Code0 + Switches$Length[i] - 1
    Prev1 <- Code0 - 1
    Next0 <- Code1 + 1
    Code <- base::substr(tmp, Code0, Code1)
    Sequence <- base::paste0(i, "-th", InlaySequence, "('", Code, "')")
    PrevText <- uj::f0(Prev1 < Prev0, "", base::substr(tmp, Prev0, Prev1))
    NextText <- uj::f0(Next0 > Next1, "", base::substr(tmp, Next0, Next1))
    Code <- uj::av(base::strsplit(Code, "", fixed = T))
    Code <- Code[Code %in% AllSwitches]
    OkCode <- base::length(Switches) == base::length(base::unique(Switches))
    if (!OkCode) {Errors <- base::c(Errors, base::paste0("The ", Sequence, "contains duplicate switches."))}
    else {
      io <- "|" %in% Code | "o" %in% Code
      iv <- "&" %in% Code; ia <- "a" %in% Code; iA <- "A" %in% Code; ib <- "b" %in% Code; ic <- "c" %in% Code
      ie <- "e" %in% Code; il <- "l" %in% Code; iN <- "n" %in% Code; ip <- "p" %in% Code; iq <- "q" %in% Code
      iQ <- "Q" %in% Code; is <- "s" %in% Code; it <- "t" %in% Code; iT <- "T" %in% Code; i0 <- "0" %in% Code
      i1 <- "1" %in% Code; i2 <- "2" %in% Code; i3 <- "3" %in% Code; i4 <- "4" %in% Code; i5 <- "5" %in% Code
      i6 <- "6" %in% Code; i7 <- "7" %in% Code; i8 <- "8" %in% Code; i9 <- "9" %in% Code
      nList <- base::length(base::which(base::c(io, iv, ia, iA, ib, ic, ie, il, iN, ip, is)))
      nQuote <- base::length(base::which(base::c(iq, iQ, it, iT)))
      nDecimal <- base::length(base::which(base::c(i0, i2, i3, i4, i5, i6, i7, i8, i9)))
      OkQuote <- nQuote < 2
      OkList <- nList < 2
      OkDecimal <- nDecimal < 2
      Errors <- base::c(Errors, uj::f0(OkList, NULL, base::paste0("The ", Sequence, HasMult, ListSwitches)),
                                uj::f0(OkQuote, NULL, base::paste0("The ", Sequence, HasMult, QuoteSwitches)),
                                uj::f0(OkDecimal, NULL, base::paste0("The ", Sequence, HasMult, DecimalSwitches)))
      if (OkList & OkQuote & OkDecimal) {
        Dot <- Dot[[i]]
        DotLen <- base::length(Dot)
        OkList <- nList == 1 | DotLen == 1
        OkDot <- nDecimal == 0 | base::is.numeric(Dot)
        Errors <- base::c(Errors,
                        uj::f0(OkList , NULL, base::paste0("..", i, NonScl, Sequence, AssumeScl)),
                        uj::f0(OkDot, NULL, base::paste0("..", i, NonNum, Sequence, AssumeNum)))
        if (OkList & OkDot) {
          Dot <- uj::f0(i0, base::sprintf("%0.0f", Dot),
                        uj::f0(i1, base::sprintf("%0.1f", Dot),
                               uj::f0(i2, base::sprintf("%0.2f", Dot),
                                      uj::f0(i3, base::sprintf("%0.3f", Dot),
                                             uj::f0(i4, base::sprintf("%0.4f", Dot),
                                                    uj::f0(i5, base::sprintf("%0.5f", Dot),
                                                           uj::f0(i6, base::sprintf("%0.6f", Dot),
                                                                  uj::f0(i7, base::sprintf("%0.7f", Dot),
                                                                         uj::f0(i8, base::sprintf("%0.8f", Dot),
                                                                                uj::f0(i9, base::sprintf("%0.9f", Dot), Dot))))))))))

          Dot <- uj::f0(iq, base::paste0("'", Dot, "'"),
                        uj::f0(iQ, base::paste0("\"", Dot, "\""),
                               uj::f0(it, base::paste0('\u2018', Dot, '\u2019'),
                                      uj::f0(iT, base::paste0('\u201C', Dot, '\u201D'), Dot))))

          Dot <- uj::f0(io, uj::ox_or(Dot),
                        uj::f0(iv, uj::ox_and(Dot),
                               uj::f0(ia, uj::ox_any(Dot),
                                      uj::f0(iA, uj::ox_all(Dot),
                                             uj::f0(ie, uj::ox_either(Dot),
                                                    uj::f0(iN, uj::ox_neither(Dot),
                                                           uj::f0(il, base::paste0(Dot, collapse = ", "),
                                                                  uj::f0(ic, base::paste0("c(", base::paste0(Dot, collapse = ", "), ")"),
                                                                         uj::f0(ip, base::paste0( "(", base::paste0(Dot, collapse = ", "), ")"),
                                                                                uj::f0(ib, base::paste0( "{", base::paste0(Dot, collapse = ", "), "}"),
                                                                                       uj::f0(is, base::paste0("[", uj::g(", ", Dot), Dot, "]"), Dot)))))))))))

          tmp <- base::paste0(PrevText, Dot, NextText)
  }}}}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  tmp
}
