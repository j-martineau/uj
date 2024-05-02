#' @encoding UTF-8
#' @family strings
#' @title Weave inlay values into a string
#' @description A reformulation of \code{\link[base]{sprintf}} relying on a template containing escape seqs which in turn contain formatting switches. Escape seqs specify where to insert inlay arguments. switches specify how to format arguments to be inlaid, and come in three families as shown in the table below
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
#' \cr\cr **Escape seqs** are formatted as `'{@@}'`, `'{@@x}'`, `'{@@xy}'`, and `'{@@xyz}` where `x`, `y`, and `z` are formatting switches and where there may only be `1` formatting switch per family.
#' \cr\cr Escape seqs in format `'{@@}'` mean 'insert \link[=atm_scl]{atomic scalar} arg *as is*'.
#' \cr\cr Escape seqs in formats `'{@@x}', '{@@xy}',` and `'{@@xyz}'` mean 'insert \link[=atm_scl]{atomic vec} arg *after applying*, respectively, switch `x`; switches `x` and `y`; and switches `x`, `y`, and `z`.'
#' \cr\cr **Ordering of switches in escape seqs** is arbitrary. Regardless of order in escape seqs, any `decimal` switches are applied first, followed by any `quote` switches, followed by any `list` switches.
#' @param tmp A \link[=cmp_chr_scl]{complete character scalar} template with embedded escape seqs. See details.
#' @param ... Arbitrary number of unnamed atomic scalar/vector arguments to be inserted into `tmp` with formatting specified in inlay escape seqs. The `n`-th argument in `...` corresponds to the `n`-th inlay escape seq in `tmp`. The number of inlay escape seqs in `tmp` must be equal to the number of `...` arguments. See details.
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
  okTmp <- ppp::.cmp_chr_scl(tmp)
  okN   <- nDots > 0
  okPPP <- base::ifelse(okN, base::all(base::sapply(base::list(...), ppp::cmp_chr)), T)
  errs  <- NULL
  if (!okN  ) {errs <- base::c(errs, "[...] is empty.")}
  if (!okTmp) {errs <- base::c(errs, "[tmp] must be a complete character scalar (?cmp_chr_scl).")}
  if (!okPPP) {errs <- base::c(errs, "Arguments in [...] must be complete character objects (?cmp_chr).")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "uj")}
  dots            <- base::list(...)                                                                                                                      # The arguments to weave into X
  nonScl          <- " is not scalar, but the "                                                                                                           # argument is not scalar
  nonNum          <- " is not numeric, but the "                                                                                                          # argument is not numeric
  switches        <- "[&|aAbcelnpsqQtT0123456789]"                                                                                                        # gregexpr pattern for all valid switches
  hasMult         <- " contains more than 1 "                                                                                                             # multiple switches from a choose-max-of-1 set
  assumeScl       <- " assumes a scalar argument."                                                                                                        # switch code assumes a scalar argument
  assumeNum       <- " assumes a numeric argument."                                                                                                       # switch code assumes a numeric argument
  codePrefix      <- "[{][@]"                                                                                                                             # gregexpr pattern for switch code prefix, i.e., '{@'
  codeSuffix      <- "[}]"                                                                                                                                # gregexpr pattern for switch code suffix, i.e., '}'
  allSwitches     <- base::c("&", "|", "a", "A", "b", "c", "e", "l", "n", "p", "s", "q", "Q", "t", "T", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9") # all possible switch values
  listSwitches    <- " list type switch (i.e., from characters in \"|&aAbcelnps\")."                                                                      # choose-max-of-1 set of list-type switches
  quoteSwitches   <- " quote type switch (i.e., from characters in \"qtQT\")."                                                                            # choose-max-of-1 set of quote-type switches
  inlaySeq        <- " valid inlay escape seq in [tmp] "                                                                                                  # infix for describing inlay escape seqs
  nMismatch       <- base::paste0("The number of arguments in [...] (", nDots, ") and number of valid inlay escape seqs in [X] ")                         # count mismatch error message value
  decimalSwitches <- " decimal places switch (i.e., from characters in \"0123456789\")."                                                                  # choose-max-of-1 set of decimal-type switches
  Pattern0        <- base::paste0(codePrefix, codeSuffix)                                                                                                 # 0-switch pattern '{@}'
  Pattern1        <- base::paste0(codePrefix, switches, codeSuffix)                                                                                       # 1-switch pattern '{@s}'
  Pattern2        <- base::paste0(codePrefix, switches, switches, codeSuffix)                                                                             # 2-switch pattern '{@ss}'
  Pattern3        <- base::paste0(codePrefix, switches, switches, switches, codeSuffix)                                                                   # 3-switch pattern '{@sss}'
  switches0       <- base::gregexpr(Pattern0, tmp)                                                                                                        # results of searching [tmp] for 0-switch patterns
  switches1       <- base::gregexpr(Pattern1, tmp)                                                                                                        # results of searching [tmp] for 1-switch patterns
  switches2       <- base::gregexpr(Pattern2, tmp)                                                                                                        # results of searching [tmp] for 2-switch patterns
  switches3       <- base::gregexpr(Pattern3, tmp)                                                                                                        # results of searching [tmp] for 3-switch patterns
  switches0       <- uj::f0(base::setequal(uj::av(switches0), -1), NULL, uj::tb(Type = 0, Position = uj::av(switches0), Length = 3))                      # tibble of 0-switch pattern positions and lengths
  switches1       <- uj::f0(base::setequal(uj::av(switches1), -1), NULL, uj::tb(Type = 1, Position = uj::av(switches1), Length = 4))                      # tibble of 1-switch pattern positions and lengths
  switches2       <- uj::f0(base::setequal(uj::av(switches2), -1), NULL, uj::tb(Type = 2, Position = uj::av(switches2), Length = 5))                      # tibble of 2-switch pattern positions and lengths
  switches3       <- uj::f0(base::setequal(uj::av(switches3), -1), NULL, uj::tb(Type = 3, Position = uj::av(switches3), Length = 6))                      # tibble of 3-switch pattern positions and lengths
  switches        <- base::rbind(switches0, switches1, switches2, switches3)                                                                              # tibble of all switch pattern positions and lengths
  switches        <- switches[base::order(switches$Position, decreasing = T), ]                                                                           # sort by decreasing position of first letter of pattern in [tmp]
  nswitches <- uj::nr(switches)                                                                                                                           # number of switch patterns found in [tmp]
  if (nswitches != nDots) {ppp::stopperr(base::paste0(nMismatch, "(", nswitches, ") don't match."), pkg = "uj")}
  dots <- base::rev(dots)
  errs <- NULL
  for (i in 1:nswitches) {
    prev0    <- 1
    next1    <- base::nchar(tmp)
    code0    <- switches$pos[i]
    code1    <- code0 + switches$Length[i] - 1
    prev1    <- code0 - 1
    next0    <- code1 + 1
    code     <- base::substr(tmp, code0, code1)
    seq      <- base::paste0(i, "-th", inlaySeq, "('", code, "')")
    prevText <- uj::f0(prev1 < prev0, "", base::substr(tmp, prev0, prev1))
    nextText <- uj::f0(next0 > next1, "", base::substr(tmp, next0, next1))
    code     <- uj::av(base::strsplit(code, "", fixed = T))
    code     <- code[code %in% allSwitches]
    okCode   <- base::length(switches) == base::length(base::unique(switches))
    if (!okCode) {errs <- base::c(errs, base::paste0("The ", seq, "contains duplicate switches."))}
    else {
      io <- "|" %in% code | "o" %in% code
      iv <- "&" %in% code; ia <- "a" %in% code; iA <- "A" %in% code; ib <- "b" %in% code; ic <- "c" %in% code
      ie <- "e" %in% code; il <- "l" %in% code; iN <- "n" %in% code; ip <- "p" %in% code; iq <- "q" %in% code
      iQ <- "Q" %in% code; is <- "s" %in% code; it <- "t" %in% code; iT <- "T" %in% code; i0 <- "0" %in% code
      i1 <- "1" %in% code; i2 <- "2" %in% code; i3 <- "3" %in% code; i4 <- "4" %in% code; i5 <- "5" %in% code
      i6 <- "6" %in% code; i7 <- "7" %in% code; i8 <- "8" %in% code; i9 <- "9" %in% code
      nList     <- base::length(base::which(base::c(io, iv, ia, iA, ib, ic, ie, il, iN, ip, is)))
      nQuote    <- base::length(base::which(base::c(iq, iQ, it, iT)))
      nDecimal  <- base::length(base::which(base::c(i0, i2, i3, i4, i5, i6, i7, i8, i9)))
      okQuote   <- nQuote < 2
      okList    <- nList < 2
      okDecimal <- nDecimal < 2
      errs <- base::c(errs, uj::f0(okList, NULL, base::paste0("The ", seq, hasMult, listSwitches)),
                            uj::f0(okQuote, NULL, base::paste0("The ", seq, hasMult, quoteSwitches)),
                            uj::f0(okDecimal, NULL, base::paste0("The ", seq, hasMult, decimalSwitches)))
      if (okList & okQuote & okDecimal) {
        dot <- dot[[i]]
        dotLen <- base::length(dot)
        okList <- nList == 1 | dotLen == 1
        okDot <- nDecimal == 0 | base::is.numeric(dot)
        errs <- base::c(errs,
                        uj::f0(okList, NULL, base::paste0("..", i, nonScl, seq, assumeScl)),
                        uj::f0(okDot , NULL, base::paste0("..", i, nonNum, seq, assumeNum)))
        if (okList & okDot) {
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

          tmp <- base::paste0(prevText, dot, nextText)
  }}}}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "uj")}
  tmp
}
