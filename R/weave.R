#' @encoding UTF-8
#' @family strings
#' @title Weave inlay values into a string
#' @description A reformulation of \code{\link[base]{sprintf}} relying on a template containing escape sequences which in turn contain formatting switches. Escape sequences specify where to insert inlay arguments. Switches specify how to format arguments to be inlaid, and come in three families.
#' \cr\cr **Decimal switches** specify number of decimal places for numeric arguments, taking the values `'0'` to `'9'`.
#' \cr\cr **Quote switches** specify quoting argument elements as follows: \tabular{lcll}{
#'          \tab **Switch** \tab **Style** \tab   **Type**
#'   \cr    \tab   `'q'`    \tab straight  \tab   single
#'   \cr    \tab   `'Q'`    \tab           \tab   double
#'   \cr    \tab   `'t'`    \tab typeset   \tab   single
#'   \cr    \tab   `'T'`    \tab           \tab   typeset
#' }
#' \cr **List switches** specify formatting arguments as comma-separated lists: \tabular{lclll}{
#'          \tab **Switch** \tab   **Type of List** \tab   **Subtype**     \tab   **Result with `arg = 1:3`**
#'   \cr    \tab   `'l'`    \tab   *list*           \tab   simple          \tab   `'1, 2, 3'`
#'   \cr    \tab   `'|'`    \tab   *Oxford comma*   \tab   or              \tab   `'1, 2, or 3'`
#'   \cr    \tab   `'&'`    \tab                    \tab   and             \tab   `'1, 2, and 3'`
#'   \cr    \tab   `'a'`    \tab                    \tab   all of          \tab   `any of 1, 2, or 3`
#'   \cr    \tab   `'A'`    \tab                    \tab   any of          \tab   `all of 1, 2, and 3`
#'   \cr    \tab   `'e'`    \tab                    \tab   either/or       \tab   `either 1, 2, or 3`
#'   \cr    \tab   `'n'`    \tab                    \tab   neither/nor     \tab   `neither 1, 2, nor 3`
#'   \cr    \tab   `'b'`    \tab   *enclosed*       \tab   braces          \tab   `'{1, 2, 3}'`
#'   \cr    \tab   `'c'`    \tab                    \tab   concatenated    \tab   `'c(1, 2, 3)'`
#'   \cr    \tab   `'p'`    \tab                    \tab   parentheses     \tab   `'(1, 2, 3)'`
#'   \cr    \tab   `'s'`    \tab                    \tab   square brackets \tab   `'[1, 2, 3]'`
#' }
#' \cr **Escape sequences** are formatted as `'{@@}'`, `'{@@x}'`, `'{@@xy}'`, and `'{@@xyz}` where `x`, `y`, and `z` are formatting switches and where there may only be `1` formatting switch per family.
#' \cr\cr Escape sequences in format `'{@@}'` mean 'insert \link[=atm_scl]{atomic scalar} arg *as is*.
#' \cr\cr Escape sequences in formats `'{@@x}', '{@@xy}',` and `'{@@xyz}'` mean 'insert \link[=atm_scl]{atomic vec} arg *after applying*, respectively, switch `x`; switches `x` and `y`; and switches `x`, `y`, and `z`.
#' \cr\cr **Ordering of switches** in escape sequences is arbitrary. Regardless of order in escape sequences, formatting switches are always applied in this order:
#' \tabular{rl}{
#'         1.  \tab Decimal switch (if any).
#'   \cr   2.  \tab Quote switch (if any).
#'   \cr   3.  \tab List switch (if any).
#' }
#' @param x A \link[=cmp_chr_scl]{complete character scalar} with embedded escape sequences. See details.
#' @param ... Arbitrary number of atomic scalar/vector arguments to be inserted into `x` with formatting specified in inlay escape sequences. The `N`-th argument in `...` corresponds to the `N`-th inlay escape sequence in `x`. The number of inlay escape sequences in `x` must be equal to the number of `...` arguments. See details.
#' @return A character scalar.
#' @export
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
weave <- function(x, ...) {
  ok.dots <- base::...length() > 0
  ok.props <- uj::f0(ok.dots, base::all(base::sapply(base::list(...), cmp_vec)), F)
  errs <- base::c(uj::f0(uj::cmp_chr_scl(x), NULL, "[x] must be a complate character scalar (?cmp_chr_scl)."),
                  uj::f0(ok.dots           , NULL, "[...] is empty."),
                  uj::f0(ok.props          , NULL, "Arguments in [...] must be complete character objects (?cmp_chr)."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  dots <- base::list(...)                                                        # The arguments to weave into {x}
  n.dots <- base::...length()                                                    # The number of such arguments
  code.prefix <- "[{][@]"                                                        # gregexpr pattern for switch code prefix, i.e., '{@'
  code.suffix <- "[}]"                                                           # gregexpr pattern for switch code suffix, i.e., '}'
  switches <- "[&|aAbcelnpsqQtT0123456789]"                                      # gregexpr pattern for all valid switches
  non.scl <- " is not scalar, but the "                                          # argument is not scalar
  non.num <- " is not numeric, but the "                                         # argument is not numeric
  assume.scl <- " assumes a scalar argument."                                    # switch code assumes a scalar argument
  assume.num <- " assumes a numeric argument."                                   # switch code assumes a numeric argument
  has.mult <- " contains more than 1 "                                           # multiple switches from a choose-max-of-1 set
  ls.switches <- " list type switch (i.e., from characters in \"|&aAbcelnps\")." # choose-max-of-1 set of list-type switches
  qt.switches <- " quote type switch (i.e., from characters in \"qtQT\")."       # choose-max-of-1 set of quote-type switches
  dc.switches <- " decimal places switch (i.e., from characters in \"0123456789\")." # choose-max-of-1 set of decimal-type switches
  inlay.sequence <- " valid inlay escape sequence in x "                         # infix for describing inlay escape sequences
  count.mismatch <- base::paste0("The number of arguments in [...] (", n.dots, ") and number of valid inlay escape sequences in [x.] ")
  all.switches <- base::c("&", "|", "a", "A", "b", "c", "e", "l", "n", "p", "s", "q", "Q", "t", "T", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  pat0 <- base::paste0(code.prefix, code.suffix)                                 # 0-switch pattern '{@}'
  pat1 <- base::paste0(code.prefix, switches, code.suffix)                       # 1-switch pattern '{@s}'
  pat2 <- base::paste0(code.prefix, switches, switches, code.suffix)             # 2-switch pattern '{@ss}'
  pat3 <- base::paste0(code.prefix, switches, switches, switches, code.suffix)   # 3-switch pattern '{@sss}'
  switches0 <- base::gregexpr(pat0, x)                                           # results of searching {x} for 0-switch patterns
  switches1 <- base::gregexpr(pat1, x)                                           # results of searching {x} for 1-switch patterns
  switches2 <- base::gregexpr(pat2, x)                                           # results of searching {x} for 2-switch patterns
  switches3 <- base::gregexpr(pat3, x)                                           # results of searching {x} for 3-switch patterns
  switches0 <- uj::f0(base::setequal(uj::av(switches0), -1), NULL, base::data.frame(type = 0, pos = uj::av(switches0), len = 3)) # tibble of 0-switch pattern positions and lengths
  switches1 <- uj::f0(base::setequal(uj::av(switches1), -1), NULL, base::data.frame(type = 1, pos = uj::av(switches1), len = 4)) # tibble of 1-switch pattern positions and lengths
  switches2 <- uj::f0(base::setequal(uj::av(switches2), -1), NULL, base::data.frame(type = 2, pos = uj::av(switches2), len = 5)) # tibble of 2-switch pattern positions and lengths
  switches3 <- uj::f0(base::setequal(uj::av(switches3), -1), NULL, base::data.frame(type = 3, pos = uj::av(switches3), len = 6)) # tibble of 3-switch pattern positions and lengths
  switches <- base::rbind(switches0, switches1, switches2, switches3)            # tibble of all switch pattern positions and lengths
  switches <- switches[base::order(switches$pos, decreasing = T), ]              # sort by decreasing position of first letter of pattern in {x}
  n.switches <- base::nrow(switches)                                             # number of switch patterns found in {x}
  ok.n <- n.switches == n.dots                                                   # match between number of switch codes and args in {...}?
  if (!ok.n) {stop(uj::format_err(pkg = "uj", count.mismatch, "(", n.switches, ") don't match."))}
  dots <- base::rev(dots)
  errs <- NULL
  for (i in 1:n.switches) {                                                      # for each switch code found in {x}
    prev0 <- 1
    next1 <- base::nchar(x)
    code0 <- switches$pos[i]
    code1 <- code0 + switches$len[i] - 1
    prev1 <- code0 - 1
    next0 <- code1 + 1
    code <- base::substr(x, code0, code1)
    sequence <- uj::p0(i, "-th", inlay.sequence, "('", code, "')")               # : string for communicating about the current switch code
    prev.text <- uj::f0(prev1 < prev0, "", base::substr(x, prev0, prev1))
    next.text <- uj::f0(next0 > next1, "", base::substr(x, next0, next1))
    code <- uj::ch(code)                                                         # : all of the characters in the switch code
    code <- code[code %in% all.switches]                                         # : get just the switches (removing '{@', and '}')
    ok.code <- base::length(switches) == base::length(base::unique(switches))    # : are all switches unique?
    if (!ok.code) {errs <- c(errs, uj::p0("The ", sequence, "contains duplicate switches."))}
    else {                                                                       # : if no duplicate switches error
      io <- "|" %in% code; iv <- "&" %in% code; ia <- "a" %in% code              # : : whether the switch code contains each valid switch
      iA <- "A" %in% code; ib <- "b" %in% code; ic <- "c" %in% code
      ie <- "e" %in% code; il <- "l" %in% code; iN <- "n" %in% code
      io <- "o" %in% code; ip <- "p" %in% code; iq <- "q" %in% code
      iQ <- "Q" %in% code; is <- "s" %in% code; it <- "t" %in% code
      iT <- "T" %in% code; i0 <- "0" %in% code; i1 <- "1" %in% code
      i2 <- "2" %in% code; i3 <- "3" %in% code; i4 <- "4" %in% code
      i5 <- "5" %in% code; i6 <- "6" %in% code; i7 <- "7" %in% code
      i8 <- "8" %in% code; i9 <- "9" %in% code
      nq <- base::length(base::which(c(iq, iQ, it, iT)))                             # : : 0 to 1 switch from choose-max-of-1 quote type set?
      nl <- base::length(base::which(c(io, iv, ia, iA, ib, ic, ie, il, iN, ip, is))) # : : 0 to 1 switch from choose-max-of-1 list type set?
      nd <- base::length(base::which(c(i0, i2, i3, i4, i5, i6, i7, i8, i9)))         # : : 0 to 1 switch from choose-max-of-1 digits of precision set?
      ok.qt <- nq < 2
      ok.ls <- nl < 2
      ok.dc <- nd < 2
      errs <- c(errs,
                uj::f0(ok.qt, NULL, uj::p0("The ", sequence, has.mult, qt.switches)),
                uj::f0(ok.ls, NULL, uj::p0("The ", sequence, has.mult, ls.switches)),
                uj::f0(ok.dc, NULL, uj::p0("The ", sequence, has.mult, dc.switches)))
      if (ok.ls & ok.qt & ok.dc) {
        dot <- dots[[i]]                                                         # : : : get the i-th arg from {...}
        dot.n <- base::length(dot)                                               # : : : get its length
        ok.ls <- uj::ANY(nl == 1, dot.n == 1)                                    # : : : is list type specified or is i-th arg scalar?
        ok.dot <- uj::ANY(nd == 0, base::is.numeric(dot))                        # : : : are no digits of precision indicated or i-th arg is numeric?
        errs <- c(errs,
                  uj::f0(ok.ls , NULL, uj::p0("..", i, non.scl, sequence, assume.scl)),
                  uj::f0(ok.dot, NULL, uj::p0("..", i, non.num, sequence, assume.num)))
        if (ok.ls & ok.dot) {                                                    # : : : if no such errors
          if      (i0) {dot <- base::sprintf("%0.0f", dot)}                      # : : : : apply decimal switches first
          else if (i1) {dot <- base::sprintf("%0.1f", dot)}
          else if (i2) {dot <- base::sprintf("%0.2f", dot)}
          else if (i3) {dot <- base::sprintf("%0.3f", dot)}
          else if (i4) {dot <- base::sprintf("%0.4f", dot)}
          else if (i5) {dot <- base::sprintf("%0.5f", dot)}
          else if (i6) {dot <- base::sprintf("%0.6f", dot)}
          else if (i7) {dot <- base::sprintf("%0.7f", dot)}
          else if (i8) {dot <- base::sprintf("%0.8f", dot)}
          else if (i9) {dot <- base::sprintf("%0.9f", dot)}
          if      (iq) {dot <- base::paste0("'", dot, "'")}                      # : : : : apply quote switches second
          else if (iQ) {dot <- base::paste0('"', dot, '"')}
          else if (it) {dot <- base::paste0('\u2018', dot, '\u2019')}
          else if (iT) {dot <- base::paste0('\u201C', dot, '\u201D')}
          if      (io) {dot <- uj::ox_or(dot)}                                   # : : : : apply list switches last
          else if (iv) {dot <- uj::ox_and(dot)}
          else if (ia) {dot <- uj::ox_any(dot)}
          else if (iA) {dot <- uj::ox_all(dot)}
          else if (ie) {dot <- uj::ox_either(dot)}
          else if (iN) {dot <- uj::ox_neither(dot)}
          else if (il) {dot <- base::paste0(dot, collapse = ", ")}
          else if (ic) {dot <- base::paste0("c(", base::paste0(dot, collapse = ", "), ")")}
          else if (ip) {dot <- base::paste0( "(", base::paste0(dot, collapse = ", "), ")")}
          else if (ib) {dot <- base::paste0( "{", base::paste0(dot, collapse = ", "), "}")}
          else if (is) {dot <- base::paste0( "[", base::paste0(dot, collapse = ", "), "]")}
          x <- uj::p0(prev.text, dot, next.text)                                 # : : : : append the plain text and formatted i-th arg to the result
  }}}}
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  x
}
