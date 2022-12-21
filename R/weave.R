#' @encoding UTF-8
#' @family strings
#' @title Weave inlay values into a string
#' @description A reformulation of \code{\link[base]{sprintf}} relying on a template containing escape sequences which in turn contain formatting switches. Escape sequences specify where to insert inlay arguments. Switches specify how to format arguments to be inlaid, and come in three families.
#' \cr\cr **Decimal switches** specify number of decimal places for numeric arguments, taking the values `'0'` to `'9'`
#' \cr\cr **Quote switches** specify quoting argument elements as follows:
#' \tabular{lcll}{
#'          \tab **Switch** \tab **Style** \tab   **Type**
#'   \cr    \tab            \tab           \tab  
#'   \cr    \tab   `'q'`    \tab straight  \tab   single
#'   \cr    \tab   `'Q'`    \tab           \tab   double
#'   \cr    \tab            \tab           \tab  
#'   \cr    \tab   `'t'`    \tab typeset   \tab   single
#'   \cr    \tab   `'T'`    \tab           \tab   typeset
#' }
#' **List switches** specify formatting arguments as comma-separated lists:
#' \tabular{lclll}{
#'          \tab **Switch** \tab   **Type of List** \tab   **Subtype**     \tab   **Result with `arg = 1:3`**
#'   \cr    \tab.           \tab                    \tab                   \tab  
#'   \cr    \tab   `'l'`    \tab   *list*           \tab   simple          \tab   `'1, 2, 3'`
#'   \cr    \tab.           \tab                    \tab                   \tab  
#'   \cr    \tab   `'|'`    \tab   *Oxford-comma*   \tab   or              \tab   `'1, 2, or 3'`
#'   \cr    \tab   `'&'`    \tab                    \tab   and             \tab   `'1, 2, and 3'`
#'   \cr    \tab   `'a'`    \tab                    \tab   all of          \tab   `any of 1, 2, or 3`
#'   \cr    \tab   `'A'`    \tab                    \tab   any of          \tab   `all of 1, 2, and 3`
#'   \cr    \tab   `'e'`    \tab                    \tab   either/or       \tab   `either 1, 2, or 3`
#'   \cr    \tab   `'n'`    \tab                    \tab   neither/nor     \tab   `neither 1, 2, nor 3`
#'   \cr    \tab.           \tab                    \tab                   \tab  
#'   \cr    \tab   `'b'`    \tab   *enclosed*       \tab   braces          \tab   `'{1, 2, 3}'`
#'   \cr    \tab   `'c'`    \tab                    \tab   concatenated    \tab   `'c(1, 2, 3)'`
#'   \cr    \tab   `'p'`    \tab                    \tab   parentheses     \tab   `'(1, 2, 3)'`
#'   \cr    \tab   `'s'`    \tab                    \tab   square brackets \tab   `'[1, 2, 3]'`
#' }
#' **Escape sequences** are formatted as `'{@@}', '{@@x}', '{@@xy}',` and `'{@@xyz}` where `x`, `y`, and `z` are formatting switches and where there may only be `1` formatting switch per family.
#' \cr\cr Escape sequences in format `'{@@}'` mean 'insert \link[=atm_scl]{atomic scalar} arg *as is*.
#' \cr\cr Escape sequences in formats `'{@@x}', '{@@xy}',` and `'{@@xyz}'` mean 'insert \link[=atm_scl]{atomic vec} arg *after applying*, respectively, switch `x`; switches `x` and `y`; and switches `x`, `y`, and `z`.
#' \cr\cr **Ordering of switches** in escape sequences is arbitrary. Regardless of order in escape sequences, formatting switches are always applied in this order:
#' \tabular{rl}{
#'       1.  \tab Decimal switch (if any).
#'   \cr 2.  \tab Quote switch (if any).
#'   \cr 3.  \tab List switch (if any).
#' }
#' @param x A \link[=cmp_chr_scl]{complete character scalar} with embedded escape sequences. See details.
#' @param ... Arbitrary number of atomic scalar/vector arguments to be inserted into `x` with formatting specified in inlay escape sequences. The `N`-th argument in `...` corresponds to the `N`-th inlay escape sequence in `x`. The number of inlay escape sequences in `x` must be equal to the number of `...` arguments. See details.
#' @return A character scalar.
#' @export
#' @examples
#' weave('{@@}', FALSE)
#' weave('{@@}', 42)
#' weave('{@@b}', 4:7)
#' weave('{@@q}', 'foo::bar')
#' weave('{@@0}', pi)
#' weave('{@@c2}', c(pi, exp(1), 42))
#' weave('{@@Q6}', pi)
#' weave('{@@ot3}', c(pi, exp(1)))
#' weave('{@@aq}', c('me', 'myself', 'I'))
weave <- function(x, ...) {
  ok.dots <- ...length() > 0
  ok.props <- f0(ok.dots, all(sapply(list(...), cmp_vec)), F)
  errs <- c(f0(cmp_chr_scl(x), NULL, "[x] must be a complate character scalar (?cmp_chr_scl)."),
            f0(ok.dots       , NULL, "[...] is empty."),
            f0(ok.props      , NULL, "Arguments in [...] must be complete character objects (?cmp_chr)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  dots <- list(...)                                                              # The arguments to weave into {x}
  n.dots <- ...length()                                                          # The number of such arguments
  code.prefix <- "[{][@]"                                                        # gregexpr pattern for switch code prefix, i.e., '{@'
  code.suffix <- "[}]"                                                           # gregexpr pattern for switch code suffix, i.e., '}'
  switches <- "[&|aAbcelnpsqQtT0123456789]"                                      # gregexpr pattern for all valid switches
  non.scl <- " is not scalar, but the "                                          # argument is not scalar
  non.num <- " is not numeric, but the "                                         # argument is not numeric
  assume.scl <- " assumes a scalar argument."                                    # switch code assumes a scalar argument
  assume.num <- " assumes a numeric argument."                                   # switch code assumes a numeric argument
  has.mult <- " contains more than 1 "                                           # multiple switches from a choose-max-of-1 set
  ls.switches <- " list type switch (i.e., from '|&aAbcelnps')."                 # choose-max-of-1 set of list-type switches
  qt.switches <- " quote type switch (i.e., from 'qtQT')."                       # choose-max-of-1 set of quote-type switches
  dc.switches <- " decimal places switch (i.e., from '0123456789')."             # choose-max-of-1 set of digits of precision switches
  inlay.sequence <- " valid inlay escape sequence in x "                         # infix for describing inlay escape sequences
  count.mismatch <- paste0("The number of arguments in [...] (", n.dots, ") and number of valid inlay escape sequences in [x.] ")
  all.switches <- c("&", "|", "a", "A", "b", "c", "e", "l", "n", "p", "s", "q", "Q", "t", "T", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  pat0 <- paste0(code.prefix, code.suffix)                                       # 0-switch pattern '{@}'
  pat1 <- paste0(code.prefix, switches, code.suffix)                             # 1-switch pattern '{@s}'
  pat2 <- paste0(code.prefix, switches, switches, code.suffix)                   # 2-switch pattern '{@ss}'
  pat3 <- paste0(code.prefix, switches, switches, switches, code.suffix)         # 3-switch pattern '{@sss}'
  switches0 <- gregexpr(pat0, x)                                                 # results of searching {x} for 0-switch patterns
  switches1 <- gregexpr(pat1, x)                                                 # results of searching {x} for 1-switch patterns
  switches2 <- gregexpr(pat2, x)                                                 # results of searching {x} for 2-switch patterns
  switches3 <- gregexpr(pat3, x)                                                 # results of searching {x} for 3-switch patterns
  switches0 <- f0(setequal(av(switches0), -1), NULL, data.frame(type = 0, pos = av(switches0), len = 3)) # tibble of 0-switch pattern positions and lengths
  switches1 <- f0(setequal(av(switches1), -1), NULL, data.frame(type = 1, pos = av(switches1), len = 4)) # tibble of 1-switch pattern positions and lengths
  switches2 <- f0(setequal(av(switches2), -1), NULL, data.frame(type = 2, pos = av(switches2), len = 5)) # tibble of 2-switch pattern positions and lengths
  switches3 <- f0(setequal(av(switches3), -1), NULL, data.frame(type = 3, pos = av(switches3), len = 6)) # tibble of 3-switch pattern positions and lengths
  switches <- rbind(switches0, switches1, switches2, switches3)                  # tibble of all switch pattern positions and lengths
  switches <- switches[order(switches$pos, decreasing = T), ]                    # sort by decreasing position of first letter of pattern in {x}
  n.switches <- nrow(switches)                                                   # number of switch patterns found in {x}
  ok.n <- n.switches == n.dots                                                   # match between number of switch codes and args in {...}?
  if (!ok.n) {stop(.errs(p0(count.mismatch, "(", n.switches, ") don't match.")))}
  dots <- rev(dots)
  errs <- NULL
  for (i in 1:n.switches) {                                                      # for each switch code found in {x}
    prev0 <- 1
    next1 <- nchar(x)
    code0 <- switches$pos[i]
    code1 <- code0 + switches$len[i] - 1
    prev1 <- code0 - 1
    next0 <- code1 + 1
    code <- substr(x, code0, code1)
    sequence <- p0(i, "-th", inlay.sequence, "('", code, "')")                   # : string for communicating about the current switch code
    prev.text <- f0(prev1 < prev0, "", substr(x, prev0, prev1))
    next.text <- f0(next0 > next1, "", substr(x, next0, next1))
    code <- ch(code)                                                             # : all of the characters in the switch code
    code <- code[code %in% all.switches]                                         # : get just the switches (removing '{@', and '}')
    ok.code <- length(switches) == length(unique(switches))                      # : are all switches unique?
    if (!ok.code) {errs <- c(errs, p0("The ", sequence, "contains duplicate switches."))}
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
      nq <- length(which(c(iq, iQ, it, iT)))                                     # : : 0 to 1 switch from choose-max-of-1 quote type set?
      nl <- length(which(c(io, iv, ia, iA, ib, ic, ie, il, iN, ip, is)))         # : : 0 to 1 switch from choose-max-of-1 list type set?
      nd <- length(which(c(i0, i2, i3, i4, i5, i6, i7, i8, i9)))                 # : : 0 to 1 switch from choose-max-of-1 digits of precision set?
      ok.qt <- nq < 2
      ok.ls <- nl < 2
      ok.dc <- nd < 2
      errs <- c(errs,
                f0(ok.qt, NULL, p0("The ", sequence, has.mult, qt.switches)),
                f0(ok.ls, NULL, p0("The ", sequence, has.mult, ls.switches)),
                f0(ok.dc, NULL, p0("The ", sequence, has.mult, dc.switches)))
      if (ok.ls & ok.qt & ok.dc) {
        dot <- dots[[i]]                                                         # : : : get the i-th arg from {...}
        dot.n <- length(dot)                                                     # : : : get its length
        ok.ls <- ANY(nl == 1, dot.n == 1)                                        # : : : is list type specified or is i-th arg scalar?
        ok.dot <- ANY(nd == 0, is.numeric(dot))                                  # : : : are no digits of precision indicated or i-th arg is numeric?
        errs <- c(errs,
                  f0(ok.ls , NULL, p0("..", i, non.scl, sequence, assume.scl)),
                  f0(ok.dot, NULL, p0("..", i, non.num, sequence, assume.num)))
        if (ok.ls & ok.dot) {                                                    # : : : if no such errors
          if      (i0) {dot <- sprintf("%0.0f", dot)}                            # : : : : apply decimal switches first
          else if (i1) {dot <- sprintf("%0.1f", dot)}
          else if (i2) {dot <- sprintf("%0.2f", dot)}
          else if (i3) {dot <- sprintf("%0.3f", dot)}
          else if (i4) {dot <- sprintf("%0.4f", dot)}
          else if (i5) {dot <- sprintf("%0.5f", dot)}
          else if (i6) {dot <- sprintf("%0.6f", dot)}
          else if (i7) {dot <- sprintf("%0.7f", dot)}
          else if (i8) {dot <- sprintf("%0.8f", dot)}
          else if (i9) {dot <- sprintf("%0.9f", dot)}
          if      (iq) {dot <- paste0("'", dot, "'")}                            # : : : : apply quote switches second
          else if (iQ) {dot <- paste0('"', dot, '"')}
          else if (it) {dot <- paste0('\u2018', dot, '\u2019')}
          else if (iT) {dot <- paste0('\u201C', dot, '\u201D')}
          if      (io) {dot <- ox_or(dot)}                                       # : : : : apply list switches last
          else if (iv) {dot <- ox_and(dot)}
          else if (ia) {dot <- ox_any(dot)}
          else if (iA) {dot <- ox_all(dot)}
          else if (ie) {dot <- ox_either(dot)}
          else if (iN) {dot <- ox_neither(dot)}
          else if (il) {dot <- paste0(dot, collapse = ", ")}
          else if (ic) {dot <- paste0("c(", paste0(dot, collapse = ", "), ")")}
          else if (ip) {dot <- paste0( "(", paste0(dot, collapse = ", "), ")")}
          else if (ib) {dot <- paste0( "{", paste0(dot, collapse = ", "), "}")}
          else if (is) {dot <- paste0( "[", paste0(dot, collapse = ", "), "]")}
          x <- p0(prev.text, dot, next.text)                                     # : : : : append the plain text and formatted i-th arg to the result
  }}}}
  if (!is.null(errs)) {stop(.errs(errs))}
  x
}
