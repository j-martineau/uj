#' @family strings
#' @encoding UTF-8
#' @title Weave inlay values into a string
#' @description A reformulation of \code{\link[base]{sprintf}}. See details.
#' @details This function relies on a template containing escape sequences which in turn contain formatting switches. Escape sequences specify where to insert inlay arguments. Switches specify how to format arguments to be inlaid, and come in three families
#' \cr\cr **`decimal`** switches specify number of decimal places for numeric arguments, taking the values `'0'` to `'9'`
#' \cr\cr **`quote`** switches specify quoting argument elements in various ways:
#' \tabular{cll}{
#'      **Switch**   \tab **Style** \tab   **Type**
#'   \cr `'q'`       \tab straight  \tab   single
#'   \cr `'Q'`       \tab straight  \tab   double
#'   \cr `'t'`       \tab typeset   \tab   single
#'   \cr `'T'`       \tab typeset   \tab   typeset
#' }
#' **`list`** switches specify formatting arguments as comma-separated lists:
#' \tabular{cll}{
#'        **Switch**   \tab **Type**  \tab   **Result: `arg = 1:3`**
#'   \cr `'a'`         \tab and       \tab   `'1, 2, and 3'`
#'   \cr `'b'`         \tab braces    \tab   `'{1, 2, 3}'`
#'   \cr `'c'`         \tab concat    \tab   `'c(1, 2, 3)' `
#'   \cr `'l'`         \tab simple    \tab   `'1, 2, 3'`
#'   \cr `'o'`         \tab or        \tab   `'1, 2, or 3'`
#'   \cr `'p'`         \tab parens    \tab   `'(1, 2, 3)'`
#'   \cr `'s'`         \tab square    \tab   `'[1, 2, 3]'`
#' }
#' **Building escape sequences**
#' \cr Escape sequences are formatted as `'{@@_}'` where `_` is a placeholder for `0` to `3` formatting switches, but with no more than `1` switch from each family. What each format signals is described below.
#' \cr\cr `'{@@}'` insert \link[=atm_scl]{atomic scalar} arg *as is*.
#' \cr\cr `'{@@x}'` insert \link[=atm_vec]{atomic vec} arg *after applying formatting switch* `x`.
#' \cr\cr `'{@@xy}'` insert \link[=atm_vec]{atomic vec} arg *after applying formatting switches* `x` and `y`.
#' \cr\cr `'{@@xyz}'` insert \link[=atm_vec]{atomic vec} arg *after applying formatting switches* `x`, `y`, and `z`.
#' \cr\cr
#' **Order of switches**
#' \cr Order of switches in escape sequences is arbitrary. Regardless of order in escape sequences, formatting switches are always applied in this order:
#' \tabular{rl}{
#'       1.  \tab Decimal switch (if any).
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
  switches <- "[abclopsqQtT0123456789]"                                          # gregexpr pattern for all valid switches
  non.scl <- " is not scalar, but the "                                          # argument is not scalar
  non.num <- " is not numeric, but the "                                         # argument is not numeric
  assume.scl <- " assumes a scalar argument."                                    # switch code assumes a scalar argument
  assume.num <- " assumes a numeric argument."                                   # switch code assumes a numeric argument
  has.mult <- " contains more than 1 "                                           # multiple switches from a choose-max-of-1 set
  ls.switches <- " list type switch (i.e., from 'abclops')."                     # choose-max-of-1 set of list-type switches
  qt.switches <- " quote type switch (i.e., from 'qQtT')."                       # choose-max-of-1 set of quote-type switches
  dc.switches <- " decimal places switch (i.e., from '0123456789')."             # choose-max-of-1 set of digits of precision switches
  inlay.sequence <- " valid inlay escape sequence in x "                         # infix for describing inlay escape sequences
  count.mismatch <- paste0("The number of arguments in [...] (", n.dots, ") and number of valid inlay escape sequences in [x.] ")
  all.switches <- c("a", "b", "c", "l", "o", "p", "s", "q", "Q", "t", "T", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
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
      ia <- "a" %in% code; ib <- "b" %in% code; ic <- "c" %in% code              # : : whether the switch code contains each valid switch
      il <- "l" %in% code; io <- "o" %in% code; ip <- "p" %in% code
      is <- "s" %in% code; iq <- "q" %in% code; iQ <- "Q" %in% code
      it <- "t" %in% code; iT <- "T" %in% code; i0 <- "0" %in% code
      i1 <- "1" %in% code; i2 <- "2" %in% code; i3 <- "3" %in% code
      i4 <- "4" %in% code; i5 <- "5" %in% code; i6 <- "6" %in% code
      i7 <- "7" %in% code; i8 <- "8" %in% code; i9 <- "9" %in% code
      nq <- length(which(c(iq, iQ, it, iT)))                                     # : : 0 to 1 switch from choose-max-of-1 quote type set?
      nl <- length(which(c(ia, ib, ic, il, io, ip, is)))                         # : : 0 to 1 switch from choose-max-of-1 list type set?
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
          if      (i0) {dot <- sprintf("%0.0f", dot)}                            # : : : : for any specified digits of precision,
          else if (i1) {dot <- sprintf("%0.1f", dot)}                            #         format i-th arg as specified
          else if (i2) {dot <- sprintf("%0.2f", dot)}
          else if (i3) {dot <- sprintf("%0.3f", dot)}
          else if (i4) {dot <- sprintf("%0.4f", dot)}
          else if (i5) {dot <- sprintf("%0.5f", dot)}
          else if (i6) {dot <- sprintf("%0.6f", dot)}
          else if (i7) {dot <- sprintf("%0.7f", dot)}
          else if (i8) {dot <- sprintf("%0.8f", dot)}
          else if (i9) {dot <- sprintf("%0.9f", dot)}
          if      (iq) {dot <- paste0("'", dot, "'")}                            # : : : : for any specified quote type,
          else if (iQ) {dot <- paste0('"', dot, '"')}                            #         enclose i-th arg in the specified type of quotes
          else if (it) {dot <- paste0('‘', dot, '’')}
          else if (iT) {dot <- paste0('“', dot, '”')}
          if      (ib) {dot <- paste0( "{", paste0(dot, collapse = ", "), "}")}  # : : : : for any specified list type,
          else if (ic) {dot <- paste0("c(", paste0(dot, collapse = ", "), ")")}  #         create a comma-separated list of the specified type
          else if (il) {dot <- paste0( "" , paste0(dot, collapse = ", "), "" )}
          else if (ip) {dot <- paste0( "(", paste0(dot, collapse = ", "), ")")}
          else if (is) {dot <- paste0( "[", paste0(dot, collapse = ", "), "]")}
          else if (ia) {dot <- ox_and(dot)}
          else if (io) {dot <- ox_or(dot)}
          x <- p0(prev.text, dot, next.text)                                     # : : : : append the plain text and formatted i-th arg to the result
  }}}}
  if (!is.null(errs)) {stop(.errs(errs))}
  x
}
