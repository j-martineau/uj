#' @name weave.
#' @family strings
#' @title Weave inlay values into a string
#' @description Inlay escape sequences to specify formatting follow the patterns
#'   and meanings given in the following table:\tabular{ll}{
#'   PATTERN            \tab MEANS INSERT ARGUMENT                           \cr
#'   \code{'{@@}'}      \tab as is (argument must be scalar).                \cr
#'   \code{'{@@x}'}     \tab after applying formatting switch \code{'x'}.    \cr
#'   \code{'{@@xy}'}    \tab after applying formatting switches \code{'x'}
#'                            and \code{'y'}.                                \cr
#'   \code{'{@@xyz}'   }\tab after applying formatting switches \code{'x'},
#'                           \code{'y'}, and \code{'z'}.                       }
#'   There are three families of switches:\tabular{ll}{
#'   SWITCH             \tab SWITCH                                          \cr
#'   FAMILY             \tab PURPOSE                                         \cr
#'   \code{decimals   } \tab specify the number of decimal points to include in
#'                            in formatting numeric arguments                \cr
#'   \code{quotes}      \tab types of quotes in which to enclose formatted
#'                            argument values                                \cr
#'   \code{list}        \tab type of list to create in formatting arguments
#'                            that may have more than one element.             }
#'   Each escape sequence can contain only 1 switch from each family, but no
#'   family is required to have a switch in any given escape sequence. In the
#'   following table, the values switches may take are summarized, giving the
#'   value, the family it belongs to, and the formatting instructions indicated
#'   by the switch.\tabular{lll}{
#'   SWITCH    \tab SWITCH           \tab FORMATTING                         \cr
#'   VALUE     \tab FAMILY           \tab SPECIFICATION                      \cr
#'   \code{'0'}\tab\code{decimals   }\tab 0 decimal places.                  \cr
#'   \code{'1'}\tab\code{decimals}   \tab 1 decimal place.                   \cr
#'   \code{'2'}\tab\code{decimals}   \tab 2 decimal places.                  \cr
#'   \code{'3'}\tab\code{decimals}   \tab 3 decimal places.                  \cr
#'   \code{'4'}\tab\code{decimals}   \tab 4 decimal places.                  \cr
#'   \code{'5'}\tab\code{decimals}   \tab 5 decimal places.                  \cr
#'   \code{'6'}\tab\code{decimals}   \tab 6 decimal places.                  \cr
#'   \code{'7'}\tab\code{decimals}   \tab 7 decimal places.                  \cr
#'   \code{'8'}\tab\code{decimals}   \tab 8 decimal places.                  \cr
#'   \code{'9'}\tab\code{decimals}   \tab 9 decimal places.                  \cr
#'   \code{'q'}\tab\code{quote}      \tab single straight quotes.            \cr
#'   \code{'Q'}\tab\code{quote}      \tab double straight quotes.            \cr
#'   \code{'t'}\tab\code{quote}      \tab single typeset quotes.             \cr
#'   \code{'T'}\tab\code{quote}      \tab double typeset quotes.             \cr
#'   \code{'b'}\tab\code{quote}      \tab comma-separated list in braces.    \cr
#'   \code{'c'}\tab\code{list}       \tab comma-separated concatenation
#'                                        statement (i.e., \code{'c(...)'}). \cr
#'   \code{'l'}\tab\code{list}       \tab comma-separated list               \cr
#'   \code{'p'}\tab\code{list}       \tab comma-separated list in parens     \cr
#'   \code{'s'}\tab\code{list}       \tab comma-separated list in square
#'                                        brackets                           \cr
#'   \code{'a'}\tab\code{list}
#'             \tab\link[=ox_and]{Oxford-comma separated 'and' list}.        \cr
#'   \code{'o'}\tab\code{list}
#'             \tab\link[=ox_or]{Oxford-comma separated 'or' list}             }
#'   Switches may be given in any order, and there can only be one switch from
#'   each family in a given inlay escape sequence. Regardless of order in the
#'   escape sequence, switches from the \code{decimals} family are processed
#'   first, followed by switches from the \code{quote} family, followed by
#'   switches from the \code{list} family.
#'   \cr\cr
#'   Example are provided in the following table, giving inlay escape sequences,
#'   the value of an argument to be formatted using the inlay escape sequence,
#'   and the result of applying the inlay escape sequence's formatting switches
#'   to the argument.\tabular{lll}{
#'   ESCAPE             \tab ARGUMENT    \tab RESULTING FORMATTED            \cr
#'   SEQUENCE           \tab VALUE       \tab CHARACTER SCALAR               \cr
#'   \code{'{@@}'}      \tab\code{FALSE}
#'                      \tab\code{'FALSE'}                                   \cr
#'   \code{'{@@}'}      \tab\code{42}
#'                      \tab\code{'42'}                                      \cr
#'   \code{'{@@b}'}     \tab\code{4:7}
#'                      \tab\code{'{4, 5, 6, 7}'}                            \cr
#'   \code{'{@@q}'}     \tab\code{'foo::bar'}
#'                      \tab\code{"'foo::bar'"}                              \cr
#'   \code{'{@@0}'}     \tab\code{pi}
#'                      \tab\code{'3'}                                       \cr
#'   \code{'{@@aQ}'}    \tab\code{c('me', 'myself', 'I')}
#'                      \tab\code{'"me", "myself", and "I"'}                 \cr
#'   \code{'{@@c2}'}    \tab\code{c(pi, exp(1), 42)}
#'                      \tab\code{'c(3.14, 2.62, 42.00)'}                    \cr
#'   \code{'{@@Q6}'}    \tab\code{pi}
#'                      \tab\code{'"3.141593"'}                              \cr
#'   \code{'{@@ot3}'   }\tab\code{c(pi, exp(1))}
#'                      \tab\code{'‘3.142’ or ‘2.718’'}                        }
#' @param x \link[cmp_chr_scl]{Complete character scalar} with inlay escape
#'   sequences (see details).
#' @param ... Arbitrary number of atomic scalar/vector arguments to be inserted
#'   into \code{x} with formatting specified in inlay escape sequences. The
#'   \code{N}-th argument in \code{...} corresponds to the \code{N}-th inlay
#'   escape sequence in \code{x}. The number of inlay escape sequences in
#'   \code{x} must be equal to the number of arguments in \code{...}.
#' @return Character scalar.
#' @export
weave. <- function() {help("weave.", package = "uj")}

#' @describeIn weave. A streamlined replacement for \code{sprintf} for basic
#'   uses.
#'   \code{x} must be a character scalar. All arguments in \code{...} must be
#'   atomic scalars or 1-dimensional vectors.
weave <- function(x, ...) {
  ok.dots <- ...length() > 0
  ok.props <- f0(ok.dots, all(sapply(list(...), cmp_vec)), F)
  errs <- c(f0(cmp_chr_scl(x), NULL, "\n \u2022 [x] must be a complate character scalar (?cmp_chr_scl)."),
            f0(ok.dots       , NULL, "\n \u2022 [...] is empty."),
            f0(ok.props      , NULL, "\n \u2022 Arguments in [...] must be complete character objects (?cmp_chr)."))
  if (idef(errs)) {stop(errs)}
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
  count.mismatch <- paste0("The number of arguments in [...] (", n.dots, ") ",   # mismatch between count of args and count of valid switch codes
                          "and number of valid inlay escape sequences in [x.] ")
  all.switches <- c("a", "b", "c", "l", "o", "p", "s", "q", "Q", "t", "T",       # all valid switch values
                    "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  pat0 <- paste0(code.prefix, code.suffix)                                       # 0-switch pattern '{@}'
  pat1 <- paste0(code.prefix, switches, code.suffix)                             # 1-switch pattern '{@s}'
  pat2 <- paste0(code.prefix, switches, switches, code.suffix)                   # 2-switch pattern '{@ss}'
  pat3 <- paste0(code.prefix, switches, switches, switches, code.suffix)         # 3-switch pattern '{@sss}'
  switches0 <- gregexpr(pat0, x)                                                 # results of searching {x} for 0-switch patterns
  switches1 <- gregexpr(pat1, x)                                                 # results of searching {x} for 1-switch patterns
  switches2 <- gregexpr(pat2, x)                                                 # results of searching {x} for 2-switch patterns
  switches3 <- gregexpr(pat3, x)                                                 # results of searching {x} for 3-switch patterns
  switches0 <- tibble::tibble(P = av(switches0), N = attr(switches0, "match.length")) # tibble of 0-switch pattern positions and lengths
  switches1 <- tibble::tibble(P = av(switches1), N = attr(switches1, "match.length")) # tibble of 1-switch pattern positions and lengths
  switches2 <- tibble::tibble(P = av(switches2), N = attr(switches2, "match.length")) # tibble of 2-switch pattern positions and lengths
  switches3 <- tibble::tibble(P = av(switches3), N = attr(switches3, "match.length")) # tibble of 3-switch pattern positions and lengths
  switches <- rbind(switches0, switches1, switches2, switches3)                  # tibble of all switch pattern positions and lengths
  switches <- switches[switches$P != -1 , ]                                      # remove no-match results (gregexpr returns -1 in that case)
  switches <- switches[order(switches$P), ]                                      # sort by position of first letter of pattern in {x}
  n.switches <- nrow(switches)                                                   # number of switch patterns found in {x}
  ok.n <- n.switches == n.dots                                                   # match between number of switch codes and args in {...}?
  if (!ok.n) {stop("\n \u2022 ", count.mismatch, "(", n.switches, ") don't match.")}
  code.stop <- 0                                                                 # position in {x} of 0-th (edge case) switch code's last character
  out <- ""                                                                      # initialize result
  errs <- NULL
  for (i in 1:n.switches) {                                                      # for each switch code found in {x}
    text.start <- code.stop + 1                                                  # : position in {x} of 1st character in plain text to add to result
    code.start <- switches$P[i]                                                  # : position in {x} of the i-th switch code's first character
    code.len <- switches$N[i]                                                    # : i-th switch code's length
    text.stop <- code.start - 1                                                  # : position in {x} of the last character in plain text to add to result
    code.stop <- code.start + code.len - 1                                       # : position in {x} of the i-th switch code's last character
    text <- f0(text.stop < text.start, "", substr(x, text.start, text.stop))     # : plain text to add to the result, if any
    code <- substr(x, code.start, code.stop)                                     # : current switch code
    switches <- ch(code)                                                         # : all of the characters in the switch code
    switches <- switches[switches %in% all.switches]                             # : get just the switches (removing '{@', and '}')
    sequence  <- paste0(i, "-th", inlay.sequence, "('", code, "')")              # : string for communicating about the current switch code
    ok.switches <- length(switches) == length(unique(switches))                  # : are all switches unique?
    if (!ok.switches) {errs <- c(errs, "\n \u2022 The ", sequence, "contains duplicate switches.")}
    if (ok.switches) {                                                           # : if no duplicate switches error
      ia <- "a" %in% code; ib <- "b" %in% code; ic <- "c" %in% code              # : : whether the switch code contains each valid switch
      il <- "l" %in% code; io <- "o" %in% code; ip <- "p" %in% code
      is <- "s" %in% code; iq <- "q" %in% code; iQ <- "Q" %in% code
      it <- "t" %in% code; iT <- "T" %in% code; i0 <- "0" %in% code
      i1 <- "1" %in% code; i2 <- "2" %in% code; i3 <- "3" %in% code
      i4 <- "4" %in% code; i5 <- "5" %in% code; i6 <- "6" %in% code
      i7 <- "7" %in% code; i8 <- "8" %in% code; i9 <- "9" %in% code
      nl <- length(which(c(ia, ib, ic, il, io, ip, is)))                         # : : 0 to 1 switch from choose-max-of-1 list type set?
      nq <- length(which(c(iq, iQ, it, iT)))                                     # : : 0 to 1 switch from choose-max-of-1 quote type set?
      nd <- length(which(c(i0, i2, i3, i4, i5, i6, i7, i8, i9)))                 # : : 0 to 1 switch from choose-max-of-1 digits of precision set?
      ok.ls <- nl < 2
      ok.qt <- nq < 2
      ok.dc <- nd < 2
      errs <- c(errs,
                f0(ok.ls, NULL, paste0("\n \u2022 The ", sequence, has.mult, ls.switches)),
                f0(ok.qt, NULL, paste0("\n \u2022 The ", sequence, has.mult, qt.switches)),
                f0(ok.dc, NULL, paste0("\n \u2022 The ", sequence, has.mult, dc.switches)))
     if (ok.ls & ok.qt & ok.dc) {
        dot <- dots[[i]]                                                         # : : : get the i-th arg from {...}
        nd <- length(dot)                                                        # : : : get its length
        ok.ls <- ANY(nl == 1, nd == 1)                                           # : : : is list type specified or is i-th arg scalar?
        ok.dot <- ANY(nd == 0, is.numeric(dot))                                  # : : : are no digits of precision indicated or i-th arg is numeric?
        errs <- c(errs,
                  f0(ok.ls , NULL, paste0("\n \u2022 ..", i, non.scl, sequence, assume.scl)),
                  f0(ok.dot, NULL, paste0("\n \u2022 ..", i, non.num, sequence, assume.num)))
        if (ok.ls & ok.dot) {                                                           # : : : if no such errors
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
          else if (il) {dot <- paste0(""  , paste0(dot, collapse = ", "), "" )}
          else if (ip) {dot <- paste0( "(", paste0(dot, collapse = ", "), ")")}
          else if (is) {dot <- paste0( "[", paste0(dot, collapse = ", "), "]")}
          else if (ia) {dot <- ox_or(dot, join = "and")}
          else if (io) {dot <- ox_or(dot)}
          out <- c(out, text, dot)                                               # : : : : append the plain text and formatted i-th arg to the result
  }}}}
  if (idef(errs)) {stop(errs)}
  text.start <- code.stop + 1                                                    # position in {x} of last plain text after the last switch code
  text.stop <- nchar(x)                                                          # get position of last character in {x}
  text <- f0(text.stop < text.start, "", substr(x, text.start, text.stop))       # get last plain text, if any
  out <- c(out, text)                                                            # append the last snippet of plain text to the result
  paste0(out, collapse = "")                                                     # and collapse the result into a character scalar
}
