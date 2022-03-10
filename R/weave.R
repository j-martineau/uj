#' @name weave
#' @family strings
#' @title Weave inlay values into a string
#' @description A replacement for \code{sprintf} for basic uses. \code{x} must
#'   be a character scalar. All arguments in \code{...} must be atomic scalars
#'   or 1-dimensional vectors.
#' @details Inlay escape sequences to specify formatting follow the patterns
#'   given in the following table with associated meanings of each pattern:
#'   \tabular{ll}{
#'   \strong{Pattern }   \tab \strong{Meaning insert argument...}
#'   \cr\code{'{@@}'}    \tab ...as is (argument must be scalar)
#'   \cr\code{'{@@x}'}   \tab ...after applying formatting switch \code{'x'}
#'   \cr\code{'{@@xy}'}  \tab ...after applying formatting switches \code{'x'}
#'                               and \code{'y'}
#'   \cr\code{'{@@xyz}' }\tab ...after applying formatting switches \code{'x'},
#'                               \code{'y'}, and \code{'z'}
#'   }
#'   There are three families of switches:
#'   \tabular{ll}{
#'     \strong{Family}    \tab\strong{Purpose}
#'     \cr\code{decimals }\tab specify the number of decimal points to include
#'                             in formatting numeric arguments
#'     \cr\code{quotes}   \tab types of quotes in which to enclose formatted
#'                             argument values
#'     \cr\code{list}     \tab type of list to create in formatting arguments
#'                             that may have more than one element.
#'   }
#'   Each escape sequence can contain only 1 switch from each family, but no
#'   family is required to have a switch in any given escape sequence. In the
#'   following table, the values switches may take are summarized, giving the
#'   value, the family it belongs to, and the formatting instructions indicated
#'   by the switch.
#'   \tabular{lll}{
#'   \strong{Switch  }\tab\strong{Switch}\tab\strong{Formatting}
#'   \cr\strong{Value}\tab\strong{Family}      \tab\strong{Specifications}
#'   \cr\code{'0'}\tab\code{decimals }\tab 0 decimal places
#'   \cr\code{'1'}\tab\code{decimals}\tab 1 decimal place
#'   \cr\code{'2'}\tab\code{decimals}\tab 2 decimal places
#'   \cr ...      \tab ...           \tab ...
#'   \cr\code{'8'}\tab\code{decimals}\tab 8 decimal places
#'   \cr\code{'9'}\tab\code{decimals}\tab 9 decimal places
#'   \cr\code{'q'}\tab\code{quote}\tab single straight quotes
#'   \cr\code{'Q'}\tab\code{quote}\tab double straight quotes.
#'   \cr\code{'t'}\tab\code{quote}\tab single typeset quotes
#'   \cr\code{'T'}\tab\code{quote}\tab double typeset quotes
#'   \cr\code{'b'}\tab\code{quote}\tab comma-separated list in braces
#'   \cr\code{'c'}\tab\code{list}\tab comma-separated list in \code{'c(...)'}
#'   \cr\code{'l'}\tab\code{list}\tab comma-separated list
#'   \cr\code{'p'}\tab\code{list}\tab comma-separated list in parentheses
#'   \cr\code{'s'}\tab\code{list}\tab comma-separated list in square brackets
#'   \cr\code{'a'}\tab\code{list}\tab
#'      \link[=ox_and]{Oxford-comma separated 'and' list}
#'   \cr\code{'o'}\tab\code{list}\tab
#'      \link[=ox_or]{Oxford-comma separated 'or' list}
#'   }
#'   Switches may be given in any order, and there can only be one switch from
#'   each family in a given inlay escape sequence. Regardless of order in the
#'   escape sequence, switches from the \code{decimals} family are processed
#'   first, followed by switches from the \code{quote} family, followed by
#'   switches from the \code{list} family.
#'   \cr\cr
#'   Example are provided in the following table, giving inlay escape sequences,
#'   the value of an argument to be formatted using the inlay escape sequence,
#'   and the result of applying the inlay escape sequence's formatting switches
#'   to the argument.
#'   \tabular{lll}{
#'   \strong{Escape}\tab\strong{Argument  }\tab\strong{Resulting Formatted}
#'   \cr\strong{Sequence  }\tab\strong{Value}\tab\strong{Character Scalar}
#'   \cr\code{'{@@}'}\tab\code{FALSE}\tab\code{'FALSE'}
#'   \cr\code{'{@@}'}\tab\code{42}\tab\code{'42'}
#'   \cr\code{'{@@b}'}\tab\code{4:7}\tab\code{'{4, 5, 6, 7}'}
#'   \cr\code{'{@@q}'}\tab\code{'foo::bar'}\tab\code{"'foo::bar'"}
#'   \cr\code{'{@@0}'}\tab\code{pi}\tab\code{'3'}
#'   \cr\code{'{@@aQ}'}\tab\code{c('me', 'myself', 'I') }\tab
#'      \code{'"me", "myself", and "I"'}
#'   \cr\code{'{@@c2}'} \tab\code{c(pi, exp(1), 42)}\tab
#'      \code{'c(3.14, 2.62, 42.00)'}
#'   \cr\code{'{@@Q6}'} \tab\code{pi}\tab\code{'"3.141593"'}
#'   \cr\code{'{@@ot3}'}\tab\code{c(pi, exp(1))}\tab\code{'‘3.142’ or ‘2.718’'}
#'   }
#' @param x An atomic scalar with inlay escape sequences (see details).
#' @param ... An arbitrary number of atomic scalar/vector arguments to be
#'   inserted into \code{x} with formatting specified in inlay escape sequences.
#'   The \code{N}-th argument in \code{...} corresponds to the \code{N}-th inlay
#'   escape sequence in \code{x}. The number of inlay escape sequences in
#'   \code{x} must be equal to the number of arguments in \code{...}.
#' @return A character scalar.
#' @export
weave <- function(x, ...) {
  VX <- cmp_chr_scl(x)
  VD <- ...length() > 0
  VL <- f0(VD, all(sapply(list(...), cmp_vec)), F)
  E <- NULL
  if (!VX) {E <- c(E, "\n  * [x] must be a non-NA character scalar.")}
  if (!VD) {E <- c(E, "\n  * [...] is empty.")}
  if (!VL) {E <- c(E, "\n  * Arguments in [...] must be complete character objects.")}
  if (xdef(E)) {stop(E)}
  Dots  <- list(...)                                                             # The arguments to weave into {x}
  DotsN <- length(Dots)                                                          # The number of such arguments
  CodePrefix    <- "[{][@]"                                                      # gregexpr pattern for switch code prefix, i.e., '{@'
  CodeSuffix    <- "[}]"                                                         # gregexpr pattern for switch code suffix, i.e., '}'
  Switches      <- "[abclopsqQtT0123456789]"                                     # gregexpr pattern for all valid switches
  NonScalar     <- " is not scalar, but the "                                    # argument is not scalar
  NonNumeric    <- " is not numeric, but the "                                   # argument is not numeric
  AssumeScalar  <- " assumes a scalar argument."                                 # switch code assumes a scalar argument
  AssumeNumeric <- " assumes a numeric argument."                                # switch code assumes a numeric argument
  HasMultiple   <- " contains more than 1 "                                      # multiple switches from a choose-max-of-1 set
  ListSwitches  <- " list type switch (i.e., from 'abclops')."                   # choose-max-of-1 set of list-type switches
  QuoteSwitches <- " quote type switch (i.e., from 'qQtT')."                     # choose-max-of-1 set of quote-type switches
  DigitSwitches <- " decimal places switch (i.e., from '0123456789')."           # choose-max-of-1 set of digits of precision switches
  InlaySequence <- " valid inlay escape sequence in x "                          # infix for describing inlay escape sequences
  CountMismatch <- paste0("The number of arguments in [...] (", DotsN, ") ",     # mismatch between count of args and count of valid switch codes
                          "and number of valid inlay escape sequences in [x] ")
  AllSwitches   <- c("a", "b", "c", "l", "o", "p", "s", "q", "Q", "t", "T",      # all valid switch values
                     "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  Pat0 <- paste0(CodePrefix,                               CodeSuffix)           # 0-switch pattern '{@}'
  Pat1 <- paste0(CodePrefix, Switches,                     CodeSuffix)           # 1-switch patterh '{@s}'
  Pat2 <- paste0(CodePrefix, Switches, Switches,           CodeSuffix)           # 2-switch patterh '{@ss}'
  Pat3 <- paste0(CodePrefix, Switches, Switches, Switches, CodeSuffix)           # 3-switch patterh '{@sss}'
  Switches0 <- gregexpr(Pat0, x)                                                 # results of searching {x} for 0-switch patterns
  Switches1 <- gregexpr(Pat1, x)                                                 # results of searching {x} for 1-switch patterns
  Switches2 <- gregexpr(Pat2, x)                                                 # results of searching {x} for 2-switch patterns
  Switches3 <- gregexpr(Pat3, x)                                                 # results of searching {x} for 3-switch patterns
  Switches0 <- tibble::tibble(P = av(Switches0), N = attr(Switches0, "match.length")) # tibble of 0-switch pattern positions and lengths
  Switches1 <- tibble::tibble(P = av(Switches1), N = attr(Switches1, "match.length")) # tibble of 1-switch pattern positions and lengths
  Switches2 <- tibble::tibble(P = av(Switches2), N = attr(Switches2, "match.length")) # tibble of 2-switch pattern positions and lengths
  Switches3 <- tibble::tibble(P = av(Switches3), N = attr(Switches3, "match.length")) # tibble of 3-switch pattern positions and lengths
  Switches  <- rbind(Switches0, Switches1, Switches2, Switches3)                 # tibble of all switch pattern positions and lengths
  Switches  <- Switches[Switches$P != -1 , ]                                     # remove no-match results (gregexpr returns -1 in that case)
  Switches  <- Switches[order(Switches$P), ]                                     # sort by position of first letter of pattern in {x}
  SwitchesN <- nrow(Switches)                                                    # number of switch patterns found in {x}
  VP        <- SwitchesN == DotsN                                                # match between number of switch codes and args in {...}?
  if (!VP) {stop("\n  * ", CountMismatch, "(", SwitchesN, ") don't match.")}
  CodeStop  <- 0                                                                 # position in {x} of 0-th (edge case) switch code's last character
  Result    <- ""                                                                # initialize result
  E <- NULL
  for (i in 1:SwitchesN) {                                                       # for each switch code found in {x}
    TextStart <- CodeStop + 1                                                    # > position in {x} of 1st character in plain text to add to result
    CodeStart <- Switches$P[i]                                                   # > position in {x} of the i-th switch code's first character
    CodeLen   <- Switches$N[i]                                                   # > i-th switch code's length
    TextStop  <- CodeStart - 1                                                   # > position in {x} of the last character in plain text to add to result
    CodeStop  <- CodeStart + CodeLen - 1                                         # > position in {x} of the i-th switch code's last character
    Text      <- f0(TextStop < TextStart, "", substr(x, TextStart, TextStop))    # > plain text to add to the result, if any
    Code      <- substr(x, CodeStart, CodeStop)                                  # > current switch code
    Switches  <- ch(Code)                                                        # > all of the characters in the switch code
    Switches  <- Switches[Switches %in% AllSwitches]                             # > get just the switches (removing '{@', and '}')
    Sequence  <- paste0(i, "-th", InlaySequence, "('", Code, "')")               # > string for communicating about the current switch code
    VS <- length(Switches) == length(unique(Switches))                           # > are all switches unique?
    if (!VS) {E <- c(E, "\n  * The ", Sequence, "contains duplicate switches.")}
    if (VS) {                                                                    # > if no such error
      Ia <- "a" %in% Code; Ib <- "b" %in% Code; Ic <- "c" %in% Code              # >> whether the switch code contains each valid switch
      Il <- "l" %in% Code; Io <- "o" %in% Code; Ip <- "p" %in% Code
      Is <- "s" %in% Code; Iq <- "q" %in% Code; IQ <- "Q" %in% Code
      It <- "t" %in% Code; IT <- "T" %in% Code; I0 <- "0" %in% Code
      I1 <- "1" %in% Code; I2 <- "2" %in% Code; I3 <- "3" %in% Code
      I4 <- "4" %in% Code; I5 <- "5" %in% Code; I6 <- "6" %in% Code
      I7 <- "7" %in% Code; I8 <- "8" %in% Code; I9 <- "9" %in% Code
      NL <- length(which(c(Ia, Ib, Ic, Il, Io, Ip, Is        ))); VL <- NL < 2   # >> 0 to 1 switch from choose-max-of-1 list type set?
      NQ <- length(which(c(Iq, IQ, It, IT                    ))); VQ <- NQ < 2   # >> 0 to 1 switch from choose-max-of-1 quote type set?
      ND <- length(which(c(I0, I2, I3, I4, I5, I6, I7, I8, I9))); VD <- ND < 2   # >> 0 to 1 switch from choose-max-of-1 digits of precision set?
      if (!VL) {E <- c(E, "The ", Sequence, HasMultiple, ListSwitches )}         # >> errors: multiple switches from same choose-max-of-1 sets?
      if (!VQ) {E <- c(E, "The ", Sequence, HasMultiple, QuoteSwitches)}
      if (!VD) {E <- c(E, "The ", Sequence, HasMultiple, DigitSwitches)}
      if (VL & VQ & VD) {                                                        # >> if no such errors
        Dot <- Dots[[i]]                                                         # >>> get the i-th arg from {...}
        ND  <- length(Dot)                                                       # >>> get its length
        VL  <- ANY(NL == 1, ND == 1)                                             # >>> is list type specified or is i-th arg scalar?
        VD  <- ANY(ND == 0, is.numeric(Dot))                                     # >>> are no digits of precision indicated or i-th arg is numeric?
        if (!VL) {E <- c(E, "\n  * ..", i, NonScalar , Sequence, AssumeScalar )}
        if (!VD) {E <- c(E, "\n  * ..", i, NonNumeric, Sequence, AssumeNumeric)}
        if (VL & VD) {                                                           # >>> if no such errors
          if      (I0) {Dot <- sprintf("%0.0f", Dot)}                            # >>>> for any specified digits of precision,
          else if (I1) {Dot <- sprintf("%0.1f", Dot)}                            #      format i-th arg as specified
          else if (I2) {Dot <- sprintf("%0.2f", Dot)}
          else if (I3) {Dot <- sprintf("%0.3f", Dot)}
          else if (I4) {Dot <- sprintf("%0.4f", Dot)}
          else if (I5) {Dot <- sprintf("%0.5f", Dot)}
          else if (I6) {Dot <- sprintf("%0.6f", Dot)}
          else if (I7) {Dot <- sprintf("%0.7f", Dot)}
          else if (I8) {Dot <- sprintf("%0.8f", Dot)}
          else if (I9) {Dot <- sprintf("%0.9f", Dot)}
          if      (Iq) {Dot <- paste0("'", Dot, "'")}                            # >>>> for any specified quote type,
          else if (IQ) {Dot <- paste0('"', Dot, '"')}                            #      enclose i-th arg in the specified type of quotes
          else if (It) {Dot <- paste0('‘', Dot, '’')}
          else if (IT) {Dot <- paste0('“', Dot, '”')}
          if      (Ib) {Dot <- paste0( "{", paste0(Dot, collapse = ", "), "}")}  # >>>> for any specified list type,
          else if (Ic) {Dot <- paste0("c(", paste0(Dot, collapse = ", "), ")")}  #      create a comma-separated list of the specified type
          else if (Il) {Dot <- paste0(""  , paste0(Dot, collapse = ", "), "" )}
          else if (Ip) {Dot <- paste0( "(", paste0(Dot, collapse = ", "), ")")}
          else if (Is) {Dot <- paste0( "[", paste0(Dot, collapse = ", "), "]")}
          else if (Ia) {Dot <- ox_or(Dot, join = "and")}
          else if (Io) {Dot <- ox_or(Dot)}
          Result <- c(Result, Text, Dot)                                         # >>>> append the plain text and formatted i-th arg to the result
  } } } }
  if (xdef(E)) {stop(E)}
  TextStart <- CodeStop + 1                                                      # position in {x} of last plain text after the last switch code
  TextStop  <- nchar(x)                                                          # get position of last character in {x}
  Text      <- f0(TextStop < TextStart, "", substr(x, TextStart, TextStop))      # get last plain text, if any
  Result    <- c(Result, Text)                                                   # append the last snippet of plain text to the result
  paste0(Result, collapse = "")                                                  # and collapse the result into a character scalar
}
