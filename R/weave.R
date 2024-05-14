# PASSED TEST 05-08-24

#' @encoding UTF-8
#' @family strings
#' @title Weave Inlay Values into a String
#' @description A reformulation of \code{\link[base]{sprintf}} relying on a template containing escape switch sequences which specify formatting. The location of escape switch sequences specify where to insert inlay arguments and the content of escape switch sequences specify how to format arguments to be inlaid, and come in four families as shown in the table below:
#' \tabular{llll}{
#'     **Switch** \tab   **Switch Type**                  \tab   **Sample**    \tab   **Text of Result**    \cr
#'     **Value**  \tab   **(and Formatting)**             \tab   **`...` Arg** \tab   **with Sample Arg**   \cr
#'     `'0'`      \tab   `decimal `(`0` decimal points)   \tab   `pi`          \tab   `'3'`                 \cr
#'     `'1'`      \tab   `decimal `(`1` decimal point)    \tab   `pi`          \tab   `'3.1'`               \cr
#'     `'2'`      \tab   `decimal `(`2` decimal points)   \tab   `pi`          \tab   `'3.14'`              \cr
#'     `'3'`      \tab   `decimal `(`3` decimal points)   \tab   `pi`          \tab   `'3.141'`             \cr
#'     `'4'`      \tab   `decimal `(`4` decimal points)   \tab   `pi`          \tab   `'3.1415'`            \cr
#'     `'5'`      \tab   `decimal `(`5` decimal points)   \tab   `pi`          \tab   `'3.14159'`           \cr
#'     `'6'`      \tab   `decimal `(`6` decimal points)   \tab   `pi`          \tab   `'3.141592'`          \cr
#'     `'7'`      \tab   `decimal `(`7` decimal points)   \tab   `pi`          \tab   `'3.1415926'`         \cr
#'     `'8'`      \tab   `decimal `(`8` decimal points)   \tab   `pi`          \tab   `'3.14159265'`        \cr
#'     `'9'`      \tab   `decimal `(`9` decimal points)   \tab   `pi`          \tab   `'3.141592653'`       \cr
#'     `'q'`      \tab   `quote   `(single straight)      \tab   `'x'`         \tab   `'x'`                 \cr
#'     `'Q'`      \tab   `quote   `(double straight)      \tab   `'x'`         \tab   `"x"`                 \cr
#'     `'t'`      \tab   `quote   `(single typeset)       \tab   `'x'`         \tab   `‘x’`                 \cr
#'     `'T'`      \tab   `quote   `(double typeset)       \tab   `'x'`         \tab   `“x”`                 \cr
#'     `'l'`      \tab   `list    `(simple list)          \tab   `1:3`         \tab   `1, 2, 3`             \cr
#'     `'|'`      \tab   `list    `(Oxford 'or')          \tab   `1:3`         \tab   `1, 2, or 3`          \cr
#'     `'&'`      \tab   `list    `(Oxford 'and')         \tab   `1:3`         \tab   `1, 2, and 3`         \cr
#'     `'a'`      \tab   `list    `(Oxford 'all of')      \tab   `1:3`         \tab   `any of 1, 2, or 3`   \cr
#'     `'A'`      \tab   `list    `(Oxford 'any of')      \tab   `1:3`         \tab   `all of 1, 2, and 3`  \cr
#'     `'e'`      \tab   `list    `(Oxford 'either/or')   \tab   `1:3`         \tab   `either 1, 2, or 3`   \cr
#'     `'n'`      \tab   `list    `(Oxford 'neither/nor') \tab   `1:3`         \tab   `neither 1, 2, nor 3` \cr
#'     `'c'`      \tab   `list    `(`c(.)` statement)     \tab   `1:3`         \tab   `'c(1, 2, 3)'`        \cr
#'     `'b'`      \tab   `paren   `(brace-enclosed)       \tab   `1`           \tab   `'{1}'`               \cr
#'     `'p'`      \tab   `paren   `(paren-enclosed)       \tab   `2`           \tab   `'(2)'`               \cr
#'     `'r'`      \tab   `paren   `(round-enclosed)       \tab   `2`           \tab   `'(2)'`               \cr
#'     `'s'`      \tab   `paren   `(square-enclosed)      \tab   `3`           \tab   `'[3]'`                 }
#' NOTE that `'p'` and `'r'` are synonyms for round parentheses.
#' \cr\cr
#' **Escape Switch Sequences**
#' \cr\cr Escape switch sequences are formatted as `'{@@}'`, `'{@@w}'`, `'{@@wx}'`, `'{@@wxy}'`, or `'{@@wxyz}` where `w`, `x`, `y`, and `z` are formatting switches from the four formatting switch families. Note that the first switch encountered from a given family is the switch that is applied. That is, if there are multiple switches from any given family, only the first is recognized and applied. Any others are ignored.
#' \cr\cr Escape switch sequences in format `'{@@}'` mean 'insert \link[=atm_scl]{atomic scalar} `...` arg *as is*'.
#' \cr\cr Escape switch sequences in format `'{@@w}'` mean 'insert \link[=atm_vec]{atomic vec} `...` arg after applying switch `w`.'
#' \cr\cr Escape switch sequences in format `'{@@wx}'` mean 'insert \link[=atm_vec]{atomic vec} `...` arg after applying switches `w` and `x`.'
#' \cr\cr Escape switch sequences in format `'{@@wxy}'` mean 'insert \link[=atm_vec]{atomic vec} `...` arg after applying switches `w`, `x`, and `y`.'
#' \cr\cr Escape switch sequences in format `'{@@wxyz}'` mean 'insert \link[=atm_vec]{atomic vec} `...` arg after applying switches `w`, `x`, `y`, and `z`.'
#' \cr\cr **Ordering of switches**
#' \cr\cr Ordering of switches within an escape switch sequence is arbitrary. Regardless of order in escape switches, any `decimal` switches are applied first, followed by any `quote` switches, followed by any `list` switches, followed by any `paren`.
#' @param tmp A \link[=cmp_chr_scl]{complete character scalar} template with embedded escape seqs. See details.
#' @param ... Arbitrary number of unnamed atomic scalar/vector arguments to be inserted into `tmp` with formatting specified in inlay escape seqs. The `n`-th argument in `...` corresponds to the `n`-th inlay escape seq in `tmp`. The number of inlay escape seqs in `tmp` must be equal to the number of `...` arguments. See details.
#' @return A character scalar.
#' @examples
#' egWeave <- function() {
#'
#'   cat("\n\n", uj::weave('{@}', FALSE))
#'   cat("\n\n", uj::weave('{@}', 42))
#'   cat("\n\n", uj::weave('{@b}', 4:7))
#'   cat("\n\n", uj::weave('{@q}', 'foo::bar'))
#'   cat("\n\n", uj::weave('{@0}', pi))
#'   cat("\n\n", uj::weave('{@c2}', c(pi, exp(1), 42)))
#'   cat("\n\n", uj::weave('{@Q6}', pi))
#'   cat("\n\n", uj::weave('{@et3}', c(pi, exp(1))))
#'   cat("\n\n", uj::weave('{@aq}', c('me', 'myself', 'I')))
#'   cat("\n\n", uj::weave('{@A2tb}', c(pi, exp(1), 42)))
#'
#'   template <- paste0('Once upon a time, I came across {@} {@r0} little pigs. They looked highly {@&}. ',
#'                      'After some discrete inquiry, I learned their names were {@T&}. I went to their ' ,
#'                      'houses, which were made of {@|}. I said {@t}. But ever so rudely, they replied ' ,
#'                      '{@Q}. So, I {@&}. Then I fried them up nicely crisp, and I ate my piggy {@9b}.' )
#'
#'   cat("\n\n")
#'
#'   cat(
#'     uj::weave(
#'       template,
#'       "three",
#'       pi,
#'       c("tasty", "well-fed", "naive"),
#'       c("Lasagna", "Ziti", "Linguini"),
#'       c("straw", "sticks", "bricks"),
#'       "little pig, little pig, let me come in",
#'       "not by the hair of my chinny chin chin",
#'       c("huffed", "puffed", "blew their houses down"),
#'       pi
#'     )
#'   )
#'
#' }
#'
#' egWeave()
#' @export
weave <- function(template, ...) {

  .valid <- function(x) {
    chars <- base::c("{", "}", "@", "&", "|", "a", "A", "b", "c", "e", "l", "n", "p", "q", "Q", "r", "s", "t", "T", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    x <- uj::av(base::strsplit(x, "", fixed = T))
    base::all(x %in% chars)
  }

  .quotes <- function(x) {
    if      (base::grepl("q", x, fixed = T)) {"q"} # '...'
    else if (base::grepl("Q", x, fixed = T)) {"Q"} # "..."
    else if (base::grepl("t", x, fixed = T)) {"t"} # “...”
    else if (base::grepl("T", x, fixed = T)) {"T"} # ‘...’
    else                                     {"z"}
  }

  .list <- function(x) {
    if      (base::grepl("l", x, fixed = T)) {"l"} # x, y, z
    else if (base::grepl("&", x, fixed = T)) {"&"} # x, y, and z
    else if (base::grepl("|", x, fixed = T)) {"|"} # x, y, or z
    else if (base::grepl("A", x, fixed = T)) {"A"} # all of x, y, and z
    else if (base::grepl("a", x, fixed = T)) {"a"} # any of x, y, and z
    else if (base::grepl("c", x, fixed = T)) {"c"} # c(x, y, z)
    else if (base::grepl("e", x, fixed = T)) {"e"} # either x, y, or z
    else if (base::grepl("n", x, fixed = T)) {"n"} # neither x, y, nor z
    else                                     {"z"}
  }

  .parens <- function(x) {
    if      (base::grepl("b", x, fixed = T)) {"b"} # {...}
    else if (base::grepl("p", x, fixed = T)) {"r"} # (...)
    else if (base::grepl("r", x, fixed = T)) {"r"} # (...)
    else if (base::grepl("s", x, fixed = T)) {"s"} # [...]
    else                                     {"z"}
  }

  .digits <- function(x) {
    if      (base::grepl("0", x, fixed = T)) { 0}
    else if (base::grepl("1", x, fixed = T)) { 1}
    else if (base::grepl("2", x, fixed = T)) { 2}
    else if (base::grepl("3", x, fixed = T)) { 3}
    else if (base::grepl("4", x, fixed = T)) { 4}
    else if (base::grepl("5", x, fixed = T)) { 5}
    else if (base::grepl("6", x, fixed = T)) { 6}
    else if (base::grepl("7", x, fixed = T)) { 7}
    else if (base::grepl("8", x, fixed = T)) { 8}
    else if (base::grepl("9", x, fixed = T)) { 9}
    else                                     {-1}
  }

  nDots <- base::...length()                                                                                                                      # the number of [...] args
  okTmp <- uj::.cmp_chr_scl(template)                                                                                                             # whether the [template] arg is appropriate
  okN   <- nDots > 0                                                                                                                              # whether the number of [...] args is appropriate
  okPPP <- base::ifelse(okN, base::all(base::sapply(base::list(...), uj::cmp_atm)), T)                                                            # whether [...] args are appropriate
  errs  <- NULL                                                                                                                                   # initialize the error container
  if (!okN  ) {errs <- base::c(errs, "[...] is empty.")}                                                                                          # if there are no [...] args, store an error
  if (!okTmp) {errs <- base::c(errs, "[template] must be a complete character scalar (?cmp_chr_scl).")}                                           # if [template] args is problematic, store an error
  if (!okPPP) {errs <- base::c(errs, "Arguments in [...] must be complete atomic objects (?cmp_atm).")}                                           # if [...] args are problematic, store an error
  if (!base::is.null(errs)) {uj::stopperr(errs)}                                                                                      # if there are any errors thus far, throw them
  nch       <- base::nchar(template)                                                                                                              # number of characters in the template
  dots      <- base::list(...)                                                                                                                    # the arguments to weave into X
  nDots     <- base::length(dots)                                                                                                                 # number of dot arguments
  prefix    <- "[{][@]"                                                                                                                           # [gregexpr] pattern for switch code prefix, i.e., '{@'
  suffix    <- "[}]"                                                                                                                              # [gregexpr] pattern for switch code suffix, i.e., '}'
  infix     <- "[&|AabcelnpqQrstT0123456789]"                                                                                                     # [gregexpr] pattern for all valid switches
  pattern0  <- base::paste0(prefix, suffix)                                                                                                       # 0-switch pattern '{@}'
  pattern1  <- base::paste0(prefix, infix, suffix)                                                                                                # 1-switch pattern '{@s}'
  pattern2  <- base::paste0(prefix, infix, infix, suffix)                                                                                         # 2-switch pattern '{@ss}'
  pattern3  <- base::paste0(prefix, infix, infix, infix, suffix)                                                                                  # 3-switch pattern '{@sss}'
  pattern4  <- base::paste0(prefix, infix, infix, infix, infix, suffix)                                                                           # 4-switch pattern '{@ssss}'
  switches0 <- base::gregexpr(pattern0, template)                                                                                                 # results of searching [template] for 0-switch patterns
  switches1 <- base::gregexpr(pattern1, template)                                                                                                 # results of searching [template] for 1-switch patterns
  switches2 <- base::gregexpr(pattern2, template)                                                                                                 # results of searching [template] for 2-switch patterns
  switches3 <- base::gregexpr(pattern3, template)                                                                                                 # results of searching [template] for 3-switch patterns
  switches4 <- base::gregexpr(pattern4, template)                                                                                                 # results of searching [template] for 4-switch patterns
  switches0 <- uj::f0(base::setequal(uj::av(switches0), -1), NULL, uj::tb(start = uj::av(switches0), nchar = 3))                                  # tibble of 0-switch pattern positions and lengths
  switches1 <- uj::f0(base::setequal(uj::av(switches1), -1), NULL, uj::tb(start = uj::av(switches1), nchar = 4))                                  # tibble of 1-switch pattern positions and lengths
  switches2 <- uj::f0(base::setequal(uj::av(switches2), -1), NULL, uj::tb(start = uj::av(switches2), nchar = 5))                                  # tibble of 2-switch pattern positions and lengths
  switches3 <- uj::f0(base::setequal(uj::av(switches3), -1), NULL, uj::tb(start = uj::av(switches3), nchar = 6))                                  # tibble of 3-switch pattern positions and lengths
  switches4 <- uj::f0(base::setequal(uj::av(switches4), -1), NULL, uj::tb(start = uj::av(switches4), nchar = 7))                                  # tibble of 4-switch pattern positions and lengths
  switches  <- base::rbind(switches0, switches1, switches2, switches3, switches4)                                                                 # tibble of all switch pattern positions and lengths
  nSwitches <- uj::nr(switches)                                                                                                                   # number of switch patterns found in [template]
  switches  <- uj::f0(nSwitches < 2, switches, switches[base::order(switches$start), ])                                                           # IF sorting is needed: sort by position of first letter of pattern in [template]
  errs      <- NULL                                                                                                                               # initialize the error container
  mssg      <- uj::p0("The number of escape switch sequences (", nSwitches, ") does not matxh the number of [...] arguments (", nDots, ")")       # build an error message
  if (nSwitches != nDots) {uj::stopperr(mssg)}                                                                                                    # IF number of [...] args mismatches number of sequences: throw an error
  for (i in nSwitches:1) {                                                                                                                        # FOR each switch escape sequence from last to first
    dot <- dots[[i]]                                                                                                                              # : get the matching [...] argument
    n2p <- base::length(dot) > 1                                                                                                                  # : whether there are multiple elements of the matching [...] arg
    start  <- switches$start[i]                                                                                                                   # : get the start character of the switch escape sequence in [template]
    stop   <- start + switches$nchar[i] - 1                                                                                                       # : get the end character of the switch escape sequence in [template
    switch <- base::substr(template, start, stop)                                                                                                 # : extract the switch escape sequence
    if (.valid(switch)) {                                                                                                                         # : IF the switch escape sequence is valid
      quoteSwitch <- .quotes(switch)                                                                                                              # : : get the type of quote switch ("z" if none)
      listSwitch  <- .list(  switch)                                                                                                              # : : get the type of list  switch ("z" if none)
      parenSwitch <- .parens(switch)                                                                                                              # : : get the type of paren switch ("z" if none)
      digitSwitch <- .digits(switch)                                                                                                              # : : get the number of digits     (-1  if none)
      numErr      <- digitSwitch > -1 & !base::is.numeric(dot)                                                                                    # : : whether there is a mismatch for expected numeric input
      if (!numErr) {                                                                                                                              # : : IF there is a mismatch
        if (digitSwitch > -1) {dot <- base::sprintf(uj::p0("%0.", digitSwitch, "f"), dot)}                                                        # : : : IF there is a digits switch   , apply
        if      (quoteSwitch == "Q") {dot <- uj::p0("\"", dot, "\"")}                                                                             # : : : IF      double straight quotes, apply
        else if (quoteSwitch == "q") {dot <- uj::p0("'" , dot, "'" )}                                                                             # : : : ELSE IF single straight quotes, apply
        else if (quoteSwitch == "T") {dot <- uj::p0("\u201c", dot, "\u201d")}                                                                     # : : : ELSE IF double typeset  quotes, apply
        else if (quoteSwitch == "t") {dot <- uj::p0("\u2018", dot, "\u2019")}                                                                     # : : : ELSE IF single typeset  quotes, apply
        if      (listSwitch  == "l") {dot <- uj::p0(dot, collapse = ", ")}                                                                        # : : : IF      simple list           , apply
        else if (listSwitch  == "&") {dot <- uj::ox_vals(dot, "and")}                                                                             # : : : ELSE IF oxford-and            , apply
        else if (listSwitch  == "|") {dot <- uj::ox_vals(dot, "or" )}                                                                             # : : : ELSE IF oxford-or             , apply
        else if (listSwitch  == "A") {if (n2p) {dot <- uj::p0("all of " , uj::ox_vals(dot, "and"))}}                                              # : : : ELSE IF all-of                , apply
        else if (listSwitch  == "a") {if (n2p) {dot <- uj::p0("any of " , uj::ox_vals(dot, "or" ))}}                                              # : : : ELSE IF none-of               , apply
        else if (listSwitch  == "e") {if (n2p) {dot <- uj::p0("either " , uj::ox_vals(dot, "or" ))}}                                              # : : : ELSE IF either/or             , apply
        else if (listSwitch  == "n") {if (n2p) {dot <- uj::p0("neither ", uj::ox_vals(dot, "nor"))}}                                              # : : : ELSE IF neither/nor           , apply
        else if (listSwitch  == "c") {dot <- uj::p0("c(", uj::p0(dot, collapse = ", "), ")")}                                                     # : : : IF      concatenate           , apply
        if      (parenSwitch == "b") {dot <- uj::p0( "{", dot, "}")}                                                                              # : : : ELSE IF braces                , apply
        else if (parenSwitch == "r") {dot <- uj::p0( "(", dot, ")")}                                                                              # : : : ELSE IF round                 , apply
        else if (parenSwitch == "s") {dot <- uj::p0( "[", dot, "]")}                                                                              # : : : ELSE IF square                , apply
        if (base::is.null(errs)) {                                                                                                                # : : : IF there are no stored errors
          if (start > 1  ) {before <- base::substr(template, 1       , start - 1)} else {before <- ""}                                            # : : : : get the template text before the switch escape sequence
          if (stop  < nch) {after  <- base::substr(template, stop + 1, nch      )} else {after  <- ""}                                            # : : : : get the template text after the switch escape sequence
          template <- uj::p0(before, dot, after)                                                                                                  # : : : : piece the template together with the inlay sequence inserted
          nch      <- base::nchar(template)                                                                                                       # : : : : reset the number of chars in the template after each insertion
        }                                                                                                                                         # : : : END IF
      } else {errs <- base::c(errs, uj::p0("Switch sequence ", i, " (", switch, ") assumes a numeric input, but ...", i, " is non-numeric."))}    # : : : ELSE: store an error
    } else {errs <- base::c(errs, uj::p0("Switch sequence ", i, " (", switch, ") is not a valid switch."))}                                       # : : ELSE: store an error
  }                                                                                                                                               # : END FOR
  if (!base::is.null(errs)) {uj::stopperr(errs)}                                                                                      # IF there are any stored errors: throw them
  template                                                                                                                                        # return the result
}
