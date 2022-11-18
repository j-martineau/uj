#' @name console.
#' @title User Updates and Interactions via the Console
#' @description This family of functions is for interacting with the user via
#'   the console: \tabular{ll}{
#'   FUNCTION          \tab WHAT THE                                         \cr
#'   NAME              \tab FUNCTION DOES                                    \cr
#'   \code{say}        \tab Collapse the message in \code{...} to a character
#'                          scalar and print it to the console or structure it
#'                          as a certain level of update and then print it to
#'                          the console. See details.                        \cr
#'   \code{cat0}       \tab Collapse \code{...} to a character scalar message
#'                          prefixed and suffixed with newlines, and print it to
#'                          the console (ensures that the message occurs on its
#'                          own line of the console).
#'   \code{continue}   \tab Print \code{"Hit [enter] or [return] to continue"}
#'                          to the console and pause execution until the user
#'                          hits the enter or return key.                      }
#' @details The function \code{say} allows for structuring user update messages
#'   at six hierarchical levels as explained in the following table where
#'   \code{___} is a placeholder the character scalar message created when
#'   collapsing \code{...}.: \tabular{lll}{
#'   MESSAGE           \tab VALUE OF          \tab UPDATE                    \cr
#'   SUPPLIED          \tab THE \code{lev}    \tab PRINTED TO                \cr
#'   IN \code{...}?    \tab ARGUMENT          \tab CONSOLE                   \cr
#'   yes               \tab \code{NA}         \tab \code{'___'}              \cr
#'   yes               \tab \code{1}          \tab \code{'\\n___'}           \cr
#'   yes               \tab \code{2}          \tab \code{'\\n| ___'}         \cr
#'   yes               \tab \code{3}          \tab \code{'\\n| > ___'}       \cr
#'   yes               \tab \code{4}          \tab \code{' > ___'}           \cr
#'   yes               \tab \code{5}          \tab \code{'..(___)'}          \cr
#'   no                \tab \code{6}          \tab \code{'.'}                \cr
#'   no                \tab \code{NA}         \tab \code{'.'}                  }
#'   Other combinations not represented in the table above will result in an
#'   error.
#'   \cr\cr
#'   To better understand this user update system, an example is provided. If
#'   the current contents of the console were this:
#'   ```
#'     > prior.call(...)
#'     > vector_of_results
#'     AN EXISTING MESSAGE
#'   ```
#'   Then executing this code:
#'   ```
#'     say("_a_random", "_message")
#'     say(); say(lev = 6)
#'     say("L1", lev = 1)
#'     say("L2", lev = 2); say("L2B", lev = 2)
#'     say("L3", lev = 3); say("L3B", lev = 3)
#'     say(); say(); say(); say(lev = 6)
#'     say("L3C", lev = 3)
#'     say("L4" , lev = 4); say("L4B", lev = 4)
#'     say("L5" , lev = 5)
#'     say(); say()
#'     say("L5B", lev = 5)
#'     say("L4C", lev = 4)
#'     say("L3D", lev = 3)
#'     say(); say()
#'     say("L1B", lev = 1)
#'     say("_another_random_message")
#'   ```
#'   Will result in a console with these contents:
#'   ```
#'     > prior.call(...)
#'     > vector_of_results
#'     AN EXISTING MESSAGE_a_random_message..
#'     L1
#'     | L2
#'     | L2B
#'     | > L3
#'     | > L3B....
#'     | > L3C > L4 > L4B..(L5)....(L5B) > L4C
#'     | > L3D..
#'     L1B_another_random_message
#'   ```
#' @param ... An arbitrary number of atomic arguments to be atomized collapsed
#'   into a character scalar message to the user. If no values are supplied and
#'   \code{lev = NA} or \code{lev = 6}, prints a single dot (period) immediately
#'   following the current contents of the console.
#' @param lev Either \code{NA} to print the message in \code{...} immediately
#'   following the current contents of the console or a value from \code{1:6} to
#'   use the console for up to six levels of user updates. See details.
#' @return \code{NULL}: called for side effects of console-based user updates or
#'   pausing execution for until user indicates execution should continue.
#' @export
console. <- function() {help("console.", package = "uj")}

#' @rdname console.
#' @export
say <- function(..., lev = NA) {
  msg <- paste(av(...), collapse = "")
  dot <- isID(msg, "") | isID(as.numeric(lev), as.numeric("6"))
  less <- isIN(as.numeric(lev), as.numeric(as.character(1:5)))
  if (dot & less) {stop("\n \u2022 [...] must not resolve to a blank string when [lev] is in c(1, 2, 3, 4, 5).")}
  if (!isID(msg, ""))
  if (notID(msg, "") & notID(lev, 6)) {
    if (!nas(lev)) {
      if (isIN(lev, 1:5)) {
        prefix <- c("\n ", "\n| ", "\n| > ", " > ", "..(")
        suffix <- c(""   , ""    , ""      , ""   ,   ")")
        cat(paste0(prefix[lev], msg, suffix[lev]))
      } else {stop("\n \u2022 [lev] must be scalar NA or a positive whole-number scalar from c(NA, 1, 2, 3, 4, 5).")}
    } else {cat(msg)}
  } else {cat(".")}
  NULL
}

#' @rdname console.
#' @export
cat0 <- function(...) {say("\n", ..., "\n")}

#' @rdname console.
#' @export
continue <- function() {readline("Hit [enter] or [return] to continue")}
