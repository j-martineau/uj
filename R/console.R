#' @name say
#' @title User Updates and Interactions via the Console
#' @description \tabular{ll}{
#'   FUNCTION     \tab WHAT IT DOES                                          \cr
#'   `say`        \tab Collapse the message in `...` to a character scalar
#'                     and print it to the console or structure it as a certain
#'                     level of update and then print it to the console.
#'                     See details.                                          \cr
#'   `cat0`       \tab Collapse `...` to a character scalar message
#'                     prefixed and suffixed with newlines, and print it to the
#'                     console (ensures that the message occurs on its own line
#'                     of the console).                                      \cr
#'   `continue`   \tab Print `"Hit [enter] or [return] to continue"` to the
#'                     console and pause execution until the user hits the enter
#'                     or return key.                                          }
#' @details The function `say` allows for structuring user update messages
#'   at six hierarchical levels as explained in the following table where
#'   `___` is a placeholder the character scalar message created when
#'   collapsing `...`:\tabular{lll}{
#'     MESSAGE     \tab VALUE OF     \tab UPDATE                             \cr
#'     SUPPLIED    \tab THE `lev``   \tab PRINTED TO                         \cr
#'     IN `...`?   \tab ARGUMENT     \tab CONSOLE                            \cr
#'     yes         \tab `NA`         \tab `'___'`                            \cr
#'     yes         \tab `1`          \tab `'\\n___'`                         \cr
#'     yes         \tab `2`          \tab `'\\n| ___'`                       \cr
#'     yes         \tab `3`          \tab `'\\n| > ___'`                     \cr
#'     yes         \tab `4`          \tab `' > ___'`                         \cr
#'     yes         \tab `5`          \tab `'..(___)'`                        \cr
#'     no          \tab `6`          \tab `'.'`                              \cr
#'     no          \tab `NA`         \tab '.'                                  }
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
#'   `lev = NA` or `lev = 6`, prints a single dot (`.`) immediately following
#'   the current contents of the console.
#' @param lev Either `NA` to print the message in `...` immediately following
#'   the current contents of the console or a value from `1:6` to use the
#'   console for up to six levels of user updates. See details.
#' @return `NULL`. Called for side effects of console-based user updates or
#'   pausing execution for until user indicates execution should continue.
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

#' @rdname say
#' @export
cat0 <- function(...) {say("\n", ..., "\n")}

#' @rdname say
#' @export
continue <- function() {readline("Hit [enter] or [return] to continue")}
