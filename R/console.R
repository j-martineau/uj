#' @encoding UTF-8
#' @family meta
#' @family user
#' @title User updates and interaction via the console
#' @details
#' \tabular{ll}{  `continue`   \tab Prints `"Hit [enter] or [return] to continue"` to the console and pausse execution until the user hits the enter or return key.                                             \cr   \tab   \cr
#'                `cat0`       \tab Collapses `...` to a character scalar message prefixed and suffixed with newlines and prints it to the console (ensures the message occurs on its own line of the console). \cr   \tab   \cr
#'                `say`        \tab Collapses `...` to a character scalar message and either (a) prints it to the console or (b) structures it as an update and prints it to the console. See the next table.                  }
#' The function `say` allows for structuring user update messages at six hierarchical levels as explained in the following table where `msg` is a placeholder for the character scalar message created when collapsing `...`:
#' \tabular{lll}{  MESSAGE     \tab VALUE OF    \tab UPDATE                \cr
#'                 SUPPLIED    \tab THE `lev`   \tab PRINTED               \cr
#'                 IN `...`?   \tab ARGUMENT    \tab TO CONSOLE            \cr
#'                 yes         \tab `NA`        \tab `'msg'`               \cr
#'                 yes         \tab `1`         \tab newline + `'msg'`     \cr
#'                 yes         \tab `2`         \tab newline + `'| msg'`   \cr
#'                 yes         \tab `3`         \tab newline + `'| > msg'` \cr
#'                 yes         \tab `4`         \tab `' > msg'`            \cr
#'                 yes         \tab `5`         \tab `'..(msg)'`           \cr
#'                 no          \tab `6`         \tab `'.'`                 \cr
#'                 no          \tab `NA`        \tab `'.'`                   }
#' Other combinations not represented in the table above will result in an error.
#' @param ... An arbitrary number of atomic arguments to be atomized collapsed into a character scalar message to the user. If no values are supplied and `lev = NA` or `lev = 6`, prints a single dot (`.`) immediately following the current contents of the console.
#' @param lev Either `NA` to print the message in `...` immediately following the current contents of the console or a value from `1:6` to use the console for up to `6` levels of user updates. See details.
#' @return `NULL`.
#' @examples
#' cat0("msg on its own line")
#' chat. <- function() {
#'   say("msg 1A", lev = 1)
#'   say("msg 2A", lev = 2)
#'   say("msg 2B", lev = 2)
#'   say("msg 3A", lev = 3)
#'   say("msg 3B", lev = 3)
#'   say()
#'   say()
#'   say()
#'   say("msg 3C", lev = 3)
#'   say("msg 4A", lev = 4)
#'   say("msg 4B", lev = 4)
#'   say("msg 5A", lev = 5)
#'   say(lev = 6)
#'   say(lev = 6)
#'   say("msg 5B", lev = 5)
#'   say("msg 3D", lev = 3)
#'   say()
#'   say()
#'   say("msg 1B", lev = 1)
#'   say(" + a_random_message")
#' }
#' chat.()
#' \dontrun{continue()}
#' @export
say <- function(..., lev = NA) {
  msg <- paste0(uj::av(...), collapse = "")
  if (msg == "") {dot <- TRUE} else if (base::as.character(lev) == "6") {dot <- TRUE}
  if (uj:::.cmp_psw_scl(lev)) {
    less <- lev %in% 1:5
    na.lev <- FALSE
    lev.6 <- lev == 6
  } else {
    less <- FALSE
    na.lev <- TRUE
    lev.6 <- F
  }
  if (!dot & less) {uj::stopperr("[...] must not resolve to a blank string when [lev] is in c(1, 2, 3, 4, 5).", PKG = "uj")}
  if (msg != "" & !lev.6) {
    if (!base::is.na(lev)) {
      if (!(lev %in% 1:5)) {uj::stopperr("[lev] must be scalar NA or a positive whole-number scalar from c(1, 2, 3, 4, 5).", PKG = "uj")}
      prefix <- base::c("\n", "\n| ", "\n| > ", " > ", "..(")
      suffix <- base::c(""   , ""    , ""      , ""   ,   ")")
      base::cat(base::paste0(prefix[lev], msg, suffix[lev]))
    } else {base::cat(msg)}
  } else {base::cat(".")}
}

#' @rdname say
#' @export
cat0 <- function(...) {uj::say("\n", ..., "\n")}

#' @rdname say
#' @export
continue <- function() {base::readline("Hit [enter] or [return] to continue")}
