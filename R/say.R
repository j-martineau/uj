#' @encoding UTF-8
#' @family meta
#' @family console
#' @title User updates and interaction via the console in default color and style
#' @param ... An arbitrary number of atomic arguments to be atomized collapsed into a character scalar message to the user. If no values are supplied and `lev = NA` or `lev = 6`, prints a single dot (`.`) immediately following the current contents of the console.
#' @param lev Either `NA` to print the message in `...` immediately following the current contents of the console or a value from `1:6` to use the console for up to `6` levels of user updates. See function `say` below.
#' @param sub Either `0`, `1`, or `2`. See function `say` below.
#' @param clear Either `TRUE` or `FALSE` indicating whether to clear the console before printing a message to it.
#' @return `NULL`.
#' @examples
#' xcon()
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
#'   say(" + a_random_mssg")
#' }
#' chat.()
#' \dontrun{continue()}
#' @export
console_help <- function() {utils::help("console_help", package = "dlg")}

#' @describeIn console_help Flattens `...` to a character scalar message and either (a) prints it to the console or (b) structures it as an update and prints it to the console. When `...` is empty, the message defaults to a single dot (`"."`). Allows for structuring user update messages at hierarchical levels as explained in the following table:
#'  \tabular{ll}{  VALUE      \tab PREFIX OF MESSAGE          \cr
#'                 OF `lev`   \tab POSTED TO CONSOLE          \cr
#'                 `0`        \tab < none >                   \cr
#'                 `1`        \tab < newline >                \cr
#'                 `2`        \tab < newline > + `'| '`       \cr
#'                 `3`        \tab < newline > + `'| > '`     \cr
#'                 `4`        \tab < newline > + `'| | > '`   \cr
#'                 `5`        \tab < newline > + `'| | | > '` \cr
#'                 `...`      \tab `...`                        }
#' @export
say <- function(..., lev = 0, sub = 0, clear = FALSE) {
  if (clear) {uj::xcon()}
  lev <- uj::f0(uj::.cmp_nnw_scl(lev), lev, 0)
  sub <- base::min(2, uj::f0(uj::.cmp_nnw_scl(sub), sub, 0))
  if      (lev == 0) {pref <- ""}
  else if (lev == 1) {pref <- "\n"}
  else               {pref <- uj::g0(base::c("\n", base::rep("| ", lev - 1)))}
  if (sub == 1) {
    pref <- uj::p0(pref, " > ")
    suff <- ""
  } else if (sub == 2) {
    pref <- uj::p0(pref, "..(")
    suff <- ")"
  } else {suff <- ""}
  msg <- uj::p0(uj::av(...), collapse = "")
  if (msg == "") {msg <- "."}
  msg <- uj::p0(pref, msg, suff)
  base::cat(msg)
}

#' @describeIn console_help Flattens `...` into a character scalar and prints it to the console preceded and succeeded by a new line.
#' @export
cat0 <- function(..., clear = FALSE) {uj::say("\n", ..., "\n", clear = clear)}

#' @describeIn console_help Prints `"Hit [enter] or [return] to continue"` to the console and pause execution until the user hits the enter or return key.
#' @export
continue <- function(clear = FALSE) {
  if (clear) {uj::xcon()}
  base::readline("Press [enter] or [return] to continue: ")
}

#' @describeIn console_help Clears the console.
#' @export
xcon <- function() {
  base::gc(verbose = FALSE)
  base::cat("\014")
}
