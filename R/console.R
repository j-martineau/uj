#' @encoding UTF-8
#' @family meta
#' @family user
#' @title User updates and interaction via the console in default color and style
#' @details
#' \tabular{ll}{  `continue`   \tab Prints `"Hit [enter] or [return] to continue"` to the console and pausse execution until the user hits the enter or return key.                                             \cr   \tab   \cr
#'                `cat0`       \tab Collapses `...` to a character scalar message prefixed and suffixed with newlines and prints it to the console (ensures the message occurs on its own line of the console). \cr   \tab   \cr
#'                `say`        \tab Collapses `...` to a character scalar message and either (a) prints it to the console or (b) structures it as an update and prints it to the console. See the next table.                  }
#' When `...` is empty, the message defaults to a single dot (`"."`).
#' \cr\cr The function `say` allows for structuring user update messages at hierarchical levels as explained in the following table:
#' \tabular{ll}{  VALUE      \tab PREFIX OF MESSAGE          \cr
#'                OF `Lev`   \tab POSTED TO CONSOLE          \cr
#'                `0`        \tab < none >                   \cr
#'                `1`        \tab < newline >                \cr
#'                `2`        \tab < newline > + `'| '`       \cr
#'                `3`        \tab < newline > + `'| > '`     \cr
#'                `4`        \tab < newline > + `'| | > '`   \cr
#'                `5`        \tab < newline > + `'| | | > '` \cr
#'                `...`      \tab `...`                        }
#' In addition, Sub-levels are defined via the argument `Sub` where `Sub = 0` leaves the message as is, `Sub = 1` prepends `' > '` to the message, and `Sub = 2`, prepends `"..("` and appends `")"` to the message.
#' @param ... An arbitrary number of atomic arguments to be atomized collapsed into a character scalar message to the user. If no values are supplied and `Lev = NA` or `Lev = 6`, prints a single dot (`.`) immediately following the current contents of the console.
#' @param Lev Either `NA` to print the message in `...` immediately following the current contents of the console or a value from `1:6` to use the console for up to `6` levels of user updates. See details.
#' @return `NULL`.
#' @examples
#' cat0("msg on its own line")
#' chat. <- function() {
#'   say("msg 1A", Lev = 1)
#'   say("msg 2A", Lev = 2)
#'   say("msg 2B", Lev = 2)
#'   say("msg 3A", Lev = 3)
#'   say("msg 3B", Lev = 3)
#'   say()
#'   say()
#'   say()
#'   say("msg 3C", Lev = 3)
#'   say("msg 4A", Lev = 4)
#'   say("msg 4B", Lev = 4)
#'   say("msg 5A", Lev = 5)
#'   say(Lev = 6)
#'   say(Lev = 6)
#'   say("msg 5B", Lev = 5)
#'   say("msg 3D", Lev = 3)
#'   say()
#'   say()
#'   say("msg 1B", Lev = 1)
#'   say(" + a_random_message")
#' }
#' chat.()
#' \dontrun{continue()}
#' @export
say <- function(..., Lev = 0, Sub = 0, .clear = FALSE) {
  if (.clear) {uj::xconsole()}
  Lev <- uj::f0(uj::cmp_nnw_scl(Lev), Lev, 0)
  Sub <- base::min(2, uj::f0(uj::cmp_nnw_scl(Sub), Sub, 0))
  if (Lev > 2) {
    Prefix1 <- "\n"
    Prefix2 <- base::paste0(base::rep.int("| ", Lev - 2), collapse = "")
    Prefix3 <- "> "
    Prefix <- base::paste0(Prefix1, Prefix2, Prefix3)
  } else if (Lev == 2) {Prefix <- "\n| "}
  else if (Lev == 1) {Prefix <- "\n"}
  else {Prefix <- ""}
  if (Sub == 1) {
    Prefix <- base::paste0(Prefix, " > ")
    Suffix <- ""
  } else if (Sub == 2) {
    Prefix <- base::paste0(Prefix, "..(")
    Suffix <- ")"
  } else {Suffix <- ""}
  Message <- base::paste0(uj::av(...), collapse = "")
  if (Message == "") {Message <- "."}
  Message <- base::paste0(Prefix, Message, Suffix)
  base::cat(Message)
}

#' @rdname say
#' @export
cat0 <- function(..., .clear = FALSE) {uj::say("\n", ..., "\n", .clear = .clear)}

#' @rdname say
#' @export
continue <- function(.clear = FALSE) {
  if (.clear) {uj::xconsole()}
  base::readline("Press [enter] or [return] to continue: ")
}
