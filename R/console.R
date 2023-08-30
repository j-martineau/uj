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
#'                OF `.LEV`   \tab POSTED TO CONSOLE          \cr
#'                `0`        \tab < none >                   \cr
#'                `1`        \tab < newline >                \cr
#'                `2`        \tab < newline > + `'| '`       \cr
#'                `3`        \tab < newline > + `'| > '`     \cr
#'                `4`        \tab < newline > + `'| | > '`   \cr
#'                `5`        \tab < newline > + `'| | | > '` \cr
#'                `...`      \tab `...`                        }
#' In addition, .SUB-levels are defined via the argument `.SUB` where `.SUB = 0` leaves the message as is, `.SUB = 1` prepends `' > '` to the message, and `.SUB = 2`, prepends `"..("` and appends `")"` to the message.
#' @param ... An arbitrary number of atomic arguments to be atomized collapsed into a character scalar message to the user. If no values are supplied and `.LEV = NA` or `.LEV = 6`, prints a single dot (`.`) immediately following the current contents of the console.
#' @param .LEV Either `NA` to print the message in `...` immediately following the current contents of the console or a value from `1:6` to use the console for up to `6` levels of user updates. See details.
#' @param .SUB Either `0`, `1`, or `2`. See details.
#' @param .CLEAR Either `TRUE` or `FALSE` indicating whether to clear the console before printing a message to it.
#' @return `NULL`.
#' @examples
#' cat0("msg on its own line")
#' chat. <- function() {
#'   say("msg 1A", .LEV = 1)
#'   say("msg 2A", .LEV = 2)
#'   say("msg 2B", .LEV = 2)
#'   say("msg 3A", .LEV = 3)
#'   say("msg 3B", .LEV = 3)
#'   say()
#'   say()
#'   say()
#'   say("msg 3C", .LEV = 3)
#'   say("msg 4A", .LEV = 4)
#'   say("msg 4B", .LEV = 4)
#'   say("msg 5A", .LEV = 5)
#'   say(.LEV = 6)
#'   say(.LEV = 6)
#'   say("msg 5B", .LEV = 5)
#'   say("msg 3D", .LEV = 3)
#'   say()
#'   say()
#'   say("msg 1B", .LEV = 1)
#'   say(" + a_random_message")
#' }
#' chat.()
#' \dontrun{continue()}
#' @export
say <- function(..., .LEV = 0, .SUB = 0, .CLEAR = FALSE) {
  if (.CLEAR) {uj::xconsole()}
  .LEV <- uj::f0(uj::cmp_nnw_scl(.LEV), .LEV, 0)
  .SUB <- base::min(2, uj::f0(uj::cmp_nnw_scl(.SUB), .SUB, 0))
  if      (.LEV == 0) {Prefix <- ""}
  else if (.LEV == 1) {Prefix <- "\n"}
  else                {Prefix <- base::paste0(base::c("\n", base::rep.int("| ", .LEV - 1)), collapse = "")}
  if (.SUB == 1) {
    Prefix <- base::paste0(Prefix, " > ")
    Suffix <- ""
  } else if (.SUB == 2) {
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
cat0 <- function(..., .CLEAR = FALSE) {uj::say("\n", ..., "\n", .CLEAR = .CLEAR)}

#' @rdname say
#' @export
continue <- function(.CLEAR = FALSE) {
  if (.CLEAR) {uj::xconsole()}
  base::readline("Press [enter] or [return] to continue: ")
}
