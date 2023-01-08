#' @encoding UTF-8
#' @family meta
#' @family user
#' @title User updates and interaction via the console
#' @description \tabular{rl}{
#'     `continue`   \tab Prints `"Hit [enter] or [return] to continue"` to the console and pausse execution until the user hits the enter or return key.
#'   \cr            \tab  
#'   \cr   `cat0`   \tab Collapses `...` to a character scalar message prefixed and suffixed with newlines and prints it to the console (ensures the message occurs on its own line of the console).
#'   \cr            \tab  
#'   \cr    `say`   \tab Collapses the message in `...` to a character scalar and either (a) prints it to the console or (b) structures it as a certain level of update and then prints it to the console. See details.
#' }
#' @details The function `say` allows for structuring user update messages at six hierarchical levels as explained in the following table where `___` is a placeholder for the character scalar message created when collapsing `...`:
#' \tabular{ccl}{
#'            MESSAGE    \tab    VALUE     \tab    UPDATE
#'   \cr     SUPPLIED    \tab OF THE `lev` \tab    PRINTED
#'   \cr    IN `...`?    \tab   ARGUMENT   \tab    TO CONSOLE
#'   \cr       yes       \tab     `NA`     \tab    `'___'`
#'   \cr       yes       \tab     `1`      \tab    newline + `'___'`
#'   \cr       yes       \tab     `2`      \tab    newline + `'| ___'`
#'   \cr       yes       \tab     `3`      \tab    newline + `'| > ___'`
#'   \cr       yes       \tab     `4`      \tab    `' > ___'`
#'   \cr       yes       \tab     `5`      \tab    `'..(___)'`
#'   \cr        no       \tab     `6`      \tab    `'.'`
#'   \cr        no       \tab     `NA`     \tab    `'.'`
#' }
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
  msg <- base::paste(uj::av(...), collapse = "")
  dot <- uj::isID(msg, "") | uj::isID(base::as.numeric(lev), base::as.numeric("6"))
  less <- uj::isIN(base::as.numeric(lev), base::as.numeric( base::as.character(1:5)))
  if (dot & less) {stop(uj:::.errs("[...] must not resolve to a blank string when [lev] is in c(1, 2, 3, 4, 5)."))}
  if (!uj::isID(msg, ""))
  if (uj::notID(msg, "") & uj::notID(lev, 6)) {
    if (!uj::nas(lev)) {
      if (uj::isIN(lev, 1:5)) {
        prefix <- base::c("\n", "\n| ", "\n| > ", " > ", "..(")
        suffix <- base::c(""   , ""    , ""      , ""   ,   ")")
        base::cat(base::paste0(prefix[lev], msg, suffix[lev]))
      } else {stop(uj:::.errs("[lev] must be scalar NA or a positive whole-number scalar from c(1, 2, 3, 4, 5)."))}
    } else {base::cat(msg)}
  } else {base::cat(".")}
}

#' @rdname say
#' @export
cat0 <- function(...) {uj::say("\n", ..., "\n")}

#' @rdname say
#' @export
continue <- function() {base::readline("Hit [enter] or [return] to continue")}
