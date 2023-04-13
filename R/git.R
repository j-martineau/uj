#' @title Git commands
#' @description Run git commands from the console or programmatically, activating and bringing a terminal to the front, executing the command, and returning focus to the console.
#' @param Message Character scalar git commit message.
#' @param AddAll Non-`NA` logical scalar indicating whether to call `git add --all` before calling `git commit -am "{Message}"`.
#' @param Console Non-`NA` logical scalar indicating whether to bring the console to the front upon conclusion.
#' @return `NULL`
#' @export
git_push <- function(Message, AddAll = T, Console = T) {
  Errors <- NULL
  if (!uj:::.cmp_str_scl(Message)) {Errors <- base::c(Errors, "[Message] must be a complete string scalar (?cmp_str_scl).")}
  if (!uj:::.cmp_lgl_scl(AddAll)) {Errors <- base::c(Errors, "[AddAll] must be scalar TRUE or scalar FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  rstudioapi::terminalActivate()
  wd <- base::getwd()
  if (AddAll) {rstudioapi::terminalExecute("git add --all", workingDir = wd)}
  rstudioapi::terminalExecute(uj::p0("git commit -am \"", Message, "\""), workingDir = wd)
  rstudioapi::terminalExecute("git push", workingDir = wd)
  if (Console) {base::cat("")}
}
