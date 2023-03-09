#' @title Git commands
#' @description Run git commands from the console or programmatically, activating and bringing a terminal to the front, executing the command, and returning focus to the console.
#' @param message Character scalar git commit message.
#' @param add.all Non-`NA` logical scalar indicating whether to call `git add --all` before calling `git commit -am "{message}"`.
#' @param console Non-`NA` logical scalar indicating whether to bring the console to the front upon conclusion.
#' @return `NULL`
#' @export
git_push <- function(message, add.all = T, console = T) {
  errs <- NULL
  if (!uj:::.cmp_str_scl(message)) {errs <- base::c(errs, "[message] must be a complete string scalar (?cmp_str_scl).")}
  if (!uj:::.cmp_lgl_scl(add.all)) {errs <- base::c(errs, "[add.all] must be scalar TRUE or scalar FALSE.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  rstudioapi::terminalActivate()
  wd <- base::getwd()
  if (add.all) {rstudioapi::terminalExecute("git add --all", workingDir = wd)}
  rstudioapi::terminalExecute(uj::p0("git commit -am \"", message, "\""), workingDir = wd)
  rstudioapi::terminalExecute("git push", workingDir = wd)
  if (console) {base::cat("")}
}
