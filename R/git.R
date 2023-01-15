#' @rdname git
#' @title Git commands
#' @description Run git commands from the console or programmatically, activating and bringing a terminal to the front, executing the command, and returning focus to the console.
#' @param message Character scalar git commit message.
#' @param add.all Non-`NA` logical scalar indicating whether to call `git add --all` before calling `git commit -am "{message}"`.
#' @param console Non-`NA` logical scalar indicating whether to bring the console to the front upon conclusion.
#' @return `NULL`
#' @export
git_push <- function(message, add.all = T, console = T) {
  errs <- base::c(uj::f0(uj::cmp_str_scl(message), NULL, "[message] must be a complete string scalar (?cmp_str_scl)."),
                  uj::f0(uj::cmp_lgl_scl(add.all), NULL, "[add.all] must be scalar TRUE or scalar FALSE."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  rstudioapi::terminalActivate()
  wd <- base::getwd()
  if (add.all) {rstudioapi::terminalExecute("git add --all", workingDir = wd)}
  rstudioapi::terminalExecute(base::paste0("git commit -am \"", message, "\""), workingDir = wd)
  rstudioapi::terminalExecute("git push", workingDir = wd)
  if (console) {base::cat("")}
}
