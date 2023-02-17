#' @encoding UTF-8
#' @title Pause execution.
#' @family meta
#' @description Pause for `x` seconds, then resume execution.
#' @param x A \link[=cmp_nng_scl]{complete non-netative numeric scalar} indicating the number of seconds to pause.
#' @return `NULL` (called for its side effect).
#' @examples
#' \dontrun{
#'   cat("\n pause(0.01)")
#'   pause(0.01)
#'   cat("\n pause(0.1)")
#'   pause(0.1)
#'   cat("\n pause(0.2)")
#'   pause(0.2)
#'   cat("\n pause(0.5)")
#'   pause(0.5)
#'   cat("\n pause(1)")
#'   pause(1)
#'   cat("\n done")
#' }
#' @export
pause <- function(x = 0.0000001) {
  uj::err_if_not(uj::cmp_nng_scl(x), "[x] must be a non-NA, non-negative, numeric scalar.", PKG = "uj")
  base::Sys.sleep(x)
}
