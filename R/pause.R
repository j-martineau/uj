#' @encoding UTF-8
#' @title Pause execution.
#' @family meta
#' @description Pause for `X` seconds, then resume execution.
#' @param X A \link[=cmp_nng_scl]{complete non-netative numeric scalar} indicating the number of seconds to pause.
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
pause <- function(X = 0.0000001) {
  if (!uj:::.cmp_nng_scl(X)) {uj::stopperr("[X] must be a non-NA, non-negative, numeric scalar.", PKG = "uj")}
  base::Sys.sleep(X)
}
