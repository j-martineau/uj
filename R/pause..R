#' @title Pause execution.
#' @description Pause for `x` seconds, then resume execution.
#' @param x A \link[=cmp_num_scl]{complete mumeric scalar} indicating the number of seconds to pause.
#' @return `NULL` (called for its side effect).
#' @examples
#' cat("\n pause(0.01)")
#' pause(0.01)
#' cat("\n pause(0.1)")
#' pause(0.1)
#' cat("\n pause(0.2)")
#' pause(0.2)
#' cat("\n pause(0.5)")
#' pause(0.5)
#' cat("\n pause(1)")
#' pause(1)
#' cat("\n done")
#' @export
pause <- function(x = 0.0000001) {Sys.sleep(x)}
