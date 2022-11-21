#' @family extensions
#' @title Pause execution.
#' @description Pause for \code{x} seconds, then resume execution.
#' @param x A \link[=cmp_num_scl]{complete mumeric scalar} indicating the number
#'   of seconds to pause.
#' @return \code{NULL} (called for its side effect).
#' @export
pause <- function(x = 0.0000001) {Sys.sleep(x)}
