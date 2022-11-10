#' @name pause.
#' @family extensions
#' @title Pause execution.
#' @param x \link[cmp_num_scl]{Complete mumeric scalar} indicating the number of
#'   seconds to pause.
#' @return \code{NULL} (called for side effect).
#' @export
pause. <- function() {help("pause.", package = "uj")}

#' @describeIn pause. Pause for \code{x} seconds, then resume execution.
#' @export
pause <- function(x = 0.0000001) {Sys.sleep(x)}
