#' @name pause_uj
#' @title Pause execution.
#' @param x Numeric scalar number of seconds to pause.
#' @return \code{NULL} (called for side effect).
#' @export
pause_uj <- function() {help("pause_uj", package = "uj")}

#' @describeIn pause_uj Pause for \code{x} seconds, then resume execution.
#' @export
pause <- function(x = 0.0000001) {Sys.sleep(x)}
