#' @title Pause for a specified number of seconds to give time for screen
#'   updates.
#' @param x Numeric scalar number of seconds to pause.
#' @return \code{NULL} (called for side effect)
#' @export
pause <- function(x = 0.0000001) {Sys.sleep(x)}
