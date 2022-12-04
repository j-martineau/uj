#' @family wraps
#' @family plotting
#' @title Thin wrappers of `scales` functions
#' @description \tabular{rl}{
#'   `sca`   \tab Thinly wraps \code{\link[scales]{alpha}}.
#' }
#' @inherit scales::alpha
#' @export
sca <- function(color, alpha = NA) {scales::alpha(color, alpha)}
