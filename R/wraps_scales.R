#' @name wraps_scales
#' @family wraps
#' @title Thin wrappers of `scales` functions
#' @description \itemize{
#'   \item **`sca`**: thinly wraps \code{\link[scales]{alpha}}.
#' }
#' @inherit scales::alpha
#' @export
sca <- function(color, alpha = NA) {scales::alpha(color, alpha)}
