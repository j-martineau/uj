#' @encoding UTF-8
#' @family colors
#' @family plots
#' @family wraps
#' @title Thin wrappers of `scales` functions
#' @description \tabular{ll}{  `sca`   \tab Thinly wraps \code{\link[scales]{alpha}}}
#' \cr\cr See the link above for complete function documentation.
#' @inherit scales::alpha
#' @examples
#' sca("red", 0.5)
#' sca("#FF00FF", 0.1)
#' @export
sca <- function(color, alpha = NA) {scales::alpha(color, alpha)}
