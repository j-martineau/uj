#' @name wraps_scales
#' @family wraps
#' @title Wraps of Functions from the \code{scales} Package
#' @description \tabular{ll}{
#'   WRAPPER NAME   \tab SCALES FUNCTION                                     \cr
#'   \code{sca}     \tab \code{\link[scales]{alpha}}.                          }
#' @export
#' @inherit scales::alpha
#' @export
sca <- function(color, alpha = NA) {scales::alpha(color, alpha)}
