#' @name wrap_scales
#' @family wraps
#' @title Wraps of Functions from the \code{scales} Package
#' @description \tabular{ll}{
#'   \code{sca}   \tab Thin wrap of \code{\link[scales]{alpha}}.               }
#' @inherit scales::alpha
#' @export
wrap_scales <- NULL

#' @rdname wrap_scales
#' @export
sca <- function(color, alpha = NA) {scales::alpha(color, alpha)}
