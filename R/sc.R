#' @name sc.
#' @family wraps
#' @title Wraps of functions from package \code{sca;es}.
#' @description The following table describes wrapper functions for the
#'   \code{scalse} package:\tabular{ll}{
#'   WRAPPER NAME   \tab READR FUNCTION                                      \cr
#'   \code{sca}     \tab \code{\link[scales]{alpha}}                           }
#' @export
sc. <- function() {help("sc.", package = "uj")}

#' @describeIn sc. Thin wrap of \code{\link[scales]{alpha}}.
#' @inherit scales::alpha
#' @export
sca <- function(color, alpha = NA) {scales::alpha(color, alpha)}
