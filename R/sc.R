#' @name sc.
#' @family wraps
#' @title Wraps of functions from package \code{sca;es}.
#' @export
sc. <- function() {help("sc.", package = "uj")}

#' @describeIn sc. Thin wrap of \code{\link[scales]{alpha}}.
#' @inherit scales::alpha
#' @export
sca <- function(color, alpha = NA) {scales::alpha(color, alpha)}
