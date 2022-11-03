#' @title Set object class
#' @param x Any object.
#' @param cls Character vector of classes.
#' @return An object of class \code{c}.
#' @export
reclass <- function(x, cls) {class(x) <- cls; x}
