#' @name reclass.
#' @title Set object class
#' @family extensions
#' @param x Any object.
#' @param cls Character vector of classes.
#' @return An object of class \code{c}.
#' @export
reclass. <- function() {help("reclass.", package = "uj")}

#' @describeIn reclass. Sets class(es) of \code{x} to \code{cls}.
#' @export
reclass <- function(x, cls) {
  err <- NULL
  if (is.null(x)) {err <- c(err, "\n • [x] is NULL.")}
  if (!cmp_chr_vec(cls)) {err <- c(err, "\n • [cls] must be a complete character vec (?cmp_chr_vec).")}
  if (idef(err)) {stop(paste0(err, collapse = ""))}
  class(x) <- cls
  x
}
