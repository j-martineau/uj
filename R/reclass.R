#' @encoding UTF-8
#' @family properties
#' @title Set object class
#' @param x An R object.
#' @param cls A \link[=cmp_chr_vec]{complete character vec} of classes to apply.
#' @return An object of class `cls`.
#' @examples
#' obj1. <- tb(letters = letters, numbers = 1:26)
#' obj2. <- reclass(obj1., c("new", class(obj1.)))
#' obj3. <- reclass(obj1., "new")
#' class(obj1.)
#' class(obj2.)
#' class(obj3.)
#' @export
reclass <- function(x, cls) {
  errs <- c(f0(idef(x)         , NULL, "[x] is NULL."),
            f0(cmp_chr_vec(cls), NULL, "[cls] must be a complete character vec (?cmp_chr_vec)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  class(x) <- cls
  x
}
