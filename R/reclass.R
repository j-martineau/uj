#' @name reclass.
#' @title Set object class
#' @family extensions
#' @param x Any object.
#' @param cls \link[=cmp_chr_vec]{Complete character vec} of classes to apply.
#' @return An object of class \code{cls}.
#' @export
reclass. <- function() {help("reclass.", package = "uj")}

#' @rdname reclass.
#' @export
reclass <- function(x, cls) {
  errs <- c(f0(idef(x)         , NULL, "\n \u2022 [x] is NULL."),
            f0(cmp_chr_vec(cls), NULL, "\n \u2022 [cls] must be a complete character vec (?cmp_chr_vec)."))
  if (idef(errs)) {stop(errs)}
  class(x) <- cls
  x
}
