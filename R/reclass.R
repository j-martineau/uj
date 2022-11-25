#' @name reclass
#' @title Set Object Class
#' @family extensions
#' @param x An R object.
#' @param cls \link[=cmp_chr_vec]{Complete character vec} of classes to apply.
#' @return An object of class `cls`.
#' @export
reclass <- function(x, cls) {
  errs <- c(f0(idef(x)         , NULL, "\n \u2022 [x] is NULL."),
            f0(cmp_chr_vec(cls), NULL, "\n \u2022 [cls] must be a complete character vec (?cmp_chr_vec)."))
  if (!is.null(errs)) {stop(errs)}
  class(x) <- cls
  x
}
