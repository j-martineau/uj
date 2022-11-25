#' @title Extract a single character
#' @description Get the `n`-th character of each element of `x`.
#' @family strings
#' @param x A \link[=ichr]{character object}.
#' @param n A \link[=cmp_psw_scl]{complete positive whole-number scalar} or
#'   \link[=cmp_psw]{complete positive whole-number object} of the same
#'   dimensions as `x`.
#' @return A link[=cmp_ch1]{complete onechar} atomic object of the same
#'   dimension as `x`.
#' @export
charn <- function(x, n) {
  errs <- c(f0(ichr(x)                                 , NULL, "\n \u2022 [x] must be an atomic object of mode 'character' (?ichr)."),
            f0(cmp_psw(n) | (iscl(n) & identical(x, n)), NULL, "\n \u2022 [n] must be a complete positive whole-number valued scalar (?cmp_psw_scl) or of the same dimension as [x]."))
  if (!is.null(errs)) {stop(errs)}
  substr(x, n, n)
}
