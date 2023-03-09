#' @encoding UTF-8
#' @family strings
#' @family chars
#' @title Get the `n`-th character element-wise.
#' @param x A \link[=pop_chr]{populated character object}.
#' @param n A \link[=cmp_psw_scl]{positive whole-number scalar} or a \link[=cmp_psw]{complete positive whole-number object} of the same dimensions as `x`.
#' @return A \link[=cmp_ch1]{complete onechar} atomic object of the same dimension as `x`.
#' @examples
#' words <- stringr::fruit[1:10]
#' words
#' chn(words, 1)
#' chn(words, 3)
#' charN(words, 10)
#' @export
chn <- function(x, n) {
  ok.x <- uj:::.pop_chr(x)
  if (uj:::.cmp_psw_scl(n)) {ok.n <- TRUE} else {ok.n <- base::length(n) == base::length(x)}
  errs <- NULL
  if (!ok.n) {errs <- base::c(errs, "[n] must be a complete positive whole-number valued scalar (?cmp_psw_scl) or of the same dimension as [x].")}
  if (!ok.x) {errs <- base::c(errs, "[x] must be an populated atomic object of mode 'character' (?pop_chr).")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  base::substr(x, n, n)
}

#' @rdname chn
#' @export
charn <- chn
