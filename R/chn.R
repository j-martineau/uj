#' @encoding UTF-8
#' @family strings
#' @family chars
#' @title Get the `N`-th character element-wise.
#' @param X A \link[=pop_chr]{populated character object}.
#' @param N A \link[=cmp_psw_scl]{positive whole-number scalar} or a \link[=cmp_psw]{complete positive whole-number object} of the same dimensions as `X`.
#' @return A \link[=cmp_ch1]{complete onechar} atomic object of the same dimension as `X`.
#' @examples
#' words <- stringr::fruit[1:10]
#' words
#' chn(words, 1)
#' chn(words, 3)
#' charN(words, 10)
#' @export
chn <- function(X, N) {
  OkX <- uj:::.pop_chr(X)
  if (uj:::.cmp_psw_scl(N)) {OkN <- TRUE} else {OkN <- base::length(N) == base::length(X)}
  Errors <- NULL
  if (!OkN) {Errors <- base::c(Errors, "[N] must be a complete positive whole-number valued scalar (?cmp_psw_scl) or of the same dimension as [X].")}
  if (!OkX) {Errors <- base::c(Errors, "[X] must be an populated atomic object of mode 'character' (?pop_chr).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  base::substr(X, N, N)
}

#' @rdname chn
#' @export
charn <- chn
