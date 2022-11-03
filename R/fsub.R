#' @title Fixed-value string substitution
#' @param x Atomic character object.
#' @param pats Character vect containing string patterns to be replaced.
#' @param subs Character vect containing replacement strings.
#' @details Recycles \code{pats} and \code{subs}
#' @return Atomic character object.
#' @export
fsub <- function(x, pats, subs) {
  bank_funs(cmp_chr_gen, x = x)
  bank_funs(cmp_chr_vec, pats = pats, subs = subs)
  err_check()
  if (!recyclable_n(c(length(pats), length(subs)))) {
    bank_err("[pats] and [subs] are not recyclable.")
    err_check()
  }
  recycle(pats = pats, subs = subs)
  for (i in 1:length(pats)) {x <- av(gsub(pats[i], subs[i], x, fixed = T))}
  x
}
