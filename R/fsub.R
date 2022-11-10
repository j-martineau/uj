#' @name fsub.
#' @title Fixed-value string substitution
#' @family strings
#' @family extensions
#' @param x \link[ichr]{Atomic character object}.
#' @param pats,subs \link[cmp_chr_vec]{Complete character vecs} containing
#'   patterns to replace and their associated replacement strings.
#' @return Atomic character object.
#' @export
fsub. <- function() {help("fsub.", package = "uj")}

#' @describeIn fsub. Replace strings in \code{pats} with corresponding
#'   (possibly recycled) strings in \code{subs}.
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
