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
  err <- f0(cmp_chr_gen(x), NULL, " • [x] must be a complete character generic (?cmp_chr_gen).")
  if (!cmp_chr_vec(pats)) {err <- c(err, " • [pats] must be a complete character vec (?cmp_chr_vec).")}
  if (!cmp_chr_vec(subs)) {err <- c(err, " • [subs] must be a complete character vec (?cmp_chr_vec).")}
  if (!recyclable_n(c(length(pats), length(subs)))) {err <- c(err, "[pats] and [subs] are not recyclable.")}
  if (idef(err)) {stop(err)}
  recycle(pats = pats, subs = subs)
  for (i in 1:length(pats)) {x <- av(gsub(pats[i], subs[i], x, fixed = T))}
  x
}
