#' @encoding UTF-8
#' @family strings
#' @title Fixed-value string substitution
#' @description Finds strings from `pats` in `x` and replaces them with corresponding values of (possibly recycled) `subs`.
#' @param x An \link[=atm_chr]{atomic character object}.
#' @param pats A \link[=cmp_chr_vec]{complete character vec} of patterns to replace.
#' @param subs A complete character vec of replacement strings
#' @return Atomic character object.
#' @examples
#' egArg1 <- c("a", "few", "words", "in", "a string")
#' agArg2 <- paste0(egArg1, collapse = " ")
#' egPats <- c("a", "f", "w", "i", "s", " ")
#' egSubs <- c("A", "F", "W", "I", "S", "_")
#' egArg1
#' egArg2
#' fsub(egArg1, egPats, egSubs)
#' fsub(egArg2, egPats, egSubs)
#' fsub(egArg1, egPats, "_")
#' fsub(egArg2, egPats, "_")
#' @export
fsub <- function(x, pats, subs) {
  errs <- NULL
  if (!ppp::.cmp_chr_gen(x)) {errs <- base::c(errs, "[x] must be a complete character generic (?cmp_chr_gen).")}
  if (!ppp::.cmp_chr_vec(pats)) {errs <- base::c(errs, "[pats] must be a complete character vec (?cmp_chr_vec).")}
  if (!ppp::.cmp_chr_vec(subs)) {errs <- base::c(errs, "[subs] must be a complete character vec (?cmp_chr_vec).")}
  if (!uj::recyclable(pats, subs)) {errs <- base::c(errs, "[pats] and [subs] are not recyclable.")}
  if (base::length(subs) > base::length(pats)) {errs <- base::c(errs, "There are more substitute strings than patterns to replace: length(subs) > length(pats).")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "uj")}
  uj::recycle(pats = pats, subs = subs)
  for (i in 1:base::length(pats)) {x <- av(base::gsub(pats[i], subs[i], x, fixed = T))}
  x
}
