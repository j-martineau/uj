#' @encoding UTF-8
#' @family strings
#' @title Fixed-value string substitution
#' @description Finds strings from `Pats` in `X` and replaces them with corresponding values of (possibly recycled) `Subs`.
#' @param X An \link[=atm_chr]{atomic character object}.
#' @param Pats A \link[=cmp_chr_vec]{complete character vec} of patterns to replace.
#' @param Subs A complete character vec of replacement strings
#' @return Atomic character object.
#' @examples
#' egArg1 <- c("a", "few", "words", "in", "a string")
#' agArg2 <- paste0(egArg1, collapse = " ")
#' egPats <- c("a", "f", "w", "i", "s", " ")
#' egSubs <- c("A", "F", "W", "I", "S", "_")
#' egArg1
#' egArg2
#' fsub(egArg1, Pats, Subs)
#' fsub(egArg2, Pats, Subs)
#' fsub(egArg1, Pats, "_")
#' fsub(egArg2, Pats, "_")
#' @export
fsub <- function(X, Pats, Subs) {
  Errors <- NULL
  if (!uj:::.cmp_chr_gen(X)) {Errors <- base::c(Errors, "[X] must be a complete character generic (?cmp_chr_gen).")}
  if (!uj:::.cmp_chr_vec(Pats)) {Errors <- base::c(Errors, "[Pats] must be a complete character vec (?cmp_chr_vec).")}
  if (!uj:::.cmp_chr_vec(Subs)) {Errors <- base::c(Errors, "[Subs] must be a complete character vec (?cmp_chr_vec).")}
  if (!uj::recyclable(Pats, Subs)) {Errors <- base::c(Errors, "[Pats] and [Subs] are not recyclable.")}
  if (base::length(Subs) > base::length(Pats)) {Errors <- base::c(Errors, "There are more substitute strings than patterns to replace: length(Subs) > length(Pats).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  uj::recycle(Pats = Pats, Subs = Subs)
  for (i in 1:base::length(Pats)) {X <- av(base::gsub(Pats[i], Subs[i], X, fixed = T))}
  X
}
