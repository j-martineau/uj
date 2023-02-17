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
#' fsub(egArg1, pats, subs)
#' fsub(egArg2, pats, subs)
#' fsub(egArg1, pats, "_")
#' fsub(egArg2, pats, "_")
#' @export
fsub <- function(x, pats, subs) {
  uj::errs_if_nots(uj::cmp_chr_gen(x)        , "[x] must be a complete character generic (?cmp_chr_gen)."                                ,
                   uj::cmp_chr_vec(pats)     , "[pats] must be a complete character vec (?cmp_chr_vec)."                                 ,
                   uj::cmp_chr_vec(subs)     , "[subs] must be a complete character vec (?cmp_chr_vec)."                                 ,
                   uj::recyclable(pats, subs), "[pats] and [subs] are not recyclable."                                                   ,
                   uj::N(subs) <= uj::N(pats), "There are more substitute strings than patterns to replace: length(subs) > length(pats).", PKG = "uj")
  uj::recycle(pats = pats, subs = subs)
  for (i in 1:uj::N(pats)) {x <- av(base::gsub(pats[i], subs[i], x, fixed = T))}
  x
}
