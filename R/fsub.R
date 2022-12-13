#' @family strings
#' @title Fixed-value string substitution
#' @description Finds strings from `pats` in `x` and replaces them with corresponding values of (possibly recycled) `subs`.
#' @param x An \link[=atm_chr]{atomic character object}.
#' @param pats,subs \link[=cmp_chr_vec]{Complete character vecs} containing patterns to replace and their associated replacement strings.
#' @return Atomic character object.
#' @examples
#' arg1. <- c("a", "few", "words", "in", "a string")
#' arg2. <- paste0(arg1., collapse = " ")
#' pats. <- c("a", "f", "w", "i", "s", " ")
#' subs. <- c("A", "F", "W", "I", "S", "_")
#'
#' arg1.
#' arg2.
#'
#' fsub(arg1., pats., subs.)
#' fsub(arg1., pats., "_")
#' fsub(arg2., pats., subs.)
#' fsub(arg2., pats., "_")
#' @export
fsub <- function(x, pats, subs) {
  errs <- c(f0(cmp_chr_gen(x)              , NULL, "[x] must be a complete character generic (?cmp_chr_gen)."),
            f0(cmp_chr_vec(pats)           , NULL, "[pats] must be a complete character vec (?cmp_chr_vec)."),
            f0(cmp_chr_vec(subs)           , NULL, "[subs] must be a complete character vec (?cmp_chr_vec)."),
            f0(recyclable(pats, subs)      , NULL, "[pats] and [subs] are not recyclable."),
            f0(length(subs) <= length(pats), NULL, "There are more substitute strings than patterns to replace: length(subs) > length(pats)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  recycle(pats = pats, subs = subs)
  for (i in 1:length(pats)) {x <- av(gsub(pats[i], subs[i], x, fixed = T))}
  x
}
