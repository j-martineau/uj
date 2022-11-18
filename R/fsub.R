#' @name fsub.
#' @title Fixed-value string substitution
#' @description Replace strings in \code{pats} with corresponding (possibly
#'   recycled) strings in \code{subs}.
#' @family strings
#' @family extensions
#' @param x \link[=atm_chr]{Atomic character object}.
#' @param pats,subs \link[=cmp_chr_vec]{Complete character vecs} containing
#'   patterns to replace and their associated replacement strings.
#' @return Atomic character object.
#' @export
fsub. <- function() {help("fsub.", package = "uj")}

#' @rdname fsub.
#' @export
fsub <- function(x, pats, subs) {
  errs <- c(f0(cmp_chr_gen(x)                             , NULL, " \u2022 [x] must be a complete character generic (?cmp_chr_gen)."),
            f0(cmp_chr_vec(pats)                          , NULL, " \u2022 [pats] must be a complete character vec (?cmp_chr_vec)."),
            f0(cmp_chr_vec(subs)                          , NULL, " \u2022 [subs] must be a complete character vec (?cmp_chr_vec)."),
            f0(recyclable_n(c(length(pats), length(subs))), NULL, " \u2022 [pats] and [subs] are not recyclable."))
  if (idef(errs)) {stop(errs)}
  recycle(pats = pats, subs = subs)
  for (i in 1:length(pats)) {x <- av(gsub(pats[i], subs[i], x, fixed = T))}
  x
}
