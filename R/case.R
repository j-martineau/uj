#' @encoding UTF-8
#' @family extensions
#' @family forks
#' @title Case as an extension of switch
#' @description Takes a name (i.e., any atomic scalar) and matches that name to the name matching an element of `...` where matching can be achieved by matching the name of an argument in `...`. When names are supplied via `NAMES`, they substituted for the names of arguments in `...`. When no match is found and `DEF = 'err'` an error is thrown, otherwise, the value of `DEF` is returned.
#' @param NAME `NULL` or an \link[=atm_scl]{atomic scalar} naming an argument in `...`. `NULL` is converted to `'NULL'` and `NA` is converted to `'NA'`. Coerced to mode character.
#' @param ... Any number of uniquely named arguments to select from to be the return value. Selection is based on which element's name equals the value of `NAME`. When there is no match, if `DEF = 'err'`, an error is thrown, otherwise `DEF` is returned as the default value.
#' @param NAMES Either `NULL` or a \link[=cmp_scl_vec]{complete character vec}. If this argument is of mode character, it is split using pipe delimiters. Its length (after potential splitting) must match the number of `...` arguments. Values of `NAMES` must be unique.
#' @param DEF The default value to return if no match is found, unless `DEF` is the character scalar `'err'`. In that case, if no match is found, an error is thrown.
#' @return Either the value of an argument selected from `...` or the value of `DEF`.
#' @examples
#' case("one", one = 1, two = letters, DEF = "default")
#' case("two", one = 1, two = letters, DEF = "default")
#' case("three", one = 1, two = letters, DEF = "default")
#' case("three", 1, letters, NAMES = c("three", "four"), DEF = "default")
#' @export
case <- function(NAME, ..., NAMES = NULL, DEF = "err") {
  ok.NAMES <- uj::f0(base::is.null(NAMES), T, uj::f0(!uj:::.atm_vec(NAMES), F, base::length(NAMES) == base::...length()))
  errs <- NULL
  if (!uj:::.cmp_scl(NAME)) {errs <- base::c(errs, "[NAME] must be a non-NA atomic scalar (?cmp_scl).")}
  if (base::...length() == 0) {errs <- base::c(errs, "[...] is empty.")}
  if (!ok.NAMES) {errs <- base::c(errs, "[NAMES] must be NULL or an atomic vector of length equal to ...length()")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  NAMES <- uj::dot_names(..., SUBS = NAMES, REQ = T, BL = F, U = T)
  i <- base::which(NAMES == NAME)
  if (uj:::.cmp_chr_scl(DEF, valid = 'err') & base::length(i) != 1) {uj::stopperr("[NAME] must match 1 argument in [...].", PKG = "uj")}
  uj::f0(base::length(i) == 1, base::...elt(i), DEF)
}
