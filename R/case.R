#' @encoding UTF-8
#' @family extensions
#' @family forks
#' @title Case as an extension of switch
#' @description Takes a name (i.e., any atomic scalar) and matches that name to the name matching an element of `...` where matching can be achieved by matching the name of an argument in `...`. When names are supplied via `names.`, they substituted for the names of arguments in `...`. When no match is found and `def. = 'err'` an error is thrown, otherwise, the value of `def.` is returned.
#' @param name. `NULL` or an \link[=atm_scl]{atomic scalar} naming an argument in `...`. `NULL` is converted to `'NULL'` and `NA` is converted to `'NA'`. Coerced to mode character.
#' @param ... Any number of uniquely named arguments to select from to be the return value. Selection is based on which element's name equals the value of `name.`. When there is no match, if `def. = 'err'`, an error is thrown, otherwise `def.` is returned as the default value.
#' @param names. Either `NULL` or a \link[=cmp_scl_vec]{complete character vec}. If this argument is of mode character, it is split using pipe delimiters. Its length (after potential splitting) must match the number of `...` arguments. Values of `names.` must be unique.
#' @param def. The default value to return if no match is found, unless `def.` is the character scalar `'err'`. In that case, if no match is found, an error is thrown.
#' @return Either the value of an argument selected from `...` or the value of `def.`.
#' @examples
#' case("one", one = 1, two = letters, def. = "default")
#' case("two", one = 1, two = letters, def. = "default")
#' case("three", one = 1, two = letters, def. = "default")
#' case("three", 1, letters, names. = c("three", "four"), def. = "default")
#' @export
case <- function(name., ..., names. = NULL, def. = "err") {
  ok.names <- uj::f0(uj::inll(names.), T, uj::f0(!uj::ivec(names.), F, base::length(names.) == base::...length()))
  errs <- base::c(uj::f0(uj::iscl(name.)      , NULL, "[name.] must be a non-NA atomic scalar (?cmp_scl)."),
                  uj::f0(base::...length() > 0, NULL, "[...] is empty."),
                  uj::f0(ok.names             , NULL, "[names.] must be NULL or an atomic vector of length equal to ...length()"))
  if (!base::is.null(errs)) {stop(.errs(errs))}
  names. <- uj::dot_names(..., subs. = names., req. = T, bl. = F, u. = T)        # get {names.} supplied for arguments in {...}
  i <- base::which(names. == name.)                                              # index any matches
  i1 <- base::length(i) == 1                                                     # whether there is a match
  err <- uj::isEQ(def., 'err')
  if (err & !i1) {stop(uj:::.errs("[name.] does not match any argument in [...]."))}
  uj::f0(i1, base::...elt(i), def.)                                              # return the matching elements of {...}, if any, otherwise, return {DEF}
}
