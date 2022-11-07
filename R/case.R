#' @name case.
#' @family extensions
#' @family forks
#' @title Case as an extension of switch
#' @param name. \code{NULL} or an atomic scalar naming an argument in
#'   \code{...}. \code{NULL} is converted to \code{'NULL'} and \code{NA} is
#'   converted to \code{'NA'}. Coerced to mode character.
#' @param ... Any number of uniquely named arguments to select from to be the
#'   return value. Selection is based on which element's name equals the value
#'   of \code{name.}. When there is no match, if \code{def.} is \code{'err'}, an
#'   error is thrown, otherwise \code{def.} is returned as the default value.
#' @param names. Either \code{NULL} or a character scalar/vector. If this
#'   argument is of mode character, it is split using pipe (\code{'|'})
#'   delimiters. Its length (after potential splitting) must match the number of
#'   arguments in \code{...}. Values of \code{names.} must be unique.
#' @param def. The default value to return if no match is found, unless
#'   \code{def.} is the character scalar \code{'err'}. In that case, if no match
#'   is found, an error is thrown.
#' @return Either the value of an argument selected from \code{...} or the value
#'   of \code{def.}.
#' @export
case. <- function() {help("case.", package = "uj")}

#' @describeIn case. Takes a name (i.e., any atomic scalar) and matches that
#'   name to the name matching an element of \code{...} where matching can be
#'   achieved by matching the name of an argument in \code{...}. When names are
#'   supplied via \code{names.}, they substituted for the names of arguments in
#'   \code{...}. When no match is found and \code{def. = "err"} an error is
#'   thrown, otherwise, the value of \code{def.} is returned.
#' @export
case <- function(name., ..., names. = NULL, def. = "err") {
  vn1. <- iscl(name.)
  vxn. <- ...length() > 0
  vn2. <- f0(inll(names.), T, f0(!ivec(names.), F, length(names.) == ...length()))
  err. <- NULL
  if (!vn1.) {err. <- c(err., "\n • [name.] must be a non-NA atomic scalar (?cmp_scl).")}
  if (!vxn.) {err. <- c(err., "\n • [...] is empty.")}
  if (!vn2.) {err. <- c(err., "\n • [names.] must be NULL or an atomic vector of length equal to ...length()")}
  if (idef(err.)) {stop(err.)}
  names. <- dot_names(..., names. = names., req. = T, bl. = F, u. = T)           # get {names.} supplied for arguments in {...}
  i. <- which(names. == name.)                                                   # index any matches
  i1. <- length(i.) == 1                                                         # whether there is a match
  err. <- isEQ(def., 'err')
  if (err. & !i1.) {stop("\n • [name.] does not match any argument in [...].")}
  f0(i1., ...elt(i.), def.)                                                      # return the matching elements of {...}, if any, otherwise, return {DEF}
}
