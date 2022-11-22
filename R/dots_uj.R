#' @name dots_uj
#' @family extensions
#' @title Manage dot arguments
#' @description Get named arguments from \code{...} with default values if
#'   missing, named and unnamed arguments from \code{...}, and names of
#'   \code{...} arguments.
#' @param ... An arbitrary number of arguments.
#' @param names. \code{NULL} or \link[=cmp_vec]{complete atomic vec} (may
#'   include \code{NA}) values). Is split along the delimiter \code{'|'} to
#'   allow for compactness in submitting multiple names. \code{NULL} will match
#'   an argument in \code{...} or element of \code{defs.} with the name
#'   \code{'NULL'}. \code{NA} values will match an argument in \code{...} or an
#'   element of \code{defs.} with the name \code{'NA'}.
#' @param name. \code{NULL} or \link[=cmp_scl]{complete atomic scalar}.
#'   \code{NULL} is replaced with \code{'NULL'} and \code{NA} is replaced with
#'   \code{'NA'}.
#' @param subs. \code{NULL} or \link[=cmp_chr_vec]{complete character vec}. If
#'   not \code{NULL}, it is split using pipes \code{'|'} as a delimiter. If
#'   there are no pipes contained in \code{names.}, it remains unchanged. When
#'   this argument is not \code{NULL}, it is substituted for the names of
#'   \code{...} arguments; thus, after splitting, its length must equal the
#'   number of \code{...} arguments. For example, the value \code{names. =
#'   c("one", "two", "three|four|five")} indicates that there should be five
#'   \code{...} arguments and the vector \code{c('one', 'two', 'three',
#'   'four', 'five')} is substituted for their names.
#' @param defs. Named \link[=ivls]{vlist} of default objects/values to return if
#'   the specified arguments are not in \code{...}. Elements of \code{defs.}
#'   must be uniquely named. If \code{defs.} is a tibble, columns with matching
#'   names are returned.
#' @param def. Default object/value to return if a specified argument is not in
#'   \code{...}. Can be, but does not need to be, a list.
#' @param req.,blank.,u. \link[=cmp_lgl_scl]{Complete logical scalars}
#'   indicating whether names are required, whether lank names are allowed, and
#'   whether names must be unique.
#' @export
dots_uj <- NULL

#' @describeIn dots_uj Extract one or more arguments from those in \code{...}
#'   based on their names matching values supplied in \code{names.}. If a
#'   supplied name matches the name of an \code{...} arguments, that argument
#'   is returned. Otherwise, the element of \code{defs.} with a matching name is
#'   returned.
#'   \cr\cr
#'   \code{names.} must be an atomic scalar/vector, and is coerced to character
#'   mode. The exceptions are \code{NULL} and \code{NA}, which are converted to
#'   \code{'NULL'} and \code{'NA'}. Any values of \code{names.} that are
#'   reserved words or non-valid object names are backtick quoted for matching.
#'   \cr\cr
#'   \code{defs.} is a named list with default values to return if there is no
#'   matching argument for a value in \code{names.}.
#'   \cr\cr
#'   Each value in \code{names.} must have a matching argument in \code{...} or
#'   a matching element in \code{defs.}.
#' @export
dots <- function(names., defs., ...) {
  dots  <- list(...)
  ok.names <- atm_vec(names.)
  ok.n <- length(dots) > 0
  errs <- c(f0(ok.names, NULL, "\n \u2022 [names.] must be a complete atomic vec (?cmp_vec)."),
            f0(ok.n    , NULL, "\n \u2022 [...] must contain at least one argument."))
  if (!is.null(errs)) {stop(errs)}
  dot.names <- names(dots);
  def.names <- names(defs.)                                                      # names of args in {...} and elements of {defs.}
  names. <- ss("|", as.character(av(names.)))                                    # atomize {names.}, convert to character, and split along {'|'}
  names.[is.na(names.)] <- 'NA'                                                  # change {NA}s to a {'NA'}
  in.dots <- names. %in% dot.names                                               # whether each value of {names.} is in the names of args in {...}
  in.defs <- names. %in% def.names                                               # whether each value of {names.} is in the names of {defs.}
  match  <- all(in.dots | in.defs)                                               # validity check (does every value of {names.} have a match?)
  if (!match) {stop("\n \u2022 Values in [names.] must match elements of [...] or of [defs.].")} # error if validity check failed
  n.names <- length(names.)                                                      # number of arguments to match
  out <- rep.int(list(NULL), n.names)                                            # initialize the results as a list of {NULL} elements
  for (i in 1:n.names) {                                                         # for each element of {names.}
    name.i <- names.[[i]]                                                        # : get the name to be matched
    if (in.dots[i]) {out[[i]] <- dots[[ which(dot.names == name.i)]]}            # : if it matches an argument in {...}, store that argument
    else            {out[[i]] <- defs.[[which(def.names == name.i)]]}            # ; otherwise, store the matching element of {defs.}
  }
  if (n.names == 1) {out <- out[[1]]}                                            # if only 1 argument was extracted, un-nest it from the results list
  out
}

#' @describeIn dots_uj A simplified version for extracting a single named
#'   argument from \code{...} or, if a matching argument is not found, its
#'   default value.
#' @export
dot <- function(name., def., ...) {
  if (!cmp_scl(name.)) {stop("\n \u2022 [name.] must be a complete atomic scalar (?cmp_scl).")}
  dots(name., def., ...)
}

#' @describeIn dots_uj If \code{names.} is \code{NULL}, the return value is
#'   \code{...names()}, otherwise, \code{names.} i returned. Throws an error in
#'   the following cases:\itemize{
#'     \item \code{0} Arguments are supplied in \code{...}, \code{names.} is not
#'           \code{NULL} and its length is not equal to the number of arguments
#'           in \code{...}.
#'     \item \code{req.} is \code{TRUE}, \code{names.} is \code{NULL}, and none
#'           of the \code{...} arguments are named.
#'     \item \code{names.} is \code{NULL} and either (a) \code{blank.} is
#'           \code{TRUE} and any argument in \code{...} is either unnamed or its
#'           name is \code{''} or (b) \code{u.} is \code{TRUE} and multiple
#'           \code{...} arguments share a name.
#'     \item \code{names.} is not \code{NULL} and either (a) \code{blank.} is
#'           \code{TRUE} and \code{names.} contains \code{''} or (b) \code{u.}
#'           is \code{TRUE} and \code{names.} contains duplicate values.       }
#' @export
dot_names <- function(..., subs. = NULL, req. = T, blank. = F, u. = T) {
  dots <- list(...)
  errs <- c(f0(length(dots) > 0               , NULL, "\n \u2022 [...] is empty."),
            f0(f0(inll(subs.), T, ivec(subs.)), NULL, "\n \u2022 [subs.] must be NULL or an atomic vector with one value per argument in [...]."),
            f0(isTF(req.)                     , NULL, "\n \u2022 [req.] must be TRUE or FALSE."),
            f0(isTF(blank.)                   , NULL, "\n \u2022 [blank.] must be TRUE or FALSE."),
            f0(isTF(u.)                       , NULL, "\n \u2022 [u.] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(errs)}
  n.dots <- ...length()
  n.names <- length(...names())
  if (inll(subs.)) {subs. <- ...names()}
  errs <- c(f0(n.names == n.dots | inll(subs.), NULL, "\n \u2022 When [subs.] is not NULL, it must contain one value per argument in [...]."),
            f0(n.names == n.dots | !req.      , NULL, "\n \u2022 When [req. = TRUE], arguments in [...] must be named or [subs.] must contain one value per argument in [...]."))
  if (!is.null(errs)) {stop(errs)}
  subs. <- av(strsplit(as.character(av(subs.)), "|", fixed = TRUE))
  subs.[is.na(subs.)] <- 'NA'
  errs <- c(f0(!blank. | notIN("", subs.), NULL, "\n \u2022 A name is blank but [blank. = FALSE]."),
            f0(!u.     | is_unq(subs.)   , NULL, "\n \u2022 Names provided are not unique but [u. = TRUE]."))
  if (!is.null(errs)) {stop(errs)}
  subs.
}

#' @describeIn dots_uj Extracts named arguments from \code{...} as a named list
#'   (does not include arguments named with blank strings).
#' @export
named_dots <- function(...) {
  if (...length() == 0) {return(NULL)}
  dots <- list(...)
  dot.names <- ...names()
  ok.names <- !is.na(dot.names)
  if (any(ok.names)) {dots[ok.names]} else {NULL}
}

#' @describeIn dots_uj Extract unnamed arguments from \code{...} as an unnamed
#'   list (includes any arguments named with blank strings).
#' @export
unnamed_dots <- function(...) {
  if (...length() == 0) {return(NULL)}
  dots <- list(...)
  dot.names <- ...names()
  na.names <- is.na(dot.names)
  if (any(na.names)) {dots[na.names]} else {NULL}
}
