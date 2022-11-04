#' @name dots_uj
#' @family meta
#' @title Manage dot arguments
#' @description Get named arguments from \code{...} with default values if
#'   missing, named and unnamed arguments from \code{...}, and names of
#'   arguments in \code{...}.
#' @param ... An arbitrary number of arguments.
#' @param names. \code{NULL} or an atomic scalar/vector (may include \code{NA})
#'   values). Is split along the delimiter \code{'|'} to allow for compactness
#'   in submitting multiple names. \code{NULL} will match an argument in
#'   \code{...} or element of \code{defs.} with the name \code{'NULL'}.
#'   \code{NA} values will match an argument in \code{...} or an element of
#'   \code{defs.} with the name \code{'NA'}.
#' @param name. \code{NULL} or an atomic scalar. \code{NULL} is replaced with
#'   \code{'NULL'} and \code{NA} is replaced with \code{'NA'}.
#' @param subs. Either \code{NULL} or a character scalar/vector. If it is a
#'   character scalar/vector, it is split using pipes \code{'|'} as a delimiter.
#'   If there are no pipes contained in \code{names.}, it remains unchanged.
#'   When this argument is not \code{NULL}, it is substituted for the names of
#'   arguments in \code{...}; thus, after splitting, its length must equal the
#'   number of arguments in \code{...}. For example, the value \code{names. =
#'   c("one", "two", "three|four|five")} indicates that there should be five
#'   arguments in \code{...} and the vector \code{c('one', 'two', 'three',
#'   'four', 'five')} is substituted for their names.
#' @param defs. List of default objects/values to return if the specified
#'   arguments are not in \code{...}. Elements of \code{defs.} must be uniquely
#'   named. If \code{defs.} is a tibble, columns with matching names are
#'   returned.
#' @param def. Default object/value to return if a specified argument is not in
#'   \code{...}. Can be, but does not need to be, a list.
#' @param req. \code{TRUE} or \code{FALSE} indicating whether names are
#'   required.
#' @param blank. \code{TRUE} or \code{FALSE} indicating whether blank names are
#'   allowed.
#' @param u. \code{TRUE} or \code{FALSE} indicating whether names must be
#'   unique.
#' @export
dots_uj <- function() {help("dots_uj", package = "uj")}

#' @describeIn dots_uj Extract one or more arguments from those in \code{...}
#'   based on their names matching values supplied in \code{names.}. If a
#'   supplied name matches the name of an arguments in \code{...}, that argument
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
  x.  <- list(...)
  vn. <- ivec(names.)
  vx. <- length(x.) > 0
  err.  <- NULL
  if (!vn.) {err. <- c(err., "\n • [names.] must be a non-NA atomic scalar or vector.")}
  if (!vx.) {err. <- c(err., "\n • [...] must contain at least one argument.")}
  if (idef(err.)) {stop(err.)}
  lx. <- names(x.); ld. <- names(defs.)                                          # names of args in {...} and elements of {defs.}
  names. <- ss(as.character(av(names.)))                                         # atomize {names.}, convert to character, and split along {'|'}
  names.[is.na(names.)] <- 'NA'                                                  # change {NA}s to a {'NA'}
  inx. <- names. %in% lx.                                                        # whether each value of {names.} is in the names of args in {...}
  ind. <- names. %in% ld.                                                        # whether each value of {names.} is in the names of {defs.}
  vn.  <- all(inx. | ind.)                                                       # validity check (does every value of {names.} have a match?)
  if (!vn) {stop("\n • Values in [names.] must match elements of [...] or of [defs.].")} # error if validity check failed
  n. <- length(names.)                                                           # number of arguments to match
  out. <- rep.int(list(NULL), n.)                                                # initialize the results as a list of {NULL} elements
  for (i. in 1:n.) {                                                             # for each element of {names.}
    name. <- names.[[i.]]                                                        # > get the name to be matched
    if (inx.[i.]) {out.[[i.]] <-    x.[[which(lx. == name.)]]}                   # > if it matches an argument in {...}, store that argument
    else          {out.[[i.]] <- defs.[[which(ld. == name.)]]}                   # > otherwise, store the matching element of {defs.}
  }
  if (n. == 1) {out. <- out.[[1]]}                                               # if only 1 argument was extracted, un-nest it from the results list
  out.
}

#' @describeIn dots_uj A simplified version for extracting a single named
#'   argument from \code{...} or, if a matching argument is not found, its
#'   default value.
#' @export
dot <- function(name., def., ...) {
  if (!cmp_scl(name.)) {stop("\n  * [name.] must be a non-NA atomic scalar.")}
  dots(name., def., ...)
}

#' @describeIn dots_uj If \code{names.} is \code{NULL}, the return value is
#'   \code{...names()}, otherwise, \code{names.} i returned. Throws an error in
#'   the following cases:\itemize{
#'     \item \code{0} Arguments are supplied in \code{...}, \code{names.} is not
#'           \code{NULL} and its length is not equal to the number of arguments
#'           in \code{...}.
#'     \item \code{req.} is \code{TRUE}, \code{names.} is \code{NULL}, and none
#'           of the arguments in \code{...} are named.
#'     \item \code{names.} is \code{NULL} and either (a) \code{blank.} is
#'           \code{TRUE} and any argument in \code{...} is either unnamed or its
#'           name is \code{''} or (b) \code{u.} is \code{TRUE} and multiple
#'           arguments in \code{...} share a name.
#'     \item \code{names.} is not \code{NULL} and either (a) \code{blank.} is
#'           \code{TRUE} and \code{names.} contains \code{''} or (b) \code{u.}
#'           is \code{TRUE} and \code{names.} contains duplicate values.       }
#' @export
dot_names <- function(..., subs. = NULL, req. = T, blank. = F, u. = T) {
  x.  <- list(...)
  vx. <- length(x.) > 0
  vn. <- f0(inll(subs.), T, ivec(subs.))
  vr. <- isTF(req.)
  vb. <- isTF(blank.)
  vu. <- isTF(u.)
  err. <- NULL
  if (!vx.) {err. <- c(err., "\n • [...] is empty.")}
  if (!vn.) {err. <- c(err., "\n • [subs.] must be NULL or an atomic vector with one value per argument in [...].")}
  if (!vr.) {err. <- c(err., "\n • [req.] must be TRUE or FALSE.")}
  if (!vb.) {err. <- c(err., "\n • [blank.] must be TRUE or FALSE.")}
  if (!vu.) {err. <- c(err., "\n • [u.] must be TRUE or FALSE.")}
  if (idef(err.)) {stop(err.)}
  nx. <- ...length()
  nl. <- length(...names())
  if (inll(subs.)) {subs. <- ...names()}                                         # if {subs.} is {NULL}, use the names of arguments in {...}
  v1. <- nl. == nx. | inll(subs.)
  v2. <- nl. == nx. | !req.
  err. <- NULL
  if (!v1.) {err. <- c(err., "\n • When [subs.] is not NULL, it must contain one value per argument in [...].")}
  if (!v2.) {err. <- c(err., "\n • When [req. = TRUE], arguments in [...] must be named or [subs.] must contain one value per argument in [...].")}
  if (idef(err.)) {stop(err.)}
  subs. <- av(strsplit(as.character(av(subs.)), "|", TRUE))                      # > atomize, convert to character, and split along {'|'}
  subs.[is.na(subs.)] <- 'NA'                                                    # > replace any {NA} values with {'NA'}
  v1. <- !blank. | notIN("", subs.)                                              # validity of the combination of {subs.} and {blank.}
  v2. <- !u.  | is_unique(subs.)                                                 # validity of the combination of {subs.} and {u.}
  err. <- NULL
  if (!v1.) {err. <- c(err., "\n • A name is blank but [blank. = FALSE].")}
  if (!v2.) {err. <- c(err., "\n • Names provided are not unique but [u. = TRUE].")}
  if (idef(err.)) {stop(err.)}
  subs.
}

#' @describeIn dots_uj Extracts named arguments from \code{...} as a named list
#'   (does not include arguments named with blank strings).
#' @export
named_dots <- function(...) {
  if (...length() == 0) {return(NULL)}
  x. <- list(...)
  n. <- ...names()
  i. <- !is.na(n.)
  if (any(i.)) {x.[i.]}
  else {NULL}
}

#' @describeIn dots_uj Extract unnamed arguments from \code{...} as an unnamed
#'   list (includes any arguments named with blank strings).
#' @export
unnamed_dots <- function(...) {
  if (...length() == 0) {return(NULL)}
  x. <- list(...)
  n. <- ...names()
  i. <- is.na(n.)
  if (any(i.)) {x.[i.]}
  else {NULL}
}
