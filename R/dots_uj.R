#' @name dots_uj
#' @encoding UTF-8
#' @family meta
#' @family dots
#' @family args
#' @title Manage `...` arguments
#' @description \tabular{rl}{
#'     `unnamed_dots`   \tab Extracts unnamed `...` arguments as an unnamed list (includes any named with blank strings).
#'   \cr                \tab  
#'   \cr `named_dots`   \tab Extracts named `...` arguments as a named list (does not include with blank-string names).
#'   \cr                \tab  
#'   \cr       `dots`   \tab Extracts one or more`...` arguments based on matching values supplied in `names.`. If a supplied name matches the name of a `...` argument, that argument is returned. Otherwise, the element of `defs.` with a matching name is returned. `names. = NULL` and `names. = NA` are converted to `'NULL'` and `'NA'`. Reserved words in `names.` should be backtick quoted.
#'   \cr                \tab  
#'   \cr        `dot`   \tab A convenience version of `dots` for extracting a single named `...` argument (or if a matching `...` argument name is not found, its default value `def.`).
#'   \cr                \tab  
#'   \cr  `dot_names`   \tab If `names. = NULL`, the return value is `...names()`, otherwise, `names.` is returned. Throws an error in the following cases:
#'                           \itemize{
#'                             \item `0 ...` Arguments are supplied.
#'                             \item `names. != NULL` and `length(names.) != ...length()`.
#'                             \item `req. = TRUE`, `names. = NULL`, and no `...` arguments are named.
#'                             \item `names. = NULL` and either (a) `blank. = TRUE` and any `...` argument is unnamed or its name is blank or (b) \item `u. = TRUE` and any `...` argument name is duplicated.
#' }}
#' @param ... An arbitrary number of arguments.
#' @param names. `NULL` or an \link[=atm_vec]{atomic vec} (may include `NA` values). Is split along the delimiter `'|'` to allow for compactness in submitting multiple names. `NULL` will match either an argument in `...` or element of `defs.` with the name `'NULL'`. `NA` values will match an argument in `...` or an element of `defs.` with the name `'NA'`.
#' @param name. `NULL` or \link[=cmp_scl]{complete atomic scalar}. `NULL` is replaced with `'NULL'` and `NA` is replaced with `'NA'`.
#' @param subs. `NULL` or \link[=cmp_chr_vec]{complete character vec}. If not `NULL`, it is split using pipes `'|'` as a delimiter. If there are no pipes contained in `names.`, it remains unchanged. When this argument is not `NULL`, it is substituted for the names of `...` arguments; thus, after splitting, its length must equal the number of `...` arguments. For example, `names. = c('one', 'two', 'three|four|five')` indicates that there should be five `...` arguments and the vector `c('one', 'two', 'three', 'four', 'five')` is substituted for their names.
#' @param defs. A named \link[=ivls]{vlist} of default objects/values to return if the specified arguments are not in `...`. Elements of `defs.` must be uniquely named. If `defs.` is a tibble, columns with matching names are returned.
#' @param def. A default object/value to return if a specified argument is not in `...`. Can be, but does not need to be, a list.
#' @param req.,blank.,u. Non-`NA` logical scalars indicating whether names are required, whether lank names are allowed, and whether names must be unique.
#' @export
dots <- function(names., defs., ...) {
  dots  <- list(...)
  ok.names <- atm_vec(names.)
  ok.n <- length(dots) > 0
  errs <- c(f0(ok.names, NULL, "[names.] must be a complete atomic vec (?cmp_vec)."),
            f0(ok.n    , NULL, "[...] must contain at least one argument."))
  if (!is.null(errs)) {stop(errs)}
  dot.names <- names(dots);
  def.names <- names(defs.)                                                      # names of args in {...} and elements of {defs.}
  names. <- ss("|", as.character(av(names.)))                                    # atomize {names.}, convert to character, and split along {'|'}
  names.[is.na(names.)] <- 'NA'                                                  # change {NA}s to a {'NA'}
  in.dots <- names. %in% dot.names                                               # whether each value of {names.} is in the names of args in {...}
  in.defs <- names. %in% def.names                                               # whether each value of {names.} is in the names of {defs.}
  match  <- all(in.dots | in.defs)                                               # validity check (does every value of {names.} have a match?)
  if (!match) {stop(.errs("Values in [names.] must match elements of [...] or of [defs.]."))}
  n.names <- length(names.)                                                      # number of arguments to match
  out <- rep.int(list(NULL), n.names)                                            # initialize the results as a list of {NULL` elements
  for (i in 1:n.names) {                                                         # for each element of {names.}
    name.i <- names.[[i]]                                                        # : get the name to be matched
    if (in.dots[i]) {out[[i]] <- dots[[ which(dot.names == name.i)]]}            # : if it matches an argument in {...}, store that argument
    else            {out[[i]] <- defs.[[which(def.names == name.i)]]}            # ; otherwise, store the matching element of {defs.}
  }
  if (n.names == 1) {out <- out[[1]]}                                            # if only 1 argument was extracted, un-nest it from the results list
  out
}

#' @rdname dots_uj
#' @export
dot <- function(name., def., ...) {
  if (!cmp_scl(name.)) {stop("[name.] must be a complete atomic scalar (?cmp_scl).")}
  dots(name., def., ...)
}

#' @rdname dots_uj
#' @export
dot_names <- function(..., subs. = NULL, req. = T, blank. = F, u. = T) {
  dots <- list(...)
  errs <- c(f0(length(dots) > 0               , NULL, "[...] is empty."),
            f0(f0(inll(subs.), T, ivec(subs.)), NULL, "[subs.] must be NULL or an atomic vector with one value per argument in [...]."),
            f0(isTF(req.)                     , NULL, "[req.] must be TRUE or FALSE."),
            f0(isTF(blank.)                   , NULL, "[blank.] must be TRUE or FALSE."),
            f0(isTF(u.)                       , NULL, "[u.] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(.errs(errs))}
  n.dots <- ...length()
  n.names <- length(...names())
  if (inll(subs.)) {subs. <- ...names()}
  errs <- c(f0(n.names == n.dots | inll(subs.), NULL, "When [subs.] is not NULL, it must contain one value per argument in [...]."),
            f0(n.names == n.dots | !req.      , NULL, "When [req. = TRUE], arguments in [...] must be named or [subs.] must contain one value per argument in [...]."))
  if (!is.null(errs)) {stop(.errs(errs))}
  subs. <- av(strsplit(as.character(av(subs.)), "|", fixed = TRUE))
  subs.[is.na(subs.)] <- 'NA'
  errs <- c(f0(!blank. | notIN("", subs.), NULL, "A name is blank but [blank. = FALSE]."),
            f0(!u.     | is_unq(subs.)   , NULL, "Names provided are not unique but [u. = TRUE]."))
  if (!is.null(errs)) {stop(.errs(errs))}
  subs.
}

#' @rdname dots_uj
#' @export
named_dots <- function(...) {
  if (...length() == 0) {return(NULL)}
  dots <- list(...)
  dot.names <- ...names()
  ok.names <- !is.na(dot.names)
  if (any(ok.names)) {dots[ok.names]} else {NULL}
}

#' @rdname dots_uj
#' @export
unnamed_dots <- function(...) {
  if (...length() == 0) {return(NULL)}
  dots <- list(...)
  dot.names <- ...names()
  na.names <- is.na(dot.names)
  if (any(na.names)) {dots[na.names]} else {NULL}
}
