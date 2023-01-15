#' @name dot_args
#' @encoding UTF-8
#' @family meta
#' @family dots
#' @family args
#' @title Manage `...` arguments
#' @description \tabular{rl}{
#'     `collapse_dots, glue_dots`   \tab Process `...` args by \link[=av]{atomizing} them and collapsing them to a character scalar.
#'   \cr                            \tab  
#' } \tabular {rl}{
#'     `unnamed_dots`   \tab Extracts unnamed `...` args as an unnamed list (includes any `...` args with blank-string names).
#'   \cr `named_dots`   \tab Extracts named `...` args as a named list (excludes any `...` args with blank-string names).
#'   \cr  `dot_names`   \tab If `names. = NULL`, the return value is `...names()`, otherwise, `names.` is returned. Throws an error in the following cases:
#'   \cr                \tab   +  `...length() == 0`
#'   \cr                \tab   +  `length(names.) > 0 & length(names.) < ...length()`
#'   \cr                \tab   +  `is.null(names.) & blank. & any(...names() == "")`
#'   \cr                \tab   +  `is.null(names.) & req. & !any(...names() != "")`
#'   \cr                \tab   +  `is.null(names.) & u. & any(duplicated(...names()))`
#'   \cr                \tab  
#' } \tabular{rl}{
#'          `str_dot`   \tab Get the value of the `n.`\eqn{^th} `...` arg if it evaluates to a complete character object, otherwise, return corresponding unquoted string from the calling argument.
#'   \cr   `str_dots`   \tab Calls `str_dot` for each `...` arg, returning either a character vector (when `glue. = FALSE`) or a character scalar (when `glue. = TRUE`)
#'   \cr                \tab  
#' } \tabular{rl}{
#'      `dots`   \tab Extracts one or more`...` args based on matching values supplied in `names.`. If a supplied name matches the name of a `...` arg, that arg is returned. Otherwise, the element of `defs.` with a matching name is returned. `names. = NULL` and `names. = NA` are converted to `'NULL'` and `'NA'`. Reserved words in `names.` should be backtick quoted.
#'   \cr `dot`   \tab A convenience version of `dots` for extracting a single named `...` arg (or if a matching `...` arg name is not found, substituting its default value `def.`).
#' }
#' @param ... An arbitrary number of arguments.
#' @param blank. A non-`NA` logical scalar indicating whether blank names are allowed.
#' @param blank A non-`NA` character scalar indicating what to return when `...` args resolve to a blank string (`""`).
#' @param names. `NULL` or an \link[=atm_vec]{atomic vec} (may include `NA` values). Is split along the delimiter `'|'` to allow for compactness in submitting multiple names. `NULL` will match either an argument in `...` or element of `defs.` with the name `'NULL'`. `NA` values will match an argument in `...` or an element of `defs.` with the name `'NA'`.
#' @param name. `NULL` or \link[=cmp_scl]{complete atomic scalar}. `NULL` is replaced with `'NULL'` and `NA` is replaced with `'NA'`.
#' @param subs. `NULL` or \link[=cmp_chr_vec]{complete character vec}. If not `NULL`, it is split using pipes `'|'` as a delimiter. If there are no pipes contained in `names.`, it remains unchanged. When this argument is not `NULL`, it is substituted for the names of `...` arguments; thus, after splitting, its length must equal the number of `...` arguments. For example, `names. = c('one', 'two', 'three|four|five')` indicates that there should be five `...` arguments and the vector `c('one', 'two', 'three', 'four', 'five')` is substituted for their names.
#' @param glue. A non-`NA` logical scalar indicating whether to glue the result (collapse into a character scalar)
#' @param defs. A named \link[=ivls]{vlist} of default objects/values to return if the specified arguments are not in `...`. Elements of `defs.` must be uniquely named. If `defs.` is a tibble, columns with matching names are returned.
#' @param def. A default object/value to return if a specified argument is not in `...`. For `str_dots` and `str_dot`, must be a \link[=cmp_chr_vec]{complete character vec}. For all others, may be, but does not need to be, a list.
#' @param req. A non-`NA` logical scalar indicating whether names are required.
#' @param sep. A \link[=cmp_chr_scl]{complete character scalar} for collapsing results to a character scalar.
#' @param n. A positive integer indicating which `...` arg to return.
#' @param u. A non-`NA` logical scalar indicating whether names must be unique.
#' @export
dots <- function(names., defs., ...) {
  dots <- base::list(...)
  ok.names <- uj::atm_vec(names.)
  okn. <- base::length(dots) > 0
  errs <- base::c(uj::f0(ok.names, NULL, "[names.] must be a complete atomic vec (?cmp_vec)."),
                  uj::f0(okn.    , NULL, "[...] must contain at least one argument."))
  if (!base::is.null(errs)) {stop(errs)}
  dot.names <- base::names(dots);
  def.names <- base::names(defs.)                                                # names of args in {...} and elements of {defs.}
  names. <- uj::ss("|", base::as.character(uj::av(names.)))                      # atomize {names.}, convert to character, and split along {'|'}
  names.[base::is.na(names.)] <- 'NA'                                            # change {NA}s to a {'NA'}
  in.dots <- names. %in% dot.names                                               # whether each value of {names.} is in the names of args in {...}
  in.defs <- names. %in% def.names                                               # whether each value of {names.} is in the names of {defs.}
  match <- base::all(in.dots | in.defs)                                          # validity check (does every value of {names.} have a match?)
  if (!match) {stop(uj::format_errs(pkg = "uj", "Values in [names.] must match elements of [...] or of [defs.]."))}
  n.names <- base::length(names.)                                                # number of arguments to match
  out <- base::rep.int(base::list(NULL), n.names)                                # initialize the results as a list of {NULL` elements
  for (i in 1:n.names) {                                                         # for each element of {names.}
    name.i <- names.[[i]]                                                        # : get the name to be matched
    if (in.dots[i]) {out[[i]] <- dots[[base::which(dot.names == name.i)]]}       # : if it matches an argument in {...}, store that argument
    else            {out[[i]] <- defs.[[base::which(def.names == name.i)]]}      # : otherwise, store the matching element of {defs.}
  }
  if (n.names == 1) {out <- out[[1]]}                                            # if only 1 argument was extracted, un-nest it from the results list
  out
}

#' @rdname dot_args
#' @export
dot <- function(name., def., ...) {
  if (!uj::cmp_scl(name.)) {stop(uj::format_errs(pkg = "uj", "[name.] must be a complete atomic scalar (?cmp_scl)."))}
  uj::dots(name., def., ...)
}

#' @rdname dot_args
#' @export
dot_names <- function(..., subs. = NULL, req. = T, blank. = F, u. = T) {
  dots <- base::list(...)
  errs <- c(uj::f0(base::length(dots) > 0                     , NULL, "[...] is empty."),
            uj::f0(uj::f0(uj::inll(subs.), T, uj::ivec(subs.)), NULL, "[subs.] must be NULL or an atomic vector with one value per argument in [...]."),
            uj::f0(uj::isTF(req.)                             , NULL, "[req.] must be TRUE or FALSE."),
            uj::f0(uj::isTF(blank.)                           , NULL, "[blank.] must be TRUE or FALSE."),
            uj::f0(uj::isTF(u.)                               , NULL, "[u.] must be TRUE or FALSE."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  n.dots <- base::...length()
  n.names <- base::length(base::...names())
  if (uj::inll(subs.)) {subs. <- base::...names()}
  errs <- base::c(uj::f0(n.names == n.dots | uj::inll(subs.), NULL, "When [subs.] is not NULL, it must contain one value per argument in [...]."),
                  uj::f0(n.names == n.dots | !req.          , NULL, "When [req. = TRUE], arguments in [...] must be named or [subs.] must contain one value per argument in [...]."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  subs. <- av(base::strsplit(base::as.character(uj::av(subs.)), "|", fixed = TRUE))
  subs.[base::is.na(subs.)] <- 'NA'
  errs <- base::c(uj::f0(!blank. | uj::notIN("", subs.), NULL, "A name is blank but [blank. = FALSE]."),
                  uj::f0(!u.     | uj::is_unq(subs.)   , NULL, "Names provided are not unique but [u. = TRUE]."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  subs.
}

#' @rdname dot_args
#' @export
named_dots <- function(...) {
  dot.names <- base::...names()
  n.dots <- .base::..length()
  n.names <- base::length(dot.names)
  ok.names <- !base::is.na(dot.names) & dot.names != ""
  uj::f0(n.dots == 0 | n.names == 0, NULL, uj::f0(!base::any(ok.names), NULL, base::list(...)[ok.names]))
}

#' @rdname dot_args
#' @export
unnamed_dots <- function(...) {
  dot.names <- base::...names()
  n.dots <- base::...length()
  n.names <- base::length(dot.names)
  ok.names <- base::is.na(dot.names) | dot.names == ""
  uj::f0(n.dots == 0, NULL, uj::f0(n.names == 0, base::list(...), uj::f0(!base::any(ok.names), NULL, base::list(...)[ok.names])))
}

#' @rdname dot_args
#' @export
glue_dots <- function(..., blank = "") {
  dots <- base::paste0(uj::av(...), collapse = "")
  uj::f0(dots == "", blank, dots)
}

#' @rdname dot_args
#' @export
collapse_dots <- glue_dots

#' @rdname dot_args
#' @export
str_dot <- function(..., n. = 1, def. = "", sep. = " ") {
  errs <- base::c(uj::f0(uj::cmp_psw_scl(n.  ), NULL, "[n.] must be a complete positive whole-number scalar (?cmp_psw_scl)."),
                  uj::f0(uj::cmp_chr_vec(def.), NULL, "[def.] must be a complete character vec (?cmp_chr_vec)."),
                  uj::f0(uj::cmp_chr_scl(sep.), NULL, "[sep.] must be a complete character scalar (?cmp_chr_scl)."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  if (n. > base::...length()) {stop(uj::format_errs(pkg = "uj", "[n.] is greater than the number of [...] args."))}
  n.calls <- base::length(base::sys.calls())
  if (n.calls == 1) {stop(uj::format_errs(uj::format_err("")))}
  gens <- 0 + (uj::caller() == "str_dots")
  print(sys.calls())
  dot.labels <- base::sys.call(base::length(base::sys.calls()) - gens)
  dot.labels <- base::as.character(dot.labels)
  dot.labels <- dot.labels[-1]
  exclude.prefixes <- base::c("n.", "def.", "sep.")
  exclude.prefixes <- base::c(base::paste0(exclude.prefixes, "="), base::paste0(exclude.prefixes, " ="))
  for (exclude.prefix in exclude.prefixes) {
    dot.label.prefixes <- substr(dot.labels, 1, nchar(exclude.prefix))
    dot.labels <- dot.labels[dot.label.prefixes != exclude.prefix]
  }
  print(dot.labels)
  dot.label <- dot.labels[n.]
  dot.value <- uj::failsafe(...elt(n.))
  dot.value <- uj::f0(!uj::isERR(dot.value) & !base::is.function(dot.value) & uj::cmp_chr(dot.value), dot.value, dot.label)
  dot.value <- base::paste0(uj::av(dot.value), collapse = sep.)
  uj::f0(dot.value != "", dot.value, base::paste0(def., collapse = sep.))
}

#' @rdname dot_args
#' @export
str_dots <- function(..., glue. = TRUE, def. = "", sep. = " ") {
  errs <- base::c(uj::f0(uj::cmp_lgl_scl(glue.), NULL, "[glue.] must be scalar TRUE or scalar FALSE."),
                  uj::f0(uj::cmp_chr_vec(def. ), NULL, "[def.] must be a complete character vec (?cmp_chr_vec)."),
                  uj::f0(uj::cmp_chr_scl(sep. ), NULL, "[sep.] must be a complete character scalar (?cmp_chr_scl)."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  n.dots <- base::...length()
  if (n.dots == 0) {return(def.)}
  result <- NULL
  for (i in n.dots) {result <- base::c(result, uj::flex_dot(i, ..., def., sep.))}
  if (glue.) {result <- base::paste0(result, collapse = sep.)}
  if (result == "") {result <- def.}
  result
}
