#' @encoding UTF-8
#' @family meta
#' @family args
#' @title Manage `...` arguments
#' @description Enhanced of functions for managing `...` arguments.
#' @details \strong{`dot_names`} and \strong{`dnames`} are enhancements of `...names` that flexibly retrieves names of all `...` args by substituting the corresponding value of `.subs` If a `...` arg's name is missing or blank (`""`), the corresponding value of `.names` is substituted Throws an error in the following circumstances:
#' \itemize{\item `...length() == 0`
#'          \item `length(.names) > 0 & length(.names) < ...length()`
#'          \item `is.null(.names) & ..bl & any(...names() == "")`
#'          \item `is.null(.names) & .req & !any(...names() != "")`
#'          \item `is.null(.names) & .u & any(duplicated(...names()))`}
#' \cr The remaining functions in this family are as follows:
#' \tabular{ll}{  `dots_by_name, ds_by_name`      \tab Enhancement of `list(...)` that extracts one or more`...` args based on matching values supplied in `.names`. If a supplied Name matches the Name of a `...`
#'                                                     arg, that arg is returned. Otherwise, the element of `.defs` with a matching Name is returned. `.names = NULL` and `.names = NA` are converted to `'NULL'` and
#'                                                     `'NA'`. Reserved words should be backtick quoted.                                                                                                               \cr   \tab   \cr
#'                `dot_by_name, d_by_name`        \tab Get a single `...` arg by name.                                                                                                                                 \cr   \tab   \cr
#'                `named_dots, named_ds`          \tab Get a named list containing named `...` arg values.                                                                                                             \cr   \tab   \cr
#'                `named_dot, named_d`            \tab Get the `.n`-th named `...` arg.                                                                                                                                \cr   \tab   \cr
#'                `anon_dots, anon_ds`            \tab Get an unnamed list containing unnamed (anonymous) `...` arg values.                                                                                            \cr   \tab   \cr
#'                `anon_dot, anon_d`              \tab Get the `.n`-th unnamed (anonymous) `...` arg.                                                                                                                  \cr   \tab   \cr
#'                `flex_dots, flex_ds`            \tab Retrieve a list of `...length()` elements where the `.n`\eqn{^{th}} element is the flexibly evaluated value of `...elt(.n)`, where flexible evaluation of `...`
#'                                                     arg values means that a `...` arg takes on the unadjusted value if \link[=base:force]{forcing} evaluation does not generate an error. Otherwise, it takes as
#'                                                      its value the string literal representing it in a function call. For example, `f(exactly, 5-2, words)` and `f("exactly", "3", "words")` give identical results
#'                                                     in the following circumstances: The usage of `f` is defined as `f(...)`, `f` treats each `...` arg as a flexdot, and in the immediate environment that calls
#'                                                     `f(...)` there are no defined objects with the names `exactly` and `words`.                                                                                     \cr   \tab   \cr
#'                `flex_dot, flex_d`              \tab Get the `.n`-th `...` arg, flexibly evaluated.                                                                                                                  \cr   \tab   \cr
#'                `dot_names, dnames, dn`         \tab Get all `...` arg names, substituting names if necessary.                                                                                                       \cr   \tab   \cr
#'                `glue_dots, g_dots, gdots, gds` \tab Collapse `...` args to a character scalar by calling \code{\link[base]{paste0}}`(`\code{\link{av}}`(...), collapse = .d)`.                                      \cr   \tab   \cr
#'                `glue_dot, gdot, gd`            \tab Get the `.n`-th `...` arg, glued.                                                                                                                               \cr   \tab   \cr
#'                `glue_flex_dots, gf_dots, gfds` \tab Combo functionality of `glue_dots` and `flex_dots` to glue flexibly evaluated `...` args into a character scalar.                                               \cr   \tab   \cr
#'                `glue_flex_dot, gf_dot, gfd`    \tab Get the `.n`-th `...` arg, flexibly evaluated and glued.                                                                                                                       }
#' @param ... An arbitrary number of arguments.
#' @param .names `NULL` or an \link[=atm_vec]{atomic vec} (may include `NA` values). Is split along the delimiter `'|'` to allow for compactness in submitting multiple .names `NULL` will match either an argument in `...` or element of `.defs` with the Name `'NULL'`. `NA` values will match an argument in `...` or an element of `.defs` with the Name `'NA'`.
#' @param .name `NULL` or \link[=cmp_scl]{complete atomic scalar}. `NULL` is replaced with `'NULL'` and `NA` is replaced with `'NA'`.
#' @param .subs `NULL` or \link[=cmp_chr_vec]{complete character vec}. If not `NULL`, it is split using pipes `'|'` as a delimiter. If there are no pipes contained in `.names`, it remains unchangeD When this argument is not `NULL`, it is substituted for the names of `...` arguments; thus, after splitting, its length must equal the number of `...` arguments. For example, `.names = c('one', 'two', 'three|four|five')` indicates that there should be five `...` arguments and the vector `c('one', 'two', 'three', 'four', 'five')` is substituted for their names.
#' @param .glue `TRUE` or `FALSE` indicating whether to glue the result (collapse into a character scalar) using the delimiter `.d`.
#' @param .defs A named \link[=VLS]{vlist} of default objects/values to return if the specified arguments are not in `...`. Elements of `.defs` must be uniquely nameD If `.defs` is a tibble, columns with matching names are returned.
#' @param .def A default object/value to return if a specified argument is not in `...`. For `glue_dots` and `glue_dot`, must be a \link[=cmp_chr_vec]{complete character vec}. For all others, may be, but does not need to be, a list.
#' @param .req `TRUE` or `FALSE` indicating whether names are required.
#' @param .d A \link[=cmp_chr_scl]{complete character scalar} for collapsing results to a character scalar.
#' @param .n A positive integer indicating which `...` arg to return.
#' @return **An object**                                 \cr\cr `dot_by_name, d_by_name`        \cr `named_dot, named_d`   \cr `anon_dot, anon_d`
#' \cr\cr  **A list**                                    \cr\cr `dots_by_name, ds_by_name`      \cr `named_dots, named_ds` \cr `anon_dots, anon_ds`
#' \cr\cr  **A character scalar**                        \cr\cr `glue_dots, glue_dot, gdot, gd` \cr `glue_flex_dots, gf_dots, gf_dot, gfdot, gfd`
#' \cr\cr  **A character vector** (when `.glue = FALSE`) \cr\cr `flex_dots, flex_ds`            \cr `flex_dot, flex_d`
#' \cr\cr  **A character scalar** (when `.glue = TRUE`)  \cr\cr `flex_dots, flex_ds`            \cr `flex_dot, flex_d`
#' \cr\cr  **A character vector**                        \cr\cr `dot_names, dnames, dn`
#' @examples
#' egdot_args <- function(...) {
#'   nDots <- ...length()
#'   if (nDots > 1) {
#'     sub.names <- paste0(letters[1:nDots], LETTERS[1:nDots])
#'     defaults <- as.list(LETTERS[1:nDots])
#'     default <- "{missing}"
#'   } else {
#'     sub.names <- NULL
#'     defaults <- "{no Dot args}"
#'     default <- "{no Dot args}"
#'   }
#'   glue.flex.dots <- flex_dots(..., .glue = TRUE, .def = default, .d = "_")
#'   glue.flex.dot2 <- flex_dot(..., .n = 2, .glue = TRUE)
#'   subbed.names   <- failsafe(dot_names(..., .subs = sub.names))
#'   named.dots     <- failsafe(named_dots(...))
#'   anon.dots      <- failsafe(anon_dots(...))
#'   dot.names      <- failsafe(dot_names(...))
#'   flex.dots      <- flex_dots(..., .defs = default)
#'   flex.dot2      <- flex_dot(..., .n = 2)
#'   glue.dots      <- failsafe(glue_dots(..., .defs = default))
#'   gf.dots        <- gf_dots(..., .def = default)
#'   dots           <- failsafe(dots_by_name(sub.names, defaults, ...))
#'   dot1           <- failsafe(dot_by_name(sub.names[1], defaults[1], ...))
#'
#'   list(glue.flex.dots = glue.flex.dots,
#'        glue.flex.dot2 = glue.flex.dot2,
#'        subbed.names   = subbed.names ,
#'        named.dots     = named.dots   ,
#'        anon.dots      = anon.dots    ,
#'        dot.names      = dot.names    ,
#'        flex.dots      = flex.dots    ,
#'        flex.dot2      = flex.dot2    ,
#'        glue.dots      = glue.dots    ,
#'        gf.dots        = gf.dots      ,
#'        dots           = dots         ,
#'        dot1           = dot1         )
#' }
#'
#' egdot_args(     "a",  b ,      "c",  d ,      "e")
#' egdot_args(Aa = "a",  b , Cc = "c",  d , Ee = "e")
#' egdot_args(Aa = "a", "b", Cc = "c", "d", Ee = "e")
#' egdot_args(letters, digits = 0:9, data.frame(letters, 0:26))
#' @export
dot_args <- function() {utils::help("dot_args")}

#' @rdname dot_args
#' @export
d_args <- dot_args

#' @rdname dot_args
#' @export
dot_by_name <- function(.name, .def, ...) {
  errs <- NULL
  if (uj::is_err(.name)) {errs <- base::c(errs, "[.name] must be a complete character scalar (?cmp_chr_scl).")}
  if (uj::is_err(.def)) {errs <- base::c(errs, "[.def] is not a valid R object.")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  if (!uj::.cmp_chr_scl(.name)) {uj::stopperr("[.name] must be a complete character scalar (?cmp_chr_scl).")}
  if (base::...length() == 0) {return(.def)}
  i <- base::which(base::...names() == .name)
  if (base::length(i) > 1) {uj::stopperr("[.name] matches the names of multiple [...] arguments.")}
  if (base::length(i) == 0) {.def} else {base::...elt(i)}
}

#' @rdname dot_args
#' @export
d_by_name <- dot_by_name

#' @rdname dot_args
#' @export
dots_by_name <- function(.names, .defs, ...) {
  errs <- NULL
  if (uj::is_err(.names)) {errs <- base::c(errs, "[.names] must be a unique character vec (?unq_chr_vec).")}
  if (uj::is_err(.defs)) {errs <- base::c(errs, "[.defs] is not a valid R object.")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  if (!uj::.unq_chr_vec(.names)) {errs <- base::c(errs, "[.names] must be a unique character vec (?unq_chr_vec).")}
  if (base::is.data.frame(.defs) | !base::is.list(.defs) | base::length(.defs) != base::length(.names)) {errs <- base::c(errs, "[.defs] must be a non-data.frame list of length equal to length(.names).")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  if (!base::setequal(.names, base::names(.defs))) {uj::stopperr("setequal(.names, names(.defs)) must be TRUE.")}
  if (base::...length() == 0) {return(.defs[.names])}
  if (base::length(.names) == 1) {return(uj::dot_by_name(.names, .defs, ...))}
  y <- NULL
  for (i in 1:base::...length()) {
    lab <- .names[i]
    dot <- base::list(uj::dot_by_name(lab, .defs[lab], ...))
    base::names(dot) <- lab
    y <- base::c(y, dot)
  }
  y
}

#' @rdname dot_args
#' @export
ds_by_name <- dots_by_name

#' @rdname dot_args
#' @export
dot_names <- function(..., .subs = NULL, .req = TRUE) {
  nDots <- base::...length()
  dotNames <- base::...names()
  okDots <- nDots > 0
  okSubs <- uj::nll_or(.subs, "unq_str_vec")
  okReq <- uj::.cmp_lgl_scl(.req)
  errs <- NULL
  if (!okDots) {errs <- base::c(errs, "[...] is empty.")}
  if (!okSubs) {errs <- base::c(errs, "[.subs] must be NULL or a unique string vec (?unq_str_vec).")}
  if (!okReq ) {errs <- base::c(errs, "[.req] must be TRUE or FALSE.")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  if (nDots == base::length(dotNames)) {
    if (base::any(dotNames == "")) {
      ind <- uj::wv(dotNames == "")
      ind <- ind[ind <= base::length(.subs)]
      dotNames[ind] <- .subs[ind]
    }
    allNamed <- uj::.unq_str_vec(dotNames)
  } else if (nDots == base::length(.subs)) {allNamed <- T}
  else {allNamed <- F}
  if (!allNamed & .req) {uj::stopperr("When [.req = TRUE], [...] arg names must resolve to a unique string vec (?unq_str_vec) of length [...length()] via the combination of [...names()] and [.subs].")}
  dotNames
}

#' @rdname dot_args
#' @export
dnames <- dot_names

#' @rdname dot_args
#' @export
dn <- dot_names

#' @rdname dot_args
#' @export
named_dots <- function(...) {
  DotNames <- base::...names()
  nDots <- base::...length()
  nNames <- base::length(DotNames)
  if (nDots == 0 | nNames == 0) {return(NULL)}
  i <- base::is.na(DotNames)
  if (base::any(i)) {return(NULL)}
  i <- DotNames != ""
  if (!base::any(i)) {NULL} else {base::list(...)[i]}
}

#' @rdname dot_args
#' @export
name_ds <- named_dots

#' @rdname dot_args
#' @export
anon_dots <- function(...) {
  DotNames <- base::...names()
  nDots <- base::...length()
  nNames <- base::length(DotNames)
  if (nDots == 0) {return(NULL)}
  else if (nNames == 0) {return(base::list(...))}
  i <- base::is.na(DotNames)
  i[!i] <- DotNames[!i] == ""
  if (!base::any(i)) {NULL} else {base::list(...)[i]}
}

#' @rdname dot_args
#' @export
anon_ds <- anon_dots

#' @rdname dot_args
#' @export
named_dot <- function(.n, ...) {
  .n <- uj::failsafe(.n)
  if (!uj::.cmp_psw_scl(.n)) {uj::stopperr("[.n] must be a positive whole-number scalar in 1:length(named_dots(...)).")}
  x <- uj::named_dots(...)
  if (.n > base::length(x)) {uj::stopperr("[.n] is greater than length(named_dots(...)).")}
  x[.n]
}

#' @rdname dot_args
#' @export
named_d <- named_dot

#' @rdname dot_args
#' @export
anon_dot <- function(.n, ...) {
  .n <- uj::failsafe(.n)
  if (!uj::.cmp_psw_scl(.n)) {uj::stopperr("[.n] must be a positive whole-number scalar in 1:length(named_dots(...)).")}
  x <- uj::anon_dots(...)
  if (.n > base::length(x)) {uj::stopperr("[.n] is greater than length(anon_dots(...)).")}
  x[.n]
}

#' @rdname dot_args
#' @export
anon_d <- anon_dot

#' @rdname dot_args
#' @export
flex_dot <- function(.n, ..., .glue = FALSE, .def = "", .d = " ") {
  .n    <- uj::failsafe(.n)
  .glue <- uj::failsafe(.glue)
  .def  <- uj::failsafe(.def)
  .d    <- uj::failsafe(.d)
  errs  <- NULL
  n     <- base::...length()
  if (!uj::.cmp_lgl_scl(.glue)) {errs <- base::c(errs, "[.glue] must be TRUE or FALSE ().")}
  if (!uj::.cmp_chr_vec(.def )) {errs <- base::c(errs, "[.def] must be a complete character vec (?cmp_chr_vec).")}
  if (!uj::.cmp_chr_scl(.d   )) {errs <- base::c(errs, "[.d] must be a complete character scalar (?cmp_chr_scl).")}
  if (!base::is.null(errs    )) {uj::stopperr(errs)}
  if (n >= .n) {
    y <- base::as.list(base::sys.call(-1))
    y <- y[2:base::length(y)][[.n]]
    if (base::is.symbol(y)) {y <- base::as.character(y)}
  } else {y <- .def}
  if (.glue) {y <- uj::p0(uj::av(y), collapse = .d)}
  y
}

#' @rdname dot_args
#' @export
flex_d <- flex_dot

#' @rdname dot_args
#' @export
flex_dots <- function(..., .glue = FALSE, .def = "", .d = " ") {
  .glue <- uj::failsafe(.glue)
  .def  <- uj::failsafe(.def)
  .d    <- uj::failsafe(.d)
  errs  <- NULL
  n     <- base::...length()
  if (!uj::.cmp_lgl_scl(.glue)) {errs <- base::c(errs, "[.glue] must be TRUE or FALSE ().")}
  if (!uj::.cmp_chr_vec(.def )) {errs <- base::c(errs, "[.def] must be a complete character vec (?cmp_chr_vec).")}
  if (!uj::.cmp_chr_scl(.d   )) {errs <- base::c(errs, "[.d] must be a complete character scalar (?cmp_chr_scl).")}
  if (!base::is.null(errs    )) {uj::stopperr(errs)}
  if (n > 0) {
    y <- base::as.list(base::sys.call(-1))
    y <- y[2:base::length(y)]
    for (i in 1:base::length(y)) {if (base::is.symbol(y[[i]])) {y[[i]] <- base::as.character(y[[i]])}}
  } else {y <- .def}
  if (.glue) {y <- base::paste0(uj::av(y), collapse = .d)}
  y
}

#' @rdname dot_args
#' @export
flex_ds <- flex_dots

#' @rdname dot_args
#' @export
glue_dot <- function(..., .n = 1, .def = "", .d = " ") {
  .n <- uj::failsafe(.n)
  .def <- uj::failsafe(.def)
  .d <- uj::failsafe(.d)
  errs <- NULL
  if (!uj::.cmp_psw_scl(.n  )) {errs <- base::c(errs, "[.n] must be a positive whole-number scalar in 1:length(named_dots(...)).")}
  if (!uj::.cmp_chr_vec(.def)) {errs <- base::c(errs, "[.def] must be a complete character vec (?cmp_chr_vec).")}
  if (!uj::.cmp_chr_scl(.d  )) {errs <- base::c(errs, "[.d] must be a complete character scalar (?cmp_chr_scl).")}
  if (!base::is.null(errs   )) {uj::stopperr(errs)}
  if (.n <= base::...length()) {
    y <- uj::failsafe(base::...elt(.n))
    if (!uj::is_err(y)) {
      base::paste0(uj::av(y), collapse = .d)
    } else {base::paste0(uj::av(.def), collapse = .d)}
  } else {base::paste0(uj::av(.def), collapse = .d)}
}

#' @rdname dot_args
#' @export
glue_d <- glue_dot

#' @rdname dot_args
#' @export
glue_dots <- function(..., .def = "", .d = " ") {
  .def <- uj::failsafe(.def)
  .d <- uj::failsafe(.d)
  errs <- NULL
  if (!uj::.cmp_chr_vec(.def)) {errs <- base::c(errs, "[.def] must be a complete character vec (?cmp_chr_vec).")}
  if (!uj::.cmp_chr_scl(.d  )) {errs <- base::c(errs, "[.d] must be a complete character scalar (?cmp_chr_scl).")}
  if (!base::is.null(errs   )) {uj::stopperr(errs)}
  y <- ""
  for (i in 1:base::...length()) {y <- base::c(y, uj::glue_dot(base::...elt(i), .n = i, .def = .def, .d = .d))}
  base::paste0(uj::av(y), collapse = .d)
}

#' @rdname dot_args
#' @export
glue_ds <- glue_dots

#' @rdname dot_args
#' @export
glue_flex_dots <- function(..., .def = "", .d = " ") {uj::flex_dots(..., .glue = T, .def = .def, .d = .d)}

#' @rdname dot_args
#' @export
glue_flex_ds <- glue_flex_dots

#' @rdname dot_args
#' @export
gf_dots <- glue_flex_dots

#' @rdname dot_args
#' @export
gf_ds <- glue_flex_dots

#' @rdname dot_args
#' @export
gfds <- gf_ds

#' @rdname dot_args
#' @export
glue_flex_dot <- function(..., .n = 1, .def = "", .d = " ") {uj::flex_dot(..., .glue = T, .n = .n, .def = .def, .d = .d)}

#' @rdname dot_args
#' @export
glue_flex_d <- glue_flex_dot

#' @rdname dot_args
#' @export
gf_dot <- glue_flex_dot

#' @rdname dot_args
#' @export
gfd <- glue_flex_dot
