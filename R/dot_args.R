#' @encoding UTF-8
#' @family meta
#' @family args
#' @title Manage `...` arguments
#' @description Enhanced of functions for managing `...` arguments.
#' @details \strong{`dot_names`} and \strong{`dnames`} are enhancements of `...names` that flexibly retrieves names of all `...` args by substituting the corresponding value of `SUBS` If a `...` arg's name is missing or blank (`""`), the corresponding value of `.names` is substituteD Throws an error in the following circumstances:
#' \itemize{\item `...length() == 0`
#'          \item `length(NAMES) > 0 & length(NAMES) < ...length()`
#'          \item `is.null(NAMES) & BLANK & any(...names() == "")`
#'          \item `is.null(NAMES) & REQ & !any(...names() != "")`
#'          \item `is.null(NAMES) & U & any(duplicated(...names()))`}
#' \cr The remaining functions in this family are as follows:
#' \tabular{ll}{  `dots_by_name`   \tab Enhancement of `list(...)` that extracts one or more`...` args based on matching values supplied in `NAMES`. If a supplied name matches the name of a `...`
#'                                      arg, that arg is returned. Otherwise, the element of `DEFS` with a matching name is returned. `NAMES = NULL` and `NAMES = NA` are converted to `'NULL'` and
#'                                      `'NA'`. Reserved words should be backtick quoted.                                                                                                               \cr   \tab   \cr
#'                `named_dots`     \tab Get a named list containing named `...` arg values.                                                                                                             \cr   \tab   \cr
#'                `anon_dots`      \tab Get an unnamed list containing unnamed (anonymous) `...` arg values.                                                                                            \cr   \tab   \cr
#'                `flex_dots`      \tab Retrieve a list of `...length()` elements where the `n`\eqn{^{th}} element is the flexibly evaluated value of `...elt(n)`, where flexible evaluation of `...`
#'                                      arg values means that a `...` arg takes on the unadjusted value if \link[=base:force]{forcing} evaluation does not generate an error. Otherwise, it takes as
#'                                      its value the string literal representing it in a function call. For example, `f(exactly, 5-2, words)` and `f("exactly", "3", "words")` give identical results
#'                                      in the following circumstances: The usage of `f` is defined as `f(...)`, `f` treats each `...` arg as a flexdot, and in the immediate environment that calls
#'                                      `f(...)` there are no defined objects with the names `exactly` and `words`.                                                                                     \cr   \tab   \cr
#'                `glue_dots`      \tab Collapse `...` args to a character scalar by calling \code{\link[base]{paste0}}`(`\code{\link{av}}`(...), collapse = D)`.                                       \cr   \tab   \cr
#'                `gf_dots`        \tab Combo functionality of `glue_dots` and `flex_dots` to glue flexibly evaluated `...` args into a character scalar.                                               \cr   \tab   \cr
#'                `dot_by_name`    \tab Get a single `...` arg by name.                                                                                                                                 \cr   \tab   \cr
#'                `named_dot`      \tab Get the `N`-th named `...` arg.                                                                                                                                 \cr   \tab   \cr
#'                `anon_dot`       \tab Get the `N`-th unnamed (anonymous) `...` arg.                                                                                                                   \cr   \tab   \cr
#'                `flex_dot`       \tab Get the `N`-th `...` arg, flexibly evaluated.                                                                                                                   \cr   \tab   \cr
#'                `glue_dot`       \tab Get the `N`-th `...` arg, glued.                                                                                                                                \cr   \tab   \cr
#'                `gf_dot`         \tab Get the `N`-th `...` arg, flexibly evaluated and glued.                                                                                                                        }
#' @param ... An arbitrary number of arguments.
#' @param BLANK `TRUE` or `FALSE` indicating whether blank names are allowed or what to return when `...` args resolve to a blank string (`""`).
#' @param NAMES `NULL` or an \link[=atm_vec]{atomic vec} (may include `NA` values). Is split along the delimiter `'|'` to allow for compactness in submitting multiple NAMES `NULL` will match either an argument in `...` or element of `DEFS` with the name `'NULL'`. `NA` values will match an argument in `...` or an element of `DEFS` with the name `'NA'`.
#' @param NAME `NULL` or \link[=cmp_scl]{complete atomic scalar}. `NULL` is replaced with `'NULL'` and `NA` is replaced with `'NA'`.
#' @param SUBS `NULL` or \link[=cmp_chr_vec]{complete character vec}. If not `NULL`, it is split using pipes `'|'` as a delimiter. If there are no pipes contained in `NAMES`, it remains unchangeD When this argument is not `NULL`, it is substituted for the names of `...` arguments; thus, after splitting, its length must equal the number of `...` arguments. For example, `NAMES = c('one', 'two', 'three|four|five')` indicates that there should be five `...` arguments and the vector `c('one', 'two', 'three', 'four', 'five')` is substituted for their names.
#' @param GLUE `TRUE` or `FALSE` indicating whether to glue the result (collapse into a character scalar) using the delimiter `D`.
#' @param DEFS A named \link[=VLS]{vlist} of default objects/values to return if the specified arguments are not in `...`. Elements of `DEFS` must be uniquely nameD If `DEFS` is a tibble, columns with matching names are returned.
#' @param DEF A default object/value to return if a specified argument is not in `...`. For `glue_dots` and `glue_dot`, must be a \link[=cmp_chr_vec]{complete character vec}. For all others, may be, but does not need to be, a list.
#' @param REQ `TRUE` or `FALSE` indicating whether names are required.
#' @param UNQ `TRUE` or `FALSE` indicating whether `NAMES` must be unique.
#' @param D A \link[=cmp_chr_scl]{complete character scalar} for collapsing results to a character scalar.
#' @param N A positive integer indicating which `...` arg to return.
#' @return **A character vector** (when `GLUE = FALSE`) \cr\cr `flex_dots, flex_dot`
#' \cr     **A character scalar** (when `GLUE = TRUE`)  \cr\cr `flex_dots, flex_dot`
#' \cr\cr  **A character vector**                       \cr\cr `dot_names, dnames`
#' \cr\cr  **A character scalar**                       \cr\cr `glue_dots, glue_dot` \cr `gf_dots, gf_dot`
#' \cr\cr  **An object**                                \cr\cr `dot_by_name, named_dot, anon_dot`
#' \cr\cr  **A list**                                   \cr\cr `dots_by_name, named_dots, anon_dots`
#' @examples
#' egdot_args <- function(...) {
#'   n.dots <- ...length()
#'   if (n.dots > 1) {
#'     sub.names <- paste0(letters[1:n.dots], LETTERS[1:n.dots])
#'     defaults <- as.list(LETTERS[1:n.dots])
#'     default <- "{missing}"
#'   } else {
#'     sub.names <- NULL
#'     defaults <- "{no dot args}"
#'     default <- "{no dot args}"
#'   }
#'   list(glue.flex.dots = flex_dots(..., GLUE = TRUE, DEF = default, D = "_") ,
#'        glue.flex.dot2 = flex_dot(..., N = 2, GLUE = TRUE)                   ,
#'        subbed.names   = failsafe(dot_names(..., SUBS = sub.names))          ,
#'        named.dots     = failsafe(named_dots(...))                           ,
#'        anon.dots      = failsafe(anon_dots(...))                            ,
#'        dot.names      = failsafe(dot_names(...))                            ,
#'        flex.dots      = flex_dots(..., DEFS = default)                      ,
#'        flex.dot2      = flex_dot(..., N = 2)                                ,
#'        glue.dots      = failsafe(glue_dots(..., DEFS = default))            ,
#'        gf.dots        = gf_dots(..., DEF = default)                         ,
#'        dots           = failsafe(dots_by_name(sub.names, defaults, ...))     ,
#'        dot1           = failsafe(dot_by_name(sub.names[1], defaults[1], ...)))
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
dot_by_name <- function(NAME, DEF, ...) {
  uj::err_if_not(uj::cmp_chr_scl(NAME), "[NAME] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  if (uj::ND0()) {return(DEF)}
  i <- uj::WV(base::...names() == NAME)
  uj::err_if(uj::N2P(i), "[NAME] matches the names of multiple [...] arguments.", PKG = "uj")
  uj::f0(uj::N0(i), DEF, base::...elt(i))
}

#' @rdname dot_args
#' @export
dots_by_name <- function(NAMES, DEFS, ...) {
  uj::err_if_not(uj::unq_chr_vec(NAMES)         , "[NAMES] must be a unique character vec (?unq_chr_vec)." , PKG = "uj")
  uj::err_if_not(uj::VLS(DEFS, n = uj::N(NAMES)), "[DEFS] must be a list of length equal to length(NAMES).", PKG = "uj")
  uj::err_if_not(uj::isSEQ(NAMES, uj::EN(DEFS)) , "setequal(NAMES, names(DEFS)) must be TRUE."             , PKG = "uj")
  if (uj::ND0()) {return(DEFS[NAMES])}
  if (uj::N1(NAMES)) {return(uj::dot_by_name(NAMES, DEFS, ...))}
  y <- NULL
  for (i in 1:uj::ND()) {
    NAME <- NAMES[i]
    dot <- base::list(uj::dot_by_name(NAME, DEFS[NAME], ...))
    uj::EN(dot) <- NAME
    y <- base::c(y, dot)
  }
  y
}

#' @rdname dot_args
#' @export
dot_names <- function(..., SUBS = NULL, REQ = TRUE, BL = FALSE, U = TRUE) {
  n.dots <- base::...length()
  dot.names <- base::...names()
  all.named <- uj::f0(n.dots == uj::N(dot.names) & uj::noneNA(dot.names), T, uj::f0(uj::N(SUBS) != n.dots, F, uj::cmp_chr_vec(SUBS)))
  uj::errs_if_nots(uj::ND0()                 , "[...] is empty."                                                                                                                   ,
                   uj::isTF1(U)              , "[U] must be TRUE or FALSE."                                                                                                        ,
                   uj::isTF1(BL)             , "[BL] must be TRUE or FALSE."                                                                                                       ,
                   uj::isTF1(REQ)            , "[REQ] must be TRUE or FALSE."                                                                                                      ,
                   uj::notT1(REQ) | all.named, "When [REQ = TRUE], all [...] args must be named or [SUBS] must be a complete character vec (?cmp_chr_vec) of length [...length()].", PKG = "uj")
  if (!BL) {dot.names[dot.names == ""] <- SUBS[dot.names == ""]}
  uj::err_if_not(U & !uj::UNQ(dot.names), "When [U = TRUE], [...names()] and [SUBS], taken together, must give unique names for all [...] args.", PKG = "uj")
  dot.names
}

#' @rdname dot_args
#' @export
named_dots <- function(...) {
  dot.names <- uj::DN()
  n.dots <- uj::ND()
  n.names <- uj::N(dot.names)
  ok.names <- uj::ok(dot.names) & dot.names != ""
  uj::f0(n.dots == 0 | n.names == 0, NULL, uj::f0(uj::none(ok.names), NULL, base::list(...)[ok.names]))
}

#' @rdname dot_args
#' @export
anon_dots <- function(...) {
  dot.names <- uj::DN()
  n.dots <- uj::ND()
  n.names <- uj::N(dot.names)
  ok.names <- uj::na(dot.names) | dot.names == ""
  uj::f0(n.dots == 0, NULL, uj::f0(n.names == 0, base::list(...), uj::f0(uj::none(ok.names), NULL, base::list(...)[ok.names])))
}

#' @rdname dot_args
#' @export
named_dot <- function(N, ...) {
  uj::err_if_not(uj::cmp_psw_scl(N), "[N] must be a positive whole-number scalar in 1:length(anon_dots(...)).", PKG = "uj")
  x <- uj::named_dots(...)
  uj::err_if(N > uj::N(x), "[N] is greater than length(named_dots(...)).")
  x[N]
}

#' @rdname dot_args
#' @export
anon_dot <- function(N, ...) {
  uj::err_if_not(uj::cmp_psw_scl(N), "[N] must be a positive whole-number scalar in 1:length(anon_dots(...)).", PKG = "uj")
  x <- uj::anon_dots(...)
  uj::err_if(N > uj::N(x), "[N] is greater than length(anon_dots(...)).")
  x[N]
}

#' @rdname dot_args
#' @export
flex_dot <- function(N, ..., GLUE = FALSE, DEF = "", D = " ") {
  uj::errs_if_nots(uj::cmp_chr_vec(DEF), "[DEF] must be a complete character vec (?cmp_chr_vec)."             ,
                   uj::cmp_psw_scl(N  ), "[N] must be a complete positive whole-number scalar (?cmp_psw_scl).",
                   uj::cmp_chr_scl(D  ), "[D] must be a complete character scalar (?cmp_chr_scl)."            , PKG = "uj")
  if (uj::ND1P()) {
    parts <- base::as.list(base::sys.call(uj::N(base::sys.calls()) - 1))
    part.names <- uj::EN(parts)
    part.labels <- uj::asCHR(parts)
    keep <- !(part.names %in% base::c("N", "GLUE", "DEF", "D"))
    keep[1] <- FALSE
    dot.value <- uj::failsafe(base::...elt(N))
    dot.label <- part.labels[keep][N]
    dot.name <- part.names[keep][N]
    y <- uj::f0(uj::notERR(dot.value) & uj::notFUN(dot.value) & uj::cmp_chr(dot.value), dot.value,
                uj::f0(dot.name == "", dot.label, uj::asCHR(uj::av(dot.value))))
    uj::f0(GLUE, uj::g(D, y), y)
  } else {DEF}
}

#' @rdname dot_args
#' @export
flex_dots <- function(..., GLUE = FALSE, DEF = "", D = " ") {
  uj::errs_if_nots(uj::cmp_lgl_scl(GLUE), "[GLUE] must be scalar TRUE or scalar FALSE."             ,
                   uj::cmp_chr_vec(DEF) , "[DEF] must be a complete character vec (?cmp_chr_vec)."  ,
                   uj::cmp_chr_scl(D)   , "[D] must be a complete character scalar (?cmp_chr_scl)." , PKG = "uj")
  if (uj::ND0()) {uj::f0(GLUE, uj::g(D, DEF), DEF)}
  y <- NULL
  for (i in 1:uj::ND()) {y <- base::c(base::list(uj::flex_dot(..., N = i, GLUE = GLUE, DEF = DEF, D = D)))}
  uj::f0(GLUE, uj::g(D, uj::av(y)), y)
}

#' @rdname dot_args
#' @export
glue_dot <- function(..., N = 1, DEF = "", D = " ") {
  uj::errs_if_nots(uj::cmp_psw_scl(N)  , "[N] must be a complete positive whole number scalar (?cmp_psw_scl).",
                   uj::cmp_chr_vec(DEF), "[DEF] must be a complete character vec (?cmp_chr_vec)."             ,
                   uj::cmp_chr_scl(D)  , "[D] must be a complete character scalar (?cmp_chr_scl)."            , PKG = "uj")
  if (N > uj::ND()) {return(DEF)}
  y <- base::...elt(N)
  uj::f0(uj::DEF(y), uj::g(D, uj::av(y)), DEF)
}

#' @rdname dot_args
#' @export
glue_dots <- function(..., DEF = "", D = " ") {
  uj::errs_if_nots(uj::cmp_chr_vec(DEF), "[DEF] must be a complete character vec (?cmp_chr_vec)."  ,
                   uj::cmp_chr_scl(D)  , "[D] must be a complete character scalar (?cmp_chr_scl)." , PKG = "uj")
  if (uj::ND0()) {return(DEF)}
  y <- NULL
  for (i in 1:uj::ND()) {y <- base::c(y, uj::glue_dot(base::...elt(i), N = i, DEF = DEF, D = D))}
  uj::g(D, y)
}

#' @rdname dot_args
#' @export
gf_dots <- function(..., DEF = "", D = " ") {uj::flex_dots(..., GLUE = T, DEF = DEF, D = D)}

#' @rdname dot_args
#' @export
gf_dot <- function(..., N = 1, DEF = "", D = " ") {uj::flex_dot(..., GLUE = T, N = N, DEF = DEF, D = D)}
