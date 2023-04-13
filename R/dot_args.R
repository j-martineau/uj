#' @encoding UTF-8
#' @family meta
#' @family args
#' @title Manage `...` arguments
#' @description Enhanced of functions for managing `...` arguments.
#' @details \strong{`dot_names`} and \strong{`dnames`} are enhancements of `...names` that flexibly retrieves names of all `...` args by substituting the corresponding value of `SUBS` If a `...` arg's Name is missing or blank (`""`), the corresponding value of `.names` is substituteD Throws an error in the following circumstances:
#' \itemize{\item `...length() == 0`
#'          \item `length(NAMES) > 0 & length(NAMES) < ...length()`
#'          \item `is.null(NAMES) & BLANK & any(...names() == "")`
#'          \item `is.null(NAMES) & REQ & !any(...names() != "")`
#'          \item `is.null(NAMES) & U & any(duplicated(...names()))`}
#' \cr The remaining functions in this family are as follows:
#' \tabular{ll}{  `dots_by_name`   \tab Enhancement of `list(...)` that extracts one or more`...` args based on matching values supplied in `NAMES`. If a supplied Name matches the Name of a `...`
#'                                      arg, that arg is returned. Otherwise, the element of `DEFS` with a matching Name is returned. `NAMES = NULL` and `NAMES = NA` are converted to `'NULL'` and
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
#' @param NAMES `NULL` or an \link[=atm_vec]{atomic vec} (may include `NA` values). Is split along the delimiter `'|'` to allow for compactness in submitting multiple NAMES `NULL` will match either an argument in `...` or element of `DEFS` with the Name `'NULL'`. `NA` values will match an argument in `...` or an element of `DEFS` with the Name `'NA'`.
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
#'   list(glue.flex.dots = flex_dots(..., GLUE = TRUE, DEF = default, D = "_") ,
#'        glue.flex.dot2 = flex_dot(..., N = 2, GLUE = TRUE)                   ,
#'        subbed.names   = failsafe(dot_names(..., SUBS = sub.names))          ,
#'        named.dots     = failsafe(named_dots(...))                           ,
#'        anon.dots      = failsafe(anon_dots(...))                            ,
#'        DotNames      = failsafe(dot_names(...))                            ,
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
  Errors <- NULL
  if (uj::is_err(NAME)) {Errors <- base::c(Errors, "[NAME] must be a complete character scalar (?cmp_chr_scl).")}
  if (uj::is_err(DEF)) {Errors <- base::c(Errors, "[DEF] is not a valid R object.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (!uj:::.cmp_chr_scl(NAME)) {uj::stopperr("[NAME] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")}
  if (base::...length() == 0) {return(DEF)}
  i <- base::which(base::...names() == NAME)
  if (base::length(i) > 1) {uj::stopperr("[NAME] matches the names of multiple [...] arguments.", PKG = "uj")}
  if (base::length(i) == 0) {DEF} else {base::...elt(i)}
}

#' @rdname dot_args
#' @export
dots_by_name <- function(NAMES, DEFS, ...) {
  Errors <- NULL
  if (uj::is_err(NAMES)) {Errors <- base::c(Errors, "[NAMES] must be a unique character vec (?unq_chr_vec).")}
  if (uj::is_err(DEFS)) {Errors <- base::c(Errors, "[DEFS] is not a valid R object.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (!uj:::.unq_chr_vec(NAMES)) {Errors <- base::c(Errors, "[NAMES] must be a unique character vec (?unq_chr_vec).")}
  if (base::is.data.frame(DEFS) | !base::is.list(DEFS) | base::length(DEFS) != base::length(NAMES)) {Errors <- base::c(Errors, "[DEFS] must be a non-data.frame list of length equal to length(NAMES).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (!base::setequal(NAMES, base::names(DEFS))) {uj::stopperr("setequal(NAMES, names(DEFS)) must be TRUE.", PKG = "uj")}
  if (base::...length() == 0) {return(DEFS[NAMES])}
  if (base::length(NAMES) == 1) {return(uj::dot_by_name(NAMES, DEFS, ...))}
  Y <- NULL
  for (i in 1:base::...length()) {
    NAME <- NAMES[i]
    Dot <- base::list(uj::dot_by_name(NAME, DEFS[NAME], ...))
    base::names(Dot) <- NAME
    Y <- base::c(Y, Dot)
  }
  Y
}

#' @rdname dot_args
#' @export
dot_names <- function(..., SUBS = NULL, REQ = TRUE, BL = FALSE, U = TRUE) {
  nDots <- base::...length()
  DotNames <- base::...names()
  OkDots <- nDots > 0
  SUBS <- uj::failsafe(SUBS)
  REQ <- uj::failsafe(REQ)
  BL <- uj::failsafe(BL)
  U <- uj::failsafe(U)
  Errors <- NULL
  if (!OkDots) {Errors <- base::c(Errors, "[...] is empty.")}
  if (!base::isTRUE(REQ) & !base::isFALSE(REQ)) {Errors <- base::c(Errors, "[REQ] must be TRUE or FALSE.")}
  if (!base::isTRUE(BL) & !base::isFALSE(BL)) {Errors <- base::c(Errors, "[BL] must be TRUE or FALSE.")}
  if (!base::isTRUE(U) & !base::isFALSE(U)) {Errors <- base::c(Errors, "[U] must be TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  AllNamed <- uj::f0(nDots != base::length(DotNames), F,
                      uj::f0(!base::any(base::is.na(DotNames)), T,
                             uj::f0(base::length(SUBS) != nDots, F, uj:::.unq_chr_vec(SUBS))))
  if (!AllNamed) {uj::stopperr("When [REQ = TRUE], all [...] args must be named or [SUBS] must be a complete character vec (?cmp_chr_vec) of length [...length()].", PKG = "uj")}
  if (!BL) {DotNames[DotNames == ""] <- SUBS[DotNames == ""]}
  if (U & !uj:::.UNQ(DotNames)) {uj::stopperr("When [U = TRUE], [...names()] and [SUBS], taken together, must give unique names for all [...] args.", PKG = "uj")}
  DotNames
}

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
named_dot <- function(N, ...) {
  N <- uj::failsafe(N)
  if (!uj:::.cmp_psw_scl(N)) {uj::stopperr("[N] must be a positive whole-number scalar in 1:length(named_dots(...)).", PKG = "uj")}
  X <- uj::named_dots(...)
  if (N > base::length(X)) {uj::stopperr("[N] is greater than length(named_dots(...)).", PKG = "uj")}
  X[N]
}

#' @rdname dot_args
#' @export
anon_dot <- function(N, ...) {
  N <- uj::failsafe(N)
  if (!uj:::.cmp_psw_scl(N)) {uj::stopperr("[N] must be a positive whole-number scalar in 1:length(named_dots(...)).", PKG = "uj")}
  X <- uj::anon_dots(...)
  if (N > base::length(X)) {uj::stopperr("[N] is greater than length(anon_dots(...)).", PKG = "uj")}
  X[N]
}

#' @rdname dot_args
#' @export
flex_dot <- function(N, ..., GLUE = FALSE, DEF = "", D = " ") {
  N <- uj::failsafe(N)
  GLUE <- uj::failsafe(GLUE)
  DEF <- uj::failsafe(DEF)
  D <- uj::failsafe(D)
  nDots <- base::...length()
  nDef <- base::length(DEF)
  Errors <- NULL
  if (!uj:::.cmp_psw_scl(N)) {Errors <- base::c(Errors, "[N] must be a positive whole-number scalar in 1:length(named_dots(...)).")}
  if (!uj:::.cmp_lgl_scl(GLUE)) {Errors <- base::c(Errors, "[GLUE] must be TRUE or FALSE ().")}
  if (!uj:::.cmp_chr_vec(DEF)) {Errors <- base::c(Errors, "[DEF] must be a complete character vec (?cmp_chr_vec).")}
  if (!uj:::.cmp_chr_scl(D)) {Errors <- base::c(Errors, "[D] must be a complete character scalar (?cmp_chr_scl).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (!(N %in% 1:nDots)) {Errors <- base::c(Errors, "[N] must be in 1:...length().")}
  if (nDef != 1 & nDef != nDots) {Errors <- base::c(Errors, "[DEF] must be of length 1 or of length ...length().")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (base::...length() > 0) {
    parts <- base::as.list(base::sys.call(base::length(base::sys.calls()) - 1))
    PartNames <- base::names(parts)
    PartLabels <- base::as.character(parts)
    nParts <- base::length(PartLabels)
    PartNums <- 1:nParts
    if (base::is.null(PartNames)) {PartNames <- base::rep.int("", nParts)}
    Drop <- 1
    if (!("N" %in% PartNames)) {Drop <- base::c(Drop, 2)}
    Drop <- base::c(Drop, base::which(PartNames %in% base::c("GLUE", "DEF", "D")))
    keep <- PartNums[!(PartNums %in% Drop)][N]
    DotError <- uj::is_err(base::...elt(N))
    DotValue <- uj::f0(DotError, NULL, base::...elt(N))
    DotLabel <- PartLabels[keep]
    DotName <- PartNames[keep]
    if      (DotError                   ) {Y <- DotLabel}
    else if (base::is.function(DotValue)) {Y <- DotLabel}
    else if (DotName != ""              ) {Y <- base::as.character(uj::av(DotValue))}
    else if (!uj:::.CMP(DotValue)       ) {Y <- DotLabel}
    else if (!uj:::.CHR(DotValue)       ) {Y <- DotLabel}
    else                                  {Y <- DotValue}
    if (base::is.null(Y)) {Y <- ""}
    if (GLUE) {base::paste0(uj::av(Y), collapse = D)} else {Y}
  } else if (nDef == 1) {DEF} else {DEF[N]}
}

#' @rdname dot_args
#' @export
flex_dots <- function(..., GLUE = FALSE, DEF = "", D = " ") {
  Errors <- NULL
  if (!uj:::.cmp_lgl_scl(GLUE)) {Errors <- base::c(Errors, "[GLUE] must be TRUE or FALSE ().")}
  if (!uj:::.cmp_chr_vec(DEF)) {Errors <- base::c(Errors, "[DEF] must be a complete character vec (?cmp_chr_vec).")}
  if (!uj:::.cmp_chr_scl(D)) {Errors <- base::c(Errors, "[D] must be a complete character scalar (?cmp_chr_scl).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (base::...length() > 0) {
    Y <- NULL
    for (i in 1:base::...length()) {Y <- base::c(Y, base::list(uj::flex_dot(..., N = i, GLUE = GLUE, DEF = DEF, D = D)))}
  } else {Y <- DEF}
  if (GLUE) {base::paste0(uj::av(Y), collapse = D)} else {Y}
}

#' @rdname dot_args
#' @export
glue_dot <- function(..., N = 1, DEF = "", D = " ") {
  N <- uj::failsafe(N)
  DEF <- uj::failsafe(DEF)
  D <- uj::failsafe(D)
  Errors <- NULL
  if (!uj:::.cmp_psw_scl(N)) {Errors <- base::c(Errors, "[N] must be a positive whole-number scalar in 1:length(named_dots(...)).")}
  if (!uj:::.cmp_chr_vec(DEF)) {Errors <- base::c(Errors, "[DEF] must be a complete character vec (?cmp_chr_vec).")}
  if (!uj:::.cmp_chr_scl(D)) {Errors <- base::c(Errors, "[D] must be a complete character scalar (?cmp_chr_scl).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (N <= base::...length()) {
    Y <- uj::failsafe(base::...elt(N))
    if (!uj::is_err(Y)) {
      base::paste0(uj::av(Y), collapse = D)
    } else {base::paste0(uj::av(DEF), collapse = D)}
  } else {base::paste0(uj::av(DEF), collapse = D)}
}

#' @rdname dot_args
#' @export
glue_dots <- function(..., DEF = "", D = " ") {
  DEF <- uj::failsafe(DEF)
  D <- uj::failsafe(D)
  Errors <- NULL
  if (!uj:::.cmp_chr_vec(DEF)) {Errors <- base::c(Errors, "[DEF] must be a complete character vec (?cmp_chr_vec).")}
  if (!uj:::.cmp_chr_scl(D)) {Errors <- base::c(Errors, "[D] must be a complete character scalar (?cmp_chr_scl).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  Y <- ""
  for (i in 1:base::...length()) {Y <- base::c(Y, uj::glue_dot(base::...elt(i), N = i, DEF = DEF, D = D))}
  base::paste0(uj::av(Y), collapse = D)
}

#' @rdname dot_args
#' @export
gf_dots <- function(..., DEF = "", D = " ") {uj::flex_dots(..., GLUE = T, DEF = DEF, D = D)}

#' @rdname dot_args
#' @export
gf_dot <- function(..., N = 1, DEF = "", D = " ") {uj::flex_dot(..., GLUE = T, N = N, DEF = DEF, D = D)}
