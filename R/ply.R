#' @encoding UTF-8
#' @family meta
#' @title Variations on `apply` functions
#' @description Apply a function over elements, rows, columns, or all dimensions; to an atomic object, a \link[=MVC]{multivec}, \link[=VEC]{vec}, \link[=VLS]{vlist}, matrix, or data.frame; and/or check the number of `TRUE` values in the result.
#' @details
#' \tabular{ll}{  `none_ply`   \tab Check for `0` resulting `TRUE` values\eqn{^{(1)}}.               \cr
#'                `one_ply`    \tab Check for `1` resulting `TRUE` values\eqn{^{(1)}}.               \cr
#'                `two_ply`    \tab Check for `2+` resulting `TRUE` values\eqn{^{(1)}}.              \cr
#'                `any_ply`    \tab Check for `1+` resulting `TRUE` values\eqn{^{(1)}}.              \cr
#'                `some_ply`   \tab Check for `2+` resulting `TRUE` values\eqn{^{(1)}}.              \cr
#'                `many_ply`   \tab Check for `3+` resulting `TRUE` values\eqn{^{(1)}}.              \cr
#'                `all_ply`    \tab Check for *only* resulting `TRUE` values\eqn{^{(1)}}.            \cr   \tab   \cr
#'                `atm_ply`    \tab Apply `.fun` to \link[=av]{atomized} `.x`.                         \cr
#'                `mvc_ply`    \tab Apply `.fun` to elements of \link[=atm_mvc]{atomic multivec} `.x`. \cr
#'                `vec_ply`    \tab Apply `.fun` to elements of \link[=atm_vec]{atomic vec} `.x`.      \cr
#'                `vls_ply`    \tab Apply `.fun` to elements of \link[=atm_vls]{atomic vlist} `.x`.    \cr   \tab   \cr
#'                `row_ply`    \tab Apply `.fun` to rows of `.x`.                                      \cr
#'                `col_ply`    \tab Apply `.fun` to columns of `.x`.                                   \cr
#'                `dim_ply`    \tab Apply `.fun` to cells of `.x`.                                     \cr   \tab   \cr
#'                `ply`        \tab Generalized `ply` function.                                      \cr   \tab     }
#'  \tabular{l}{  \eqn{^{(1)}} These functions assume that applying `.fun` produces `'logical'` results.             }
#' @section The `.proc` argument: When not `NULL`, the `.proc` argument is an optional list with up to seven named elements, which give processing instructions as follows:
#' \tabular{ll}{  **Name + value**    \tab **Processing instructions**                                  \cr
#'                `$arg = '{spec}'`   \tab Check `.x` for match to \code{\link[=ppp]{spec}}\eqn{^{(1)}}. \cr
#'                `$out = '{spec}'`   \tab Check result for match to `'{spec}'`.\eqn{^{(1)}}            \cr   \tab   \cr
#'                `$agg = 'none'`     \tab Inspect result for `0` `TRUE` values.                        \cr
#'                `$agg = 'one'`      \tab Inspect result for `1` `TRUE` values.                        \cr
#'                `$agg = 'two'`      \tab Inspect result for `2+` `TRUE` values.                       \cr
#'                `$agg = 'any'`      \tab Inspect result for *any* `TRUE` values.                      \cr
#'                `$agg = 'all'`      \tab Inspect result for *only* `TRUE` values.                     \cr   \tab   \cr
#'                `$na = 'err'`       \tab Throw error if result has any `NA` values.                   \cr
#'                `$na = FALSE`       \tab Replace resulting `NA`s with `FALSE`.                        \cr
#'                `$na = TRUE`        \tab Replace resulting `NA`s with `TRUE`.                         \cr   \tab   \cr
#'                `$a1 = TRUE`        \tab \link[=av]{Atomize} `.x`.                                     \cr
#'                `$a2 = TRUE`        \tab Atomize the result.                                          \cr   \tab   \cr
#'                `$s = TRUE`         \tab \link[base:simplify2array]{Simplify} the result.             \cr   \tab     }
#'  \tabular{l}{  \eqn{^{(1)}} `{spec}` is a placeholder for a \link[=is_prop_spec]{valid property spec}.              }
#' @param .x An object to apply `.fun` to.
#' @param .fun Function or character scalar name of a function to apply to `.x`.
#' @param ... An arbitrary number of additional arguments to be passed to the function `.fun`.
#' @param .dim A \link[=cmp_nnw_vec]{complete non-negative whole-number vec} giving dimension(s) of `.x` to apply the function `.fun` to (`0` indicates applying to elements of a vector or \link[=VLS]{vlist} vs. applying to every cell for arrays and data.frames).
#' @param .proc Either `NULL` or a list of named elements with processing instructions. See *the* `.proc` *argument*.
#' @examples
#' egNumVec <- 1:5
#' egNumMat <- matrix(1:25, nrow = 5)
#' egChrDtf <- dtf(az = letters[1:10], AZ = LETTERS[1:10], nm = as.character(0:9))
#' egChrVls <- list(az = letters, AZ = LETTERS, nm = as.character(0:9))
#' egComp <- function(.x, comp, value) {if (comp == "<") {.x < value} else {.x > value}}
#'
#' egNumVec
#' egNumMat
#' egChrDtf
#' egChrVls
#'
#' none_ply(egNumVec, egComp, ">", value = 6)
#' none_ply(egNumVec, egComp, ">", value = 4)
#' any_ply(egNumVec, egComp, ">", value = 6)
#' any_ply(egNumVec, egComp, ">", value = 4)
#' all_ply(egNumVec, egComp, "<", value = 6)
#' all_ply(egNumVec, egComp, "<", value = 4)
#' one_ply(egNumVec, egComp, ">", value = 6)
#' one_ply(egNumVec, egComp, ">", value = 4)
#' two_ply(egNumVec, egComp, ">", value = 2)
#' two_ply(egNumVec, egComp, ">", value = 5)
#' two_ply(egNumMat, egComp, ">", value = 15, .dim = 2)
#' dim_ply(egNumMat, egComp, ">", value = 15)
#' dim_ply(egChrDtf, toupper)
#' row_ply(egNumMat, sum)
#' col_ply(egChrDtf, paste0, collapse = "")
#' vls_ply(egChrVls, paste0, collapse = "")
#' @export
ply <- function(.x, .fun, ..., .dim = 0, .proc = NULL) {
  if (base::length(.x) == 0) {ppp::stopperr("[.x] is empty.", pkg = "uj")}
  vNames <- base::c("a1", "a2", "s", "na", "arg", "out", "agg")
  vAggs  <- base::c("none", "any", "all", "one", "two")
  pNames <- base::names(.proc)
  procN  <- base::length(.proc)
  nameN  <- base::length(pNames)
  nameV  <- uj::f0(procN == 0, T, procN == nameN & base::all(pNames %in% vNames))
  s      <- uj::f0(!base::is.null(.proc$s), .proc$s, F)
  a1     <- uj::f0(!base::is.null(.proc$a1), .proc$a1, F)
  a2     <- uj::f0(!base::is.null(.proc$a1), .proc$a2, F)
  na     <- uj::f0(!base::is.null(.proc$na), .proc$na, F)
  agg    <- .proc$agg
  arg    <- .proc$arg
  out    <- .proc$out
  d0     <- base::ifelse(ppp::cmp_nnw_scl(.dim), .dim == 0, F)
  okDim  <- d0 | ppp::cmp_psw_vec(.dim)
  okXdim <- uj::f0(d0 | !okDim, T, base::all(.dim %in% 1:base::length(base::dim(.x))))
  okProc <- uj::f0(base::is.null(.proc), T, base::is.list(.proc) & nameV)
  okNA   <- uj::f0(uj:::.lgl_scl(na   ), T, uj::f0(ppp::.cmp_chr_scl(na), na == "err", F))
  okAgg  <- uj::f0(base::is.null(agg  ), T, agg %in% vAggs)
  okArg  <- uj::f0(base::is.null(arg  ), T, uj::is_prop_spec(arg))
  okOut  <- uj::f0(base::is.null(out  ), T, uj::is_prop_spec(out))
  okA1   <- uj::f0(base::is.null(a1   ), T, ppp::cmp_lgl_scl(a1))
  okA2   <- uj::f0(base::is.null(a1   ), T, ppp::cmp_lgl_scl(a2))
  okS    <- uj::f0(base::is.null(s    ), T, ppp::cmp_lgl_scl(s))
  errs <- NULL
  if (!uj:::..fun(.fun)) {errs <- base::c(errs, "[.fun] is not a function or the name of function.")}
  if (!okXdim) {errs <- base::c(errs, "[.dim] contains a value larger than the number of defined dimensions of [.x].")}
  if (!okProc) {errs <- base::c(errs, "Elements of [.proc] must be uniquely named with names from c('a1', 'a2', 's', 'na', 'arg', 'out').")}
  if (!okAgg ) {errs <- base::c(errs, "When supplied, [.proc$agg] must be 'none', 'any', 'all', 'one', or 'two'.")}
  if (!okArg ) {errs <- base::c(errs, "When supplied, [.proc$arg] must be a valid property specification as validated by isVALIDspec(.).")}
  if (!okOut ) {errs <- base::c(errs, "When supplied, [.proc$out] must be a valid property specification as validated by isVALIDspec(.).")}
  if (!okDim ) {errs <- base::c(errs, "[.dim] must be 0 or a complete positive whole-number vec (?cmp_psw_vec).")}
  if (!okNA  ) {errs <- base::c(errs, "When supplied, [.proc$na] must be TRUE, FALSE, NA, or 'err'.")}
  if (!okA1  ) {errs <- base::c(errs, "When supplied, [.proc$a1] must be TRUE or FALSE.")}
  if (!okA2  ) {errs <- base::c(errs, "When supplied, [.proc$a2] must be TRUE or FALSE.")}
  if (!okS   ) {errs <- base::c(errs, "When supplied, [.proc$s] must be TRUE or FALSE.")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "uj")}
  if (a1) {.x <- uj::av(.x)}
  if (!base::is.null(arg)) {okMatch <- uj::PPP(.x, arg)} else {okMatch <- FALSE}
  if (!okMatch) {ppp::stopperr("[.x] does not match [.proc$arg = '", arg, "'].", pkg = "uj")}
  if (d0) {
    if (base::is.arrary(.x) | base::is.data.frame(.x)) {.x <- base::apply(.x, 1:base::length(base::dim(.x)), .fun, ...)}
    else if (base::is.null(.x) | uj:::.vec(.x)) {.x <- base::sapply(.x, .fun, ...)}
    else if (base::is.list(.x)) {.x <- base::lapply(.x, .fun, ...)}
    else {.x}
  }
  else {.x <- base::apply(.x, .dim, .fun, ...)}
  if (s) {.x <- base::simplify2array(.x)}
  if (a2) {.x <- uj::av(.x)}
  if (!uj::f0(base::is.null(out), T, uj::PPP(.x, out))) {ppp::stopperr("[.x] does not match [.proc$out = '", out, "'].", pkg = "uj")}
  if (!base::is.null(agg)) {
    err <- uj::f0(ppp::.cmp_chr_scl(na), na == "err", F)
    errs <- NULL
    if (!uj:::.LGL(.x)) {errs <- base::c(errs, "[.proc$agg] is not NULL, but results of applying [.fun] are not of mode logical.")}
    if (err & base::any(base::is.na(uj::av(.x)))) {errs <- base::c(errs, "Applying [.fun] produced NA values, but [.proc$na = 'err'].")}
    if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "uj")}
    if (!err) {.x[base::is.na(.x)] <- na}
    if      (agg == "all" ) {base::length(which(.x)) == base::length(.x)}
    else if (agg == "none") {base::length(which(.x)) == 0}
    else if (agg == "one" ) {base::length(which(.x)) == 1}
    else if (agg == "two" ) {base::length(which(.x)) == 2}
    else if (agg == "any" ) {base::length(which(.x)) > 0}
    else if (agg == "some") {base::length(which(.x)) > 1}
    else if (agg == "many") {base::length(which(.x)) > 2}
    else {.x}
  }
  .x
}

#' @rdname ply
#' @export
none_ply <- function(.x, .fun, ..., .dim = 0, .proc = NULL) {
  .proc <- "none"
  uj::ply(.x, .fun, ..., .dim = .dim, .proc = .proc)
}

#' @rdname ply
#' @export
any_ply <- function(.x, .fun, ..., .dim = 0, .proc = NULL) {
  .proc$agg <- "any"
  uj::ply(.x, .fun, ..., .dim = .dim, .proc = .proc)
}

#' @rdname ply
#' @export
some_ply <- function(.x, .fun, ..., .dim = 0, .proc = NULL) {
  .proc$agg <- "some"
  uj::ply(.x, .fun, ..., .dim = .dim, .proc = .proc)
}

#' @rdname ply
#' @export
many_ply <- function(.x, .fun, ..., .dim = 0, .proc = NULL) {
  .proc$agg <- "many"
  uj::ply(.x, .fun, ..., .dim = .dim, .proc = .proc)
}

#' @rdname ply
#' @export
all_ply <- function(.x, .fun, ..., .dim = 0, .proc = NULL) {
  .proc$agg <- "all"
  uj::ply(.x, .fun, ..., .dim = .dim, .proc = .proc)
}

#' @rdname ply
#' @export
one_ply <- function(.x, .fun, ..., .dim = 0, .proc = NULL) {
  .proc$agg <- "one"
  uj::ply(.x, .fun, ..., .dim = .dim, .proc = .proc)
}

#' @rdname ply
#' @export
two_ply <- function(.x, .fun, ..., .dim = 0, .proc = NULL) {
  .proc$agg <- "two"
  uj::ply(.x, .fun, ..., .dim = .dim, .proc = .proc)
}

#' @rdname ply
#' @export
atm_ply <- function(.x, .fun, ..., .proc = NULL) {
  .proc$a1 <- TRUE
  uj::ply(.x, .fun, ..., .dim = 0, .proc = .proc)
}

#' @rdname ply
#' @export
mvc_ply <- function(.x, .fun, ..., .proc = NULL) {
  .proc$arg <- 'mvc'
  uj::ply(.x, .fun, ..., .dim = 0, .proc = .proc)
}

#' @rdname ply
#' @export
vec_ply <- function(.x, .fun, ..., .proc = NULL) {
  .proc$arg <- 'vec'
  uj::ply(.x, .fun, ..., .dim = 0, .proc = .proc)
}

#' @rdname ply
#' @export
row_ply <- function(.x, .fun, ..., .proc = NULL) {
  .proc$arg <- 'd2d'
  uj::ply(.x, .fun, ..., .dim = 1, .proc = .proc)
}

#' @rdname ply
#' @export
col_ply <- function(.x, .fun, ..., .proc = NULL) {
  .proc$arg <- 'd2d'
  uj::ply(.x, .fun, ..., .dim = 2, .proc = .proc)
}

#' @rdname ply
#' @export
dim_ply <- function(.x, .fun, ..., .proc = NULL) {
  nd <- base::length(base::dim(.x))
  d <- uj::f0(nd < 2, 0, 1:nd)
  uj::ply(.x, .fun, ..., .dim = d, .proc = .proc)
}

#' @rdname ply
#' @export
vls_ply <- function(.x, .fun, ..., .proc = NULL) {
  .proc$arg <- 'pop_vls'
  uj::ply(.x, .fun, ..., .dim = 0, .proc = .proc)
}
