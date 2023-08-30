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
#'                `atm_ply`    \tab Apply `.FUN` to \link[=av]{atomized} `.X`.                         \cr
#'                `mvc_ply`    \tab Apply `.FUN` to elements of \link[=atm_mvc]{atomic multivec} `.X`. \cr
#'                `vec_ply`    \tab Apply `.FUN` to elements of \link[=atm_vec]{atomic vec} `.X`.      \cr
#'                `vls_ply`    \tab Apply `.FUN` to elements of \link[=atm_vls]{atomic vlist} `.X`.    \cr   \tab   \cr
#'                `row_ply`    \tab Apply `.FUN` to rows of `.X`.                                      \cr
#'                `col_ply`    \tab Apply `.FUN` to columns of `.X`.                                   \cr
#'                `dim_ply`    \tab Apply `.FUN` to cells of `.X`.                                     \cr   \tab   \cr
#'                `ply`        \tab Generalized `ply` function.                                      \cr   \tab     }
#'  \tabular{l}{  \eqn{^{(1)}} These functions assume that applying `.FUN` produces `'logical'` results.             }
#' @section The `.PROC` argument: When not `NULL`, the `.PROC` argument is an optional list with up to seven named elements, which give processing instructions as follows:
#' \tabular{ll}{  **Name + value**    \tab **Processing instructions**                                  \cr
#'                `$arg = '{spec}'`   \tab Check `.X` for match to \code{\link[=ppp]{spec}}\eqn{^{(1)}}. \cr
#'                `$out = '{spec}'`   \tab Check result for match to `'{spec}'`.\eqn{^{(1)}}            \cr   \tab   \cr
#'                `$agg = 'none'`     \tab Inspect result for `0` `TRUE` values.                        \cr
#'                `$agg = 'one'`      \tab Inspect result for `1` `TRUE` values.                        \cr
#'                `$agg = 'two'`      \tab Inspect result for `2+` `TRUE` values.                       \cr
#'                `$agg = 'any'`      \tab Inspect result for *any* `TRUE` values.                      \cr
#'                `$agg = 'all'`      \tab Inspect result for *only* `TRUE` values.                     \cr   \tab   \cr
#'                `$na = 'err'`       \tab Throw error if result has any `NA` values.                   \cr
#'                `$na = FALSE`       \tab Replace resulting `NA`s with `FALSE`.                        \cr
#'                `$na = TRUE`        \tab Replace resulting `NA`s with `TRUE`.                         \cr   \tab   \cr
#'                `$a1 = TRUE`        \tab \link[=av]{Atomize} `.X`.                                     \cr
#'                `$a2 = TRUE`        \tab Atomize the result.                                          \cr   \tab   \cr
#'                `$s = TRUE`         \tab \link[base:simplify2array]{Simplify} the result.             \cr   \tab     }
#'  \tabular{l}{  \eqn{^{(1)}} `{spec}` is a placeholder for a \link[=is_prop_spec]{valid property spec}.              }
#' @param .X An object to apply `.FUN` to.
#' @param .FUN Function or character scalar name of a function to apply to `.X`.
#' @param ... An arbitrary number of additional arguments to be passed to the function `.FUN`.
#' @param .DIM A \link[=cmp_nnw_vec]{complete non-negative whole-number vec} giving dimension(s) of `.X` to apply the function `.FUN` to (`0` indicates applying to elements of a vector or \link[=VLS]{vlist} vs. applying to every cell for arrays and data.frames).
#' @param .PROC Either `NULL` or a list of named elements with processing instructions. See *the* `.PROC` *argument*.
#' @examples
#' egNumVec <- 1:5
#' egNumMat <- matrix(1:25, nrow = 5)
#' egChrDtf <- dtf(az = letters[1:10], AZ = LETTERS[1:10], nm = as.character(0:9))
#' egChrVls <- list(az = letters, AZ = LETTERS, nm = as.character(0:9))
#' egComp <- function(.X, comp, value) {if (comp == "<") {.X < value} else {.X > value}}
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
#' two_ply(egNumMat, egComp, ">", value = 15, .DIM = 2)
#' dim_ply(egNumMat, egComp, ">", value = 15)
#' dim_ply(egChrDtf, toupper)
#' row_ply(egNumMat, sum)
#' col_ply(egChrDtf, paste0, collapse = "")
#' vls_ply(egChrVls, paste0, collapse = "")
#' @export
ply <- function(.X, .FUN, ..., .DIM = 0, .PROC = NULL) {
  if (base::length(.X) == 0) {uj::stopperr("[.X] is empty.", .PKG = "uj")}
  vNames <- base::c("a1", "a2", "s", "na", "arg", "out", "agg")
  vAggs <- base::c("none", "any", "all", "one", "two")
  Names <- base::names(.PROC)
  ProcN <- base::length(.PROC)
  NameN <- base::length(Names)
  NameV <- uj::f0(ProcN == 0, T, ProcN == NameN & base::all(Names %in% vNames))
  s <- uj::f0(!base::is.null(.PROC$s), .PROC$s, F)
  a1 <- uj::f0(!base::is.null(.PROC$a1), .PROC$a1, F)
  a2 <- uj::f0(!base::is.null(.PROC$a1), .PROC$a2, F)
  na <- uj::f0(!base::is.null(.PROC$na), .PROC$na, F)
  agg <- .PROC$agg
  arg <- .PROC$arg
  out <- .PROC$out
  D0 <- base::ifelse(uj::cmp_nnw_scl(.DIM), .DIM == 0, F)
  OkDim <- D0 | uj::cmp_psw_vec(.DIM)
  Ok.Xdim <- uj::f0(D0 | !OkDim, T, base::all(.DIM %in% 1:base::length(base::dim(.X))))
  OkProc <- uj::f0(base::is.null(.PROC), T, base::is.list(.PROC) & NameV)
  OkNA <- uj::f0(uj:::.lgl_scl(na), T, uj::f0(uj:::.cmp_chr_scl(na), na == "err", F))
  OkAgg <- uj::f0(base::is.null(agg), T, agg %in% vAggs)
  OkArg <- uj::f0(base::is.null(arg), T, uj::is_prop_spec(arg))
  OkOut <- uj::f0(base::is.null(out), T, uj::is_prop_spec(out))
  OkA1 <- uj::f0(base::is.null(a1), T, uj::cmp_lgl_scl(a1))
  OkA2 <- uj::f0(base::is.null(a1), T, uj::cmp_lgl_scl(a2))
  OkS <- uj::f0(base::is.null(s), T, uj::cmp_lgl_scl(s))
  Errors <- NULL
  if (!uj:::..FUN(.FUN)) {Errors <- base::c(Errors, "[.FUN] is not a function or the name of function.")}
  if (!Ok.Xdim) {Errors <- base::c(Errors, "[.DIM] contains a value larger than the number of defined dimensions of [.X].")}
  if (!OkProc) {Errors <- base::c(Errors, "Elements of [.PROC] must be uniquely named with names from c('a1', 'a2', 's', 'na', 'arg', 'out').")}
  if (!OkAgg) {Errors <- base::c(Errors, "When supplied, [.PROC$agg] must be 'none', 'any', 'all', 'one', or 'two'.")}
  if (!OkArg) {Errors <- base::c(Errors, "When supplied, [.PROC$arg] must be a valid property specification as validated by isVALIDspec(.).")}
  if (!OkOut) {Errors <- base::c(Errors, "When supplied, [.PROC$out] must be a valid property specification as validated by isVALIDspec(.).")}
  if (!OkDim) {Errors <- base::c(Errors, "[.DIM] must be 0 or a complete positive whole-number vec (?cmp_psw_vec).")}
  if (!OkNA) {Errors <- base::c(Errors, "When supplied, [.PROC$na] must be TRUE, FALSE, NA, or 'err'.")}
  if (!OkA1) {Errors <- base::c(Errors, "When supplied, [.PROC$a1] must be TRUE or FALSE.")}
  if (!OkA2) {Errors <- base::c(Errors, "When supplied, [.PROC$a2] must be TRUE or FALSE.")}
  if (!OkS) {Errors <- base::c(Errors, "When supplied, [.PROC$s] must be TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  if (a1) {.X <- uj::av(.X)}
  if (!base::is.null(arg)) {OkMatch <- uj::PPP(.X, arg)} else {OkMatch <- FALSE}
  if (!OkMatch) {uj::stopperr("[.X] does not match [.PROC$arg = '", arg, "'].", .PKG = "uj")}
  if (D0) {
    if (base::is.arrary(.X) | base::is.data.frame(.X)) {.X <- base::apply(.X, 1:base::length(base::dim(.X)), .FUN, ...)}
    else if (base::is.null(.X) | uj:::.VEC(.X)) {.X <- base::sapply(.X, .FUN, ...)}
    else if (base::is.list(.X)) {.X <- base::lapply(.X, .FUN, ...)}
    else {.X}
  }
  else {.X <- base::apply(.X, .DIM, .FUN, ...)}
  if (s) {.X <- base::simplify2array(.X)}
  if (a2) {.X <- uj::av(.X)}
  if (!uj::f0(base::is.null(out), T, uj::PPP(.X, out))) {uj::stopperr("[.X] does not match [.PROC$out = '", out, "'].", .PKG = "uj")}
  if (!base::is.null(agg)) {
    Err <- uj::f0(uj:::.cmp_chr_scl(na), na == "err", F)
    Errors <- NULL
    if (!uj:::.LGL(.X)) {Errors <- base::c(Errors, "[.PROC$agg] is not NULL, but results of applying [.FUN] are not of mode logical.")}
    if (err & base::any(base::is.na(uj::av(.X)))) {Errors <- base::c(Errors, "Applying [.FUN] produced NA values, but [.PROC$na = 'err'].")}
    if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
    if (!Err) {.X[base::is.na(.X)] <- na}
    if      (agg == "all" ) {base::length(which(.X)) == length(.X)}
    else if (agg == "none") {base::length(which(.X)) == 0}
    else if (agg == "one" ) {base::length(which(.X)) == 1}
    else if (agg == "two" ) {base::length(which(.X)) == 2}
    else if (agg == "any" ) {base::length(which(.X)) > 0}
    else if (agg == "some") {base::length(which(.X)) > 1}
    else if (agg == "many") {base::length(which(.X)) > 2}
    else {.X}
  }
  .X
}

#' @rdname ply
#' @export
none_ply <- function(.X, .FUN, ..., .DIM = 0, .PROC = NULL) {
  .PROC <- "none"
  uj::ply(.X, .FUN, ..., .DIM = .DIM, .PROC = .PROC)
}

#' @rdname ply
#' @export
any_ply <- function(.X, .FUN, ..., .DIM = 0, .PROC = NULL) {
  .PROC$agg <- "any"
  uj::ply(.X, .FUN, ..., .DIM = .DIM, .PROC = .PROC)
}

#' @rdname ply
#' @export
some_ply <- function(.X, .FUN, ..., .DIM = 0, .PROC = NULL) {
  .PROC$agg <- "some"
  uj::ply(.X, .FUN, ..., .DIM = .DIM, .PROC = .PROC)
}

#' @rdname ply
#' @export
many_ply <- function(.X, .FUN, ..., .DIM = 0, .PROC = NULL) {
  .PROC$agg <- "many"
  uj::ply(.X, .FUN, ..., .DIM = .DIM, .PROC = .PROC)
}

#' @rdname ply
#' @export
all_ply <- function(.X, .FUN, ..., .DIM = 0, .PROC = NULL) {
  .PROC$agg <- "all"
  uj::ply(.X, .FUN, ..., .DIM = .DIM, .PROC = .PROC)
}

#' @rdname ply
#' @export
one_ply <- function(.X, .FUN, ..., .DIM = 0, .PROC = NULL) {
  .PROC$agg <- "one"
  uj::ply(.X, .FUN, ..., .DIM = .DIM, .PROC = .PROC)
}

#' @rdname ply
#' @export
two_ply <- function(.X, .FUN, ..., .DIM = 0, .PROC = NULL) {
  .PROC$agg <- "two"
  uj::ply(.X, .FUN, ..., .DIM = .DIM, .PROC = .PROC)
}

#' @rdname ply
#' @export
atm_ply <- function(.X, .FUN, ..., .PROC = NULL) {
  .PROC$a1 <- TRUE
  uj::ply(.X, .FUN, ..., .DIM = 0, .PROC = .PROC)
}

#' @rdname ply
#' @export
mvc_ply <- function(.X, .FUN, ..., .PROC = NULL) {
  .PROC$arg <- 'mvc'
  uj::ply(.X, .FUN, ..., .DIM = 0, .PROC = .PROC)
}

#' @rdname ply
#' @export
vec_ply <- function(.X, .FUN, ..., .PROC = NULL) {
  .PROC$arg <- 'vec'
  uj::ply(.X, .FUN, ..., .DIM = 0, .PROC = .PROC)
}

#' @rdname ply
#' @export
row_ply <- function(.X, .FUN, ..., .PROC = NULL) {
  .PROC$arg <- 'd2d'
  uj::ply(.X, .FUN, ..., .DIM = 1, .PROC = .PROC)
}

#' @rdname ply
#' @export
col_ply <- function(.X, .FUN, ..., .PROC = NULL) {
  .PROC$arg <- 'd2d'
  uj::ply(.X, .FUN, ..., .DIM = 2, .PROC = .PROC)
}

#' @rdname ply
#' @export
dim_ply <- function(.X, .FUN, ..., .PROC = NULL) {
  nd <- base::length(base::dim(.X))
  d <- uj::f0(nd < 2, 0, 1:nd)
  uj::ply(.X, .FUN, ..., .DIM = d, .PROC = .PROC)
}

#' @rdname ply
#' @export
vls_ply <- function(.X, .FUN, ..., .PROC = NULL) {
  .PROC$arg <- 'pop_vls'
  uj::ply(.X, .FUN, ..., .DIM = 0, .PROC = .PROC)
}
