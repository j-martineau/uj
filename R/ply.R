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
#'                `atm_ply`    \tab Apply `FUN` to \link[=av]{atomized} `x`.                         \cr
#'                `mvc_ply`    \tab Apply `FUN` to elements of \link[=atm_mvc]{atomic multivec} `x`. \cr
#'                `vec_ply`    \tab Apply `FUN` to elements of \link[=atm_vec]{atomic vec} `x`.      \cr
#'                `vls_ply`    \tab Apply `FUN` to elements of \link[=atm_vls]{atomic vlist} `x`.    \cr   \tab   \cr
#'                `row_ply`    \tab Apply `FUN` to rows of `x`.                                      \cr
#'                `col_ply`    \tab Apply `FUN` to columns of `x`.                                   \cr
#'                `dim_ply`    \tab Apply `FUN` to cells of `x`.                                     \cr   \tab   \cr
#'                `ply`        \tab Generalized `ply` function.                                      \cr   \tab     }
#'  \tabular{l}{  \eqn{^{(1)}} These functions assume that applying `FUN` produces `'logical'` results.             }
#' @section The `PROC` argument: When not `NULL`, the `PROC` argument is an optional list with up to seven named elements, which give processing instructions as follows:
#' \tabular{ll}{  **Name + value**    \tab **Processing instructions**                                  \cr
#'                `$arg = '{spec}'`   \tab Check `x` for match to \code{\link[=ppp]{spec}}\eqn{^{(1)}}. \cr
#'                `$out = '{spec}'`   \tab Check result for match to `'{spec}'`.\eqn{^{(1)}}            \cr   \tab   \cr
#'                `$agg = 'none'`     \tab Inspect result for `0` `TRUE` values.                        \cr
#'                `$agg = 'one'`      \tab Inspect result for `1` `TRUE` values.                        \cr
#'                `$agg = 'two'`      \tab Inspect result for `2+` `TRUE` values.                       \cr
#'                `$agg = 'any'`      \tab Inspect result for *any* `TRUE` values.                      \cr
#'                `$agg = 'all'`      \tab Inspect result for *only* `TRUE` values.                     \cr   \tab   \cr
#'                `$na = 'err'`       \tab Throw error if result has any `NA` values.                   \cr
#'                `$na = FALSE`       \tab Replace resulting `NA`s with `FALSE`.                        \cr
#'                `$na = TRUE`        \tab Replace resulting `NA`s with `TRUE`.                         \cr   \tab   \cr
#'                `$a1 = TRUE`        \tab \link[=av]{Atomize} `x`.                                     \cr
#'                `$a2 = TRUE`        \tab Atomize the result.                                          \cr   \tab   \cr
#'                `$s = TRUE`         \tab \link[base:simplify2array]{Simplify} the result.             \cr   \tab     }
#'  \tabular{l}{  \eqn{^{(1)}} `{spec}` is a placeholder for a \link[=is_prop_spec]{valid property spec}.              }
#' @param x An object to apply `FUN` to.
#' @param FUN Function or character scalar name of a function to apply to `x`.
#' @param ... An arbitrary number of additional arguments to be passed to the function `FUN`.
#' @param DIM A \link[=cmp_nnw_vec]{complete non-negative whole-number vec} giving dimension(s) of `x` to apply the function `FUN` to (`0` indicates applying to elements of a vector or \link[=VLS]{vlist} vs. applying to every cell for arrays and data.frames).
#' @param PROC Either `NULL` or a list of named elements with processing instructions. See *the* `PROC` *argument*.
#' @examples
#' egNumVec <- 1:5
#' egNumMat <- matrix(1:25, nrow = 5)
#' egChrDtf <- dtf(az = letters[1:10], AZ = LETTERS[1:10], nm = as.character(0:9))
#' egChrVls <- list(az = letters, AZ = LETTERS, nm = as.character(0:9))
#' egComp <- function(x, comp, value) {if (comp == "<") {x < value} else {x > value}}
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
#' two_ply(egNumMat, egComp, ">", value = 15, DIM = 2)
#' dim_ply(egNumMat, egComp, ">", value = 15)
#' dim_ply(egChrDtf, toupper)
#' row_ply(egNumMat, sum)
#' col_ply(egChrDtf, paste0, collapse = "")
#' vls_ply(egChrVls, paste0, collapse = "")
#' @export
ply <- function(x, FUN, ..., DIM = 0, PROC = NULL) {
  if (base::length(x) == 0) {uj::stopperr("[x] is empty.", PKG = "uj")}
  vnames <- base::c("a1", "a2", "s", "na", "arg", "out", "agg")
  vaggs <- base::c("none", "any", "all", "one", "two")
  names <- base::names(PROC)
  procn <- base::length(PROC)
  namen <- base::length(names)
  namev <- uj::f0(procn == 0, T, procn == namen & base::all(names %in% vnames))
  s <- uj::f0(!base::is.null(PROC$s), PROC$s, F)
  a1 <- uj::f0(!base::is.null(PROC$a1), PROC$a1, F)
  a2 <- uj::f0(!base::is.null(PROC$a1), PROC$a2, F)
  na <- uj::f0(!base::is.null(PROC$na), PROC$na, F)
  agg <- PROC$agg
  arg <- PROC$arg
  out <- PROC$out
  d0 <- base::ifelse(uj::cmp_nnw_scl(DIM), DIM == 0, F)
  ok.dim <- d0 | uj::cmp_psw_vec(DIM)
  ok.xdim <- uj::f0(d0 | !ok.dim, T, base::all(DIM %in% 1:base::length(base::dim(x))))
  ok.proc <- uj::f0(base::is.null(PROC), T, uj::is_lst(PROC) & namev)
  ok.na <- uj::f0(uj:::.lgl_scl(na), T, uj::f0(uj:::.cmp_chr_scl(na), na == "err", F))
  ok.agg <- uj::f0(base::is.null(agg), T, agg %in% vaggs)
  ok.arg <- uj::f0(base::is.null(arg), T, uj::is_prop_spec(arg))
  ok.out <- uj::f0(base::is.null(out), T, uj::is_prop_spec(out))
  ok.a1 <- uj::f0(base::is.null(a1), T, uj::cmp_lgl_scl(a1))
  ok.a2 <- uj::f0(base::is.null(a1), T, uj::cmp_lgl_scl(a2))
  ok.s <- uj::f0(base::is.null(s), T, uj::cmp_lgl_scl(s))
  errs <- NULL
  if (!uj:::.FUN(FUN)) {errs <- base::c(errs, "[FUN] is not a function or the name of function.")}
  if (!ok.dim) {errs <- base::c(errs, "[DIM] must be 0 or a complete positive whole-number vec (?cmp_psw_vec).")}
  if (!ok.xdim) {errs <- base::c(errs, "[DIM] contains a value larger than the number of defined dimensions of [x].")}
  if (!ok.proc) {errs <- base::c(errs, "Elements of [PROC] must be uniquely named with names from c('a1', 'a2', 's', 'na', 'arg', 'out').")}
  if (!ok.na) {errs <- base::c(errs, "When supplied, [PROC$na] must be TRUE, FALSE, NA, or 'err'.")}
  if (!ok.agg) {errs <- base::c(errs, "When supplied, [PROC$agg] must be 'none', 'any', 'all', 'one', or 'two'.")}
  if (!ok.arg) {errs <- base::c(errs, "When supplied, [PROC$arg] must be a valid property specification as validated by isVALIDspec(.).")}
  if (!ok.out) {errs <- base::c(errs, "When supplied, [PROC$out] must be a valid property specification as validated by isVALIDspec(.).")}
  if (!ok.s) {errs <- base::c(errs, "When supplied, [PROC$s] must be TRUE or FALSE.")}
  if (!ok.a1) {errs <- base::c(errs, "When supplied, [PROC$a1] must be TRUE or FALSE.")}
  if (!ok.a2) {errs <- base::c(errs, "When supplied, [PROC$a2] must be TRUE or FALSE.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  if (a1) {x <- uj::av(x)}
  if (!base::is.null(arg)) {ok.match <- uj::PPP(x, arg)} else {ok.match <- FALSE}
  if (!ok.match) {uj::stopperr("[x] does not match [PROC$arg = '", arg, "'].", PKG = "uj")}
  if (d0) {
    if (base::is.arrary(x) | base::is.data.frame(x)) {x <- base::apply(x, 1:base::length(base::dim(x)), FUN, ...)}
    else if (base::is.null(x) | uj:::.VEC(x)) {x <- base::sapply(x, FUN, ...)}
    else if (base::is.list(x)) {x <- base::lapply(x, FUN, ...)}
    else {x}
  }
  else {x <- base::apply(x, DIM, FUN, ...)}
  if (s) {x <- base::simplify2array(x)}
  if (a2) {x <- uj::av(x)}
  if (!uj::f0(base::is.null(out), T, uj::PPP(x, out))) {uj::stopperr("[x] does not match [PROC$out = '", out, "'].", PKG = "uj")}
  if (!base::is.null(agg)) {
    err <- uj::f0(uj:::.cmp_chr_scl(na), na == "err", F)
    if (!uj:::.LGL(x)) {errs <- base::c(errs, "[PROC$agg] is not NULL, but results of applying [FUN] are not of mode logical.")}
    if (err & base::any(base::is.na(uj::av(x)))) {errs <- base::c(errs, "Applying [FUN] produced NA values, but [PROC$na = 'err'].")}
    if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
    if (!err) {x[base::is.na(x)] <- na}
    if      (agg == "all" ) {base::length(which(x)) == length(x)}
    else if (agg == "none") {base::length(which(x)) == 0}
    else if (agg == "one" ) {base::length(which(x)) == 1}
    else if (agg == "two" ) {base::length(which(x)) == 2}
    else if (agg == "any" ) {base::length(which(x)) > 0}
    else if (agg == "some") {base::length(which(x)) > 1}
    else if (agg == "many") {base::length(which(x)) > 2}
    else {x}
  }
  x
}

#' @rdname ply
#' @export
none_ply <- function(x, FUN, ..., DIM = 0, PROC = NULL) {
  PROC$agg <- "none"
  uj::ply(x, FUN, ..., DIM = DIM, PROC = PROC)
}

#' @rdname ply
#' @export
any_ply <- function(x, FUN, ..., DIM = 0, PROC = NULL) {
  PROC$agg <- "any"
  uj::ply(x, FUN, ..., DIM = DIM, PROC = PROC)
}

#' @rdname ply
#' @export
some_ply <- function(x, FUN, ..., DIM = 0, PROC = NULL) {
  PROC$agg <- "some"
  uj::ply(x, FUN, ..., DIM = DIM, PROC = PROC)
}

#' @rdname ply
#' @export
many_ply <- function(x, FUN, ..., DIM = 0, PROC = NULL) {
  PROC$agg <- "many"
  uj::ply(x, FUN, ..., DIM = DIM, PROC = PROC)
}

#' @rdname ply
#' @export
all_ply <- function(x, FUN, ..., DIM = 0, PROC = NULL) {
  PROC$agg <- "all"
  uj::ply(x, FUN, ..., DIM = DIM, PROC = PROC)
}

#' @rdname ply
#' @export
one_ply <- function(x, FUN, ..., DIM = 0, PROC = NULL) {
  PROC$agg <- "one"
  uj::ply(x, FUN, ..., DIM = DIM, PROC = PROC)
}

#' @rdname ply
#' @export
two_ply <- function(x, FUN, ..., DIM = 0, PROC = NULL) {
  PROC$agg <- "two"
  uj::ply(x, FUN, ..., DIM = DIM, PROC = PROC)
}

#' @rdname ply
#' @export
atm_ply <- function(x, FUN, ..., PROC = NULL) {
  PROC$a1 <- TRUE
  uj::ply(x, FUN, ..., DIM = 0, PROC = PROC)
}

#' @rdname ply
#' @export
mvc_ply <- function(x, FUN, ..., PROC = NULL) {
  PROC$arg <- 'mvc'
  uj::ply(x, FUN, ..., DIM = 0, PROC = PROC)
}

#' @rdname ply
#' @export
vec_ply <- function(x, FUN, ..., PROC = NULL) {
  PROC$arg <- 'vec'
  uj::ply(x, FUN, ..., DIM = 0, PROC = PROC)
}

#' @rdname ply
#' @export
row_ply <- function(x, FUN, ..., PROC = NULL) {
  PROC$arg <- 'd2d'
  uj::ply(x, FUN, ..., DIM = 1, PROC = PROC)
}

#' @rdname ply
#' @export
col_ply <- function(x, FUN, ..., PROC = NULL) {
  PROC$arg <- 'd2d'
  uj::ply(x, FUN, ..., DIM = 2, PROC = PROC)
}

#' @rdname ply
#' @export
dim_ply <- function(x, FUN, ..., PROC = NULL) {
  nd <- base::length(base::dim(x))
  d <- uj::f0(nd < 2, 0, 1:nd)
  uj::ply(x, FUN, ..., DIM = d, PROC = PROC)
}

#' @rdname ply
#' @export
vls_ply <- function(x, FUN, ..., PROC = NULL) {
  PROC$arg <- 'pop_vls'
  uj::ply(x, FUN, ..., DIM = 0, PROC = PROC)
}
