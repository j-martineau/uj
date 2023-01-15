#' @encoding UTF-8
#' @family meta
#' @title Variations on `apply` functions
#' @description \tabular{rl}{
#'       `norply`   \tab Checks for `0` resulting `TRUE` values\eqn{^a}.
#'   \cr `oneply`   \tab Checks for `1` resulting `TRUE` values\eqn{^a}.
#'   \cr `twoply`   \tab Checks for `2+` resulting `TRUE` values\eqn{^a}.
#'   \cr `anyply`   \tab Checks for *any* resulting `TRUE` values\eqn{^a}.
#'   \cr `allply`   \tab Checks for *only* resulting `TRUE` values\eqn{^a}.
#'   \cr `atmply`   \tab Applies `fun` to \link[=av]{atomized} `x`.
#'   \cr `mvcply`   \tab Applies `fun` to elements of \link[=atm_mvc]{atomic multivec} `x`.
#'   \cr `vecply`   \tab Applies `fun` to elements of \link[=atm_vec]{atomic vec} `x`.
#'   \cr `vlsply`   \tab Applies `fun` to elements of \link[=atm_vls]{atomic vlist} `x`.
#'   \cr `rowply`   \tab Applies `fun` to rows of `x`.
#'   \cr `colply`   \tab Applies `fun` to columns of `x`.
#'   \cr `dimply`   \tab Applies `fun` to cells of `x`.
#'   \cr    `ply`   \tab Generalized `ply` function.
#' }
#' \eqn{^{a.}} These functions assume that applying `fun` produces `'logical'` results.
#' @section The `proc` Argument: When not `NULL`, the `proc` argument is an optional list with up to seven named elements, which give processing instructions as follows:
#' \tabular{rll}{
#'       *Name* \tab   *Value*   \tab   *Instructions*
#'   \cr `$arg` \tab   `'XXX'`   \tab   Checks `x` for match to \link[=ppp]{spec} `'XXX'`.
#'   \cr `$out` \tab   `'XXX'`   \tab   Checks result for match to spec `'XXX'`.
#'   \cr `$agg` \tab   `'nor'`   \tab   Inspect result for `0` `TRUE` values.
#'   \cr `$agg` \tab   `'one'`   \tab   Inspect result for `1` `TRUE` values.
#'   \cr `$agg` \tab   `'two'`   \tab   Inspect result for `2+` `TRUE` values.
#'   \cr `$agg` \tab   `'any'`   \tab   Inspect result for *any* `TRUE` values.
#'   \cr `$agg` \tab   `'all'`   \tab   Inspect result for *only* `TRUE` values.
#'   \cr  `$na` \tab   `'err'`   \tab   Throw error if result has any `NA`'s.
#'   \cr  `$na` \tab   `FALSE`   \tab   Replace resulting `NA`'s with `FALSE`.
#'   \cr  `$na` \tab   `TRUE`    \tab   Replace resulting `NA`'s with `TRUE`.
#'   \cr  `$a1` \tab   `TRUE`    \tab   \link[=av]{Atomize} `x`.
#'   \cr  `$a2` \tab   `TRUE`    \tab   Atomize the result.
#'   \cr   `$s` \tab   `TRUE`    \tab   \link[base:simplify2array]{Simplify} the result.
#' }
#' @param x An object to apply `fun` to.
#' @param fun Function or character scalar name of a function to apply to `x`.
#' @param ... An arbitrary number of additional arguments to be passed to the function `fun`.
#' @param dim. A \link[=cmp_nnw_vec]{complete non-negative whole-number vec} giving dimension(s) of `x` to apply the function `fun` to (`0` indicates applying to elements of a vector or \link[=ivls]{vlist} vs. applying to every cell for arrays and data.frames).
#' @param proc. `NULL` or a list of named elements with processing instructions. See *the* `proc` *argument* section.
#' @examples
#' NumVec. <- 1:5
#' NumMat. <- matrix(1:25, nrow = 5)
#' ChrDtf. <- dtf(az = letters[1:10], AZ = LETTERS[1:10], nm = as.character(0:9))
#' ChrVls. <- list(az = letters, AZ = LETTERS, nm = as.character(0:9))
#' Comp. <- function(x, comp, value) {if (comp == "<") {x < value} else {x > value}}
#'
#' NumVec.
#' NumMat.
#' ChrDtf.
#' ChrVls.
#'
#' norply(NumVec., Comp., ">", value = 6)
#' norply(NumVec., Comp., ">", value = 4)
#'
#' anyply(NumVec., Comp., ">", value = 6)
#' anyply(NumVec., Comp., ">", value = 4)
#'
#' allply(NumVec., Comp., "<", value = 6)
#' allply(NumVec., Comp., "<", value = 4)
#'
#' oneply(NumVec., Comp., ">", value = 6)
#' oneply(NumVec., Comp., ">", value = 4)
#'
#' twoply(NumVec., Comp., ">", value = 2)
#' twoply(NumVec., Comp., ">", value = 5)
#' twoply(NumMat., Comp., ">", value = 15, dim. = 2)
#'
#' dimply(NumMat., Comp., ">", value = 15)
#' dimply(ChrDtf., toupper)
#'
#' rowply(NumMat., sum)
#' colply(ChrDtf., paste0, collapse = "")
#'
#' vlsply(ChrVls., paste0, collapse = "")
#' @export
ply <- function(x, fun, ..., dim. = 0, proc. = NULL) {
  if (base::length(x) == 0) {stop("[x] is empty.")}
  vnames <- base::c("a1", "a2", "s", "na", "arg", "out", "agg")
  vaggs <- base::c("nor", "any", "all", "one", "two")
  names <- base::names(proc.)
  procn <- base::length(proc.)
  namen <- base::length(names)
  namev <- uj::f0(procn == 0, T, procn == namen & base::all(names %in% vnames))
  d0 <- uj::isEQ(dim., 0)
  s <- uj::f0(uj::idef(proc.$s), proc.$s, F)
  a1 <- uj::f0(uj::idef(proc.$a1), proc.$a1, F)
  a2 <- uj::f0(uj::idef(proc.$a1), proc.$a2, F)
  na <- uj::f0(uj::idef(proc.$na), proc.$na, F)
  agg <- proc.$agg
  arg <- proc.$arg
  out <- proc.$out
  ok.dim <- d0 | uj::cmp_psw_vec(dim.)
  ok.xdim <- uj::f0(d0 | !ok.dim, T, uj::allIN(dim., 1:base::length(base::dim(x))))
  ok.proc <- uj::f0(uj::inll(proc.), T, base::is.list(proc.) & namev)
  ok.na <- uj::f0(uj::isLG(na), T, uj::isEQ(na, "err"))
  ok.agg <- uj::f0(uj::inll(agg), T, uj::isIN(agg, vaggs))
  ok.arg <- uj::f0(uj::inll(arg), T, uj::is_prop_spec(arg))
  ok.out <- uj::f0(uj::inll(out), T, uj::is_prop_spec(out))
  errs <- base::c(uj::f0(uj::ifun(fun), NULL, "[fun] is not a function or the name of function."),
            uj::f0(ok.dim       , NULL, "[dim.] must be 0 or a complete positive whole-number vec (?cmp_psw_vec)."),
            uj::f0(ok.xdim      , NULL, "[dim.] contains a value larger than the number of defined dimensions of [x]."),
            uj::f0(ok.proc      , NULL, "Elements of [proc.] must be uniquely named with names from c('a1', 'a2', 's', 'na', 'arg', 'out')."),
            uj::f0(ok.na        , NULL, "When supplied, [proc.$na] must be TRUE, FALSE, NA, or 'err'."),
            uj::f0(ok.agg       , NULL, "When supplied, [proc.$agg] must be 'nor', 'any', 'all', 'one', or 'two'."),
            uj::f0(ok.arg       , NULL, "When supplied, [proc.$arg] must be a valid property specification as validated by is_valid_spec(.)."),
            uj::f0(ok.out       , NULL, "When supplied, [proc.$out] must be a valid property specification as validated by is_valid_spec(.)."),
            uj::f0(uj::isTF(s)  , NULL, "When supplied, [proc.$s] must be TRUE or FALSE."),
            uj::f0(uj::isTF(a1) , NULL, "When supplied, [proc.$a1] must be TRUE or FALSE."),
            uj::f0(uj::isTF(a2) , NULL, "When supplied, [proc.$a2] must be TRUE or FALSE."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  if (a1) {x <- uj::av(x)}
  if (uj::idef(arg)) {if (!uj::ippp(x, arg)) {stop(uj::format_errs(pkg = "uj", uj::p0("[x] does not match [proc.$arg = '", arg, "'].")))}}
  if (uj::isEQ(dim., 0)) {
    x <- uj::f0(uj::iarr(x) | base::is.data.frame(x), base::apply(x, 1:base::length(base::dim(x)), fun, ...),
            uj::f0(uj::inll(x) | uj::ivec(x), base::sapply(x, fun, ...),
               uj::f0(uj::ivls(x), base::lapply(x, fun, ...), x)))
  } else {x <- base::apply(x, dim., fun, ...)}
  if (s) {x <- base::simplify2array(x)}
  if (a2) {x <- uj::av(x)}
  if (uj::idef(out)) {if (!uj::ippp(x, out)) {stop(uj::format_errs(pkg = "uj", uj::p0("[x] does not match [proc.$out = '", out, "'].")))}}
  if (uj::idef(agg)) {
    err <- uj::isEQ(na, 'err')
    errs <- base::c(uj::f0(uj::ilgl(x)                      , NULL, "[proc.$agg] is not NULL, but results of applying [fun] are not of mode logical."),
                    uj::f0(!err | !base::any(base::is.na(x)), NULL, "Applying [fun] produced NA values, but [proc.$na = 'err']."))
    if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
    if (!err) {x[base::is.na(x)] <- na}
    x <- uj::f0(agg == "nor", uj::norT(x), uj::f0(agg == "any", uj::anyT(x), uj::f0(agg == "all", uj::allT(x), uj::f0(agg == "one", uj::oneT(x), uj::f0(agg == "two", uj::twoT(x), x)))))
  }
  x
}

#' @rdname ply
#' @export
norply <- function(x, fun, ..., dim. = 0, proc. = NULL) {
  proc.$agg <- "nor"
  uj::ply(x, fun, ..., dim. = dim., proc. = proc.)
}

#' @rdname ply
#' @export
anyply <- function(x, fun, ..., dim. = 0, proc. = NULL) {
  proc.$agg <- "any"
  uj::ply(x, fun, ..., dim. = dim., proc. = proc.)
}

#' @rdname ply
#' @export
allply <- function(x, fun, ..., dim. = 0, proc. = NULL) {
  proc.$agg <- "all"
  uj::ply(x, fun, ..., dim. = dim., proc. = proc.)
}

#' @rdname ply
#' @export
oneply <- function(x, fun, ..., dim. = 0, proc. = NULL) {
  proc.$agg <- "one"
  uj::ply(x, fun, ..., dim. = dim., proc. = proc.)
}

#' @rdname ply
#' @export
twoply <- function(x, fun, ..., dim. = 0, proc. = NULL) {
  proc.$agg <- "two"
  uj::ply(x, fun, ..., dim. = dim., proc. = proc.)
}

#' @rdname ply
#' @export
atmply <- function(x, fun, ..., proc. = NULL) {
  proc.$a1 <- TRUE
  uj::ply(x, fun, ..., dim. = 0, proc. = proc.)
}

#' @rdname ply
#' @export
mvcply <- function(x, fun, ..., proc. = NULL) {
  proc.$arg <- 'mvc'
  uj::ply(x, fun, ..., dim. = 0, proc. = proc.)
}

#' @rdname ply
#' @export
vecply <- function(x, fun, ..., proc. = NULL) {
  proc.$arg <- 'vec'
  uj::ply(x, fun, ..., dim. = 0, proc. = proc.)
}

#' @rdname ply
#' @export
rowply <- function(x, fun, ..., proc. = NULL) {
  proc.$arg <- 'd2D'
  uj::ply(x, fun, ..., dim. = 1, proc. = proc.)
}

#' @rdname ply
#' @export
colply <- function(x, fun, ..., proc. = NULL) {
  proc.$arg <- 'd2D'
  uj::ply(x, fun, ..., dim. = 2, proc. = proc.)
}

#' @rdname ply
#' @export
dimply <- function(x, fun, ..., proc. = NULL) {
  d <- uj::f0(uj::ddd(x) < 2, 0, 1:base::length(base::dim(x)))
  uj::ply(x, fun, ..., dim. = d, proc. = proc.)
}

#' @rdname ply
#' @export
vlsply <- function(x, fun, ..., proc. = NULL) {
  proc.$arg <- 'pop_vls'
  uj::ply(x, fun, ..., dim. = 0, proc. = proc.)
}
