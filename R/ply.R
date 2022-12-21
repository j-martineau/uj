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
#'   \cr `mvcply`   \tab Applies `fun` to elements of \link[=atm_mvc]{atomic multivec} `x`.
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
#'     **Name** \tab   **Value** \tab   **Instructions**
#'   \cr `$arg` \tab   `'XXX'`   \tab   Checks `x` for match to \link[=ppp]{spec} `'XXX'`.
#'   \cr `$out` \tab   `'XXX'`   \tab   Checks result for match to spec `'XXX'`.
#'   \cr `$agg` \tab   `'nor'`   \tab   Inspect result for `0` `TRUE` values.
#'   \cr `$agg` \tab   `'one'`   \tab   Inspect result for `1` `TRUE` values.
#'   \cr `$agg` \tab   `'two'`   \tab   Inspect result for `2+` `TRUE` values.
#'   \cr `$agg` \tab   `'any'`   \tab   Inspect result for *any* `TRUE` values.
#'   \cr `$agg` \tab   `'all'`   \tab   Inspect result for *only* `TRUE` values.
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
#' @param proc. `NULL` or a list of named elements with processing instructions. See *the `proc` argument* section.
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
  if (length(x) == 0) {stop("[x] is empty.")}
  vnames <- c("a1", "a2", "s", "na", "arg", "out", "agg")
  vaggs <- c("nor", "any", "all", "one", "two")
  names <- names(proc.)
  procn <- length(proc.)
  namen <- length(names)
  namev <- f0(procn == 0, T, procn == namen & all(names %in% vnames))
  d0 <- isEQ(dim., 0)
  s <- f0(idef(proc.$s), proc.$s, F)
  a1 <- f0(idef(proc.$a1), proc.$a1, F)
  a2 <- f0(idef(proc.$a1), proc.$a2, F)
  na <- f0(idef(proc.$na), proc.$na, F)
  agg <- proc.$agg
  arg <- proc.$arg
  out <- proc.$out
  ok.dim <- d0 | cmp_psw_vec(dim.)
  ok.xdim <- f0(d0 | !ok.dim, T, allIN(dim., 1:length(dim(x))))
  ok.proc <- f0(inll(proc.), T, is.list(proc.) & namev)
  ok.na <- f0(isLG(na), T, isEQ(na, "err"))
  ok.agg <- f0(inll(agg), T, isIN(agg, vaggs))
  ok.arg <- f0(inll(arg), T, is_prop_spec(arg))
  ok.out <- f0(inll(out), T, is_prop_spec(out))
  errs <- c(f0(ifun(fun), NULL, "[fun] is not a function or the name of function."),
            f0(ok.dim   , NULL, "[dim.] must be 0 or a complete positive whole-number vec (?cmp_psw_vec)."),
            f0(ok.xdim  , NULL, "[dim.] contains a value larger than the number of defined dimensions of [x]."),
            f0(ok.proc  , NULL, "Elements of [proc.] must be uniquely named with names from c('a1', 'a2', 's', 'na', 'arg', 'out')."),
            f0(ok.na    , NULL, "When supplied, [proc.$na] must be TRUE, FALSE, NA, or 'err'."),
            f0(ok.agg   , NULL, "When supplied, [proc.$agg] must be 'nor', 'any', 'all', 'one', or 'two'."),
            f0(ok.arg   , NULL, "When supplied, [proc.$arg] must be a valid property specification as validated by is_valid_spec(.)."),
            f0(ok.out   , NULL, "When supplied, [proc.$out] must be a valid property specification as validated by is_valid_spec(.)."),
            f0(isTF(s)  , NULL, "When supplied, [proc.$s] must be TRUE or FALSE."),
            f0(isTF(a1) , NULL, "When supplied, [proc.$a1] must be TRUE or FALSE."),
            f0(isTF(a2) , NULL, "When supplied, [proc.$a2] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (a1) {x <- av(x)}
  if (idef(arg)) {if (!ippp(x, arg)) {stop(.errs(p0("[x] does not match [proc.$arg = '", arg, "'].")))}}
  if (isEQ(dim., 0)) {
    x <- f0(iarr(x) | is.data.frame(x), apply(x, 1:length(dim(x)), fun, ...),
            f0(inll(x) | ivec(x), sapply(x, fun, ...),
               f0(ivls(x), lapply(x, fun, ...), x)))
  } else {x <- apply(x, dim., fun, ...)}
  if (s) {x <- simplify2array(x)}
  if (a2) {x <- av(x)}
  if (idef(out)) {if (!ippp(x, out)) {stop(.errs(p0("[x] does not match [proc.$out = '", out, "'].")))}}
  if (idef(agg)) {
    err <- isEQ(na, 'err')
    errs <- c(f0(ilgl(x)              , NULL, "[proc.$agg] is not NULL, but results of applying [fun] are not of mode logical."),
              f0(!err | !any(is.na(x)), NULL, "Applying [fun] produced NA values, but [proc.$na = 'err']."))
    if (!is.null(errs)) {stop(.errs(errs))}
    if (!err) {x[is.na(x)] <- na}
    x <- f0(agg == "nor", norT(x), f0(agg == "any", anyT(x), f0(agg == "all", allT(x), f0(agg == "one", oneT(x), f0(agg == "two", twoT(x), x)))))
  }
  x
}

#' @rdname ply
#' @export
norply <- function(x, fun, ..., dim. = 0, proc. = NULL) {
  proc.$agg <- "nor"
  ply(x, fun, ..., dim. = dim., proc. = proc.)
}

#' @rdname ply
#' @export
anyply <- function(x, fun, ..., dim. = 0, proc. = NULL) {
  proc.$agg <- "any"
  ply(x, fun, ..., dim. = dim., proc. = proc.)
}

#' @rdname ply
#' @export
allply <- function(x, fun, ..., dim. = 0, proc. = NULL) {
  proc.$agg <- "all"
  ply(x, fun, ..., dim. = dim., proc. = proc.)
}

#' @rdname ply
#' @export
oneply <- function(x, fun, ..., dim. = 0, proc. = NULL) {
  proc.$agg <- "one"
  ply(x, fun, ..., dim. = dim., proc. = proc.)
}

#' @rdname ply
#' @export
twoply <- function(x, fun, ..., dim. = 0, proc. = NULL) {
  proc.$agg <- "two"
  ply(x, fun, ..., dim. = dim., proc. = proc.)
}

#' @rdname ply
#' @export
atmply <- function(x, fun, ..., proc. = NULL) {
  proc.$a1 <- TRUE
  ply(x, fun, ..., dim. = 0, proc. = proc.)
}

#' @rdname ply
#' @export
mvcply <- function(x, fun, ..., proc. = NULL) {
  proc.$arg <- 'mvc'
  ply(x, fun, ..., dim. = 0, proc. = proc.)
}

#' @rdname ply
#' @export
vecply <- function(x, fun, ..., proc. = NULL) {
  proc.$arg <- 'vec'
  ply(x, fun, ..., dim. = 0, proc. = proc.)
}

#' @rdname ply
#' @export
rowply <- function(x, fun, ..., proc. = NULL) {
  proc.$arg <- 'd2D'
  ply(x, fun, ..., dim. = 1, proc. = proc.)
}

#' @rdname ply
#' @export
colply <- function(x, fun, ..., proc. = NULL) {
  proc.$arg <- 'd2D'
  ply(x, fun, ..., dim. = 2, proc. = proc.)
}

#' @rdname ply
#' @export
dimply <- function(x, fun, ..., proc. = NULL) {
  d <- f0(ddd(x) < 2, 0, 1:length(dim(x)))
  ply(x, fun, ..., dim. = d, proc. = proc.)
}

#' @rdname ply
#' @export
vlsply <- function(x, fun, ..., proc. = NULL) {
  proc.$arg <- 'pop_vls'
  ply(x, fun, ..., dim. = 0, proc. = proc.)
}
