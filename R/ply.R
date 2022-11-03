#' @name ply
#' @family meta
#' @title Variations on \code{apply} functions.
#' @description Generalized \code{apply(.)} with a wide range of options.
#' @section The \code{proc.} Argument: When not \code{NULL}, the \code{proc.}
#'   argument can be a list with up to seven optional named elements, which give
#'   processing instructions as follows: \tabular{ll}{
#'     NAME-VALUE PAIR   \tab PROCESSING INSTRUCTION                         \cr
#'     \code{s = TRUE}   \tab link[base:simplify2array]{Simplify} the result.\cr
#'     \code{a1 = TRUE}  \tab link[=av]{Atomize} \code{x} first.             \cr
#'     \code{a2 = TRUE}  \tab link[=av]{Atomize} the result.                 \cr
#'     \code{na = TRUE}  \tab Replace resulting \code{NA}s with \code{TRUE}. \cr
#'     \code{na = FALSE} \tab Replace resulting \code{NA}s with \code{FALSE}.\cr
#'     \code{na = 'err'} \tab Throw error if result contains any \code{NA}s. \cr
#'     \code{agg = 'nor'}\tab Does result contain zero \code{TRUE} values?   \cr
#'     \code{agg = 'any'}\tab Does result contain any \code{TRUE} values?    \cr
#'     \code{agg = 'all'}\tab Does result contain only \code{TRUE} values?   \cr
#'     \code{agg = 'one'}\tab Does result contain exactly one \code{TRUE}
#'                            value?                                         \cr
#'     \code{agg = 'two'}\tab Does result contain two+ \code{TRUE} values.   \cr
#'     \code{arg = xxx}  \tab Inspect \code{x.} for properties specified in the
#'                            character vector represented by \code{xxx.} and
#'                            throw an error if they are not met.            \cr
#'     \code{out = xxx}  \tab Inspect the result for properties specified in the
#'                            character vector represented by \code{xx.x} and
#'                            throw an error if they are not met.              }
#' @param x. An object to apply \code{fun.} to.
#' @param fun. A function or name of a function to apply to \code{x.}.
#' @param dim. A non-negative whole number vector giving the dimension(s) of
#'   \code{x.} to apply the function \code{fun.} to (\code{0} indicates applying
#'   to elements of an object, where for arrays, and tibbles, this means
#'   applying to every cell).
#' @param ... An arbitrary number of additional arguments to be passed to the
#'   function \code{fun.}.
#' @param proc. \code{NULL} or a list of named elements with processing
#'   instructions. See section \strong{The \code{proc.} Argument}.
#' @export
ply <- function(x., fun., dim., ..., proc. = NULL) {
  if (length(x.) == 0) {stop("\n • [x.] is empty.")}
  vnames. <- c("a1", "a2", "s", "na", "arg", "out", "agg")
  vaggs. <- c("nor", "any", "all", "one", "two")
  names. <- names(proc.)
  procn. <- length(proc.)
  namen. <- length(names.)
  namev. <- f0(procn. == 0, T, procn. == namen. & all(names. %in% vnames.))
  d0.    <- isEQ(dim., 0)
  a1.    <- f0(idef(proc.$a1), proc.$a1, F)
  a2.    <- f0(idef(proc.$a1), proc.$a2, F)
  s.     <- f0(idef(proc.$a ), proc.$a , F)
  na.    <- f0(idef(proc.$na), proc.$na, F)
  agg.   <- proc.$agg
  arg.   <- proc.$arg
  out.   <- proc.$out
  vd.    <- f0(d0., T, cmp_psw_vec(dim.))
  vdx.   <- f0(d0. | !vd., T, allIN(dim., 1:length(dim(x.))))
  vp.    <- f0(inll(proc.), T, is.list(proc.) & namev.)
  va1.   <- isTF(a1.)
  va2.   <- isTF(a2.)
  vs.    <- isTF(s.)
  vna.   <- f0(isLG(na. ), T, isEQ(na., "err"))
  vagg.  <- f0(inll(agg.), T, isIN(agg., vaggs.))
  varg.  <- f0(inll(arg.), T, is_valid_xxx(arg.))
  vout.  <- f0(inll(out.), T, is_valid_xxx(out.))
  errs.   <- NULL
  if (!ifun(fun.)) {errs. <- c(errs., "\n • [fun.] is not a function or the name of function.")}
  if (!vd.  ) {errs. <- c(errs., "\n • [dim.] must be 0 or a vector of positive whole numbers.")}
  if (!vs.  ) {errs. <- c(errs., "\n • When supplied, [proc.$s] must be TRUE or FALSE.")}
  if (!vp.  ) {errs. <- c(errs., "\n • Elements of [proc.] must be uniquely named with names from c('a1', 'a2', 's', 'na', 'arg', 'out').")}
  if (!va1. ) {errs. <- c(errs., "\n • When supplied, [proc.$a1] must be TRUE or FALSE.")}
  if (!va2. ) {errs. <- c(errs., "\n • When supplied, [proc.$a2] must be TRUE or FALSE.")}
  if (!vdx. ) {errs. <- c(errs., "\n • [dim.] contains a value larger than the number of defined dimensions of [x.].")}
  if (!vna. ) {errs. <- c(errs., "\n • When supplied, [proc.$na] must be TRUE, FALSE, NA, or 'err'." )}
  if (!vagg.) {errs. <- c(errs., "\n • When supplied, [proc.$agg] must be 'nor', 'any', 'all', 'one', or 'two'.")}
  if (!varg.) {errs. <- c(errs., "\n • When supplied, [proc.$arg] must be a valid property specification as validated by is_valid_props().")}
  if (!vout.) {errs. <- c(errs., "\n • When supplied, [proc.$out] must be a valid property specification as validated by is_valid_props().")}
  if (idef(errs.)) {stop(errs.)}
  if (a1.) {x. <- av(x.)}
  if (idef(arg.)) {if (!is_xxx(x., arg.)) {stop("\n • [x.] does not match [proc.$arg = '", arg., "'].")}}
  if (isEQ(dim., 0)) {
    nd. <- length(dim(x.))
    x. <- f0(iarr(x.) | tibble::is_tibble(x.), apply(x., 1:nd., fun., ...),
          f0(inll(x.) | ivec(x.), sapply(x., fun., ...),
          f0(any_vls(x.), lapply(x., fun., ...), x.)))
  }
  else {x. <- apply(x., dim., fun., ...)}
  if (s. ) {x. <- simplify2array(x.)}
  if (a2.) {x. <- av(x.)}
  if (idef(out.)) {if (!is_xxx(x., out.)) {stop("\n • [x.] does not match [proc.$out = '", out., "'].")}}
  if (idef(agg.)) {
    err. <- isEQ(na., 'err')
    vlg. <- ilgl(x.)
    vna. <- !err. | !any(is.na(x.))
    errs. <- NULL
    if (!vlg.) {errs. <- c(errs., "\n • [proc.$agg] is not NULL, but results of applying [fun.] are not of mode logical.")}
    if (!vna.) {errs. <- c(errs., "\n • Applying [fun.] produced NA values, but [proc.$na = 'err'].")}
    if (idef(errs.)) {stop(errs.)}
    if (!err.) {x.[is.na(x.)] <- na.}
    if      (agg. == "nor") {x. <- norT(x.)}
    else if (agg. == "any") {x. <- anyT(x.)}
    else if (agg. == "all") {x. <- allT(x.)}
    else if (agg. == "one") {x. <- oneT(x.)}
    else if (agg. == "two") {x. <- twoT(x.)}
  }
  x.
}

#' @describeIn ply Are there \strong{zero} \code{TRUE} values in the result of
#'   applying \code{fun.}? Assumes that applying \code{fun.} results in logical
#'   values.
#' @export
norply <- function(x., fun., dim., ..., proc. = NULL) {proc.$agg <- "nor"; ply(x., fun., dim., ..., proc. = proc.)}

#' @describeIn ply Are there \strong{any} \code{TRUE} values in the result of
#'   applying \code{fun.}? Assumes that applying \code{fun.} results in logical
#'   values.
anyply <- function(x., fun., dim., ..., proc. = NULL) {proc.$agg <- "any"; ply(x., fun., dim., ..., proc. = proc.)}

#' @describeIn ply Are there \strong{only} \code{TRUE} values in the result of
#'   applying \code{fun.}? Assumes that applying \code{fun.} results in logical
#'   values.
allply <- function(x., fun., dim., ..., proc. = NULL) {proc.$agg <- "all"; ply(x., fun., dim., ..., proc. = proc.)}

#' @describeIn ply Is there exactly \strong{one} \code{TRUE} values in the
#'   result of applying \code{fun.}? Assumes that applying \code{fun.} results
#'   in logical values.
#' @export
oneply <- function(x., fun., dim., ..., proc. = NULL) {proc.$agg <- "one"; ply(x., fun., dim., ..., proc. = proc.)}

#' @describeIn ply Are there \strong{two or more} \code{TRUE} values in the
#'   result of applying \code{fun.}? Assumes that applying \code{fun.} results
#'   in logical values.
#' @export
twoply <- function(x., fun., dim., ..., proc. = NULL) {proc.$agg <- "two"; ply(x., fun., dim., ..., proc. = proc.)}

#' @describeIn ply \code{\link[=av]{Atomize}} \code{x} and apply \code{fun.} to
#'   the resulting atomic vector.
#' @export
atmply <- function(x., fun., ..., proc. = NULL) {proc.$a1 <- T; ply(x., fun., 0, ..., proc. = proc.)}

#' @describeIn ply Apply \code{fun.} to each element of \code{x.}, assumed to be
#'   an \link[=is_atm_mvect]{atomic mvect}.
#' @export
mvcply <- function(x., fun., ..., proc. = NULL) {proc.$arg <- 'mvc'; ply(x., fun., 0, ..., proc. = proc.)}

#' @describeIn ply Apply \code{FUN} to each element of \code{x.}, assumed to be
#'   an \link[=is_atm_vect]{atomic vect}.
#' @export
vecply <- function(x., fun., ..., proc. = NULL) {proc.$arg <- 'vec'; ply(x., fun., 0, ..., proc. = proc.)}

#' @describeIn ply Apply \code{fun.} to each row of \code{x.}, assumed to be a
#'   matrix or \link[tibble:is_tibble]{tibble}.
#' @export
rowply <- function(x., fun., ..., proc. = NULL) {proc.$arg <- 'd2D'; ply(x., fun., 1, ..., proc. = proc.)}

#' @describeIn ply Apply \code{fun.} to each column of \code{x.}, assumed to be
#'   a matrix or \link[tibble:is_tibble]{tibble}.
colply <- function(x., fun., ..., proc. = NULL) {proc.$arg <- 'd2D'; ply(x., fun., 2, ..., proc. = proc.)}

#' @describeIn ply Apply \code{fun.} across all dimensions of \code{x.}.
#' @export
dimply <- function(x., fun., ..., proc. = NULL) {dim. <- f0(ddd(x.) < 2, 0, 1:length(dim(x.))); ply(x., fun., dim., ..., proc. = proc.)}

#' @describeIn ply Apply \code{fun.} to each element of \code{x.},
#'   assumed to be a \link[=is_gen_vlist]{generic vlist}.
#' @export
vlsply <- function(x., fun., ..., proc. = NULL) {proc.$arg <- 'pop_vls'; ply(x., fun., 0, ..., proc. = proc.)}
