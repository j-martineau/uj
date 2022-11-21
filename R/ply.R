#' @name ply
#' @family extensions
#' @title Variations on \code{apply} Functions.
#' @description Generalized \code{apply(.)} with a wide range of options.
#' @section The \code{proc.} Argument: When not \code{NULL}, the \code{proc.}
#'   argument can be a list with up to seven optional named elements, which give
#'   processing instructions as follows: \tabular{ll}{
#'   NAME‑VALUE PAIR      \tab PROCESSING INSTRUCTION                        \cr
#'   \code{s = TRUE}      \tab link[base:simplify2array]{Simplify} the
#'                             result.\cr
#'   \code{a1 = TRUE}     \tab link[=a]{Atomize} \code{x} first.             \cr
#'   \code{a2 = TRUE}     \tab link[=a]{Atomize} the result.                 \cr
#'   \code{na = TRUE}     \tab Replace resulting \code{NA}s with \code{TRUE}.\cr
#'   \code{na = FALSE}    \tab Replace resulting \code{NA}s with
#'                             \code{FALSE}.                                 \cr
#'   \code{na = 'err'}    \tab Throw error if result contains any \code{NA}s.\cr
#'   \code{agg = 'nor'   }\tab Does result contain zero \code{TRUE} values?  \cr
#'   \code{agg = 'any'}   \tab Does result contain any \code{TRUE} values?   \cr
#'   \code{agg = 'all'}   \tab Does result contain only \code{TRUE} values?  \cr
#'   \code{agg = 'one'}   \tab Does result contain exactly one \code{TRUE}
#'                             value?                                        \cr
#'   \code{agg = 'two'}   \tab Does result contain two+ \code{TRUE} values.  \cr
#'   \code{arg = '***'}   \tab Inspect \code{x} for properties specified in the
#'                             character vector represented by \code{***} and
#'                             throw an error if they are not met.           \cr
#'   \code{out = '***'}   \tab Inspect the result for properties specified in
#'                             the character vector represented by \code{***}
#'                             and throw an error if they are not met.         }
#' @param x An object to apply \code{fun} to.
#' @param fun Function or name of a function to apply to \code{x}.
#' @param dim \link[=cmp_nnw_vec]{Complete non-negative whole-number vec} giving
#'   the dimension(s) of \code{x} to apply the function \code{fun} to (\code{0}
#'   indicates applying to elements of an object, where for arrays, and tibbles,
#'   this means applying to every cell).
#' @param ... An arbitrary number of additional arguments to be passed to the
#'   function \code{fun}.
#' @param proc. \code{NULL} or a list of named elements with processing
#'   instructions. See section \strong{The \code{proc.} Argument}.
#' @export
ply <- function(x, fun, dim, ..., proc. = NULL) {
  if (length(x) == 0) {stop("\n \u2022 [x] is empty.")}
  vnames <- c("a1", "a2", "s", "na", "arg", "out", "agg")
  vaggs <- c("nor", "any", "all", "one", "two")
  names <- names(proc.)
  procn <- length(proc.)
  namen <- length(names)
  namev <- f0(procn == 0, T, procn == namen & all(names %in% vnames))
  d0 <- isEQ(dim, 0)
  s <- f0(idef(proc.$s), proc.$s, F)
  a1 <- f0(idef(proc.$a1), proc.$a1, F)
  a2 <- f0(idef(proc.$a1), proc.$a2, F)
  na <- f0(idef(proc.$na), proc.$na, F)
  agg <- proc.$agg
  arg <- proc.$arg
  out <- proc.$out
  ok.dim <- d0 | cmp_psw_vec(dim)
  ok.xdim <- f0(d0 | !ok.dim, T, allIN(dim, 1:length(dim(x))))
  ok.proc <- f0(inll(proc.), T, is.list(proc.) & namev)
  ok.na <- f0(isLG(na), T, isEQ(na, "err"))
  ok.agg <- f0(inll(agg), T, isIN(agg, vaggs))
  ok.arg <- f0(inll(arg), T, is_valid_ppp(arg))
  ok.out <- f0(inll(out), T, is_valid_ppp(out))
  errs <- c(f0(ifun(fun), NULL, "\n \u2022 [fun] is not a function or the name of function."),
            f0(ok.dim   , NULL, "\n \u2022 [dim] must be 0 or a complete positive whole-number vec (?cmp_psw_vec)."),
            f0(isTF(s)  , NULL, "\n \u2022 When supplied, [proc.$s] must be TRUE or FALSE."),
            f0(ok.proc  , NULL, "\n \u2022 Elements of [proc.] must be uniquely named with names from c('a1', 'a2', 's', 'na', 'arg', 'out')."),
            f0(isTF(a1) , NULL, "\n \u2022 When supplied, [proc.$a1] must be TRUE or FALSE."),
            f0(isTF(a2) , NULL, "\n \u2022 When supplied, [proc.$a2] must be TRUE or FALSE."),
            f0(ok.xdim  , NULL, "\n \u2022 [dim] contains a value larger than the number of defined dimensions of [x]."),
            f0(ok.na    , NULL, "\n \u2022 When supplied, [proc.$na] must be TRUE, FALSE, NA, or 'err'."),
            f0(ok.agg   , NULL, "\n \u2022 When supplied, [proc.$agg] must be 'nor', 'any', 'all', 'one', or 'two'."),
            f0(ok.arg   , NULL, "\n \u2022 When supplied, [proc.$arg] must be a valid property specification as validated by is_valid_xxx()."),
            f0(ok.out   , NULL, "\n \u2022 When supplied, [proc.$out] must be a valid property specification as validated by is_valid_xxx()."))
  if (idef(errs)) {stop(errs)}
  if (a1) {x <- av(x)}
  if (idef(arg)) {if (!ippp(x, arg)) {stop("\n \u2022 [x] does not match [proc.$arg = '", arg, "'].")}}
  if (isEQ(dim, 0)) {x <- f0(iarr(x) | is.data.frame(x), apply(x, 1:length(dim(x)), fun, ...), f0(inll(x) | ivec(x), sapply(x, fun, ...), f0(ivls(x), lapply(x, fun, ...), x)))}
  else {x <- apply(x, dim, fun, ...)}
  if (s) {x <- simplify2array(x)}
  if (a2) {x <- av(x)}
  if (idef(out)) {if (!ippp(x, out)) {stop("\n \u2022 [x] does not match [proc.$out = '", out, "'].")}}
  if (idef(agg)) {
    err <- isEQ(na, 'err')
    errs <- c(f0(ilgl(x)              , NULL, "\n \u2022 [proc.$agg] is not NULL, but results of applying [fun] are not of mode logical."),
              f0(!err | !any(is.na(x)), NULL, "\n \u2022 Applying [fun] produced NA values, but [proc.$na = 'err']."))
    if (idef(errs)) {stop(errs)}
    if (!err) {x[is.na(x)] <- na}
    x <- f0(agg == "nor", norT(x), f0(agg == "any", anyT(x), f0(agg == "all", allT(x), f0(agg == "one", oneT(x), f0(agg == "two", twoT(x), x)))))
  }
  x
}

#' @describeIn ply Are there \strong{zero} \code{TRUE} values in the result
#'   of applying \code{fun}? Assumes that applying \code{fun} results in
#'   logical values.
#' @export
norply <- function(x, fun, dim, ..., proc. = NULL) {proc.$agg <- "nor"; ply(x, fun, dim, ..., proc. = proc.)}

#' @describeIn ply Are there \strong{any} \code{TRUE} values in the result of
#'   applying \code{fun}? Assumes that applying \code{fun} results in logical
#'   values.
#' @export
anyply <- function(x, fun, dim, ..., proc. = NULL) {proc.$agg <- "any"; ply(x, fun, dim, ..., proc. = proc.)}

#' @describeIn ply Are there \strong{only} \code{TRUE} values in the result
#'   of applying \code{fun}? Assumes that applying \code{fun} results in
#'   logical values.
#' @export
allply <- function(x, fun, dim, ..., proc. = NULL) {proc.$agg <- "all"; ply(x, fun, dim, ..., proc. = proc.)}

#' @describeIn ply Is there exactly \strong{one} \code{TRUE} values in the
#'   result of applying \code{fun}? Assumes that applying \code{fun} results
#'   in logical values.
#' @export
oneply <- function(x, fun, dim, ..., proc. = NULL) {proc.$agg <- "one"; ply(x, fun, dim, ..., proc. = proc.)}

#' @describeIn ply Are there \strong{two or more} \code{TRUE} values in the
#'   result of applying \code{fun}? Assumes that applying \code{fun} results
#'   in logical values.
#' @export
twoply <- function(x, fun, dim, ..., proc. = NULL) {proc.$agg <- "two"; ply(x, fun, dim, ..., proc. = proc.)}

#' @describeIn ply Atomize \code{x} and apply \code{fun} to the resulting
#'   atomic vector.
#' @export
atmply <- function(x, fun, ..., proc. = NULL) {proc.$a1 <- T; ply(x, fun, 0, ..., proc. = proc.)}

#' @describeIn ply Apply \code{fun} to each element of \code{x}, assumed to
#'   be an \link[atm_mvc]{atomic mvect}.
#' @export
mvcply <- function(x, fun, ..., proc. = NULL) {proc.$arg <- 'mvc'; ply(x, fun, 0, ..., proc. = proc.)}

#' @describeIn ply Apply \code{FUN} to each element of \code{x}, assumed to
#'   be an \link[atm_vec]{atomic vect}.
#' @export
vecply <- function(x, fun, ..., proc. = NULL) {proc.$arg <- 'vec'; ply(x, fun, 0, ..., proc. = proc.)}

#' @describeIn ply Apply \code{fun} to each row of \code{x}, assumed to be
#'   a matrix or \link[is_dtf]{dtf}.
#' @export
rowply <- function(x, fun, ..., proc. = NULL) {proc.$arg <- 'd2D'; ply(x, fun, 1, ..., proc. = proc.)}

#' @describeIn ply Apply \code{fun} to each column of \code{x}, assumed to
#'   be a matrix or \link[is_dtf]{dtf}.
colply <- function(x, fun, ..., proc. = NULL) {proc.$arg <- 'd2D'; ply(x, fun, 2, ..., proc. = proc.)}

#' @describeIn ply Apply \code{fun} across all dimensions of \code{x}.
#' @export
dimply <- function(x, fun, ..., proc. = NULL) {dim <- f0(ddd(x) < 2, 0, 1:length(dim(x))); ply(x, fun, dim, ..., proc. = proc.)}

#' @describeIn ply Apply \code{fun} to each element of \code{x}, assumed to
#'   be a \link[is_vls]{vlist}.
#' @export
vlsply <- function(x, fun, ..., proc. = NULL) {proc.$arg <- 'pop_vls'; ply(x, fun, 0, ..., proc. = proc.)}
