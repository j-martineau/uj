#' @name ply
#' @family meta
#' @title Variations on \code{apply(.)}
#' @details \strong{\code{ply}}
#'   \cr Generalized \code{apply(.)} with a wide range of options.
#'   \cr\cr
#'   \strong{\code{norply}, \code{anyply}, \code{allply}, \code{oneply},
#'   \code{twoply}}
#'   \cr The functions assume applying \code{fun.} produces an object of logical
#'   mode. They check, respectively, \code{TRUE} if zero, any, all, exactly one,
#'   or at least two values in the result are \code{TRUE}.
#'   \cr\cr
#'   \strong{\code{atmply}}
#'   \cr \code{\link[=a]{Atomizes}} \code{x} and applies \code{fun.} to the
#'   resulting atomic vector.
#'   \cr\cr
#'   \strong{\code{mvcply}}
#'   \cr Applies \code{fun.} to each element of \code{x}, which is assumed to be
#'   an \link[=is_atm_mvect]{atomic mvect}..
#'   \cr \cr
#'   \strong{\code{vecply}}
#'   \cr Applies \code{FUN} to each element of \code{x}, which is assumed to be
#'   an \link[=is_atm_vext]{atomic vect}.
#'   \cr \cr
#'   \strong{\code{rowply}}
#'   \cr Applies \code{fun.} to each row of \code{x}, which is assumed to be a
#'   matrix or \link[tibble:is_tibble]{tibble}.
#'   \cr \cr
#'   \strong{\code{colply}}
#'   \cr Applies \code{fun.} to each column of \code{x}, which is assumed to be
#'   a matrix or \link[tibble:is_tibble]{tibble}.
#'   \cr \cr
#'   \strong{\code{dimply}}
#'   \cr Applies \code{fun.} across all dimensions of \code{x}, where
#'   \code{dots} is created \emph{in the calling function} with the command
#'   \code{list(...)}.
#'   \cr \cr
#'   \strong{\code{vlsply}}
#'   \cr Applies \code{fun.} to each element of \code{x}, where \code{x} is
#'   assumed to be a \link[=is_gen_vlist]{generic vlist}.
#'   \cr\cr
#'   \strong{The \code{proc.} Argument}
#'   \cr When not \code{NULL}, the \code{proc.} argument can be a list with
#'   up to seven optional named elements, which give processing instructions as
#'   follows:
#'   \tabular{lll}{
#'   \strong{Name}\tab\strong{Value} \tab\strong{Processing Instruction}
#'   \cr\code{a1}\tab\code{TRUE} \tab\link[=a]{Atomize} \code{x} before applying
#'                               \code{fun.}.
#'   \cr\code{a2}\tab\code{TRUE} \tab\link[=a]{Atomize} \code{x} after applying
#'                               \code{fun.}.
#'   \cr\code{s} \tab\code{TRUE} \tab\link[base:simplify2array]{Simplify} the
#'                               result.
#'   \cr\code{na}\tab\code{TRUE} \tab Replace any resulting \code{NA} values
#'                               with \code{TRUE}.
#'   \cr\code{na}\tab\code{FALSE}\tab Replace any resulting \code{NA} values
#'                               with \code{FALSE}.
#'   \cr\code{na}\tab\code{'err'}\tab Throw an error if the result contains any
#'                               \code{NA} values.
#'   \cr\code{arg}\tab\code{\link[uj:is_prop]{prop}}
#'                \tab Inspect \code{x} for the specified properties before
#'                     processing and throw an error if they are not met.
#'   \cr\code{out}\tab \code{\link[uj:is_prop]{prop}}
#'                \tab After processing, inspect the result for the specified
#'                     properties and throw an error if they are not met.
#'   \cr\code{agg}\tab\code{'nor'}\tab Evaluate whether the result contains zero
#'                    \code{TRUE} values.
#'   \cr\code{agg}\tab\code{'any'}\tab Evaluate whether the result contains any
#'                    \code{TRUE} values.
#'   \cr\code{agg}\tab\code{'all'}\tab Evaluate whether the result contains only
#'                    \code{TRUE} values.
#'   \cr\code{agg}\tab\code{'one'}\tab Evaluate whether the result contains
#'                    exactly one \code{TRUE} values.
#'   \cr\code{agg}\tab\code{'two'}\tab Evaluate whether the result contains two
#'                    or more \code{TRUE} values.
#'   }
#' @param x An object to apply the function \code{fun.} to.
#' @param fun. A function or name of a funciton to apply to \code{x}.
#' @param dim. A non-negative whole number vector giving the dimension(s) of
#'   \code{x} to apply the function \code{fun.} to. \code{0} indicates applying
#'   to elements of an object (for arrays, and tibbles, this means applying
#'   to every cell).
#' @param ... An arbitrary number of additional arguments to be passed to the
#'   function \code{fun.}.
#' @param proc. \code{NULL} or a list of named elements with processing
#'   instructions. See details.
#' @export
ply <- function(x, fun., dim., ..., proc. = NULL) {
  if (length(x) == 0) {stop("\n  * [x] is empty.")}
  ValidNames <- c("a1", "a2", "s", "na", "arg", "out", "agg")
  ValidAgg   <- c("nor", "any", "all", "one", "two")
  Names <- names(proc.)
  ProcN <- length(proc.)
  NameN <- length(Names)
  NameV <- f0(ProcN == 0, T, ProcN == NameN & all(Names %in% ValidNames))
  D0    <- isEQ(dim., 0)
  a1    <- f0(xdef(proc.$a1), proc.$a1, F)
  a2    <- f0(xdef(proc.$a1), proc.$a2, F)
  s     <- f0(xdef(proc.$a ), proc.$a , F)
  na    <- f0(xdef(proc.$na), proc.$na, F)
  agg   <- proc.$agg
  arg   <- proc.$arg
  out   <- proc.$out
  VD    <- f0(D0, T, cmp_psw_vec(dim.))
  VDX   <- f0(D0 | !VD, T, allIN(dim., 1:length(dim(x))))
  VP    <- f0(xnll(proc.), T, is.list(proc.) & NameV)
  VA1   <- isTF(a1)
  VA2   <- isTF(a2)
  VS    <- isTF(s)
  VNA   <- f0(isLG(na ), T, isEQ(na, "err"))
  VAGG  <- f0(xnll(agg), T, isIN(agg, ValidAgg))
  VARG  <- f0(xnll(arg), T, is_valid_props(arg))
  VOUT  <- f0(xnll(out), T, is_valid_props(out))
  E <- NULL
  if (!xfun(fun.)) {E <- c(E, "\n  * [fun.] is not a function or the name of function.")}
  if (!VD  ) {E <- c(E, "\n  * [dim.] must be 0 or a vector of positive whole numbers.")}
  if (!VDX ) {E <- c(E, "\n  * [dim.] contains a value larger than the number of defined dimensions of [x].")}
  if (!VP  ) {E <- c(E, "\n  * Elements of [proc.] must be uniquely named with names from c('a1', 'a2', 's', 'na', 'arg', 'out').")}
  if (!VA1 ) {E <- c(E, "\n  * When supplied, [proc.$a1] must be TRUE or FALSE.")}
  if (!VA2 ) {E <- c(E, "\n  * When supplied, [proc.$a2] must be TRUE or FALSE.")}
  if (!VS  ) {E <- c(E, "\n  * When supplied, [proc.$s] must be TRUE or FALSE." )}
  if (!VNA ) {E <- c(E, "\n  * When supplied, [proc.$na] must be TRUE, FALSE, NA, or 'err'." )}
  if (!VAGG) {E <- c(E, "\n  * When supplied, [proc.$agg] must be 'nor', 'any', 'all', 'one', or 'two'.")}
  if (!VARG) {E <- c(E, "\n  * When supplied, [proc.$arg] must be a valid property specification as validated by is_valid_props().")}
  if (!VOUT) {E <- c(E, "\n  * When supplied, [proc.$out] must be a valid property specification as validated by is_valid_props().")}
  if (xdef(E)) {stop(E)}
  if (a1) {x <- av(x)}
  if (xdef(arg)) {if (!is_prop(x, arg)) {stop("\n  * [x] does not match [proc.$arg = '", arg, "'].")}}
  if (isEQ(dim., 0)) {
    ND <- length(dim(x))
    x <- f0(xarr(x) | tibble::is_tibble(x), apply(x, 1:ND, fun., ...),
         f0(xnll(x) | xvec(x), sapply(x, fun., ...),
         f0(is_vlist(x), lapply(x, fun., ...), x)))
  }
  else {x <- apply(x, dim., fun., ...)}
  if (s ) {x <- simplify2array(x)}
  if (a2) {x <- av(x)}
  if (xdef(out)) {if (!is_prop(x, out)) {stop("\n  * [x] does not match [proc.$out = '", out, "'].")}}
  if (xdef(agg)) {
    ERR <- isEQ(na, 'err')
    VLG <- xlgc(x)
    VNA <- !E | !any(is.na(x))
    E   <- NULL
    if (!VLG) {E <- c(E, "\n  * [proc.$agg] is not NULL, but results of applying [fun.] are not of mode logical.")}
    if (!VNA) {E <- c(E, "\n  * Applying [fun.] produced NA values, but [proc.$na = 'err'].")}
    if (xdef(E)) {stop(E)}
    if (!ERR) {x[is.na(x)] <- na}
    if      (agg == "nor") {x <- norT(x)}
    else if (agg == "any") {x <- anyT(x)}
    else if (agg == "all") {x <- allT(x)}
    else if (agg == "one") {x <- oneT(x)}
    else if (agg == "two") {x <- twoT(x)}
  }
  x
}

#' @rdname ply
#' @export
norply <- function(x, fun., dim., ..., proc. = NULL) {
  proc.$agg <- "nor"
  ply(x, fun., dim., ..., proc. = proc.)
}

#' @rdname ply
#' @export
anyply <- function(x, fun., dim., ..., proc. = NULL) {
  proc.$agg <- "any"
  ply(x, fun., dim., ..., proc. = proc.)
}

#' @rdname ply
#' @export
allply <- function(x, fun., dim., ..., proc. = NULL) {
  proc.$agg <- "all"
  ply(x, fun., dim., ..., proc. = proc.)
}

#' @rdname ply
#' @export
oneply <- function(x, fun., dim., ..., proc. = NULL) {
  proc.$agg <- "one"
  ply(x, fun., dim., ..., proc. = proc.)
}

#' @rdname ply
#' @export
twoply <- function(x, fun., dim., ..., proc. = NULL) {
  proc.$agg <- "two"
  ply(x, fun., dim., ..., proc. = proc.)
}

#' @rdname ply
#' @export
atmply <- function(x, fun., ..., proc. = NULL) {
  proc.$a1 <- T
  ply(x, fun., 0, ..., proc. = proc.)
}

#' @rdname ply
#' @export
mvcply <- function(x, fun., ..., proc. = NULL) {
  proc.$arg <- 'mvc'
  ply(x, fun., 0, ..., proc. = proc.)
}

#' @rdname ply
#' @export
vecply <- function(x, fun., ..., proc. = NULL) {
  proc.$arg <- 'vec'
  ply(x, fun., 0, ..., proc. = proc.)
}

#' @rdname ply
#' @export
rowply <- function(x, fun., ..., proc. = NULL) {
  proc.$arg <- 'd2D'
  ply(x, fun., 1, ..., proc. = proc.)
}

#' @rdname ply
#' @export
colply <- function(x, fun., ..., proc. = NULL) {
  proc.$arg <- 'd2D'
  ply(x, fun., 2, ..., proc. = proc.)
}

#' @rdname ply
#' @export
dimply <- function(x, fun., ..., proc. = NULL) {
  dim. <- f0(ddim(x) < 2, 0, 1:length(dim(x)))
  ply(x, fun., dim., ..., proc. = proc.)
}

#' @rdname ply
#' @export
vlsply <- function(x, fun., ..., proc. = NULL) {
  proc.$arg <- 'pop_vls'
  ply(x, fun., 0, ..., proc. = proc.)
}
