#' @name ply
#' @title Variations on `apply` functions
#' @description (`norply`, `anyply`, `allply`, `oneply`, and `twoply` assume the result of applying `fun` is of mode 'logical')
#' \itemize{
#'   \item **`ply`**: offers all functionality specialized `ply` functions described below
#'   \item **`norply`**: evaluates whether there are *0* `TRUE` values in the result of applying `fun`.
#'   \item **`anyply`**: evaluates whether there are *any* `TRUE` values in the result of applying `fun`.
#'   \item **`allply`**: evaluates whether there are *only* `TRUE` values in the result of applying `fun`.
#'   \item **`oneply`**: evaluates whether there is *exactly 1* `TRUE` value in the result of applying `fun`.
#'   \item **`twoply`**: evaluates whether there are *2+* `TRUE` values in the result of applying `fun`.
#'   \item **`atmply`**: \link[=av]{Atomize} `x` and apply `fun` to elements of the resulting atomic vector.
#'   \item **`mvcply`**: apply `fun` to elements of \link[=atm_vec]{atomic multivec} `x`.
#'   \item **`vecply`**: apply `fun` to elements of \link[=atm_vec]{atomic vec} `x`.
#'   \item **`rowply`**: apply `fun` to rows of matrix/data.frame `x`.
#'   \item **`colply`**: apply `fun` to columns of matrix/data.frame `x`.
#'   \item **`dimply`**: apply `fun` to cells of array/data.frame `x`.
#'   \item **`vlsply`**: apply `fun` to elements of \link[=ivls]{vlist} `x`.
#' }
#' @section The `proc` Argument: When not `NULL`, the `proc` argument is an optional list with up to seven named elements, which give processing instructions as follows:
#' \itemize{
#'   \item **`s = TRUE`**: \link[base:simplify2array]{simplify} the result.
#'   \item **`a1 = TRUE`**: \link[=a]{atomize} `x` as a first step.
#'   \item **`a2 = TRUE`**: atomize the result.
#'   \item **`na = TRUE`**: replace resulting `NA` values with `TRUE`.
#'   \item **`na = FALSE`**: replace resulting `NA` values with `FALSE`.
#'   \item **`na = 'err'`**: throw error if result contains any `NA`s.
#'   \item **`agg = 'nor'`**: inspect result for *0* `TRUE` values.
#'   \item **`agg = 'any'`**: inspect result for *any* `TRUE` values.
#'   \item **`agg = 'all'`**: inspect result for *only* `TRUE` values.
#'   \item **`agg = 'one'`**: inspect result for *exactly 1* `TRUE` value.
#'   \item **`agg = 'two'`**: inspect result for *2+* `TRUE` values.
#'   \item **`arg = 'xxx'`**: inspect `x` for \link[=ppp]{properties} in the string `xxx` and throw an error if not met.
#'   \item **`out = 'yyy'`**: inspect the result for properties given in the string `yyy` and throw an error if not met.
#' }
#' @param x An object to apply `fun` to.
#' @param fun Function or character scalar name of a function to apply to `x`.
#' @param dim A \link[=cmp_nnw_vec]{complete non-negative whole-number vec} giving dimension(s) of `x` to apply the function `fun` to (`0` indicates applying to elements of a vector or \link[=ivls]{vlist} vs. applying to every cell for arrays and data.frames).
#' @param ... An arbitrary number of additional arguments to be passed to the function `fun`.
#' @param proc. `NULL` or a list of named elements with processing instructions. See the *the `proc` argument* section.
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
  if (!is.null(errs)) {stop(errs)}
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
    if (!is.null(errs)) {stop(errs)}
    if (!err) {x[is.na(x)] <- na}
    x <- f0(agg == "nor", norT(x), f0(agg == "any", anyT(x), f0(agg == "all", allT(x), f0(agg == "one", oneT(x), f0(agg == "two", twoT(x), x)))))
  }
  x
}

#' @rdname ply
#' @export
norply <- function(x, fun, dim, ..., proc. = NULL) {proc.$agg <- "nor"; ply(x, fun, dim, ..., proc. = proc.)}

#' @rdname ply
#' @export
anyply <- function(x, fun, dim, ..., proc. = NULL) {proc.$agg <- "any"; ply(x, fun, dim, ..., proc. = proc.)}

#' @rdname ply
#' @export
allply <- function(x, fun, dim, ..., proc. = NULL) {proc.$agg <- "all"; ply(x, fun, dim, ..., proc. = proc.)}

#' @rdname ply
#' @export
oneply <- function(x, fun, dim, ..., proc. = NULL) {proc.$agg <- "one"; ply(x, fun, dim, ..., proc. = proc.)}

#' @rdname ply
#' @export
twoply <- function(x, fun, dim, ..., proc. = NULL) {proc.$agg <- "two"; ply(x, fun, dim, ..., proc. = proc.)}

#' @rdname ply
#' @export
atmply <- function(x, fun, ..., proc. = NULL) {proc.$a1 <- T; ply(x, fun, 0, ..., proc. = proc.)}

#' @rdname ply
#' @export
mvcply <- function(x, fun, ..., proc. = NULL) {proc.$arg <- 'mvc'; ply(x, fun, 0, ..., proc. = proc.)}

#' @rdname ply
#' @export
vecply <- function(x, fun, ..., proc. = NULL) {proc.$arg <- 'vec'; ply(x, fun, 0, ..., proc. = proc.)}

#' @rdname ply
#' @export
rowply <- function(x, fun, ..., proc. = NULL) {proc.$arg <- 'd2D'; ply(x, fun, 1, ..., proc. = proc.)}

#' @rdname ply
#' @export
colply <- function(x, fun, ..., proc. = NULL) {proc.$arg <- 'd2D'; ply(x, fun, 2, ..., proc. = proc.)}

#' @rdname ply
#' @export
dimply <- function(x, fun, ..., proc. = NULL) {dim <- f0(ddd(x) < 2, 0, 1:length(dim(x))); ply(x, fun, dim, ..., proc. = proc.)}

#' @rdname ply
#' @export
vlsply <- function(x, fun, ..., proc. = NULL) {proc.$arg <- 'pop_vls'; ply(x, fun, 0, ..., proc. = proc.)}
