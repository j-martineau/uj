#' @encoding UTF-8
#' @family forks
#' @title Enhancements of \code{\link[base]{ifelse}}.
#' @description Return different types of objects for `TRUE` and `FALSE` and return `NULL`  conditional on the number of `TRUE` values.
#' @param na An object of any type for `f1`. An atomic scalar \link[=compatible]{compatible} with `y` and `n` for `fork`, with the additional possibility of `na = 'err'` to indicate an error should be thrown if any values in `x` are `NA`.
#' @param err Either `'err'` or an object to be returned when `x` is not an atomic scalar in `c(TRUE, FALSE, NA)`.
#' @param x A logical scalar (if not,`x` it is replaced by `FALSE`).
#' @param y,n Any valid R object.
#' @param d,.d A character scalar delimiter for collapsing objects into scalar character objects. If `.d` is not a character scalar, it is replaced by `" "`.
#' @param .cond A character scalar in `c('all', 'any', 'none')`. If `.cond` is not of an allowed value, it is replaced by `'all'`.
#' @examples
#' fork(c(TRUE, FALSE, TRUE, NA), 1, 2)
#' fork(c(TRUE, FALSE, TRUE, NA), 1, 2, na = 0)
#' fork(c(TRUE, FALSE, TRUE, NA), 1, 2, na = NA)
#' fork(c(TRUE, FALSE, TRUE, NA), 1:4, 5:8)
#'
#' f0(NA, data.frame(letters = letters), 0:26)
#' f0(TRUE, data.frame(letters = letters), 0:26)
#' f0(FALSE, data.frame(letters = letters), 0:26)
#' f0(list(1, "a"), data.frame(letters = letters), 0:26)
#' f0(c(.bad.varname.), data.frame(letters = letters), 0:26)
#'
#' f1(NA, data.frame(letters = letters), 0:26)
#' f1(NA, data.frame(letters = letters), 0:26, na = NA)
#' f1(TRUE, data.frame(letters = letters), 0:26)
#' f1(FALSE, data.frame(letters = letters), 0:26)
#' f1(list(1, "a"), data.frame(letters = letters), 0:26)
#' f1(list(1, "a"), data.frame(letters = letters), 0:26)
#' f1(c(.bad.varname.), data.frame(letters = letters), 0:26)
#' f1(list(1, "a"), data.frame(letters = letters), 0:26, err = "error")
#' f1(c(.bad.varname.), data.frame(letters = letters), 0:26, err = "error")
#'
#' nll_if(TRUE, "an error", "message")
#' nll_if(FALSE, "an error", "message")
#' nll_if(41, "an error", "message")
#'
#' nll_ifs(t1 = TRUE, t2 = FALSE, t3 = FALSE, "not", "any", .cond = "all")
#' nll_ifs(t1 = TRUE, t2 = FALSE, t3 = FALSE, "not", "all", .cond = "any")
#' nll_ifs(t1 = TRUE, t2 = FALSE, t3 = FALSE, "not", "none", .cond = "none")
#' nll_if_any(t1 = TRUE, t2 = FALSE, t3 = FALSE, "not", "any")
#' nll_if_all(t1 = TRUE, t2 = FALSE, t3 = FALSE, "not", "all")
#' nll_if_none(t1 = TRUE, t2 = FALSE, t3 = FALSE, "not", "none")
#' @export
fork_help <- function() {utils::help("fork_help", package = "uj")}

#' @describeIn fork_help If `x` is scalar `TRUE`, returns `y`. If `x` is anything else, returns `n`.
#' @export
f0 <- function(x, y, n) {if (base::isTRUE(uj::failsafe(x))) {uj::failsafe(y)} else {uj::failsafe(n)}}

#' @describeIn fork_help Evaluates logical scalar or logical vector `x` and return an object of the same length as `x` where:
#' \itemize{\item `TRUE` values of `x` are replaced by corresponding values of `y`.
#'          \item `FALSE` values of `x` are replaced by corresponding values of `n`.
#'          \item `NA` values of `x` are replaced by `na` (unless `na = 'err'`, in which case if there are any `NA` values in `x`, throws an error). }
#'   Returns a length-`length(x)` atomic object.
#' @export
fork <- function(x, y, n, na = n) {
  nX <- base::length(x)
  nY <- base::length(y)
  nN <- base::length(n)
  nNA <- base::length(na)
  errNA <- uj::f0(nNA == 1 & base::is.character(na), na == 'err', F)
  naNA <- uj::f0(nNA == 1 & base::is.atomic(na), base::isna(na), F)
  incNas <- errNA | naNA
  okX <- uj::.cmp_lgl_vec(x)
  okY <- uj::f0(!uj::.VEC(y), F, uj::f0(!okX, T, nY %in% base::c(1, base::max(1, nX))))
  okN <- uj::f0(!uj::.VEC(n), F, uj::f0(!okX, T, nN %in% base::c(1, base::max(1, nX))))
  okNA <- uj::f0(!uj::.VEC(na), F, uj::f0(!okX, T, nNA %in% base::c(1, base::max(1, nX))))
  errs <- NULL
  if (!okX) {errs <- base::c(errs, "[x] must be a logical vec (?cmp_lgl_vec).")}
  if (!okY) {errs <- base::c(errs, "[y] must be of length 1 or a vector of the same length as [x].")}
  if (!okN) {errs <- base::c(errs, "[n] must be of length 1 or a vector of the same length as [x].")}
  if (!okNA) {errs <- base::c(errs, "[na] must be of length 1 or a vector of the same length as [x].")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  okTny <- okX & okY & okN
  okArg <- uj::f0(!errNA | !okX, T, uj::.cmp_lgl_vec(x))
  okTny <- uj::f0(!okTny, NULL, uj::f0(incNas, uj::compatible(y, n, na), uj::compatible(y, n)))
  if (!okArg) {errs <- base::c(errs, "[na = 'err'] but [x] contains NA values.")}
  if (!okTny) {errs <- base::c(errs, uj::f0(incNas, "[y], [n], and [na] must be of compatible (?compatible) modes.", "[y] and [n] must be of compatible (?compatible) modes."))}
  if (nY  == 1) {y <- base::rep.int(y, nX)}
  if (nN  == 1) {n <- base::rep.int(y, nX)}
  if (nNA == 1) {na <- base::rep.int(na, nX)}
  y <- base::rep.int(NA, nX)
  iT <- base::sapply(x, isTRUE)
  iF <- base::sapply(x, isFALSE)
  iN <- base::isna(x)
  y[iT] <- y[iT]
  y[iF] <- n[iF]
  y[iN] <- na[iN]
  y
}

#' @describeIn fork_help Error-checked version of `f0`. Evaluates and processes logical scalar `x` in the following manner:
#' \itemize{\item If `x = TRUE`, returns `y`.
#'          \item If `x = FALSE`, returns `y`.
#'          \item If `x = NA`, returns `na` unless `na = 'err'`, in which case, an error is thrown.
#'          \item If `x` is neither a logical scalar nor scalar `NA`, returns `err` unless `err = 'err'`, in which case an error is thrown. }
#' @export
f1 <- function(x, y, n, na = n, err = n) {
  errErr <- uj::f0(uj::.cmp_chr_scl(err), err == "err", F)
  errNAS <- uj::f0(uj::.cmp_chr_scl(na), na == "err", F)
  naX <- uj:::.NA0(x)
  x <- uj::failsafe(x)
  if (base::isTRUE(x)) {y}
  else if (base::isFALSE(x)) {n}
  else if (naX & !errNAS) {na}
  else if (!uj::.lgl_scl(x) & !errErr) {err}
  else if (naX) {uj::stopperr("[x] must be atomic, scalar, and TRUE, FALSE, or NA.")}
  else {uj::stopperr("[x] must be atomic, scalar, and TRUE or FALSE.")}
}

#' @describeIn fork_help If `x` is scalar `TRUE`, returns `NULL`, otherwise collapses `...` args to a character scalar using delimiter `d` and returns the result.
#' @export
nll_if <- function(x, ..., d = " ") {
  if (!uj::.cmp_chr_scl(d)) {d <-  " "}
  uj::f0(base::isTRUE(x), NULL, base::paste0(uj::av(...), collapse = d))
}

#' @describeIn fork_help Calls `nll_if_none(..., d = .d)` when `.cond = 'none'`. Calls `nll_if_any(..., d = .d)` when `.cond = 'any`. Calls `nll_if_all(..., d = .d)` when `.cond` takes any other value (including `'all'`)
#' @export
nll_ifs <- function(..., .d = " ", .cond = "all") {
  .bad_dots <- function(STACK) {uj::stopperr("There must be both named and unnamed [...] args.", fun = "nll_ifs", stack = STACK)}
  if (!uj::.cmp_chr_scl(uj::failsafe(.d))) {.d <- " "}
  if (!uj::.cmp_chr_scl(.cond, valid = base::c("all", "any", "none"))) {.cond <- "all"}
  dots <- base::list(...)
  labs <- base::names(dots)
  if (base::length(labs) == 0) {.bad_dots(uj::callers())}
  isNamed <- !base::isna(labs)
  isNamed[isNamed] <- labs[isNamed] != ""
  if (base::all(isNamed) | !base::any(isNamed)) {.bad_dots(uj::callers())}
  known <- dots[isNamed]
  anon <- base::paste0(uj::av(dots[!isNamed]), collapse = .d)
  for (i in 1:base::length(known)) {
    True <- base::isTRUE(uj::failsafe(known[[i]]))
    if (.cond == "any" & True) {return(NULL)}
    else if (.cond == "all" & !True) {return(known)}
    else if (.cond == "none" & True) {return(known)}
  }
  if (.cond == "any") {NULL} else {known}
}

#' @describeIn fork_help Returns `NULL` if any named `...` argument is scalar `TRUE`. Otherwise, returns a message composed of unnamed `...` args collapsed into a character scalar using the delimiter `.d`.
#' @export
nll_if_any <- function(..., .d = " ") {uj::nll_ifs(..., .d = " ", .cond = "any")}

#' @describeIn fork_help Returns `NULL` if all named `...` arguments are scalar `TRUE`. Otherwise, returns a message composed of unnamed `...` args collapsed into a character scalar using the delimiter `.d`.
#' @export
nll_if_all <- function(..., .d = " ") {uj::nll_ifs(..., .d = " ", .cond = "all")}

#' @describeIn fork_help Returns `NULL` if no named `...` argument is scalar `TRUE`. Otherwise, returns a message composed of unnamed `...` args collapsed into a character scalar using the delimiter `.d`.
#' @export
nll_if_none <- function(..., .d = " ") {uj::nll_ifs(..., .d = " ", .cond = "none")}
