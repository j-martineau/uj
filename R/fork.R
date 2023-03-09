#' @name fork
#' @encoding UTF-8
#' @family extensions
#' @family forks
#' @title Enhancements of \code{\link[base]{ifelse}}.
#' @description Return different types of objects for `TRUE` and `FALSE` and return `NULL`  conditional on the number of `TRUE` values.
#' @details **`fork`**
#' \cr\cr Evaluates logical scalar or logical vector `test` and return an object of the same length as `test` where:
#' \itemize{\item `TRUE` values of `test` are replaced by corresponding values of `yes`.
#'          \item `FALSE` values of `test` are replaced by corresponding values of `no`.
#'          \item `NA` values of `test` are replaced by `na` (unless `na = 'err'`, in which case if there are any `NA` values in `test`, throws an error). }
#' \cr\cr **`f0`**
#' \cr\cr If `test` is scalar `TRUE`, returns `yes`. If `test` is anything else, returns `no`.
#' \cr\cr **`f1`**
#' \cr\cr Error-checked version of `f0`. Evaluates and processes logical scalar `test` in the following manner:
#' \itemize{\item If `test = TRUE`, returns `yes`.
#'          \item If `test = FALSE`, returns `no`.
#'          \item If `test = NA`, returns `na` unless `na = 'err'`, in which case, an error is thrown.
#'          \item If `test` is neither a logical scalar nor scalar `NA`, returns `err` unless `err = 'err'`, in which case an error is thrown. }
#' \cr\cr **Functions beginning with `nll_if`**
#' \cr\cr These functions are useful for compiling error messages. They thus return `NULL` if error checks are passed and a message if they are not.
#' \cr\cr **`nll_if`**
#' \cr\cr If `x` is scalar `TRUE`, returns `NULL`, otherwise collapses `...` args to a character scalar using delimiter `d` and returns the result.
#' \cr\cr **`nll_if_<cond.>`**
#' \cr\cr These functions take both named and unnamed `...` args. Named `...` args other than `D` are evaluated for `TRUE`-ness (any value that is not scalar `TRUE` is considered `FALSE`). Unnamed `...` args are \link[=glue_dots]{collapsed} into a character scalar value named `D` using the delimiter in arg `d.`
#' \tabular{ll}{  `nll_if_none`   \tab Returns `D` upon encountering a `TRUE` named `...` arg. Returns `NULL` if none is encountered. \cr   \tab   \cr
#'                `nll_if_any`    \tab Returns `NULL` upon encountering a `TRUE` named `...` arg. Returns `D` if none is encountered. \cr   \tab   \cr
#'                `nll_if_all`    \tab Returns `D` upon encountering a non-`TRUE` named `...` arg. Returns `NULL` if none is encountered.            }
#' \cr\cr **`nll_ifs`**
#' \cr\cr Calls `nll_if_none(..., D = D)` when `COND = 'none'`. Calls `nll_if_any(..., D = D)` when `COND = 'any`. Calls `nll_if_all(..., D = D)` when `COND` takes any other value (including `'all'`)
#' \cr\cr **`nlls_ifs`**
#' \cr\cr Conditionally compiles messages into a character vector. Each non-`TRUE` odd-numbered `...` arg's message (\link[=glue_dots]{collapsed} from the following `...` arg) is added to the compilation. If all odd-numbered `...` args are `TRUE`, returns `NULL`.
#' @param x A logical vector.
#' @param y,n \link[=atm_scl]{Atomic scalars} or \link[=atm_vec]{atomic vecs} of the same length as `x`.
#' @param na An object of any type for `f1`. An atomic scalar \link[=compatible]{compatible} with `yes` and `no` for `fork`, with the additional possibility of `na = 'err'` to indicate an error should be thrown if any values in `test` are `na`.
#' @param err Either `'err'` or an object to be returned when `test` is not an atomic scalar in `c(TRUE, FALSE, NA)`.
#' @param X A logical scalar (if not,`X` it is replaced by `FALSE`).
#' @param Y,N Any valid R object.
#' @param D A character scalar delimiter for collapsing objects into scalar character objects. If `D` is not a character scalar, it is replaced by `" "`.
#' @param COND A character scalar in `c('all', 'any', 'none')`. If `COND` is not of an allowed value, it is replaced by `'all'`.
#' @return **A length-**`length(x)` **atomic object**      \cr\cr `fork`
#' \cr\cr  **An arbitrary object**                         \cr\cr `f0, f1`
#' \cr\cr  **A character scalar or the** `NULL` **object** \cr\cr `nll_if, nll_ifs, nll_if_all` \cr `nll_if_any, nll_if_none`
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
#' f0(c(.bad.var.name.), data.frame(letters = letters), 0:26)
#'
#' f1(NA, data.frame(letters = letters), 0:26)
#' f1(NA, data.frame(letters = letters), 0:26, na = NA)
#' f1(TRUE, data.frame(letters = letters), 0:26)
#' f1(FALSE, data.frame(letters = letters), 0:26)
#' f1(list(1, "a"), data.frame(letters = letters), 0:26)
#' f1(list(1, "a"), data.frame(letters = letters), 0:26)
#' f1(c(.bad.var.name.), data.frame(letters = letters), 0:26)
#' f1(list(1, "a"), data.frame(letters = letters), 0:26, err = "error")
#' f1(c(.bad.var.name.), data.frame(letters = letters), 0:26, err = "error")
#'
#' nll_if(TRUE, "an error", "message")
#' nll_if(FALSE, "an error", "message")
#' nll_if(41, "an error", "message")
#'
#' nll_ifs(t1 = TRUE, t2 = FALSE, t3 = FALSE, "not", "any", COND = "all")
#' nll_ifs(t1 = TRUE, t2 = FALSE, t3 = FALSE, "not", "all", COND = "any")
#' nll_ifs(t1 = TRUE, t2 = FALSE, t3 = FALSE, "not", "none", COND = "none")
#' nll_if_any(t1 = TRUE, t2 = FALSE, t3 = FALSE, "not", "any")
#' nll_if_all(t1 = TRUE, t2 = FALSE, t3 = FALSE, "not", "all")
#' nll_if_none(t1 = TRUE, t2 = FALSE, t3 = FALSE, "not", "none")
#' @export
f0 <- function(X, Y, N) {if (base::isTRUE(uj::failsafe(X))) {uj::failsafe(Y)} else {uj::failsafe(N)}}

#' @rdname fork
#' @export
fork <- function(X, Y, N, NAS = N) {
  nx <- base::length(X)
  ny <- base::length(Y)
  nn <- base::length(N)
  nna <- base::length(NAS)
  nas.err <- uj::f0(nna == 1 & base::is.character(NAS), NAS == 'err', F)
  nas.na <- uj::f0(nna == 1 & base::is.atomic(NAS), base::is.na(NAS), F)
  inc.nas <- nas.err | nas.na
  ok.x <- uj:::.cmp_lgl_vec(X)
  ok.y <- uj::f0(!uj:::.VEC(Y), F, uj::f0(!ok.x, T, ny %in% base::c(1, base::max(1, nx))))
  ok.n <- uj::f0(!uj:::.VEC(N), F, uj::f0(!ok.x, T, nn %in% base::c(1, base::max(1, nx))))
  ok.na <- uj::f0(!uj:::.VEC(NAS), F, uj::f0(!ok.x, T, nna %in% base::c(1, base::max(1, nx))))
  errs <- NULL
  if (!ok.x) {errs <- base::c(errs, "[X] must be a logical vec (?cmp_lgl_vec).")}
  if (!ok.y) {errs <- base::c(errs, "[Y] must be of length 1 or a vector of the same length as [x].")}
  if (!ok.n) {errs <- base::c(errs, "[N] must be of length 1 or a vector of the same length as [x].")}
  if (!ok.na) {errs <- base::c(errs, "[NAS] must be of length 1 or a vector of the same length as [x].")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  ok.tny <- ok.x & ok.y & ok.n
  ok.arg <- uj::f0(!nas.err | !ok.x, T, uj:::.cmp_lgl_vec(X))
  ok.tny <- uj::f0(!ok.tny, NULL, uj::f0(inc.nas, uj::compatible(Y, N, NAS), uj::compatible(Y, N)))
  if (!ok.arg) {errs <- base::c(errs, "[NAS = 'err'] but [X] contains NA values.")}
  if (!ok.tny) {errs <- base::c(errs, uj::f0(inc.nas, "[Y], [N], and [NAS] must be of compatible (?compatible) modes.", "[Y] and [N] must be of compatible (?compatible) modes."))}
  if (ny  == 1) {Y <- base::rep.int(Y, nx)}
  if (nn  == 1) {N <- base::rep.int(Y, nx)}
  if (nna == 1) {NAS <- base::rep.int(NAS, nx)}
  out <- base::rep.int(NA, nx)
  iT <- base::sapply(X, isTRUE)
  iF <- base::sapply(X, isFALSE)
  iN <- base::is.na(X)
  out[iT] <- Y[iT]
  out[iF] <- N[iF]
  out[iN] <- NAS[iN]
  out
}

#' @rdname fork
#' @export
f1 <- function(X, Y, N, NAS = N, ERR = N) {
  err.err <- uj::f0(uj:::.cmp_chr_scl(ERR), ERR == "err", F)
  nas.err <- uj::f0(uj:::.cmp_chr_scl(NAS), NAS == "err", F)
  nax <- uj:::.NA0(x)
  X <- uj::failsafe(X)
  if (base::isTRUE(X)) {Y}
  else if (base::isFALSE(X)) {N}
  else if (nax & !nas.err) {NAS}
  else if (!uj:::.lgl_scl(X) & !err.err) {ERR}
  else if (nax) {uj::stopperr("[x] must be atomic, scalar, and TRUE, FALSE, or NA.", PKG = "uj")}
  else {uj::stopperr("[x] must be atomic, scalar, and TRUE or FALSE.", PKG = "uj")}
}

#' @rdname fork
#' @export
nll_if <- function(X, ..., D = " ") {
  if (!uj:::.cmp_chr_scl(D)) {D <-  " "}
  uj::f0(base::isTRUE(X), NULL, base::paste0(uj::av(...), collapse = D))
}

#' @rdname fork
#' @export
nll_ifs <- function(..., D = " ", COND = "all") {
  bad_dots <- function(STACK) {uj::stopperr("There must be both named and unnamed [...] args.", FUN = "nll_ifs", PKG = "uj", STACK = STACK)}
  if (!uj:::.cmp_chr_scl(uj::failsafe(D))) {D <- " "}
  if (!uj:::.cmp_chr_scl(COND, valid = base::c("all", "any", "none"))) {COND <- "all"}
  dots <- base::list(...)
  labs <- base::names(dots)
  if (base::length(labs) == 0) {bad_dots(uj::callers())}
  is.named <- !base::is.na(labs)
  is.named[is.named] <- labs[is.named] != ""
  if (base::all(is.named) | !base::any(is.named)) {bad_dots(uj::callers())}
  known <- dots[is.named]
  anon <- base::paste0(uj::av(dots[!is.named]), collapse = D)
  for (i in 1:base::length(known)) {
    true <- base::isTRUE(uj::failsafe(known[[i]]))
    if      (COND == "any"  &  true) {return(NULL)}
    else if (COND == "all"  & !true) {return(anon)}
    else if (COND == "none" &  true) {return(anon)}
  }
  if (COND == "any") {NULL} else {anon}
}

#' @rdname fork
#' @export
nll_if_any <- function(..., D = " ") {uj::nll_ifs(..., D = " ", COND = "any")}

#' @rdname fork
#' @export
nll_if_all <- function(..., D = " ") {uj::nll_ifs(..., D = " ", COND = "all")}

#' @rdname fork
#' @export
nll_if_none <- function(..., D = " ") {uj::nll_ifs(..., D = " ", COND = "none")}

#' @rdname fork
#' @export
nll_ifs <- function(..., D = " ") {
  if (!uj:::.cmp_chr_scl(uj::failsafe(D))) {D <- " "}
  ndots <- base::...length()
  npairs <- ndots / 2
  has.pairs <- npairs == base::round(npairs)
  if (ndots == 0 | !has.pairs) {uj::stopperr("The number of [...] args must be even and greater than 0.", PKG = "uj")}
  y <- NULL
  for (i in base::seq(from = 1, to = ndots - 1, by = 2)) {
    test <- uj::failsafe(base::...elt(i))
    mssg <- uj::failsafe(base::...elt(i + 1))
    y <- base::c(y, uj::nll_if(test, mssg, D = D))
  }
  y
}
