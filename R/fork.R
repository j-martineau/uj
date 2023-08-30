#' @name fork
#' @encoding UTF-8
#' @family extensions
#' @family forks
#' @title Enhancements of \code{\link[base]{ifelse}}.
#' @description Return different types of objects for `TRUE` and `FALSE` and return `NULL`  conditional on the number of `TRUE` values.
#' @details **`fork`**
#' \cr\cr Evaluates logical scalar or logical vector `x` and return an object of the same length as `x` where:
#' \itemize{\item `TRUE` values of `x` are replaced by corresponding values of `y`.
#'          \item `FALSE` values of `x` are replaced by corresponding values of `n`.
#'          \item `NA` values of `x` are replaced by `.NA` (unless `.NA = 'err'`, in which case if there are any `NA` values in `x`, throws an error). }
#' \cr\cr **`f0`**
#' \cr\cr If `x` is scalar `TRUE`, returns `y`. If `x` is anything else, returns `n`.
#' \cr\cr **`f1`**
#' \cr\cr Error-checked version of `f0`. Evaluates and processes logical scalar `x` in the following manner:
#' \itemize{\item If `x = TRUE`, returns `y`.
#'          \item If `x = FALSE`, returns `y`.
#'          \item If `x = NA`, returns `.NA` unless `.NA = 'err'`, in which case, an error is thrown.
#'          \item If `x` is neither a logical scalar nor scalar `NA`, returns `err` unless `.ERR = 'err'`, in which case an error is thrown. }
#' \cr\cr **Functions beginning with `nll_if`**
#' \cr\cr These functions are useful for compiling error messages. They thus return `NULL` if error checks are passed and a message if they are not.
#' \cr\cr **`nll_if`**
#' \cr\cr If `x` is scalar `TRUE`, returns `NULL`, otherwise collapses `...` args to a character scalar using delimiter `.D` and returns the result.
#' \cr\cr **`nll_if_<cond.>`**
#' \cr\cr These functions take both named and unnamed `...` args. Named `...` args other than `.D` are evaluated for `TRUE`-ness (any value that is not scalar `TRUE` is considered `FALSE`). Unnamed `...` args are \link[=glue_dots]{collapsed} into a character scalar value named `.D` using the delimiter in arg `.D`
#' \tabular{ll}{  `nll_if_none`   \tab Returns `.D` upon encountering a `TRUE` named `...` arg. Returns `NULL` if none is encountered. \cr   \tab   \cr
#'                `nll_if_any`    \tab Returns `NULL` upon encountering a `TRUE` named `...` arg. Returns `.D` if none is encountered. \cr   \tab   \cr
#'                `nll_if_all`    \tab Returns `.D` upon encountering a non-`TRUE` named `...` arg. Returns `NULL` if none is encountered.            }
#' \cr\cr **`nll_ifs`**
#' \cr\cr Calls `nll_if_none(..., .D = .D)` when `.COND = 'none'`. Calls `nll_if_any(..., .D = .D)` when `.COND = 'any`. Calls `nll_if_all(..., .D = .D)` when `.COND` takes any other value (including `'all'`)
#' \cr\cr **`nlls_ifs`**
#' \cr\cr Conditionally compiles messages into a character vector. Each non-`TRUE` odd-numbered `...` arg's message (\link[=glue_dots]{collapsed} from the following `...` arg) is added to the compilation. If all odd-numbered `...` args are `TRUE`, returns `NULL`.
#' @param .NA An object of any type for `f1`. An atomic scalar \link[=compatible]{compatible} with `y` and `n` for `fork`, with the additional possibility of `.NA = 'err'` to indicate an error should be thrown if any values in `x` are `NA`.
#' @param .ERR Either `'err'` or an object to be returned when `x` is not an atomic scalar in `c(TRUE, FALSE, NA)`.
#' @param x A logical scalar (if not,`x` it is replaced by `FALSE`).
#' @param y,n Any valid R object.
#' @param .D A character scalar delimiter for collapsing objects into scalar character objects. If `.D` is not a character scalar, it is replaced by `" "`.
#' @param .COND A character scalar in `c('all', 'any', 'none')`. If `.COND` is not of an allowed value, it is replaced by `'all'`.
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
#' nll_ifs(t1 = TRUE, t2 = FALSE, t3 = FALSE, "not", "any", .COND = "all")
#' nll_ifs(t1 = TRUE, t2 = FALSE, t3 = FALSE, "not", "all", .COND = "any")
#' nll_ifs(t1 = TRUE, t2 = FALSE, t3 = FALSE, "not", "none", .COND = "none")
#' nll_if_any(t1 = TRUE, t2 = FALSE, t3 = FALSE, "not", "any")
#' nll_if_all(t1 = TRUE, t2 = FALSE, t3 = FALSE, "not", "all")
#' nll_if_none(t1 = TRUE, t2 = FALSE, t3 = FALSE, "not", "none")
#' @export
f0 <- function(x, y, n) {if (base::isTRUE(uj::failsafe(x))) {uj::failsafe(y)} else {uj::failsafe(n)}}

#' @rdname fork
#' @export
fork <- function(x, y, n, .NA = n) {
  nX <- base::length(x)
  nY <- base::length(y)
  nN <- base::length(n)
  nNA <- base::length(.NA)
  ErrNA <- uj::f0(nNA == 1 & base::is.character(.NA), .NA == 'err', F)
  NANA <- uj::f0(nNA == 1 & base::is.atomic(.NA), base::is.na(.NA), F)
  IncNas <- ErrNA | NANA
  OkX <- uj:::.cmp_lgl_vec(x)
  OkY <- uj::f0(!uj:::.VEC(y), F, uj::f0(!OkX, T, nY %in% base::c(1, base::max(1, nX))))
  OkN <- uj::f0(!uj:::.VEC(n), F, uj::f0(!OkX, T, nN %in% base::c(1, base::max(1, nX))))
  OkNA <- uj::f0(!uj:::.VEC(.NA), F, uj::f0(!OkX, T, nNA %in% base::c(1, base::max(1, nX))))
  Errors <- NULL
  if (!OkX) {Errors <- base::c(Errors, "[x] must be a logical vec (?cmp_lgl_vec).")}
  if (!OkY) {Errors <- base::c(Errors, "[y] must be of length 1 or a vector of the same length as [x].")}
  if (!OkN) {Errors <- base::c(Errors, "[n] must be of length 1 or a vector of the same length as [x].")}
  if (!OkNA) {Errors <- base::c(Errors, "[.NA] must be of length 1 or a vector of the same length as [x].")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  OkTny <- OkX & OkY & OkN
  OkArg <- uj::f0(!ErrNA | !OkX, T, uj:::.cmp_lgl_vec(x))
  OkTny <- uj::f0(!OkTny, NULL, uj::f0(IncNas, uj::compatible(y, n, .NA), uj::compatible(y, n)))
  if (!OkArg) {Errors <- base::c(Errors, "[.NA = 'err'] but [x] contains NA values.")}
  if (!OkTny) {Errors <- base::c(Errors, uj::f0(IncNas, "[y], [n], and [.NA] must be of compatible (?compatible) modes.", "[y] and [n] must be of compatible (?compatible) modes."))}
  if (nY  == 1) {y <- base::rep.int(y, nX)}
  if (nN  == 1) {n <- base::rep.int(y, nX)}
  if (nNA == 1) {.NA <- base::rep.int(.NA, nX)}
  y <- base::rep.int(NA, nX)
  iT <- base::sapply(x, isTRUE)
  iF <- base::sapply(x, isFALSE)
  iN <- base::is.na(x)
  y[iT] <- y[iT]
  y[iF] <- n[iF]
  y[iN] <- .NA[iN]
  y
}

#' @rdname fork
#' @export
f1 <- function(x, y, n, .NA = n, .ERR = n) {
  ErrErr <- uj::f0(uj:::.cmp_chr_scl(.ERR), .ERR == "err", F)
  ErrNAS <- uj::f0(uj:::.cmp_chr_scl(.NA), .NA == "err", F)
  NaX <- uj:::.NA0(x)
  x <- uj::failsafe(x)
  if (base::isTRUE(x)) {y}
  else if (base::isFALSE(x)) {n}
  else if (NaX & !ErrNAS) {.NA}
  else if (!uj:::.lgl_scl(x) & !ErrErr) {.ERR}
  else if (NaX) {uj::stopperr("[x] must be atomic, scalar, and TRUE, FALSE, or NA.", .PKG = "uj")}
  else {uj::stopperr("[x] must be atomic, scalar, and TRUE or FALSE.", .PKG = "uj")}
}

#' @rdname fork
#' @export
nll_if <- function(x, ..., .D = " ") {
  if (!uj:::.cmp_chr_scl(.D)) {.D <-  " "}
  uj::f0(base::isTRUE(x), NULL, base::paste0(uj::av(...), collapse = .D))
}

#' @rdname fork
#' @export
nll_ifs <- function(..., .D = " ", .COND = "all") {
  .bad_dots <- function(STACK) {uj::stopperr("There must be both named and unnamed [...] args.", FUN = "nll_ifs", .PKG = "uj", STACK = STACK)}
  if (!uj:::.cmp_chr_scl(uj::failsafe(.D))) {.D <- " "}
  if (!uj:::.cmp_chr_scl(.COND, Valid = base::c("all", "any", "none"))) {.COND <- "all"}
  Dots <- base::list(...)
  Labs <- base::names(Dots)
  if (base::length(Labs) == 0) {.bad_dots(uj::callers())}
  IsNamed <- !base::is.na(Labs)
  IsNamed[IsNamed] <- Labs[IsNamed] != ""
  if (base::all(IsNamed) | !base::any(IsNamed)) {.bad_dots(uj::callers())}
  Known <- Dots[IsNamed]
  Anon <- base::paste0(uj::av(Dots[!IsNamed]), collapse = .D)
  for (i in 1:base::length(Known)) {
    True <- base::isTRUE(uj::failsafe(Known[[i]]))
    if (.COND == "any" & True) {return(NULL)}
    else if (.COND == "all" & !True) {return(Anon)}
    else if (.COND == "none" & True) {return(Anon)}
  }
  if (.COND == "any") {NULL} else {Anon}
}

#' @rdname fork
#' @export
nll_if_any <- function(..., .D = " ") {uj::nll_ifs(..., .D = " ", .COND = "any")}

#' @rdname fork
#' @export
nll_if_all <- function(..., .D = " ") {uj::nll_ifs(..., .D = " ", .COND = "all")}

#' @rdname fork
#' @export
nll_if_none <- function(..., .D = " ") {uj::nll_ifs(..., .D = " ", .COND = "none")}

#' @rdname fork
#' @export
nll_ifs <- function(..., .D = " ") {
  if (!uj:::.cmp_chr_scl(uj::failsafe(.D))) {.D <- " "}
  nDots <- base::...length()
  nPairs <- nDots / 2
  HasPairs <- nPairs == base::round(nPairs)
  if (nDots == 0 | !HasPairs) {uj::stopperr("The number of [...] args must be even and greater than 0.", .PKG = "uj")}
  y <- NULL
  for (i in base::seq(from = 1, to = nDots - 1, by = 2)) {
    Test <- uj::failsafe(base::...elt(i))
    Message <- uj::failsafe(base::...elt(i + 1))
    y <- base::c(y, uj::nll_if(Test, Message, .D = .D))
  }
  y
}
