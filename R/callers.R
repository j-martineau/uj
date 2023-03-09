# internals ####

#' @encoding UTF-8
#' @family environments
#' @family meta
#' @title Function names and counts in the call stack
#' @description This family of functions addresses the function call stack *from the perspective of a function that calls one of the functions in this family*. Consistent with that perspective, the basic terminology this family of functions uses is the following:
#' \tabular{ll}{  **Terminology**   \tab **Function identity**                       \cr
#'                "command line"    \tab The command line                            \cr
#'                "self"            \tab Function that called a `callers` function   \cr
#'                "caller"          \tab Parent of self                              \cr
#'                "caller2"         \tab Grandparent of self                         \cr
#'                "ancestor"        \tab Function called from the command line       \cr
#'                "callers"         \tab Complete ancestry (excluding self)          \cr
#'                "lineage"         \tab Complete lineage (including self)             }
#' @details Consistent with this terminology introduced in the *description*, the following function calls are associated with a *single generation* in a lineage, where `K` is the number of generations in the lineage:
#' \tabular{ll}{  *Generations*     \tab *Associated*                                \cr
#'                *backward*        \tab *function calls*                            \cr
#'                `0`               \tab `self()`                                    \cr
#'                `1`               \tab `caller(), caller1(), callerN(1)`           \cr
#'                `2`               \tab `caller2(), callerN(2)`                     \cr
#'                `3`               \tab `callerN(3)`                                \cr
#'                `4`               \tab `callerN(4)`                                \cr
#'                `...`             \tab `...`                                       \cr
#'                `K-1`             \tab `ancestor(), callerN(K-1)`                  \cr
#'                `K`               \tab `callerN(K)` (returns `'{ command line }'`).  }
#' \cr\cr Similarly, the following functions return values associated with at least `1` generation:
#' \tabular{ll}{  `ncallers`   \tab Number of calling functions (excludes *self*).           \cr   \tab   \cr
#'                `callersN`   \tab Names of the calling function(s) `N` generation(s) back. \cr   \tab   \cr
#'                `callers`    \tab Names of all calling functions, (excludes *self*).       \cr   \tab   \cr
#'                `lineage`    \tab Names of all lineage functions (includes *self*).                       }
#' @param n A \link[cmp_psw_vec]{complete positive whole-number scalar} giving the number of generations back in the function call stack to go.
#' @param scl `TRUE` or `FALSE` indicating wehther to collapse package and function into a character scalar rather than as a two element list with one element for packages and another for functions.
#' @param vec `TRUE` or `FALSE` indicating whether to represent both package and function in a character vector rather than as a two element list with one element for packages and another for functions.
#' @param ... An arbitrary number of \link[=cmp_psw_vec]{complete positive whole-number vecs} giving the number(s) of generations back in the function call stack to go.
#' @return **An integer scalar**                                            \cr\cr `ncallers`
#' \cr\cr  **A `list(pkg = <character vector>, fun = <character vector>)`** \cr\cr All others when `vec = FALSE` or `scl = FALSE`.
#' \cr\cr  **A character vector** (when `vec = TRUE`)                       \cr\cr `callers, lineage, callersN`
#' \cr\cr  **A character scalar** (when `scl = TRUE`)                       \cr\cr `ancestor, caller, caller1, caller2, callerN, self`
#' @examples
#' egD <- function() {list(self      = self()                   ,
#'                         caller    = caller()                 ,
#'                         callers   = callers()                ,
#'                         caller2   = caller2(err = F)         ,
#'                         caller3   = callerN(3, err = F)      ,
#'                         callers23 = callersN(2, 3, err = F)  ,
#'                         ncallers  = ncallers()               ,
#'                         ancestor  = ancestor()               ,
#'                         lineage   = lineage()                )}
#' egC <- function() {list(self      = self()                   ,
#'                         caller    = caller()                 ,
#'                         callers   = callers()                ,
#'                         caller2   = caller2(err = F)         ,
#'                         caller3   = callerN(3, err = F)      ,
#'                         callers23 = callersN(2, 3, err = F)  ,
#'                         ncallers  = ncallers()               ,
#'                         ancestor  = ancestor()               ,
#'                         lineage   = lineage()                ,
#'                         egD       = egD()                    )}
#' egB <- function() {list(self      = self()                   ,
#'                         caller    = caller()                 ,
#'                         callers   = callers()                ,
#'                         caller2   = caller2(err = F)         ,
#'                         caller3   = callerN(3, err = F)      ,
#'                         callers23 = callersN(2, 3, err = F)  ,
#'                         ncallers  = ncallers()               ,
#'                         ancestor  = ancestor()               ,
#'                         lineage   = lineage()                ,
#'                         egC       = egC()                    )}
#' egA <- function() {list(self      = self()                   ,
#'                         caller    = caller()                 ,
#'                         callers   = callers()                ,
#'                         caller2   = caller2(err = F)         ,
#'                         caller3   = callerN(3, err = F)      ,
#'                         callers23 = callersN(2, 3, err = F)  ,
#'                         ncallers  = ncallers()               ,
#'                         ancestor  = ancestor()               ,
#'                         lineage   = lineage()                ,
#'                         egB       = egB()                    )}
#' egA()
#' av(egA())
#' @export
callers <- function(vec = TRUE) {
  if (!uj::fs_t(vec)) {vec <- F}
  stack <- base::c(base::rev(base::as.character(base::sys.calls())), "..command.line..")
  if (base::length(stack) == 2) {stack <- base::c(stack, "..command.line..")}
  uj:::.pkg_fun(stack[3:base::length(stack)], 35, uj::f0(vec, "vec", "vls"))
}

#' @rdname callers
#' @export
caller <- function(scl = TRUE) {
  if (!uj::fs_t(scl)) {scl <- F}
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..command.line..")
  if (base::length(stack) == 2) {stack <- base::c(stack, "..command.line..")}
  uj:::.pkg_fun(stack[3], 100, uj::f0(scl, "scl", "vls"))
}

#' @rdname callers
#' @export
caller1 <- caller

#' @rdname callers
#' @export
caller2 <- function(scl = TRUE) {
  if (!uj::fs_t(scl)) {scl <- F}
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..command.line..")
  if (base::length(stack) == 2) {stack <- base::c(stack, "..command.line..")}
  if (base::length(stack) == 3) {stack <- base::c(stack, "..command.line..")}
  uj:::.pkg_fun(stack[4], 100, uj::f0(scl, "scl", "vls"))
}

#' @rdname callers
#' @export
callerN <- function(n, err = TRUE, scl = TRUE) {
  if (!uj::fs_t(err)) {err <- T}
  if (!uj::fs_t(scl)) {scl <- T}
  if (!uj:::.cmp_psw_scl(N)) {uj::stopperr("[n] must be a complete positive whole number scalar (?cmp_psw_scl).", PKG = "uj")}
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..command.line..")
  if (base::length(stack) == 2) {stack <- base::c(stack, "..command.line..")}
  stack <- stack[3:base::length(stack)]
  N <- base::length(stack)
  if (err & n > N) {uj::stopperr("[n] is greater than number of calling functions.", PKG = "uj")}
  n <- base::min(n, N)
  uj:::.pkg_fun(stack[n], 100,  uj::f0(scl, "scl", "vls"))
}

#' @rdname callers
#' @export
callersN <- function(..., err = TRUE, vec = TRUE) {
  if (!uj::fs_t(err)) {err <- T}
  if (!uj::fs_t(vec)) {vec <- T}
  Ns <- uj::av(...)
  if (!uj:::.cmp_psw_vec(Ns)) {uj::stopperr("All [...] args must be complete positive whole-number vecs (?cmp_psw_vec).", PKG = "uj")}
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..command.line..")
  if (base::length(stack) == 2) {stack <- base::c(stack, "..command.line..")}
  stack <- stack[3:base::length(stack)]
  N <- base::length(stack)
  if (err & base::any(Ns > N)) {uj::stopperr("A value in [...] is greater than number of calling functions.", PKG = "uj")}
  uj:::.pkg_fun(stack[Ns[Ns <= N]], 35, uj::f0(vec, "vec", "vls"))
}

#' @rdname callers
#' @export
ncallers <- function() {
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..command.line..")
  if (base::length(stack) == 2) {stack <- base::c(stack, "..command.line..")}
  stack <- stack[3:base::length(stack)]
  base::length(uj:::.pkg_fun(stack, 35, T))
}

#' @rdname callers
#' @export
lineage <- function(vec = TRUE) {
  if (!uj::fs_t(vec)) {vec <- F}
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..command.line..")
  stack <- stack[2:base::length(stack)]
  uj:::.pkg_fun(stack, 35, uj::vec("vec", "vls"))
}

#' @rdname callers
#' @export
self <- function(scl = TRUE) {
  if (!uj::fs_t(scl)) {scl <- F}
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..command.line..")[2]
  uj:::.pkg_fun(stack, 100, uj::f0(scl, "scl", "vls"))
}

#' @rdname callers
#' @export
ancestor <- function(scl = TRUE) {
  if (!uj::fs_t(scl)) {scl <- F}
  stack <- uj::callers()
  stack[base::max(1, base::length(stack) - 1)]
  uj:::.pkg_fun(stack, 100, uj::f0(scl, "scl", "vls"))
}
