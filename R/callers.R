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
#' \tabular{ll}{  `ncallers`        \tab Number of calling functions (excludes *self*).           \cr   \tab   \cr
#'                `callersN`        \tab Names of the calling function(s) `n` generation(s) back. \cr   \tab   \cr
#'                `callers`         \tab Names of all calling functions, (excludes *self*).       \cr   \tab   \cr
#'                `lineage`         \tab Names of all lineage functions (includes *self*).                       }
#' @param n A \link[cmp_psw_vec]{complete positive whole-number scalar} giving the number of generations back in the function call stack to go.
#' @param .ERR `TRUE` or `FALSE` indicating whether to throw an error if one is encountered.
#' @param .SCL `TRUE` or `FALSE` indicating whether to collapse package and function into a character scalar rather than as a two element list with one element for packages and another for functions.
#' @param .VEC `TRUE` or `FALSE` indicating whether to represent both package and function in a character vector rather than as a two element list with one element for packages and another for functions.
#' @param ... An arbitrary number of \link[=cmp_psw_vec]{complete positive whole-number vecs} giving the number(s) of generations back in the function call stack to go.
#' @return **An integer scalar**                                            \cr\cr `ncallers`
#' \cr\cr  **A `list(pkg = <character vector>, fun = <character vector>)`** \cr\cr  All others when `vec = FALSE` or `scl = FALSE`.
#' \cr\cr  **A character vector** (when `vec = TRUE`)                       \cr\cr `callers, lineage, callersN`
#' \cr\cr  **A character scalar** (when `scl = TRUE`)                       \cr\cr `ancestor, caller, caller1, caller2, callerN, self`
#' @examples
#' egD <- function() {list(self      = self()                   ,
#'                         caller    = caller()                 ,
#'                         callers   = callers()                ,
#'                         caller2   = caller2(.ERR = F)        ,
#'                         caller3   = callerN(3, .ERR = F)     ,
#'                         callers23 = callersN(2, 3, .ERR = F) ,
#'                         ncallers  = ncallers()               ,
#'                         ancestor  = ancestor()               ,
#'                         lineage   = lineage()                )}
#' egC <- function() {list(self      = self()                   ,
#'                         caller    = caller()                 ,
#'                         callers   = callers()                ,
#'                         caller2   = caller2(.ERR = F)        ,
#'                         caller3   = callerN(3, .ERR = F)     ,
#'                         callers23 = callersN(2, 3, .ERR = F) ,
#'                         ncallers  = ncallers()               ,
#'                         ancestor  = ancestor()               ,
#'                         lineage   = lineage()                ,
#'                         egD       = egD()                    )}
#' egB <- function() {list(self      = self()                   ,
#'                         caller    = caller()                 ,
#'                         callers   = callers()                ,
#'                         caller2   = caller2(.ERR = F)        ,
#'                         caller3   = callerN(3, .ERR = F)     ,
#'                         callers23 = callersN(2, 3, .ERR = F) ,
#'                         ncallers  = ncallers()               ,
#'                         ancestor  = ancestor()               ,
#'                         lineage   = lineage()                ,
#'                         egC       = egC()                    )}
#' egA <- function() {list(self      = self()                   ,
#'                         caller    = caller()                 ,
#'                         callers   = callers()                ,
#'                         caller2   = caller2(.ERR = F)        ,
#'                         caller3   = callerN(3, .ERR = F)     ,
#'                         callers23 = callersN(2, 3, .ERR = F) ,
#'                         ncallers  = ncallers()               ,
#'                         ancestor  = ancestor()               ,
#'                         lineage   = lineage()                ,
#'                         egB       = egB()                    )}
#' egA()
#' av(egA())
#' @export
callers <- function(.VEC = TRUE) {
  if (!uj::fs_t(.VEC)) {.VEC <- F}
  Stack <- base::c(base::rev(base::as.character(base::sys.calls())), "..R..::..command.line..()")
  if (base::length(Stack) == 2) {Stack <- base::c(Stack, "..R..::..command.line..()")}
  uj:::.stack2pkgfuns(Stack[3:base::length(Stack)], 35, .VEC = .VEC)
}

#' @rdname callers
#' @export
caller <- function(.SCL = TRUE) {
  if (!uj::fs_t(.SCL)) {.SCL <- F}
  Stack <- base::sys.calls()
  Stack <- base::c(base::rev(base::as.character(Stack)), "..R..::..command.line..()")
  if (base::length(Stack) == 2) {Stack <- base::c(Stack, "..R..::..command.line..()")}
  uj:::.stack2pkgfuns(Stack[3], 100, .VEC = .SCL)
}

#' @rdname callers
#' @export
caller1 <- caller

#' @rdname callers
#' @export
caller2 <- function(.SCL = TRUE) {
  if (!uj::fs_t(.SCL)) {.SCL <- F}
  Stack <- base::sys.calls()
  Stack <- base::c(base::rev(base::as.character(Stack)), "..R..::..command.line..()")
  if (base::length(Stack) == 2) {Stack <- base::c(Stack, "..R..::..command.line..()")}
  if (base::length(Stack) == 3) {Stack <- base::c(Stack, "..R..::..command.line..()")}
  uj:::.stack2pkgfuns(Stack[4], 100, .VEC = .SCL)
}

#' @rdname callers
#' @export
callerN <- function(n, .ERR = TRUE, .SCL = TRUE) {
  if (!uj::fs_t(.ERR)) {.ERR <- T}
  if (!uj::fs_t(.SCL)) {.SCL <- T}
  if (!uj:::.cmp_psw_scl(n)) {uj::stopperr("[n] must be a complete positive whole number scalar (?cmp_psw_scl).", .PKG = "uj")}
  Stack <- base::sys.calls()
  Stack <- base::c(base::rev(base::as.character(Stack)), "..R..::..command.line..()")
  if (base::length(Stack) == 2) {Stack <- base::c(Stack, "..R..::..command.line..()")}
  Stack <- Stack[3:base::length(Stack)]
  nStack <- base::length(Stack)
  if (.ERR & n > nStack) {uj::stopperr("[n] is greater than number of calling functions.", .PKG = "uj")}
  n <- base::min(n, nStack)
  uj:::.stack2pkgfuns(Stack[n], 100, .VEC = .SCL)
}

#' @rdname callers
#' @export
callersN <- function(..., .ERR = TRUE, .VEC = TRUE) {
  if (!uj::fs_t(.ERR)) {.ERR <- T}
  if (!uj::fs_t(.VEC)) {.VEC <- T}
  Ns <- uj::av(...)
  if (!uj:::.cmp_psw_vec(Ns)) {uj::stopperr("All [...] args must be complete positive whole-number vecs (?cmp_psw_vec).", .PKG = "uj")}
  Stack <- base::sys.calls()
  Stack <- base::c(base::rev(base::as.character(Stack)), "..R..::..command.line..()")
  if (base::length(Stack) == 2) {Stack <- base::c(Stack, "..R..::..command.line..()")}
  Stack <- Stack[3:base::length(Stack)]
  nStack <- base::length(Stack)
  if (err & base::any(Ns > nStack)) {uj::stopperr("A value in [...] is greater than number of calling functions.", .PKG = "uj")}
  uj:::.stack2pkgfuns(Stack[Ns[Ns <= nStack]], 35, .VEC = .VEC)
}

#' @rdname callers
#' @export
ncallers <- function() {
  Stack <- base::sys.calls()
  Stack <- base::c(base::rev(base::as.character(Stack)), "..R..::..command.line..()")
  if (base::length(Stack) == 2) {Stack <- base::c(Stack, "..R..::..command.line..()")}
  Stack <- Stack[3:base::length(Stack)]
  base::length(uj:::.stack2pkgfuns(Stack, 35, T))
}

#' @rdname callers
#' @export
lineage <- function(.VEC = TRUE) {
  if (!uj::fs_t(.VEC)) {.VEC <- F}
  Stack <- base::sys.calls()
  Stack <- base::c(base::rev(base::as.character(Stack)), "..R..::..command.line..()")
  Stack <- Stack[2:base::length(Stack)]
  uj:::.stack2pkgfuns(Stack, 35, .VEC = .VEC)
}

#' @rdname callers
#' @export
self <- function(.SCL = TRUE) {
  if (!uj::fs_t(.SCL)) {.SCL <- F}
  Stack <- base::sys.calls()
  Stack <- base::c(base::rev(base::as.character(Stack)), "..R..::..command.line..()")[2]
  uj:::.stack2pkgfuns(Stack, 100, .VEC = .SCL)
}

#' @rdname callers
#' @export
ancestor <- function(.SCL = TRUE) {
  if (!uj::fs_t(.SCL)) {.SCL <- F}
  Stack <- uj::callers()
  Stack[base::max(1, base::length(Stack) - 1)]
  uj:::.stack2pkgfuns(Stack, 100, .VEC = .SCL)
}
