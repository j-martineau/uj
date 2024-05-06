# internals ####

.getfun <- function(x) {
  def <- "..unknown.."
  x   <- uj::av(base::strsplit(x, ":::", fixed = T))
  x   <- uj::av(base::strsplit(x, "::" , fixed = T))
  N   <- base::length(x)
  if (N == 0) {x <- ""} else if (N > 1) {x <- x[2]}
  x   <- uj::av(base::strsplit(x, "(", fixed = T))
  x   <- uj::av(base::strsplit(x, " ", fixed = T))
  n   <- base::length(x)
  if (N == 0) {x <- ""} else if (n > 1) {x <- x[1]}
  if (x == "") {def} else {x}
}

.getpkg <- function(x) {
  def <- "..??.."
  x   <- uj::av(base::strsplit(x, ":::", fixed = T))
  x   <- uj::av(base::strsplit(x, ":: ", fixed = T))
  n   <- base::length(x)
  if (n < 2) {def} else {x[1]}
}

.getsep <- function(x) {if (base::length(uj::av(base::strsplit(x, ":::", fixed = T))) > 1) {":::"} else {"::"}}

.stack2pkgfuns <- function(stack, max.len = 35, vec = T, pkg.only = F, fun.only = F) {
  def  <- "..??.."
  funs <- base::sapply(stack, uj:::.getfun)
  pkgs <- base::sapply(stack, uj:::.getpkg)
  seps <- base::sapply(stack, uj:::.getsep)
  ok   <- funs != def
  funs <- funs[ok]
  pkgs <- pkgs[ok]
  seps <- seps[ok]
  pkgs[pkgs == "..R.."] <- "[R]"
  funs[funs == "..command.line.."] <- "[command.line]"
  pkgs[pkgs == def] <- "[??]"
  funs[funs == def] <- "[??]"
  if (pkg.only) {pkgs} else if (fun.only) {funs} else if (vec) {base::paste0(pkgs, seps, funs)} else {tibble::tibble(pkg = pkgs, fun = funs)}
}

.stack2funs <- function(stack, max.len) {
  def  <- "..??.."
  funs <- base::sapply(stack, uj:::.getfun)
  ok   <- funs != def
  funs <- funs[ok]
  if (base::length(funs) == 0) {funs <- def}
  funs[funs == "..command.line.."] <- "[command.line]"
  funs[funs == def               ] <- "[??]"
  funs
}

.stack2pkgs <- function(stack, max.len) {
  def  <- "..??.."
  funs <- base::sapply(stack, uj:::.getfun)
  pkgs <- base::sapply(stack, uj:::.getpkg)
  ok   <- funs != def
  pkgs <- pkgs[ok]
  if (base::length(pkgs) == 0) {pkgs <- def}
  pkgs[pkgs == "..R.."] <- "[R]"
  pkgs[pkgs ==  def   ] <- "[??]"
  pkgs
}

.fun_pkg_stack <- function(fun, pkg, stack, .fun, .stack) {
  if (fun   == "") {fun   <- .fun     }
  if (pkg   == "") {pkg   <- "..??.." }
  if (stack == "") {stack <- .stack   }
  fun   <- uj:::.stack2funs(fun, 1000)
  pkg   <- uj:::.stack2pkgs(pkg, 1000)
  stack <- base::paste0(uj:::.stack2pkgfuns(stack, max.len = 35, vec = T), collapse = " >> ")
  base::list(fun = fun, pkg = pkg, stack = stack)
}

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
#' @param err `TRUE` or `FALSE` indicating whether to throw an error if one is encountered.
#' @param scl `TRUE` or `FALSE` indicating whether to collapse package and function into a character scalar rather than as a two element list with one element for packages and another for functions.
#' @param vec `TRUE` or `FALSE` indicating whether to represent both package and function in a character vector rather than as a two element list with one element for packages and another for functions.
#' @param ... An arbitrary number of \link[=cmp_psw_vec]{complete positive whole-number vecs} giving the number(s) of generations back in the function call stack to go.
#' @return **An integer scalar**                                            \cr\cr `ncallers`
#' \cr\cr  **A `list(pkg = <character vector>, fun = <character vector>)`** \cr\cr  All others when `vec = FALSE` or `scl = FALSE`.
#' \cr\cr  **A character vector** (when `vec = TRUE`)                       \cr\cr `callers, lineage, callersN`
#' \cr\cr  **A character scalar** (when `scl = TRUE`)                       \cr\cr `ancestor, caller, caller1, caller2, callerN, self`
#' @examples
#' egD <- function() {list(self      = self()                   ,
#'                         caller    = caller()                 ,
#'                         callers   = callers()                ,
#'                         caller2   = caller2(err = F)        ,
#'                         caller3   = callerN(3, err = F)     ,
#'                         callers23 = callersN(2, 3, err = F) ,
#'                         ncallers  = ncallers()               ,
#'                         ancestor  = ancestor()               ,
#'                         lineage   = lineage()                )}
#' egC <- function() {list(self      = self()                   ,
#'                         caller    = caller()                 ,
#'                         callers   = callers()                ,
#'                         caller2   = caller2(err = F)        ,
#'                         caller3   = callerN(3, err = F)     ,
#'                         callers23 = callersN(2, 3, err = F) ,
#'                         ncallers  = ncallers()               ,
#'                         ancestor  = ancestor()               ,
#'                         lineage   = lineage()                ,
#'                         egD       = egD()                    )}
#' egB <- function() {list(self      = self()                   ,
#'                         caller    = caller()                 ,
#'                         callers   = callers()                ,
#'                         caller2   = caller2(err = F)        ,
#'                         caller3   = callerN(3, err = F)     ,
#'                         callers23 = callersN(2, 3, err = F) ,
#'                         ncallers  = ncallers()               ,
#'                         ancestor  = ancestor()               ,
#'                         lineage   = lineage()                ,
#'                         egC       = egC()                    )}
#' egA <- function() {list(self      = self()                   ,
#'                         caller    = caller()                 ,
#'                         callers   = callers()                ,
#'                         caller2   = caller2(err = F)        ,
#'                         caller3   = callerN(3, err = F)     ,
#'                         callers23 = callersN(2, 3, err = F) ,
#'                         ncallers  = ncallers()               ,
#'                         ancestor  = ancestor()               ,
#'                         lineage   = lineage()                ,
#'                         egB       = egB()                    )}
#' egA()
#' av(egA())
#' @export
callers <- function(vec = TRUE) {
  if (!uj::fs_t(vec)) {vec <- F}
  stack <- base::c(base::rev(base::as.character(base::sys.calls())), "..r..::..command.line..()")
  if (base::length(stack) == 2) {stack <- base::c(stack, "..r..::..command.line..()")}
  uj:::.stack2pkgfuns(stack[3:base::length(stack)], 35, vec = vec)
}

#' @rdname callers
#' @export
caller <- function(scl = TRUE) {
  if (!uj::fs_t(scl)) {scl <- F}
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..r..::..command.line..()")
  if (base::length(stack) == 2) {stack <- base::c(stack, "..r..::..command.line..()")}
  uj:::.stack2pkgfuns(stack[3], 100, vec = scl)
}

#' @rdname callers
#' @export
caller1 <- caller

#' @rdname callers
#' @export
caller2 <- function(scl = TRUE) {
  if (!uj::fs_t(scl)) {scl <- F}
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..r..::..command.line..()")
  if (base::length(stack) == 2) {stack <- base::c(stack, "..r..::..command.line..()")}
  if (base::length(stack) == 3) {stack <- base::c(stack, "..r..::..command.line..()")}
  uj:::.stack2pkgfuns(stack[4], 100, vec = scl)
}

#' @rdname callers
#' @export
callerN <- function(n, err = TRUE, scl = TRUE) {
  if (!uj::fs_t(err)) {err <- T}
  if (!uj::fs_t(scl)) {scl <- T}
  if (!uj::.cmp_psw_scl(n)) {uj::stopperr("[n] must be a complete positive whole number scalar (?cmp_psw_scl).", pkg = "uj")}
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..r..::..command.line..()")
  if (base::length(stack) == 2) {stack <- base::c(stack, "..r..::..command.line..()")}
  stack <- stack[3:base::length(stack)]
  nstack <- base::length(stack)
  if (err & n > nstack) {uj::stopperr("[n] is greater than number of calling functions.", pkg = "uj")}
  n <- base::min(n, nstack)
  uj:::.stack2pkgfuns(stack[n], 100, vec = scl)
}

#' @rdname callers
#' @export
callersN <- function(..., err = TRUE, vec = TRUE) {
  if (!uj::fs_t(err)) {err <- T}
  if (!uj::fs_t(vec)) {vec <- T}
  Ns <- uj::av(...)
  if (!uj::.cmp_psw_vec(Ns)) {uj::stopperr("All [...] args must be complete positive whole-number vecs (?cmp_psw_vec).", pkg = "uj")}
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..r..::..command.line..()")
  if (base::length(stack) == 2) {stack <- base::c(stack, "..r..::..command.line..()")}
  stack <- stack[3:base::length(stack)]
  nstack <- base::length(stack)
  if (err & base::any(Ns > nstack)) {uj::stopperr("A value in [...] is greater than number of calling functions.", pkg = "uj")}
  uj:::.stack2pkgfuns(stack[Ns[Ns <= nstack]], 35, vec = vec)
}

#' @rdname callers
#' @export
ncallers <- function() {
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..r..::..command.line..()")
  if (base::length(stack) == 2) {stack <- base::c(stack, "..r..::..command.line..()")}
  stack <- stack[3:base::length(stack)]
  base::length(uj:::.stack2pkgfuns(stack, 35, T))
}

#' @rdname callers
#' @export
lineage <- function(vec = TRUE) {
  if (!uj::fs_t(vec)) {vec <- F}
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..r..::..command.line..()")
  stack <- stack[2:base::length(stack)]
  uj:::.stack2pkgfuns(stack, 35, vec = vec)
}

#' @rdname callers
#' @export
self <- function(scl = TRUE) {
  if (!uj::fs_t(scl)) {scl <- F}
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..r..::..command.line..()")[2]
  uj:::.stack2pkgfuns(stack, 100, vec = scl)
}

#' @rdname callers
#' @export
ancestor <- function(scl = TRUE) {
  if (!uj::fs_t(scl)) {scl <- F}
  stack <- uj::callers()
  stack[base::max(1, base::length(stack) - 1)]
  uj:::.stack2pkgfuns(stack, 100, vec = scl)
}
