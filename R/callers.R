# internals ####

.bad_if_not_name <- function(x) {uj::f0(uj::LEN(x) == 0, "< bad >", uj::f0(uj::allIN(x, base::c(base::letters, base::LETTERS, 0:9, ".", "_")), x, "< bad >"))}

.pkg_fun <- function(stack, lim, vec = T, pkg.only = F, fun.only = F) {
  bad <- "< bad >"
  unknown1 <- "..unknown.."
  unknown2 <- "{ unknown }"
  command1 <- "..command.line.."
  command2 <- "{ command line }"
  posP <- base::regexpr("(" , stack, fixed = T) - 1
  pos2 <- base::regexpr("::", stack, fixed = T) - 1
  pos3 <- base::regexpr(":::", stack, fixed = T) - 1
  iP <- posP > 0
  i2 <- pos2 > 0
  i3 <- pos3 > 0
  fun <- pkg <- base::rep.int(unknown1, uj::N(stack))
  pkg[i2] <- base::substr(stack[i2], 1, pos2[i2])
  pkg[i3] <- base::substr(stack[i3], 1, pos3[i3])
  fun.start <- base::pmax(1, pos2, pos3)
  fun[iP] <- base::substr(stack[iP], fun.start[iP], posP[iP])
  nch.pkg <- uj::LEN(pkg)
  nch.fun <- uj::LEN(fun)
  abb.pkg <- lim < nch.pkg
  abb.fun <- lim < nch.fun
  last.pkg <- base::pmin(lim, nch.pkg - 3)
  last.fun <- base::pmin(lim, nch.fun - 3)
  pkg[abb.pkg] <- uj::p0(base::substr(pkg[abb.pkg], 1, last.pkg[abb.pkg]), "...")
  fun[abb.fun] <- uj::p0(base::substr(fun[abb.fun], 1, last.fun[abb.fun]), "...")
  pkg <- base::sapply(pkg, uj:::.bad_if_not_name)
  fun <- base::sapply(fun, uj:::.bad_if_not_name)
  wild <- base::c(bad, unknown1)
  bad.fun <- uj::isIN1(fun, wild)
  pkg <- pkg[!bad.fun]
  fun <- fun[!bad.fun]
  i2 <- i2[!bad.fun]
  i3 <- i3[!bad.fun]
  pkg[pkg %in% wild] <- unknown1
  pkg[fun == command1] <- command2
  fun[fun == command1] <- command2
  if (uj::N0(pkg)) {
    pkg <- fun <- "{ command line }"
    i3 <- F
  } else {pkg[pkg == unknown1] <- unknown2}
  if (!pkg.only & !fun.only) {
    if (vec) {
      sep <- base::rep("::", uj::N(pkg))
      sep[i3] <- uj::p0(sep[i3], ":")
      uj::p0(pkg, sep, fun)
    } else {base::list(pkg = pkg, fun = fun)}
  } else if (pkg.only) {pkg}
  else {fun}
}

# exported ####

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
#'                `K`               \tab `callerN(K)` (returns `'{ command line }'`).  }
#' \cr\cr Similarly, the following functions return values associated with at least `1` generation:
#' \tabular{ll}{  `ncallers`    \tab Number of calling functions (excludes *self*).           \cr   \tab   \cr
#'                `callersN`    \tab Names of the calling function(s) `N` generation(s) back. \cr   \tab   \cr
#'                `callers`     \tab Names of all calling functions, (excludes *self*).       \cr   \tab   \cr
#'                `lineage`     \tab Names of all lineage functions (includes *self*).                       }
#' @param n A \link[cmp_psw_vec]{complete positive whole-number scalar} giving the number of generations back in the function call stack to go.
#' @param scl `TRUE` or `FALSE` indicating wehther to collapse package and function into a character scalar rather than as a two element list with one element for packages and another for functions.
#' @param vec `TRUE` or `FALSE` indicating whether to represent both package and function in a character vector rather than as a two element list with one element for packages and another for functions.
#' @param ... An arbitrary number of \link[=cmp_psw_vec]{complete positive whole-number vecs} giving the number(s) of generations back in the function call stack to go.
#' @return **An integer scalar**                                            \cr `ncallers`
#' \cr\cr  **A `list(pkg = <character vector>, fun = <character vector>)`** \cr All others when `vec = FALSE` or `scl = FALSE`.
#' \cr\cr  **A character vector** (when `vec = TRUE`)                       \cr `callers, lineage, callersN`
#' \cr\cr  **A character scalar** (when `scl = TRUE`)                       \cr `ancestor, caller, caller1, caller2, callerN, self`
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
  if (uj::notT(vec)) {vec <- F}
  stack <- base::c(base::rev(uj::asCHR(base::sys.calls())), "..command.line..")
  if (uj::N2(stack)) {stack <- base::c(stack, "..command.line..")}
  uj:::.pkg_fun(uj::Nth_plus(stack, 3), 35, uj::f0(vec, "vec", "vls"))
}

#' @rdname callers
#' @export
caller <- function(scl = TRUE) {
  if (uj::notT(scl)) {scl <- F}
  stack <- base::sys.calls()
  stack <- base::c(base::rev(uj::asCHR(stack)), "..command.line..")
  if (uj::N2(stack)) {stack <- base::c(stack, "..command.line..")}
  uj:::.pkg_fun(stack, 100, uj::f0(scl, "scl", "vls"))
}

#' @rdname callers
#' @export
caller1 <- caller

#' @rdname callers
#' @export
caller2 <- function(scl = TRUE) {
  if (uj::notT(scl)) {scl <- F}
  stack <- base::sys.calls()
  stack <- base::c(base::rev(uj::asCHR(stack)), "..command.line..")
  if (uj::N2(stack)) {stack <- base::c(stack, "..command.line..")}
  if (uj::N(stack) == 3) {stack <- base::c(stack, "..command.line..")}
  uj:::.pkg_fun(stack[4], 100, uj::f0(scl, "scl", "vls"))
}

#' @rdname callers
#' @export
callerN <- function(n, err = TRUE, scl = TRUE) {
  if (uj::notT(err)) {err <- T}
  if (uj::notT(scl)) {scl <- T}
  uj::err_if_not(uj::cmp_psw_scl(n), "[n] must be a complete positive whole number scalar (?cmp_psw_scl).", PKG = "uj")
  stack <- base::sys.calls()
  stack <- base::c(base::rev(uj::asCHR(stack)), "..command.line..")
  if (uj::N2(stack)) {stack <- base::c(stack, "..command.line..")}
  stack <- stack[3:uj::N(stack)]
  N <- uj::N(stack)
  uj::err_if(err & n > N, "[n] is greater than number of calling functions.", PKG = "uj")
  n <- base::min(n, N)
  uj:::.pkg_fun(stack[n], 100,  uj::f0(scl, "scl", "vls"))
}

#' @rdname callers
#' @export
callersN <- function(..., err = TRUE, vec = TRUE) {
  if (notT(err)) {err <- T}
  if (notT(vec)) {vec <- T}
  Ns <- uj::av(...)
  uj::err_if_not(uj::cmp_psw_vec(Ns), "All [...] args must be complete positive whole-number vecs (?cmp_psw_vec).", PKG = "uj")
  stack <- base::sys.calls()
  stack <- base::c(base::rev(uj::asCHR(stack)), "..command.line..")
  if (uj::N2(stack)) {stack <- base::c(stack, "..command.line..")}
  stack <- uj::Nth_plus(stack, 3)
  N <- uj::N(stack)
  uj::err_if(err & base::any(Ns > N), "A value in [...] is greater than number of calling functions.", PKG = "uj")
  uj:::.pkg_fun(stack[Ns[Ns <= N]], 35, uj::f0(vec, "vec", "vls"))
}

#' @rdname callers
#' @export
ncallers <- function() {
  stack <- base::sys.calls()
  stack <- base::c(base::rev(uj::asCHR(stack)), "..command.line..")
  if (uj::N2(stack)) {stack <- base::c(stack, "..command.line..")}
  stack <- uj::Nth_plus(stack, 3)
  uj::N(uj:::.pkg_fun(stack, 35, T))
}

#' @rdname callers
#' @export
lineage <- function(vec = TRUE) {
  if (uj::notT(vec)) {vec <- F}
  stack <- base::sys.calls()
  stack <- base::c(base::rev(uj::asCHR(stack)), "..command.line..")
  stack <- uj::Nth_plus(stack, 2)
  uj::.pkg_fun(stack, 35, uj::vec("vec", "vls"))
}

#' @rdname callers
#' @export
self <- function(scl = TRUE) {
  if (uj::notT(scl)) {scl <- F}
  stack <- base::sys.calls()
  stack <- base::c(base::rev(uj::asCHR(stack)), "..command.line..")[2]
  uj:::.pkg_fun(stack, 100, uj::f0(scl, "scl", "vls"))
}

#' @rdname callers
#' @export
ancestor <- function(scl = TRUE) {
  if (notT(scl)) {scl <- F}
  stack <- uj::callers()
  stack[base::max(1, uj::N(stack) - 1)]
  uj:::.pkg_fun(stack, 100, uj::f0(scl, "scl", "vls"))
}
