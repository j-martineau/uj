# internals ####

#' @encoding UTF-8
#' @family utils
#' @title Reduce the Call Stack to Just a Vector of Function Names
#' @param stack A character vector.
#' @return A character vector.
#' @examples
#' egStack2Funs <- function() {
#'   egStack2FunsF <- function() {
#'     stack <- uj::callers()
#'     uj::stack2funs(stack)
#'   }
#'   egStack2FunsE <- function() {egStack2FunsF()}
#'   egStack2FunsD <- function() {egStack2FunsE()}
#'   egStack2FunsC <- function() {egStack2FunsD()}
#'   egStack2FunsB <- function() {egStack2FunsC()}
#'   egStack2FunsA <- function() {egStack2FunsB()}
#'   egStack2FunsA()
#' }
#' egStack2Funs()
#' @export
stack2funs <- function(stack) {
  fun <- function(x) {
    x <- uj::av(base::strsplit(x, "(", fixed = T))[1]
    x <- uj::av(base::strsplit(x, " ", fixed = T))[1]
    x <- uj::av(base::strsplit(x, ":::", fixed = T))
    x <- uj::av(base::strsplit(x, "::" , fixed = T))
    x <- x[[base::length(x)]]
    if (x == "") {"..??.."} else {x}
  }
  y <- base::sapply(stack, fun)
  y[y == "..command.line.."] <- "[command.line]"
  uj::av(y)
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
#' @return **An integer scalar**                      \cr\cr `ncallers`
#' \cr\cr  **A character vector** (when `vec = TRUE`) \cr\cr `callers, lineage, callersN`
#' \cr\cr  **A character scalar** (when `scl = TRUE`) \cr\cr `ancestor, caller, caller1, caller2, callerN, self`
#' @examples
#' egCallers <- function() {
#'   egCallersD <- function() {
#'     self       <- self()
#'     caller     <- caller()
#'     callers    <- callers()
#'     caller2    <- caller2(err = F)
#'     caller3    <- callerN(3, err = F)
#'     callers23  <- callersN(2, 3, err = F)
#'     ncallers   <- ncallers()
#'     ancestor   <- ancestor()
#'     lineage    <- lineage()
#'     list(
#'       self       = self     ,
#'       caller     = caller   ,
#'       callers    = callers  ,
#'       caller2    = caller2  ,
#'       caller3    = caller3  ,
#'       callers23  = callers23,
#'       ncallers   = ncallers ,
#'       ancestor   = ancestor ,
#'       lineage    = lineage
#'     )
#'   }
#'   egCallersC <- function() {
#'     self       <- self()
#'     caller     <- caller()
#'     callers    <- callers()
#'     caller2    <- caller2(err = F)
#'     caller3    <- callerN(3, err = F)
#'     callers23  <- callersN(2, 3, err = F)
#'     ncallers   <- ncallers()
#'     ancestor   <- ancestor()
#'     lineage    <- lineage()
#'     egCallersD <- egCallersD()
#'     list(
#'       self       = self     ,
#'       caller     = caller   ,
#'       callers    = callers  ,
#'       caller2    = caller2  ,
#'       caller3    = caller3  ,
#'       callers23  = callers23,
#'       ncallers   = ncallers ,
#'       ancestor   = ancestor ,
#'       lineage    = lineage  ,
#'       egCallersD = egCallersD
#'     )
#'   }
#'   egCallersB <- function() {
#'     self       <- self()
#'     caller     <- caller()
#'     callers    <- callers()
#'     caller2    <- caller2(err = F)
#'     caller3    <- callerN(3, err = F)
#'     callers23  <- callersN(2, 3, err = F)
#'     ncallers   <- ncallers()
#'     ancestor   <- ancestor()
#'     lineage    <- lineage()
#'     egCallersC <- egCallersC()
#'     list(
#'       self       = self     ,
#'       caller     = caller   ,
#'       callers    = callers  ,
#'       caller2    = caller2  ,
#'       caller3    = caller3  ,
#'       callers23  = callers23,
#'       ncallers   = ncallers ,
#'       ancestor   = ancestor ,
#'       lineage    = lineage  ,
#'       egCallersC = egCallersC
#'     )
#'   }
#'   egCallersA <- function() {
#'     self       <- self()
#'     caller     <- caller()
#'     callers    <- callers()
#'     caller2    <- caller2(err = F)
#'     caller3    <- callerN(3, err = F)
#'     callers23  <- callersN(2, 3, err = F)
#'     ncallers   <- ncallers()
#'     ancestor   <- ancestor()
#'     lineage    <- lineage()
#'     egCallersB <- egCallersB()
#'     list(
#'       self       = self     ,
#'       caller     = caller   ,
#'       callers    = callers  ,
#'       caller2    = caller2  ,
#'       caller3    = caller3  ,
#'       callers23  = callers23,
#'       ncallers   = ncallers ,
#'       ancestor   = ancestor ,
#'       lineage    = lineage  ,
#'       egCallersB = egCallersB
#'     )
#'   }
#' }
#' egCallers()
#' @export
callers <- function() {
  stack <- base::c(base::rev(base::as.character(base::sys.calls())), "..command.line..()")
  if (base::length(stack) == 2) {stack <- base::c(stack, "..command.line..()")}
  stack <- uj::av(stack)
  uj::stack2funs(stack[3:base::length(stack)])
}

#' @rdname callers
#' @export
caller <- function() {
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..command.line..()")
  if (base::length(stack) == 2) {stack <- base::c(stack, "..command.line..()")}
  stack <- uj::av(stack)
  uj::stack2funs(stack[3])
}

#' @rdname callers
#' @export
caller1 <- caller

#' @rdname callers
#' @export
caller2 <- function(err = TRUE) {
  if (!uj::fs_tf(err)) {err <- T}
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..command.line..()")
  if (base::length(stack) == 2) {stack <- base::c(stack, "..command.line..()")}
  stack <- uj::av(stack)
  if (base::length(stack) < 4) {
    if (err) {uj::stopperr({uj::stopperr("There is no grandparent calling function.")})}
    else {NA_character_}
  } else {uj::stack2funs(stack[4])}
}

#' @rdname callers
#' @export
callerN <- function(n, err = TRUE) {
  if (!uj::fs_tf(err)) {err <- T}
  if (!uj::.cmp_psw_scl(n)) {uj::stopperr("[n] must be a complete positive whole number scalar (?cmp_psw_scl).")}
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "::..command.line..()")
  if (base::length(stack) == 2) {stack <- base::c(stack, "::..command.line..()")}
  stack <- stack[3:base::length(stack)]
  nstack <- base::length(stack)
  if (n <= nstack) {
    n <- base::min(n, nstack)
    stack <- uj::av(stack)
    uj::stack2funs(stack[n])
  } else if (err) {uj::stopperr("[n] is greater than number of calling functions.")}
  else {NA_character_}
}

#' @rdname callers
#' @export
callersN <- function(..., err = TRUE) {
  if (!uj::fs_tf(err)) {err <- T}
  Ns <- uj::av(...)
  if (!uj::.cmp_psw_vec(Ns)) {uj::stopperr("All [...] args must be complete positive whole-number vecs (?cmp_psw_vec).")}
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..command.line..()")
  if (base::length(stack) == 2) {stack <- base::c(stack, "..command.line..()")}
  stack <- stack[3:base::length(stack)]
  nstack <- base::length(stack)
  stack <- uj::stack2funs(stack)
  if (!base::any(Ns > nstack)) {
    stack <- uj::av(stack)
    stack[Ns[Ns <= nstack]]
  } else if (!err) {
    nOK <- Ns[Ns <= stack]
    nNA <- Ns[Ns >  stack]
    base::c(stack[nOK], base::rep(NA_character_, base::length(nNA)))
  }
  else {uj::stopperr("A value in [...] is greater than number of calling functions.")}
}

#' @rdname callers
#' @export
ncallers <- function() {
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..command.line..()")
  if (base::length(stack) == 2) {stack <- base::c(stack, "..command.line..()")}
  stack <- stack[3:base::length(stack)]
  stack <- uj::av(stack)
  base::length(uj::stack2funs(stack))
}

#' @rdname callers
#' @export
lineage <- function() {
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..command.line..()")
  stack <- stack[2:base::length(stack)]
  stack <- uj::av(stack)
  uj::stack2funs(stack)
}

#' @rdname callers
#' @export
self <- function() {
  stack <- base::sys.calls()
  stack <- base::c(base::rev(base::as.character(stack)), "..command.line..()")[2]
  stack <- uj::av(stack)
  uj::stack2funs(stack)
}

#' @rdname callers
#' @export
ancestor <- function() {
  stack <- uj::callers()
  stack <- stack[base::max(1, base::length(stack) - 1)]
  stack <- uj::av(stack)
  uj::stack2funs(stack)
}
