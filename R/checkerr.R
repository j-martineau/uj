# internals ####

.has_dots <- function(...) {base::...length() > 0}

.all_named <- function(...) {
  ndots <- base::...length()
  labs <- base::...names()
  if (ndots == 0) {F}
  else if (base::length(labs) != ndots) {F}
  else {!base::any(base::is.na(labs))}
}

.ox_vals <- function(x, join) {
  n <- uj::N(x)
  if (n == 1) {x}
  else if (n == 2) {uj::p0(x[1], " ", join, " ", x[2])}
  else {uj::p0(uj::g(", ", x[1:(n - 1)]), ", ", join, " ", x[n])}
}

.fun_pkg_stack <- function(f, p, s, c1, cs) {
  pkg_fun <- function(stack, lim, vec = T, pkg.only = F, fun.only = F) {
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
  if (!uj::cmp_chr_scl(uj::failsafe(f))) {f <- ""}
  if (!uj::cmp_chr_scl(uj::failsafe(p))) {p <- ""}
  if (!uj::cmp_chr_vec(uj::failsafe(s))) {s <- ""}
  if (base::length(f) != 1) {f <- "{ command line }"}
  if (base::length(p) != 1) {p <- "{ unknown }"}
  if (base::length(s) == 1) {if (s == "") {s <- uj::callers()}}
  if (f == "") {f <- pkg_fun(caller, 100, F)$fun[1]}
  if (p == "") {p <- pkg_fun(caller, 100, F)$pkg[1]}
  stack <- pkg_fun(s, 35, T)
  if (base::length(s) > 2) {s <- uj::Nth_plus(s, 2)} else {s <- "command line"}
  s <- base::paste0("callstack = { ", base::paste0(s, collapse = " > "), " }")
  base::list(fun = f, pkg = p, stack = s)
}

# exported ####

#' @name checker
#' @encoding UTF-8
#' @family properties
#' @family errs
#' @title Error checking, banking, and processing
#' @description Bank error messages in the immediate environment of a function to allow for exhaustive error checking before throwing an exception. Results in a possibly multiple-error, accumulated message to be processed upon completion of error checking.
#' @details **Primary stopping functions**
#' \tabular{ll}{  `stopperr`        \tab Stops execution by:                                                                                                                                                                                                             \cr
#'                                  \tab \enumerate{\item Posting an \code{\link{alert}} header the console with the following components: an error `'ERROR'` title, a subheader identifying the function where the error originated (from `FUN`), and another subheader identifying the package that function belongs to (from `PKG`).
#'                                                  \item Posting one or more error message to the console following the header, each preceded by a bullet, where each element of each `...` arg is a separate error message.
#'                                                  \item Creating a \code{\link[base]{simpleError}} object with an error message consisting of an \link[=lineage]{abbreviated function call lineage} given as a vector in `STACK`.
#'                                                  \item Attaching the function identity, package identity, and associated message(s) to the `simpleError` object as attributes.
#'                                                  \item Archiving the `simpleError` object in the global variable `.last_UJ_error.` (which can be retrieved by calling `getter()` and can be purged by calling `purger()`). Allows for error tracing in the circumstance that R purges the last error in final error processing.
#'                                                  \item Calling `stop` with the `simpleError` object as the argument.                                                                                                                                   } \cr   \tab   \cr
#'                `checkerr`        \tab Calls `stopper` with any error messages banked by the functions described in sections *error banking* *utilities functions* and *condition-based error-checking functions*. If none are banked, does nothing.      \cr   \tab   \cr
#'                `purgerr`         \tab Purges the most recent \code{\link[base]{simpleError}} object generated by this family of functions.                                                                                                               \cr   \tab   \cr
#'                `getterr`         \tab Gets the most recent `simpleError` object generated by this family of functions.                                                                                                                                                  }
#' \cr Both `stopperr` and `checkerr` can identify error-generating functions further up the call stack than the function in which they are called:
#' \itemize{\item `stopper` uses args `FUN`, `PKG`, and `STACK` to identify the error-generating function.
#'          \item `checker` uses args `GENS` and `PKG` to identify the error-generating function.}
#' \cr\cr **Secondary stopping functions**
#' \cr\cr These functions are designed to be called directly from the function where an error is generated. It gathers the name of the function generating the error rather than requiring the user to provide the function name. These functions operate as follows:
#' \tabular{ll}{  `errs`            \tab Calls `stopperr` treating each element of each `...` arg as a separate error message, allowing for compiling multiple error message before processing.                                                              \cr   \tab   \cr
#'                `err`             \tab Calls `stopperr` with a single error message constructed by \link[=collapse_dots]{collapsing} all elements of all `...` args into a character scalar error message before processing.                                              }
#' \cr\cr **Multiple-error conditional stopping functions**
#' \cr\cr These functions conditionally compile multiple errors, and if any are compiled, they notify the user and stop execution.
#' \tabular{ll}{  `errs_if_nots`    \tab Conditionally compiles errors, treating each odd-numbered `...` arg as a test and each even-numbered `...` arg as the corresponding error message if the test is `FALSE`, and calls `stopperr` if any are compiled. \cr   \tab   \cr
#'                `errs_if_pop`     \tab Calls `stopperr` if there are any `...` args, treating each `...` arg as a separate error message.                                                                                                                  \cr   \tab   \cr
#'                `errs_ifs`        \tab Conditionally compiles errors, treating each odd-numbered `...` arg as a test and each even-numbered `...` arg as the corresponding error message if the test is `TRUE`, and calls `stopperr` if any are compiled.                 }
#' \cr\cr **Single-error conditional stopping functions**
#' \cr\cr These functions conditionally construct a single error, and if one is constructed, they notify the user and stop execution.
#' \tabular{ll}{  `err_if_pop`      \tab If there are any `...` args, collapsing them into a character scalar error message, and calls `stopperr`.                                                                                                           \cr   \tab   \cr
#'                `err_if_not`      \tab If `TEST = FALSE`, collapses `...` args to a character scalar error message and calls `stopperr`.                                                                                                                   \cr   \tab   \cr
#'                `err_if`          \tab If `TEST = TRUE`, collapses `...` args to a character scalar error message and calls `stopperr`.                                                                                                                                   }
#' \cr\cr **Utility functions**
#' \cr\cr These functions purge and retrieve the most recent error generated by this family of functions.
#' \cr\cr **Primary error banking functions**
#' \cr\cr Error banking utility functions: These functions are utilities for banking user-defined error messages within a function to allow for checking for multiple errors in separate statements and banking those error messages as they are checked, waiting to process banked error messages until an error checking block is completed. These functions also allow for generating and checking for error messages further up the call stack than the function in which the error banking/processing occurs by specifying the number of generations back in the call stack where error banking/processing occurs in `GENS`:
#' \tabular{ll}{  `banked_errs`    \tab Retrieves the bank of error message stored in the environment of the function `GENS` generations back in the call stack.                                                                                            \cr   \tab   \cr
#'                `bankerrs`       \tab Banks each element of \link[=cmp_chr_vec]{complete character vec} as an individual error message.                                                                                                                   \cr   \tab   \cr
#'                `bankerr`        \tab Banks an arbitrary error message (built by \link[=collapse_dots]{collapsing} `...` args) in the environment of the function `GENS` generations back in the call stack.                                                            }
#' \cr\cr **Error checking / conditional error banking functions**
#' \cr\cr **`checkerr`** checks for any banked error messages. If there are any, processes them and stops execution. Otherwise, does nothing.
#' \cr\cr The remaining **`check_{props}`** functions in the following table check objects for specific properties and automatically generate errors only if those properties are not met:
#' \tabular{ll}{  `check_nas_or`    \tab A named `...` arg is neither `NULL` nor satisfies any property function named in `FUNS`.                                                                                                                           \cr   \tab   \cr
#'                `check_nll_or`    \tab A named `...` arg is neither scalar `NA` nor satisfies any property function named in `FUNS`.                                                                                                                      \cr   \tab   \cr
#'                `check_chars`     \tab A named `...` arg contains characters not supplied in `CHARS`.                                                                                                                                                     \cr   \tab   \cr
#'                `check_when`      \tab The first named `...` arg *is* the `n`-th value in `WHENS`, but the second *is not* the `n`-th value in `VALS`.                                                                                                    \cr   \tab   \cr
#'                `check_dots`      \tab A `...` arg\eqn{^{(1)}} fails to satisfy the \link[=is_prop_spec]{property spec} in `SPEC`.                                                                                                                        \cr   \tab   \cr
#'                `check_spec`      \tab A named `...` arg fails to satisfy the \link[=is_prop_spec]{property spec} in `SPEC`\eqn{^{(2)}}.                                                                                                                  \cr   \tab   \cr
#'                `check_funs`      \tab A named `...` arg fails to satisfy *any* of the \link[=prop_funs]{property function(s)} named in `FUNS`.                                                                                                           \cr   \tab   \cr
#'                `check_vals`      \tab A named `...` arg contains values not supplied in `VALS`.                                                                                                                                                          \cr   \tab   \cr
#'                `check_fail`      \tab A named `...` arg produces an error when submitted to \code{\link[base]{identity}}.                                                                                                                                \cr   \tab   \cr
#'                `check_cls`       \tab A named `...` arg is not of any class named in `CLS`.                                                                                                                                                              \cr   \tab   \cr
#'                `check_lgl`       \tab A named `...` arg is neither `TRUE`, `FALSE`, `NA` (if `NAS = TRUE`), nor contained in `EXTRAS`.                                                                                                                   \cr   \tab   \cr
#'                `check_pop`       \tab A named `...` arg is either `NULL` or otherwise of length `0`.                                                                                                                                                     \cr   \tab   \cr
#'                `check_tf`        \tab A named `...` arg is neither scalar `TRUE` nor scalar `FALSE`.                                                                                                                                                     \cr   \tab   \cr
#'                `check_t`         \tab A named `...` arg is `FALSE`\eqn{^{(3)}}.                                                                                                                                                                                         }
#'  \tabular{l}{  \eqn{^{(1)}} Named if `NAMED = TRUE`.                                                                                                             \cr
#'                \eqn{^{(2)}} May be scalar `NA` if `NAS = TRUE`.                                                                                                  \cr
#'                \eqn{^{(3)}} Collapses *unnamed* `...` args to an error message template, replacing the escape sequence `'{@@}'` with the *named* `...` arg's name. }
#' @section The `...` arguments: Arguments supplied in `...` differ across functions in terms of whether they are named, how many named and/or unnamed `...` args there are, and their \link[=ppp]{property requirements} as follows:
#' \tabular{llll}{                  \tab    **Number of** `...` **args**          \tab                                            \tab           \cr
#'                  **Function**    \tab    *NAMED*                               \tab   *UNNAMED*                                \tab   *TOTAL* \cr
#'                  `check_chars`   \tab    `1+` (\code{\link{atm_str}})          \tab   `1+` (\code{\link{atm_chr}}\eqn{^{(1)}}) \tab   `2+`    \cr
#'                  `check_when`    \tab    `2 ` (\code{\link{atm_scl}})          \tab   `2 ` (\code{\link{atm_scl}})             \tab   `4`     \cr
#'                  `check_vals`    \tab    `1+` (\code{\link{ATM}}\eqn{^{(1)}})  \tab   `1+` (\code{\link{ATM}}\eqn{^{(1)}})     \tab   `2+`    \cr
#'                  `check_dots`    \tab    `1+` (any object\eqn{^{(2)}})         \tab   `0 ` (any object\eqn{^{(2)}})            \tab   `1+`    \cr
#'                  `check_fail`    \tab    `1+` (any object)                     \tab   `0`                                      \tab   `1+`    \cr
#'                  `check_funs`    \tab    `1+` (any object)                     \tab   `0`                                      \tab   `1+`    \cr
#'                  `check_spec`    \tab    `1+` (any object)                     \tab   `0`                                      \tab   `1+`    \cr
#'                  `check_cls`     \tab    `1+` (any object)                     \tab   `0`                                      \tab   `1+`    \cr
#'                  `check_lgl`     \tab    `1+` (\code{\link{atm_scl}})          \tab   `0`                                      \tab   `1+`    \cr
#'                  `check_pop`     \tab    `1+` (any object)                     \tab   `0`                                      \tab   `1+`    \cr
#'                  `check_tf`      \tab    `1+` (any object)                     \tab   `0`                                      \tab   `1+`    \cr
#'                  `check_t`       \tab    `1+` (\code{\link{isTF1}})            \tab   `0`                                      \tab   `1+`    \cr
#'                  `stopperr`      \tab    `0+` (\code{\link{cmp_chr_vec}})      \tab   `0+` (\code{\link{cmp_chr_vec}})         \tab   `1+`    \cr
#'                  `errs`          \tab    `0+` (\code{\link{cmp_chr_vec}})      \tab   `0+` (\code{\link{cmp_chr_vec}})         \tab   `1+`    \cr
#'                  `err`           \tab    `0+` (\code{\link{cmp_chr_vec}})      \tab   `0+` (\code{\link{cmp_chr_vec}})         \tab   `1+`      }
#'    \tabular{l}{  \eqn{^{(1)}} When `A = TRUE`.                    \cr
#'                  \eqn{^{(2)}} When `NAMED = TRUE` (otherwise `0+`). }
#' @param ... Differs by function in terms of whether they are named, how many there are, and their \link[=ppp]{property requirements} as described in section *the* `...` *arguments*.
#' @param A `TRUE` or `FALSE` indicating whether to \link[=av]{atomize} `...` args.
#' @param D A non-`NA` character scalar delimiter for collapsing `...` into a an error message.
#' @param FUN A character scalar naming the function generating an error or errors.
#' @param NAS `TRUE` or `FALSE` indicating whether `NA` values qualify as `'logical'`.
#' @param PKG A character scalar naming the package `FUN` is a part of. The package is identified as `'unknown'` when `PKG = ""`.
#' @param ERRS A character vector of individual error messages.
#' @param GENS A \link[=cmp_nnw_scl]{complete non-negative whole-number scalar} indicating the number of generations back in the call stack in which to bank and/or check for error messages.
#' @param FUNS A \link[=cmp_chr_vec]{complete character vec} containing `1` or more \link[=prop_funs]{property function} names.
#' @param SPEC A \link[=cmp_chr_scl]{complete character scalar} containing a \link[=is_prop_spec]{property spec}.
#' @param NAMED `TRUE` or `FALSE` indicating whether `...` args must uniquely named without using `""`.
#' @param STACK An optional character vector naming the lineage of the function generating the error. If `NULL`, retrieves the stack under the assumption that the immediate calling function is where the error is generated.
#' @param WHENS A \link[=pop_atm]{populated atomic object} of length `length(VALS)`.
#' @param VALS A \link[=cmp_atm]{complete atomic object} of length `length(WHENS)`.
#' @param EXTRAS `NULL` or a \link[=cmp_atm]{complete atomic object} containing additional valid values.
#' @return **A **\code{\link[base]{simpleError}} **object** \cr\cr `getterr`
#' \cr\cr  **A character vector**                           \cr\cr `banked_errs`
#' \cr\cr  All others are called for their side effects.
#' @examples
#' egStopper <- function() {stopperr('stopper demo', PKG = 'uj')}
#' egErrs <- function() {errs('errs demo1', 'errs demo2', PKG = 'uj')}
#' egErr <- function() {err('err', 'demo', PKG = 'uj')}
#' egErrors <- function(..., tf = NA, lgl = 42, not = FALSE, pop = NULL,
#'                      fail = simpleError('error'), funs = 2:4, spec = 42,
#'                      vals = 42, class = 42, nas.or = NULL, nll.or = NA,
#'                      chars = '5', whenA = "errorA", whenB = "errorB") {
#'   bankerr(...elt(1))
#'   bankerrs(...elt(2), ...elt(3))
#'   check_tf(tf = tf)
#'   check_lgl(lgl = lgl)
#'   check_t(not = not)
#'   check_pop(pop = pop)
#'   check_fail(fail = fail)
#'   check_funs(c('cmp_ch1_vec', 'cmp_ngw_vec'), funs = funs)
#'   check_spec('cmp_ch1_vec|nll|nas', spec = spec)
#'   check_vals(letters, vals = vals)
#'   check_cls('data.frame', class)
#'   check_nas_or(c('cmp_ch1_vec', 'cmp_ngw_vec'), nas.or = nas.or)
#'   check_nll_or(c('cmp_ch1_vec', 'cmpNGWvec'), nll.or = nll.or)
#'   check_chars(letters, chars = chars)
#'   check_when(whenA = whenA, whenB = whenB, c('errorA', ''), c('errorB', ''))
#'   checkerr(PKG = 'uj')
#' }
#' \dontrun{
#'   egstopperr()
#'   getterr()
#'   purgerr()
#'   getter()
#'   egErrs()
#'   egErrs()
#'   egErrors()
#' }
#' @export
stopperr <- function(..., FUN = "", PKG = "", STACK = "") {
  fun_pkg_stack <- function(f, p, s, c1, cs) {
    pkg_fun <- function(stack, lim, vec = T, pkg.only = F, fun.only = F) {
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
    if (!uj::cmp_chr_scl(uj::failsafe(f))) {f <- ""}
    if (!uj::cmp_chr_scl(uj::failsafe(p))) {p <- ""}
    if (!uj::cmp_chr_vec(uj::failsafe(s))) {s <- ""}
    if (base::length(f) != 1) {f <- "{ command line }"}
    if (base::length(p) != 1) {p <- "{ unknown }"}
    if (base::length(s) == 1) {if (s == "") {s <- uj::callers()}}
    if (f == "") {f <- pkg_fun(caller, 100, F)$fun[1]}
    if (p == "") {p <- pkg_fun(caller, 100, F)$pkg[1]}
    stack <- pkg_fun(s, 35, T)
    if (base::length(s) > 2) {s <- uj::Nth_plus(s, 2)} else {s <- "command line"}
    s <- base::paste0("callstack = { ", base::paste0(s, collapse = " > "), " }")
    base::list(fun = f, pkg = p, stack = s)
  }
  if (assertthat::is.error(base::identity(FUN))) {FUN <- ""} else if (!base::is.character(FUN) | base::length(FUN) != 1) {FUN <- ""} else if (base::is.na(FUN)) {FUN <- ""}
  if (assertthat::is.error(base::identity(PKG))) {PKG <- ""} else if (!base::is.character(PKG) | base::length(PKG) != 1) {PKG <- ""} else if (base::is.na(PKG)) {PKG <- ""}
  if (assertthat::is.error(base::identity(STACK))) {STACK <- ""} else if (!base::is.character(STACK) | base::length(STACK) != 1) {STACK <- ""} else if (base::is.na(STACK)) {STACK <- ""}
  errs <- uj::as.character(uj::av(uj::failsafe(base::list(...))))
  errs[base::trimws(errs, which = "both") == ""] <- "{ unknown error }"
  errs <- base::unique(errs)
  where <- fun_pkg_stack(FUN, PKG, STACK, uj::caller(), uj::callers())
  .rwb <- function(x) {crayon::bgRed(crayon::white(crayon::bold(x)))}
  .bwp <- function(x) {crayon::bgBlack(crayon::white(x))}
  .byi <- function(x) {crayon::bgBlack(crayon::yellow(crayon::italic(x)))}
  err.lab <- base::gsub(" ", " ", .rwb("\n ERROR GENERATED IN"), fixed = T)
  fun.lab <- base::gsub(" ", " ", .byi("\n function:"         ), fixed = T)
  pkg.lab <- base::gsub(" ", " ", .byi("\n package: "         ), fixed = T)
  fun.val <- base::gsub(" ", " ", .bwp(FUN                    ), fixed = T)
  pkg.val <- base::gsub(" ", " ", .bwp(PKG                    ), fixed = T)
  pad.nch <- base::max(base::c(uj::LEN(fun.val), uj::LEN(pkg.val)))
  pad.val <- base::paste0(base::rep.int(" ", pad.nch), collapse = "")
  fun.val <- .bwp(base::substr(uj::paste0(fun.val, pad.val), 1, pad.nch))
  pkg.val <- .bwp(base::substr(uj::paste0(pkg.val, pad.val), 1, pad.nch))
  pad.val <- .rwb(pad.val)
  message <- uj::paste0(uj::paste0("\n  \u2022 ", errs), collapse = "")
  err.obj <- base::simpleError(where$stack, call = NULL)
  base::attr(err.obj, "function") <- where$fun
  base::attr(err.obj, "package") <- where$pkg
  base::attr(err.obj, "message") <- message
  base::cat(err.lab, pad.val)
  base::cat(fun.lab, fun.val)
  base::cat(pkg.lab, pkg.val)
  base::cat(message)
  base::assign(".last_UJ_error.", err.obj, envir = .GlobalEnv)
  stop(err.obj, call. = F)
}

#' @rdname checker
#' @export
getterr <- function() {
  if (base::exists(".last_UJ_error.", envir = base::.GlobalEnv)) {base::get(".last_UJ_error.", envir = .GlobalEnv)}
  else {NULL}
}

#' @rdname checker
#' @export
purgerr <- function() {
  if (base::exists(".last_UJ_error.", envir = .GlobalEnv)) {base::rm(".last_UJ_error.", envir = .GlobalEnv)}
}

#' @rdname checker
#' @export
err_if <- function(TEST, ..., FUN = "", PKG = "", STACK = "", D = " ") {
  if (assertthat::is.error(base::identity(D))) {D <- " "} else if (!base::is.character(D) | base::length(D) != 1) {D <- " "} else if (base::is.na(D)) {D <- " "}
  if (assertthat::is.error(base::identity(FUN))) {FUN <- ""} else if (!base::is.character(FUN) | base::length(FUN) != 1) {FUN <- ""} else if (base::is.na(FUN)) {FUN <- ""}
  if (assertthat::is.error(base::identity(PKG))) {PKG <- ""} else if (!base::is.character(PKG) | base::length(PKG) != 1) {PKG <- ""} else if (base::is.na(PKG)) {PKG <- ""}
  if (assertthat::is.error(base::identity(STACK))) {STACK <- ""} else if (!base::is.character(STACK) | base::length(STACK) != 1) {STACK <- ""} else if (base::is.na(STACK)) {STACK <- ""}
  if (uj::isT1(TEST)) {
    ERR <- base::paste0(uj::av(uj::failsafe(base::list(...))), collapse = D)
    if (ERR == "") {err <- "{ unknown error }"}
    where <- uj:::.fun_pkg_stack(FUN, PKG, STACK, uj::caller(), uj::callers())
    uj::stopperr(ERR, FUN = where$fun, PKG = where$pkg, STACK = where$stack)
  }
}

#' @rdname checker
#' @export
err_if_not <- function(TEST, ..., FUN = "", PKG = "", STACK = "", D = " ") {
  if (assertthat::is.error(base::identity(D))) {D <- " "} else if (!base::is.character(D) | base::length(D) != 1) {D <- " "} else if (base::is.na(D)) {D <- " "}
  if (assertthat::is.error(base::identity(FUN))) {FUN <- ""} else if (!base::is.character(FUN) | base::length(FUN) != 1) {FUN <- ""} else if (base::is.na(FUN)) {FUN <- ""}
  if (assertthat::is.error(base::identity(PKG))) {PKG <- ""} else if (!base::is.character(PKG) | base::length(PKG) != 1) {PKG <- ""} else if (base::is.na(PKG)) {PKG <- ""}
  if (assertthat::is.error(base::identity(STACK))) {STACK <- ""} else if (!base::is.character(STACK) | base::length(STACK) != 1) {STACK <- ""} else if (base::is.na(STACK)) {STACK <- ""}
  if (uj::notT1(uj::failsafe(TEST))) {
    ERR <- base::paste0(uj::av(uj::failsafe(base::list(...))), collapse = D)
    if (ERR == "") {err <- "{ unknown error }"}
    where <- uj:::.fun_pkg_stack(FUN, PKG, STACK, uj::caller(), uj::callers())
    uj::stopperr(ERR, FUN = where$fun, PKG = where$pkg, STACK = where$stack)
  }
}

#' @rdname checker
#' @export
errs_ifs <- function(..., FUN = "", PKG = "", STACK = "", D = " ") {
  if (assertthat::is.error(base::identity(D))) {D <- " "} else if (!base::is.character(D) | base::length(D) != 1) {D <- " "} else if (base::is.na(D)) {D <- " "}
  if (assertthat::is.error(base::identity(FUN))) {FUN <- ""} else if (!base::is.character(FUN) | base::length(FUN) != 1) {FUN <- ""} else if (base::is.na(FUN)) {FUN <- ""}
  if (assertthat::is.error(base::identity(PKG))) {PKG <- ""} else if (!base::is.character(PKG) | base::length(PKG) != 1) {PKG <- ""} else if (base::is.na(PKG)) {PKG <- ""}
  if (assertthat::is.error(base::identity(STACK))) {STACK <- ""} else if (!base::is.character(STACK) | base::length(STACK) != 1) {STACK <- ""} else if (base::is.na(STACK)) {STACK <- ""}
  ndots <- base::...length()
  if (ndots / 2 == base::round(ndots / 2)) {
    if (!uj::cmp_chr_scl(uj::failsafe(D))) {D <- " "}
    ERRS <- NULL
    for (i in 1:(ndots - 1)) {
      TEST <- uj::failsafe(base::...elt(i))
      if (uj::isT1(TEST)) {
        ERR <- base::paste0(uj::av(uj::failsafe(uj::failsafe(base::...elt(i + 1)))), collapse = D)
        if (ERR == "") {ERR <- "{ unknown error }"}
        ERRS <- base::c(ERRS, ERR)
      }
    }
    if (!base::is.null(ERRS)) {
      ERRS <- base::unique(ERRS)
      where <- uj:::.fun_pkg_stack(FUN, PKG, STACK, uj::caller(), uj::callers())
      uj::stopperr(ERRS, FUN = where$fun, PKG = where$pkg, STACK = where$stack)
    }
  } else {uj::stopperr("There must be an even number of [...] args.", FUN = "errs_ifs", PKG = "uj", STACK = uj::callers())}
}

#' @rdname checker
#' @export
errs_if_nots <- function(..., FUN = "", PKG = "", STACK = "", D = " ") {
  if (assertthat::is.error(base::identity(D))) {D <- " "} else if (!base::is.character(D) | base::length(D) != 1) {D <- " "} else if (base::is.na(D)) {D <- " "}
  if (assertthat::is.error(base::identity(FUN))) {FUN <- ""} else if (!base::is.character(FUN) | base::length(FUN) != 1) {FUN <- ""} else if (base::is.na(FUN)) {FUN <- ""}
  if (assertthat::is.error(base::identity(PKG))) {PKG <- ""} else if (!base::is.character(PKG) | base::length(PKG) != 1) {PKG <- ""} else if (base::is.na(PKG)) {PKG <- ""}
  if (assertthat::is.error(base::identity(STACK))) {STACK <- ""} else if (!base::is.character(STACK) | base::length(STACK) != 1) {STACK <- ""} else if (base::is.na(STACK)) {STACK <- ""}
  ndots <- base::...length()
  if (ndots / 2 == base::round(ndots / 2)) {
    if (!uj::cmp_chr_scl(uj::failsafe(D))) {D <- " "}
    ERRS <- NULL
    for (i in 1:(ndots - 1)) {
      TEST <- uj::failsafe(base::...elt(i))
      if (uj::isT1(TEST)) {
        ERR <- base::paste0(uj::av(uj::failsafe(uj::failsafe(base::...elt(i + 1)))), collapse = D)
        if (ERR == "") {ERR <- "{ unknown error }"}
        ERRS <- base::c(ERRS, ERR)
      }
    }
    if (!base::is.null(ERRS)) {
      ERRS <- base::unique(ERRS)
      where <- uj:::.fun_pkg_stack(FUN, PKG, STACK, uj::caller(), uj::callers())
      uj::stopperr(ERRS, FUN = where$fun, PKG = where$pkg, STACK = where$stack)
    }
  } else {uj::stopperr("There must be an even number of [...] args.", FUN = "errs_ifs", PKG = "uj", STACK = uj::callers())}
}

#' @rdname checker
#' @export
err_if_pop <- function(..., FUN = "", PKG = "", STACK = "", D = " ") {
  if (assertthat::is.error(base::identity(D))) {D <- " "} else if (!base::is.character(D) | base::length(D) != 1) {D <- " "} else if (base::is.na(D)) {D <- " "}
  if (assertthat::is.error(base::identity(FUN))) {FUN <- ""} else if (!base::is.character(FUN) | base::length(FUN) != 1) {FUN <- ""} else if (base::is.na(FUN)) {FUN <- ""}
  if (assertthat::is.error(base::identity(PKG))) {PKG <- ""} else if (!base::is.character(PKG) | base::length(PKG) != 1) {PKG <- ""} else if (base::is.na(PKG)) {PKG <- ""}
  if (assertthat::is.error(base::identity(STACK))) {STACK <- ""} else if (!base::is.character(STACK) | base::length(STACK) != 1) {STACK <- ""} else if (base::is.na(STACK)) {STACK <- ""}
  if (base::...length() > 0) {
    ERR <- NULL
    for (i in 1:base::...length()) {
      piece <- uj::failsafe(base::...elt(i))
      if (!base::is.null(piece)) {
        if (!assertthat::is.error(piece)) {ERR <- base::c(ERR, base::as.character(piece))}
        else {ERR <- base::c(ERR, "")}
      }
    }
    if (base::length(ERR) > 0) {
      ERR <- base::paste0(ERR, collapse = D)
      if (ERR == "") {ERR <- "{ unknown error }"}
      where <- uj:::.fun_pkg_stack(FUN, PKG, STACK, uj::caller(), uj::callers())
      uj::stopperr(ERR, FUN = where$fun, PKG = where$pkc, STACK = where$stack)
    }
  }
}

#' @rdname checker
#' @export
errs_if_pop <- function(..., FUN = "", PKG = "", STACK = "") {
  if (assertthat::is.error(base::identity(FUN))) {FUN <- ""} else if (!base::is.character(FUN) | base::length(FUN) != 1) {FUN <- ""} else if (base::is.na(FUN)) {FUN <- ""}
  if (assertthat::is.error(base::identity(PKG))) {PKG <- ""} else if (!base::is.character(PKG) | base::length(PKG) != 1) {PKG <- ""} else if (base::is.na(PKG)) {PKG <- ""}
  if (assertthat::is.error(base::identity(STACK))) {STACK <- ""} else if (!base::is.character(STACK) | base::length(STACK) != 1) {STACK <- ""} else if (base::is.na(STACK)) {STACK <- ""}
  ERRS <- uj::failsafe(base::list(...))
  ERRS <- base::as.character(uj::av(ERRS))
  ERRS[base::is.na(ERRS)] <- "{ unknown error }"
  if (base::length(ERRS) == 1) {if (ERRS == "") {ERRS <- NULL}}
  if (base::length(ERRS) > 0) {
    ERRS[ERRS == ""] <- "{ unknown error }"
    ERRS <- base::unique(ERRS)
    where <- uj:::.fun_pkg_stack(FUN, PKG, STACK, uj::caller(), uj::callers())
    uj::stopperr(ERRS, FUN = where$fun, PKG = where$pkg, STACK = where$stack)
  }
}


#' @rdname checker
#' @export
errs <- function(ERRS, PKG = "", STACK = "") {
  if (assertthat::is.error(base::identity(PKG))) {PKG <- ""} else if (!base::is.character(PKG) | base::length(PKG) != 1) {PKG <- ""} else if (base::is.na(PKG)) {PKG <- ""}
  if (assertthat::is.error(base::identity(STACK))) {STACK <- ""} else if (!base::is.character(STACK) | base::length(STACK) != 1) {STACK <- ""} else if (base::is.na(STACK)) {STACK <- ""}
  ERRS <- uj::failsafe(ERRS)
  ERRS <- base::trimws(base::as.character(uj::av(ERRS)), which = "both")
  ERRS[base::is.na(ERRS)] <- "{ unknown error }"
  ERRS[ERRS == ""] <- " { unknown error }"
  ERRS <- base::unique(ERRS)
  uj::stopperr(ERRS, FUN = uj::caller(), PKG = PKG, STACK = uj::callers())
}

#' @rdname checker
#' @export
err <- function(..., PKG = "", STACK = "", D = " ") {
  if (assertthat::is.error(base::identity(D))) {D <- " "} else if (!base::is.character(D) | base::length(D) != 1) {D <- " "} else if (base::is.na(D)) {D <- " "}
  if (assertthat::is.error(base::identity(PKG))) {PKG <- ""} else if (!base::is.character(PKG) | base::length(PKG) != 1) {PKG <- ""} else if (base::is.na(PKG)) {PKG <- ""}
  if (assertthat::is.error(base::identity(STACK))) {STACK <- ""} else if (!base::is.character(STACK) | base::length(STACK) != 1) {STACK <- ""} else if (base::is.na(STACK)) {STACK <- ""}
  ERR <- base::paste0(uj::av(uj::failsafe(base::list(...))), collapse = D)
  ERR <- base::trimws(ERR, which = "both")
  if (base::is.na(ERR)) {ERR <- "{ unknown error }"}
  if (ERR != "") {ERR <- "{ unknown error }"}
  uj::stopperr(ERR, FUN = uj::caller(), PKG = PKG, STACK = uj::callers())
}

#' @rdname checker
#' @export
banked_errs <- function(GENS = 0) {
  if (!uj::cmp_psw_scl(uj::failsafe(GENS))) {GENS <- 0}
  GENS <- GENS + 1
  caller <- base::parent.frame(GENS)
  if (base::exists(".uj_ERR_BANK_uj.", envir = caller, inherits = F)) {base::get(".uj_ERR_BANK_uj.", envir = caller, inherits = F)} else {NULL}
}

#' @rdname checker
#' @export
checkerr <- function(GENS = 0, PKG = "") {
  if (!uj::cmp_psw_scl(uj::failsafe(GENS))) {GENS <- 0}
  GENS <- GENS + 1
  if (assertthat::is.error(base::identity(PKG))) {PKG <- ""} else if (!base::is.character(PKG) | base::length(PKG) != 1) {PKG <- ""} else if (base::is.na(PKG)) {PKG <- ""}
  stack <- uj::callers()
  stack <- stack[GENS:uj::N(stack)]
  caller <- base::parent.frame(GENS)
  if (base::exists(".uj_ERR_BANK_uj.", envir = caller, inherits = F)) {
    FUN <- uj::callerN(GENS + 1)
    ERRS <- base::get(".uj_ERR_BANK_uj.", envir = caller, inherits = F)
    base::rm(list = ".uj_ERR_BANK_uj.", envir = caller, inherits = F);
    uj::stopperr(ERRS, FUN = FUN, PKG = PKG, STACK = stack)
  }
}

#' @rdname checker
#' @export
bankerr <- function(..., GENS = 0, D = "") {
  if (assertthat::is.error(base::identity(D))) {D <- " "} else if (!base::is.character(D) | base::length(D) != 1) {D <- " "} else if (base::is.na(D)) {D <- " "}
  if (!uj::cmp_psw_scl(uj::failsafe(GENS))) {GENS <- 0}
  GENS <- GENS + 1
  ERR <- base::paste0(uj::av(uj::failsafe(base::list(...))), collapse = D)
  ERR <- base::trimws(ERR, which = "both")
  if (base::is.na(ERR)) {ERR <- "{ unknown error }"}
  if (ERR == "") {ERR <- "{ unknown error }"}
  ERRS <- base::c(uj::banked_errs(GENS), ERR)
  caller <- base::parent.frame(GENS)
  base::assign(".uj_ERR_BANK_uj.", ERRS, envir = caller)
}

#' @rdname checker
#' @export
bankerrs <- function(..., GENS = 0) {
  if (!uj::cmp_psw_scl(uj::failsafe(GENS))) {GENS <- 0}
  GENS <- GENS + 1
  if (base::...length() > 0) {
    ERRS <- NULL
    for (i in 1:base::...length()) {
      ERR <- base::paste0(base::as.character(uj::av(uj::failsafe(base::...elt(i)))), collapse = "")
      ERR <- base::trimws(ERR, which = "both")
      if (base::is.na(ERR)) {ERR <- "{ unknown error }"}
      if (ERR == "") {ERR <- "{ unknown error }"}
      ERRS <- base::c(ERRS, ERR)
    }
  } else {ERRS <- "{ unknown error } "}
  ERRS <- base::unique(base::c(uj::banked_errs(GENS), ERRS))
  caller <- base::parent.frame(GENS)
  base::assign(".uj_ERR_BANK_uj.", ERRS, envir = caller)
}

#' @rdname checker
#' @export
check_t <- function(..., D = " ") {
  if (assertthat::is.error(base::identity(D))) {D <- " "} else if (!base::is.character(D) | base::length(D) != 1) {D <- " "} else if (base::is.na(D)) {D <- " "}
  x <- uj::named_dots(...)
  anon <- uj::anon_dots(...)
  mssg <- base::paste0(uj::av(anon), collapse = D)
  labs <- base::names(x)
  uj::errs_if_nots(base::length(x) > 0                                    , "There are no named [...] args."                                                                                  ,
                   base::length(labs) == base::length(base::unique(labs)) , "Named [...] args must be uniquely named."                                                                        ,
                   base::length(anon) > 0                                 , "There are no unnamed [...] args."                                                                                ,
                   base::any(base::grepl("{@}", mssg, fixed = TRUE))      , "At least 1 unnamed [...] arg must contain the escape sequence '{@}' for inserting the names of named [...] args.", PKG = "uj")
  for (i in 1:uj::N(x)) {if (!x[[1]]) {uj::bankerr(base::gsub("{@}", uj::p0("[", labs[i], "]"), mssg), GENS = 1)}}
}

#' @rdname checker
#' @export
check_tf <- function(...) {
  uj::errs_if_nots(base::...length() > 0 , "There are no [...] args"                                  ,
                   uj:::.all_named(...), "All [...] args must be uniquely named without using \"\".", PKG = "uj")
  dots <- base::list(...)
  ok <- base::sapply(dots, base::isTRUE) | base::sapply(dots, base::isFALSE)
  for (i in 1:base::length(ok)) {if (!ok[i]) {uj::bankerr("[", uj::DN()[i], "] must be scalar TRUE or scalar FALSE.", GENS = 1, D = "")}}
}

#' @rdname checker
#' @export
check_lgl <- function(..., NAS = FALSE, EXTRAS = NULL) {
  NAS <- uj::f0(uj::isTF1(NAS), NAS, F)
  EXTRAS <- uj::f0(uj::cmp_atm(EXTRAS), EXTRAS, NULL)
  uj::errs_if_nots(base::...length() > 0, "There are no [...] args"                                  ,
                   uj:::.all_named(...) , "All [...] args must be uniquely named without using \"\".", PKG = "uj")
  x <- base::list(...)
  uj::err_if_not(base::all(base::sapply(x, uj::atm_scl)), "all [...] args must be atomic scalars.", PKG = "uj")
  ok <- base::sapply(x, base::isFALSE)
  if (NAS) {ok <- ok & base::all(base::is.na(x))}
  if (uj::DEF(EXTRAS)) {ok <- ok & base::sapply(x, uj::IN, EXTRAS)}
  if (!base::all(ok)) {
    labs <- base::paste0("[", base::...names()[!ok], "]")
    if (base::length(labs) > 1) {mults <- base::c("s", "")} else {mults <- base::c("", "s")}
    vals <- base::c("TRUE", "FALSE")
    if (NAS) {vals <- base::c(vals, "NA")}
    if (uj::DEF(EXTRAS)) {
      if (base::is.character(EXTRAS)) {vals <- base::c(vals, base::paste0("'", EXTRAS, "'"))}
      else {vals <- base::c(vals, base::as.character(EXTRAS))}
    }
    err <- base::paste0("Argument", mults[1], uj:::.ox_vals(labs, "and"), "contain", mults[2], " one or more values not in {", uj:::.ox_vals(vals, "and"), "}.")
    uj::bankerr(err, GENS = 1)
  }
}

#' @rdname checker
#' @export
check_nll_or <- function(FUNS, ..., VALS = NULL) {
  ok.FUNS <- uj::cmp_chr_vec(FUNS)
  if (ok.FUNS) {
    FUNS <- uj::av(base::strsplit(FUNS, "|", TRUE))
    ok.FUNS <- base::all(base::sapply(FUNS, uj::is_prop_fun))
  }
  labs <- base::...names()
  nx <- base::...length()
  ok.x <- nx > 0
  if (!ok.x) {ok.labs <- T} else {ok.labs <- uj:::.all_named(...)}
  if (base::is.null(VALS)) {ok.VALS <- T}
  else {ok.VALS <- uj::cmp_atm(VALS)}
  uj::errs_if_nots(ok.x   , "[...] arguments must be supplied."                             ,
                   ok.labs, "[...] args must be uniquely named without using blank strings.",
                   ok.FUNS, "[FUNS] must contain 1+ function names found in prop_funs()."   ,
                   ok.VALS, "[VALS] must be NULL or complete and atomic (?cmp_atm)."        , PKG = "uj")
  errs <- uj::p0("[", labs, "] must be NULL or ", uj::spec_concise(FUNS))
  for (i in 1:nx) {if (!base::is.null(base::...elt(i))) {
    ok <- F
    for (fun in FUNS) {
      ok <- ok | base::eval(base::parse(text = base::paste0(fun, "(base::...elt(i))")))
      if (ok & !base::is.null(VALS)) {ok <- ok & base::all(base::...elt(i) %in% VALS)}
    }
    if (!ok) {uj::bankerr(errs[i], GENS = 1)}
  }}
}

#' @rdname checker
#' @export
check_nas_or <- function(FUNS, ..., VALS = NULL) {
  ok.FUNS <- uj::cmp_chr_vec(FUNS)
  if (ok.FUNS) {
    FUNS <- uj::av(base::strsplit(FUNS, "|", TRUE))
    ok.FUNS <- base::all(base::sapply(FUNS, uj::is_prop_fun))
  }
  labs <- base::...names()
  nx <- base::...length()
  ok.x <- nx > 0
  if (!ok.x) {ok.labs <- T} else {ok.labs <- uj:::.all_named(...)}
  if (base::is.null(VALS)) {ok.VALS <- T}
  else {ok.VALS <- uj::cmp_atm(VALS)}
  uj::errs_if_nots(ok.x   , "[...] arguments must be supplied."                             ,
                   ok.labs, "[...] args must be uniquely named without using blank strings.",
                   ok.FUNS, "[FUNS] must contain 1+ function names found in prop_funs()."   ,
                   ok.VALS, "[VALS] must be NULL or complete and atomic (?cmp_atm)."        , PKG = "uj")
  errs <- uj::p0("[", labs, "] must be scalar NA or ", uj::spec_concise(FUNS))
  for (i in 1:nx) {if (!uj::NAS(base::...elt(i))) {
    ok <- F
    for (fun in FUNS) {
      ok <- ok | base::eval(base::parse(text = base::paste0(fun, "(base::...elt(i))")))
      if (ok & !base::is.null(VALS)) {ok <- ok & base::all(base::...elt(i) %in% VALS)}
    }
    if (!ok) {uj::bankerr(errs[i], GENS = 1)}
  }}
}

#' @rdname checker
#' @export
check_cls <- function(CLS, ...) {
  x <- uj::named_dots(...)
  anon <- uj::anon_dots(...)
  labs <- base::names(x)
  ok.n <- base::length(x) + base::length(anon) > 0
  ok.lab <- base::length(anon) == 0
  ok.unq <- base::length(labs) == base::length(base::unique(labs))
  uj::errs_if_nots(ok.n                , "There are no [...] args."                              ,
                   ok.unq              , "[...] arg names must be unique."                       ,
                   uj::cmp_chr_vec(CLS), "[CLS] must be a complete character vec (?cmp_chr_vec).",
                   ok.lab              , "All [...] args must be named."                         , PKG = "uj")
  CLS <- uj::av(base::strsplit(CLS, "|", fixed = T))
  mssg <- base::paste0("'", CLS, "'")
  n <- nsdr::length(mssg)
  if (n == 1) {mssg <- base::paste0("class ", mssg)}
  else if (n == 2) {mssg <- base::paste0("class ", mssg[1], " or ", mssg[2])}
  else {mssg <- base::paste0("any class in { ", paste0(mssg, collapse = ", "), " }")}
  for (i in 1:base::length(x)) {if (!base::any(CLS %in% base::class(x[[i]]))) {uj::bankerr("[", labs[i], "] must be of ", mssg, ".", GENS = 1, D = "")}}
}

#' @rdname checker
#' @export
check_pop <- function(...) {
  named <- uj::named_dots(...)
  blank <- uj::anon_dots(...)
  labs <- base::names(named)
  ok.n <- base::...length() > 0
  ok.named <- base::length(blank) == 0
  ok.blank <- !base::any(labs == "")
  ok.unq <- base::length(labs) == base::length(base::unique(labs))
  uj::errs_if_nots(ok.n    , "There are no [...] args."                        ,
                   ok.named, "All [...] args must be named."                   ,
                   ok.blank, "[...] arg names may not be blank strings (\"\").",
                   ok.unq  , "[...] arg names must be unique."                 , PKG = "uj")
  for (i in 1:base::length(named)) {if (base::length(named[[i]]) == 0) {uj::bankerr("[", labs[i], "] is NULL or empty.", GENS = 1, D = "")}}
}

#' @rdname checker
#' @export
check_funs <- function(FUNS, ..., VALS = NULL) {
  uj::err_if_not(uj::cmp_chr_vec(FUNS), "[FUNS] must be a complete character vec (?cmp_chr_vec).", PKG = "uj")
  FUNS <- uj::av(base::strsplit(FUNS, "|", TRUE))
  labs <- base::...names()
  n.dots <- base::...length()
  ok.n <- n.dots > 0
  ok.named <- base::length(labs) == n.dots
  ok.blank <- !base::any(labs == "")
  ok.unq <- base::length(labs) == base::length(base::unique(labs))
  ok.funs <- base::all(base::sapply(FUNS, uj::is_prop_fun))
  ok.vals <- base::ifelse(base::is.null(VALS), T, uj::cmp_atm(VALS))
  uj::errs_if_nots(ok.n    , "[...] arguments must be supplied."                            ,
                   ok.named, "all [...] args must be named."                                ,
                   ok.blank, "[...] arg names may not be blank strings (\"\")"              ,
                   ok.unq  , "[...] arg names must be unique."                              ,
                   ok.funs , "[FUNS] contains a function name not found in uj::prop_funs().",
                   ok.vals , "[VALS] must be NULL or complete and atomic (?cmp_atm)."       , PKG = "uj")
  errs <- uj::p0("[", labs, "] must have the following properties: ", uj::spec_concise(FUNS))
  for (i in 1:nx) {
    ok <- FALSE
    for (fun in FUNS) {
      ok <- ok | base::eval(base::parse(text = uj::p0(fun, "(base::...elt(i))")))
      if (ok & !base::is.null(VALS)) {ok <- ok & base::all(base::...elt(i) %in% VALS)}
    }
    if (!ok) {uj::bankerr(errs[i], GENS = 1)}
  }
}

#' @rdname checker
#' @export
check_spec <- function(SPEC, ..., NAS = F) {
  n.dots <- base::...length()
  labs <- base::...names()
  ok.n <- n.dots > 0
  ok.nas <- uj::isTF1
  ok.named <- base::length(labs) == n.dots
  ok.blank <- !base::any(labs == "")
  ok.unq <- base::length(labs) == base::length(base::unique(labs))
  ok.spec <- uj::cmp_chr_scl(SPEC)
  if (ok.spec) {ok.spec <- ok.spec & uj::is_prop_spec(SPEC)}
  uj::errs_if_nots(ok.n    , "[...] is empty."                                              ,
                   ok.nas  , "[NAS] must be TRUE or FALSE."                                 ,
                   ok.named, "all [...] args mut be named."                                 ,
                   ok.blank, "[...] arg names may not be blank."                            ,
                   ok.unq  , "[...] arg names must be unique."                              ,
                   ok.spec , "[SPEC] is not a valid property specification (?is_prop_spec).", PKG = "uj")
  errs <- uj::p0("[", labs, "] must be ", uj::spec_concise(SPEC), ".")
  for (i in 1:n.dots) {
    val <- F
    if (NAS) {val <- uj::NAS(base::...elt(i))}
    if (!val) {val <- uj::PPP(base::...elt(i), SPEC)}
    if (!val) {uj::bankerr(errs[i], GENS = 1)}
  }
}

#' @rdname checker
#' @export
check_vals <- function(VALS, ..., A = TRUE, NAS = FALSE) {
  valid <- function(y) {
    if (base::is.atomic(y)) {base::all(base::sapply(uj::av(y), uj::IN, VALS))}
    else if (base::is.data.frame(y)) {base::all(base::apply(y, 2, valid))}
    else if (base::is.list(y)) {base::all(base::sapply(y, valid))}
    else {F}
  }
  n.dots <- base::...length()
  labs <- base::...names()
  ok.n <- n.dots > 0
  ok.a <- uj::isTF1(A)
  ok.nas <- uj::isTF1(NAS)
  ok.named <- base::length(labs) == n.dots
  ok.blank <- !base::any(labs == "")
  ok.unq <- base::length(labs) == base::length(base::unique(labs))
  ok.vals <- uj::cmp_atm(VALS)
  uj::errs_if_nots(ok.n    , "[...] is empty."                                                   ,
                   ok.a    , "[A] must be TRUE or FALSE."                                        ,
                   ok.nas  , "[NAS] must be TRUE or FALSE."                                      ,
                   ok.vals , "[VALS] must be complete and atomic (?cmp_atm)."                    ,
                   ok.named, "all [...] args must be named."                                     ,
                   ok.blank, "[...] arg names may not be blank strings (\"\")."                  ,
                   ok.unq  , "[...] arg names must be unique."                                   , PKG = "uj")
  x <- base::list(...)
  atm <- base::sapply(x, uj::pop_atm)
  uj::err_if_not(!A | base::all(atm), "When [A = TRUE], all [...] args must be populated and atomic (?pop_atm).", PKG = "uj")
  if (!A) {
    vls <- base::sapply(x, atm_vls)
    dtf <- base::sapply(x, atm_dtf)
    uj::err_if_not(base::all(atm | vls | dtf), "All [...] args must be populated and atomic (?pop_atm), atomic vlists (?atm_vls), or atomic data.frames (?atm_dtf).", PKG = "uj")
  }
  for (i in 1:nx) {if (!valid(x[[i]])) {uj::bankerr("[", labs[i], "] contains 1 or more values not in [VALS].", GENS = 1, D = "")}}
}

#' @rdname checker
#' @export
check_chars <- function(CHARS, ..., A = TRUE) {
  valid <- function(y) {
    if (base::is.character(y)) {
      y <- base::paste0(uj::av(y), collapse = "")
      y <- base::strsplit(y, "", fixed = T)
      y <- uj::av(y)
      base::all(y %in% CHARS)
    } else if (base::is.data.frame(y)) {base::all(base::apply(y, 2, valid))}
    else if (base::is.list(y)) {base::all(base::sapply(y, valid))}
    else {F}
  }
  n.dots <- base::...length()
  labs <- base::...names()
  ok.n <- n.dots > 0
  ok.a <- uj::isTF1(A)
  ok.named <- base::length(labs) == n.dots
  ok.blank <- !base::any(labs == "")
  ok.unq <- base::length(labs) == base::length(base::unique(labs))
  ok.chars <- uj::cmp_chr(CHARS)
  uj::errs_if_nots(ok.n    , "[...] is empty."                                           ,
                   ok.a    , "[A] must be TRUE or FALSE."                                ,
                   ok.chars, "[CHARS] must be complete and of mode character (?cmp_chr).",
                   ok.named, "all [...] args must be named."                             ,
                   ok.blank, "[...] arg names may not be blank strings (\"\")."          ,
                   ok.unq  , "[...] arg names must be unique."                           , PKG = "uj")
  x <- base::list(...)
  chr <- base::sapply(x, uj::pop_chr)
  uj::err_if_not(!A | base::all(chr), "When [A = TRUE], all [...] args must be populated and of mode character (?pop_chr).", PKG = "uj")
  if (!A) {
    vls <- base::sapply(x, chr_vls)
    dtf <- base::sapply(x, chr_dtf)
    uj::err_if_not(base::all(chr | vls | dtf), "All [...] args must be populated and of mode character (?pop_chr), character vlists (?chr_vls), or character data.frames (?chr_dtf).", PKG = "uj")
  }
  for (i in 1:nx) {if (!valid(x[[i]])) {uj::bankerr("[", labs[i], "] contains 1 or more characters not in [CHARS].", GENS = 1, D = "")}}
}

#' @rdname checker
#' @export
check_dots <- function(SPEC, ..., NAMED = FALSE) {
  n.dots <- base::...length()
  labs <- base::...names()
  ok.NAMED <- uj::isTF1(NAMED)
  ok.spec <- base::ifelse(uj::cmp_chr_scl(SPEC), uj::is_prop_spec(SPEC), F)
  ok.n <- n.dots > 0
  ok.named <- !uj::isT1(NAMED) | base::length(labs) == n.dots
  ok.blank <- !base::any(labs == "")
  ok.unq <- base::length(labs) == base::length(base::unique(labs))
  uj::errs_if_nots(ok.NAMED, "[NAMED] must be TRUE or FALSE."                   ,
                   ok.n    , "[...] is empty."                                  ,
                   ok.spec , "[SPEC] must be complete and character (?cmp_chr).",
                   ok.named, "all [...] args must be named with [NAMED = TRUE].",
                   ok.blank, "[...] arg names may not be blank strings (\"\")." ,
                   ok.unq  , "[...] arg names must be unique."                  , PKG = "uj")
  if (!base::all(base::sapply(base::list(...), uj::PPP, spec = SPEC))) {uj::uj("All [...] args must be ", uj::spec_concise(SPEC), ".", GENS = 1, D = "")}
}

#' @rdname checker
#' @export
check_when <- function(WHENS, VALS, ...) {
  labs <- base::...names()
  n.dots <- base::...length()
  ok.n <- n.dots == 2
  ok.named <- base::length(labs) == n.dots
  ok.blank <- !base::any(labs == "")
  ok.unq <- base::length(labs) == base::length(base::unique(labs))
  ok.whens <- uj::POP(WHENS) & uj::ATM(WHENS)
  ok.vals <- uj::POP(VALS) & uj::POP(VALS)
  ok.neq <- base::length(WHENS) == length(VALS)
  ok.scl <- uj::atm_scl(..1) & uj::atm_scl(..2)
  ok.whens2 <- base::ifelse(ok.whens & ok.vals, uj::compatible(WHENS, ..1), T)
  ok.vals2 <- base::ifelse(ok.whens & ok.vals, uj::compatible(VALS, ..2), T)
  uj::errs_if_nots(ok.n     , "There must be two [...] args"                              ,
                   ok.named , "all [...] args must be named."                             ,
                   ok.blank , "[...] arg names may not be blank."                         ,
                   ok.unq   , "[...] arg names must be unique."                           ,
                   ok.vals  , "[VALS] must be non-empty and atomic."                      ,
                   ok.whens , "[WHENS] must be non-empty and atomic."                     ,
                   ok.neq   , "[WHENS] and [VALS] must be of the same length."            ,
                   ok.scl   , "Both args in [...] must be atomic and scalar (?atm_scl)."  ,
                   ok.whens2, "[VALS] and [..2] are of incompatible (?compatible) modes." ,
                   ok.vals2 , "[WHENS] and [..1] are of incompatible (?compatible) modes.", PKG = "uj")
  labs1 <- base::paste0("[", labs[1], "]")
  labs2 <- base::paste0("[", labs[2], "]")
  when <- ..1
  val <- ..2
  i <- base::which(base::sapply(WHENS, uj::isEQ, y = when))
  if (base::length(i) > 0) {
    match <- VALS[i[1]]
    if (uj::notEQ1(val, match)) {
      if (base::is.character(match)) {match <- base::paste0("'", match, "'")}
      uj::bankerr("When ", labs1, " is ", when, ", ", labs2, " must be ", match, ".", GENS = 1, D = "")
  }}
}

#' @rdname checker
#' @export
check_fail <- function(...) {
  n.dots <- base::...length()
  labs <- base::...names()
  ok.n <- n.dots > 0
  ok.named <- base::length(labs) == n.dots
  ok.blank <- !base::any(labs == "")
  ok.unq <- base::length(labs) == base::length(base::unique(labs))
  uj::errs_if_nots(ok.n    , "[...] is empty."                                  ,
                   ok.named, "all [...] args must be named with [NAMED = TRUE].",
                   ok.blank, "[...] arg names may not be blank strings (\"\")." ,
                   ok.unq  , "[...] arg names must be unique."                  , PKG = "uj")
  for (i in 1:n.dots) {
    x <- uj::failsafe(base::...elt(i))
    if (assertthat::is.error(x)) {uj::bankerr("evaluating arg [", labs[i], "] produced an error: ", uj::av(x), GENS = 1, D = "")}
  }
}
