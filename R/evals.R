.conform <- function(...) {
  x <- base::list(...)
  ns <- uj::NS(x)
  if (base::all(base::sapply(x, uj::atm_vec))) {uj::allIN(ns, 1, base::max(ns))}
  else if (base::all(base::sapply(x, uj::isATM))) {uj::NU1(base::sapply(base::lapply(x, base::dim), base::paste0, collapse = "."))}
}

#' @name evals
#' @encoding UTF-8
#' @family extensions
#' @family logicals
#' @title Enhancements of `base` logical functions
#' @details The left-hand column of the following table gives a function call from this family and the right column shows how they extend of base logical functions:
#' \tabular{ll}{  `notIN(x, y, ...)`    \tab `!(x %in% y)`          \cr   \tab  }
#' \tabular{ll}{  `notIN(x, y, ...)`    \tab `!(x %in% y)`          \cr
#'                `lacks(x, y, ...)`    \tab `!(y %in% x)`          \cr
#'                `ismf(x, y, ...)`     \tab `!(x %in% y)`          \cr
#'                `isin(x, y, ...)`     \tab `x %in% y`             \cr   \tab  }
#' \tabular{ll}{  `one(x, y, ...)`      \tab `xor(x, y)`            \cr
#'                `has(x, y, ...)`      \tab `y %in% x`             \cr
#'                `nor(x, y, ...)`      \tab `!(x | y)`             \cr
#'                `and(x, y, ...)`      \tab `x & y`                \cr   \tab  }
#' \tabular{ll}{  `or(x, y, ...)`       \tab `x | y`                \cr   \tab  }
#' \tabular{ll}{  `TEST(x, ...)`        \tab `isTRUE(x)`            \cr   \tab  }
#' \tabular{ll}{  `not(x, ...)`         \tab `!x`                     }
#' \cr\cr The next set of functions evaluate an arbitrary number of `...` arguments expected to be logical scalars for whether:
#' \tabular{ll}{  `ANY`     \tab Any `...` arg is scalar `TRUE`.    \cr
#'                `ALL`     \tab All `...` args are scalar `TRUE`.  \cr
#'                `NOR`     \tab `0 ...` args are scalar `TRUE`.    \cr
#'                `ONE`     \tab `1 ...` arg is scalar `TRUE`.      \cr
#'                `TWO`     \tab `2+ ...` args are scalar `TRUE`.     }
#' \cr\cr The remainder of functions in this family sweep across or within `...` arguments, as specified by the arguments `across` and `within`:
#' \tabular{ll}{  `tests`   \tab Logically indexes `TRUE` values.   \cr   \tab  }
#' \tabular{ll}{  `ones`    \tab Checks for exactly `1 TRUE` value. \cr
#'                `alls`    \tab Checks for only `TRUE` values.     \cr
#'                `anys`    \tab Checks for any `TRUE` values.      \cr
#'                `twos`    \tab Checks for `2+ TRUE` values.       \cr
#'                `nors`    \tab Checks for `0 TRUE` values.        \cr   \tab  }
#' \tabular{ll}{  `w`       \tab Numerically indexes `TRUE` values.   }
#' @param x An \link[=atm_lgl]{atomic logical object} for all functions other than `isIN`, `isMF`, `has`, and `lacks`. Otherwise, an atomic object.
#' @param y An atomic logical object. for all functions other than `isIN`, `isMF`, `has`, and `lacks`. Otherwise, an atomic object \link[=compatible]{compatible} with `x`.
#' @param ... An arbitrary number of \link[=lgl_vec]{logical vecs} to be processed.
#' @param na `TRUE` or `FALSE` indicating what value should replace `NA` values.
#' @param err `TRUE` or `FALSE`. `err = TRUE` indicates `TRUE` should be substituted for non-logical values, `err = FALSE` indicates `FALSE` should be substituted for non-logical values, `err = NA` indicates an error should be thrown if a non-logical value is encountered.
#' @param a `TRUE` or `FALSE` indicating whether to atomize `...` before processing. This creates a single atomic vector of all atomic elements contained in all `...` arguments and effectively changes the behavior of `or` to `any`, `and` to `all`, and `not` to `!any`.
#' @param not `TRUE` or `FALSE` indicating whether to negate values in arguments supplied in `...` before processing.
#' @param agg A \link[=cmp_chr_scl]{complete character scalar} in `c('nor', 'one', 'any', 'two', 'all')` used to specify, respectively, that 0, 1, any, 2 or more, and all values must be `TRUE`.
#' @param sweep \link[=cmp_chr_scl]{Character scalar}. `NA`, `across`, `within`, and `both` indicate no sweeping, sweeping corresponding elements across arguments, sweeping within argument, and sweeping across and within all arguments, respectively.
#' @examples
#' egS1 <- TRUE
#' egS2 <- TRUE
#' egS3 <- FALSE
#' egV1 <- sample(c(TRUE, FALSE), 5, replace = TRUE)
#' egV2 <- sample(c(TRUE, FALSE), 5, replace = TRUE)
#' egV3 <- sample(c(TRUE, FALSE), 5, replace = TRUE)
#'
#' egV1
#' egV2
#' egV3
#'
#' not(egV3)
#' or(egV2, egV3)
#' and(egV2, egV3)
#' nor(egV2, egV3)
#' one(egV2, egV3)
#'
#' TEST(egS1)
#' TEST(NA, na = FALSE)
#' TEST("seven", err = FALSE)
#'
#' ANY(egS1, egS2, egS3)
#' ALL(egS1, egS2, egS3)
#' NOR(egS1, egS2, egS3)
#' ONE(egS1, egS2, egS3)
#' TWO(egS1, egS2, egS3)
#'
#' tests(egV1)
#' tests(egV1, egV2, egV3, sweep = "across", agg = "any")
#' tests(egV1, egV2, egV3, sweep = "within", agg = "nor")
#' tests(egV1, egV2, egV3, sweep = "both", agg = "two")
#'
#' ones(egV1, egV2, egV3, sweep = "across")
#' twos(egV1, egV2, egV3, sweep = "within")
#' nors(egV1, egV2, egV3, sweep = "both")
#' anys(egV1, egV2, egV3, sweep = "within")
#' alls(egV1, egV2, egV3, sweep = "across")
#'
#' isIN("a", letters)
#' isIN(c("a", "b", "c"), letters, agg = "all")
#'
#' isMF(c("a", "b", "1"), letters, agg = "two")
#'
#' has(letters, "1")
#' has(letters, "a")
#'
#' lacks(letters, "1")
#' lacks(letters, "a")
#' @export
not <- function(x, na = 'err') {
  uj::errs_if_nots(uj::pop_lgl(x)                              , "[x] must be a populated logical object (?pop_lgl).",
                 uj::isIN1(na, TRUE, FALSE, NA, 'err')      , "[na] must be TRUE, FALSE, NA, or 'err'."          ,
                 uj::isEQ1(na, 'err') & uj::anyNA(uj::av(x)), "[x] may not contain NA values."                   , PKG = "uj")
  if (uj::isTF1(na)) {x[uj::na(x)] <- na}
  !x
}

#' @rdname evals
#' @export
and <- function(x, y, na = 'err') {
  uj::errs_if_nots(uj::pop_lgl(x)                              , "[x] must be a populated logical object (?pop_lgl).",
                 uj::pop_lgl(y)                              , "[x] must be a populated logical object (?pop_lgl).",
                 uj:::.conform(x, y)                        , "[x] and y must be conformable."                   ,
                 uj::isIN1(na, TRUE, FALSE, NA, 'err')      , "[na] must be TRUE, FALSE, NA, or 'err'."          ,
                 uj::isEQ1(na, 'err') & uj::anyNA(uj::av(x)), "[x] may not contain NA values."                   , PKG = "uj")
  if (uj::isTF1(na)) {
    x[uj::na(x)] <- na
    y[uj::na(x)] <- na
  }
  x & y
}

#' @rdname evals
#' @export
or <- function(x, y, na = 'err') {
  uj::errs_if_nots(uj::pop_lgl(x)                           , "[x] must be a populated logical object (?pop_lgl).",
                 uj::pop_lgl(y)                           , "[x] must be a populated logical object (?pop_lgl).",
                 uj:::.conform(x, y)                     , "[x] and y must be conformable."                   ,
                 uj::isIN1(na, TRUE, FALSE, NA, 'err')      , "[na] must be TRUE, FALSE, NA, or 'err'."          ,
                 uj::isEQ1(na, 'err') & uj::anyNA(uj::av(x)), "[x] may not contain NA values."                   , PKG = "uj")
  if (uj::isTF1(na)) {
    x[uj::na(x)(x)] <- na
    y[uj::na(x)(x)] <- na
  }
  x | y
}

#' @rdname evals
#' @export
nor <- function(x, y, na = 'err') {
  uj::errs_if_nots(
    uj::pop_lgl(x)                               , "[x] must be a populated logical object (?pop_lgl).",
    uj::pop_lgl(y)                               , "[x] must be a populated logical object (?pop_lgl).",
    uj:::.conform(x, y)                         , "[x] and y must be conformable."                   ,
    uj::isIN1(na, TRUE, FALSE, NA, 'err')       , "[na] must be TRUE, FALSE, NA, or 'err'."          ,
    uj::isEQ1(na, 'err') & uj:::anyNA(uj::av(x)), "[x] may not contain NA values."                   , PKG = "uj")
  if (uj::isTF1(na)) {
    x[uj::na(x)] <- na
    y[uj::na(x)] <- na
  }
  !(x | y)
}

#' @rdname evals
#' @export
one <- function(x, y, na = 'err') {
  uj::errs_if_nots(
    uj::pop_lgl(x)                              , "[x] must be a populated logical object (?pop_lgl).",
    uj::pop_lgl(y)                              , "[x] must be a populated logical object (?pop_lgl).",
    uj:::.conform(x, y)                        , "[x] and y must be conformable."                   ,
    uj::isIN1(na, TRUE, FALSE, NA, 'err')      , "[na] must be TRUE, FALSE, NA, or 'err'."          ,
    uj::isEQ1(na, 'err') & uj::anyNA(uj::av(x)), "[x] may not contain NA values."                   , PKG = "uj")
  if (uj::isTF1(na)) {
    x[uj::na(x)] <- na
    y[uj::na(x)] <- na
  }
  base::xor(x, y)
}

#' @rdname evals
#' @export
TEST <- function(x, na = FALSE, err = NA) {
  uj::errs_if_nots(uj::lglSCL(x) | uj::isLG1(err)        , "[x] is not a logical scalar."            ,
                 uj::isTF1(na)                         , "[na] must be TRUE or FALSE."             ,
                 uj::isLG1(err) | uj::isEQ1(err, 'err'), "[err] must be TRUE, FALSE, NA, or 'err'.", PKG = "uj")
  uj::f0(uj::notLG(x), err, uj::f0(uj::na(x), na, x))
}

#' @rdname evals
#' @export
ANY <- function(..., err = NA) {
  uj::errs_if_nots(base::all(base::sapply(base::list(...), uj::lglSCL)), "All arguments in [...] must be logical scalars (?lglSCL).",
                 uj::isLG1(err) | uj::isEQ1(err, 'na')               , "[err] must be TRUE, FALSE, NA, or 'na'."                  , PKG = "uj")
  for (i in 1:uj::ND()) {
    x <- tryCatch(base::...elt(i) == T, error = function(e) NA, finally = NULL)
    uj::err_if_not(uj::isDIF1(err, "na") | uj::cmp_lgl_scl(x), "[..", i, "] did not resolve to TRUE or FALSE.", PKG = "uj")
    if (uj::isERR(x)) {return(err)} else if (x) {return(T)}
  }
  F
}

#' @rdname evals
#' @export
ALL <- function(..., err = NA) {
  uj::errs_if_nots(base::all(base::sapply(base::list(...), uj::lglSCL)), "All arguments in [...] must be logical scalars (?lglSCL).",
                 uj::isLG1(err) | uj::isEQ1(err, 'na')               , "[err] must be TRUE, FALSE, NA, or 'na'."                  , PKG = "uj")
  for (i in 1:uj::ND()) {
    x <- tryCatch(base::...elt(i) == T, error = function(e) NA, finally = NULL)
    uj::err_if_not(uj::isDIF1(err, "na") | uj::cmp_lgl_scl(x), "[..", i, "] did not resolve to TRUE or FALSE.", PKG = "uj")
    if (uj::isERR(x)) {return(err)} else if (!x) {return(F)}
  }
  T
}

#' @rdname evals
#' @export
NOR <- function(..., err = NA) {
  uj::errs_if_nots(base::all(base::sapply(base::list(...), uj::lglSCL)), "All arguments in [...] must be logical scalars (?lglSCL).",
                 uj::isLG1(err) | uj::isEQ1(err, 'na')               , "[err] must be TRUE, FALSE, NA, or 'na'."                  , PKG = "uj")
  for (i in 1:uj::ND()) {
    x <- tryCatch(base::...elt(i) == T, error = function(e) NA, finally = NULL)
    uj::err_if_not(uj::isDIF1(err, "na") | uj::cmp_lgl_scl(x), "[..", i, "] did not resolve to TRUE or FALSE.", PKG = "uj")
    if (uj::isERR(x)) {return(err)} else if (x) {return(F)}
  }
  T
}

#' @rdname evals
#' @export
ONE <- function(..., err = NA) {
  uj::errs_if_nots(base::all(base::sapply(base::list(...), uj::lglSCL)), "All arguments in [...] must be logical scalars (?lglSCL).",
                 uj::isLG1(err) | uj::isEQ1(err, 'na')               , "[err] must be TRUE, FALSE, NA, or 'na'."                  , PKG = "uj")
  n <- 0
  for (i in 1:uj::ND()) {
    x <- tryCatch(base::...elt(i) == T, error = function(e) NA, finally = NULL)
    uj::err_if_not(uj::isDIF1(err, "na") | uj::cmp_lgl_scl(x), "[..", i, "] did not resolve to TRUE or FALSE.", PKG = "uj")
    if (uj::isERR(x)) {return(err)}
    n <- n + x
    if (n > 1) {return(F)}
  }
  n == 1
}

#' @rdname evals
#' @export
TWO <- function(..., err = NA) {
  uj::errs_if_nots(base::all(base::sapply(base::list(...), uj::lglSCL)), "All arguments in [...] must be logical scalars (?lglSCL).",
                   uj::isLG1(err) | uj::isEQ1(err, 'na')             , "[err] must be TRUE, FALSE, NA, or 'na'."                  , PKG = "uj")
  n <- 0
  for (i in 1:uj::ND()) {
    x <- tryCatch(base::...elt(i) == T, error = function(e) NA, finally = NULL)
    uj::err_if_not(uj::isDIF1(err, "na") | uj::cmp_lgl_scl(x), "[..", i, "] did not resolve to TRUE or FALSE.", PKG = "uj")
    if (uj::isERR(x)) {return(err)}
    n <- n + x
    if (n > 1) {return(T)}
  }
  F
}

#' @rdname evals
#' @export
tests <- function(..., a = FALSE, nas = FALSE, not = FALSE, agg = NA, sweep = NA) {
  x <- uj::f0(uj::isT1(a), base::list(uj::av(...)), base::list(...))
  n <- uj::N(x)
  ns <- uj::f0(n == 0, NULL, uj::NS(x))
  ok.s <- uj::f0(uj::isNAS(sweep), T, uj::isIN1(sweep, "across", "within", "both"))
  ok.a <- uj::f0(uj::isNAS(agg  ), T, uj::isIN1(agg  , "nor"   , "one"   , "any", "two", "all"))
  ok.sa <- (uj::isNAS(agg) & uj::isNAS(sweep)) | (uj::isOKS(agg) & uj::isOKS(sweep))
  ok.r <- uj::f0(n < 2, T, base::all(base::round(base::max(ns) / ns) == base::max(ns) / ns))
  uj::errs_if_nots(uj::isTF1(a)  , "[a] must be TRUE or FALSE."                            ,
                 uj::isTF1(nas), "[nas] must be TRUE or FALSE."                          ,
                 uj::isTF1(not), "[not] must be TRUE or FALSE."                          ,
                 ok.r       , "[...] arguments are not recyclable (?recyclable)."        ,
                 ok.s       , "[sweep] must be NA, 'across', 'within', or 'both'."       ,
                 ok.a       , "[agg] must be NA, 'nor', 'one', 'any', 'two', 'all'."     ,
                 ok.sa      , "[agg] and [sweep] must both be NA or must both be non-NA.", PKG = 'uj')
  for (i in 1:uj::N(x)) {
    arg <- x[[i]]
    arg[uj::na(arg)] <- nas
    if (not) {arg <- !arg}
    x[[i]] <- arg
  }
  if (uj::notNAS(sweep) | uj::notNAS(agg)) {
    maxn <- base::max(uj::NS(x))
    reps <- maxn / uj::NS(x)
    if (sweep != "both") {
      args <- base::matrix(NA, nrow = maxn, ncol = uj::N(x))
      for (i in 1:uj::N(x)) {args[, i] <- base::rep.int(x[[i]], reps[i])}
      x <- args
      nw <- uj::f0(sweep == "within", base::colSums(1 * x), base::rowSums(1 * x))
    } else {nw <- base::sum(1 * uj::av(x))}
    uj::f0(agg == "nor", nw == 0, uj::f0(agg == "any", nw >= 1, uj::f0(agg == "all", nw == uj::N(x), uj::f0(agg == "one", nw == 1, nw > 1))))
  } else {x}
}

#' @rdname evals
#' @export
nors <- function(..., a = FALSE, na = FALSE, sweep = "across") {
  if (uj::isIN1(sweep, "across", "both", "within")) {uj::tests(..., a = a, na = na, agg = "nor", sweep = sweep)}
  else {uj::stopper("[sweep] must be 'across', 'within', or 'both'.", FUN = "nors", PKG = "uj", STACK = uj::callers())}
}

#' @rdname evals
#' @export
anys <- function(..., a = FALSE, na = FALSE, sweep = "across") {
  if (uj::isIN1(sweep, "across", "both", "within")) {uj::tests(..., a = a, na = na, agg = "any", sweep = sweep)}
  else {uj::stopper("[sweep] must be 'across', 'within', or 'both'.", FUN = "anys", PKG = "uj", STACK = uj::callers())}
}

#' @rdname evals
#' @export
alls <- function(..., a = FALSE, na = FALSE, sweep = "across") {
  if (uj::isIN1(sweep, "across", "both", "within")) {uj::tests(..., a = a, na = na, agg = "all", sweep = sweep)}
  else {uj::stopper("[sweep] must be 'across', 'within', or 'both'.", FUN = "alls", PKG = "uj", STACK = uj::callers())}
}

#' @rdname evals
#' @export
ones <- function(..., a = FALSE, na = FALSE, sweep = "across") {
  if (uj::isIN1(sweep, "across", "both", "within")) {uj::tests(..., a = a, na = na, agg = "one", sweep = sweep)}
  else {uj::stopper("[sweep] must be 'across', 'within', or 'both'.", FUN = "ones", PKG = "uj", STACK = uj::callers())}
}

#' @rdname evals
#' @export
twos <- function(..., a = FALSE, na = FALSE, sweep = "across") {
  if (uj::isIN1(sweep, "across", "both", "within")) {uj::tests(..., a = a, na = na, agg = "two", sweep = sweep)}
  else {uj::stopper("[sweep] must be 'across', 'within', or 'both'.", FUN = "twos", PKG = "uj", STACK = uj::callers())}
}

#' @rdname evals
#' @export
isin <- function(x, y, na = FALSE, agg = NA, not = FALSE) {
  if (uj::DEF(x) & uj::DEF(y)) {
    ok.x <- uj::atm_vec(x)
    ok.y <- uj::atm_vec(y)
    ok.xy <- uj::f0(!ok.x | !ok.y, T, uj::compatible(x, y))
    ok.agg <- uj::f0(uj::isNAS(agg), T, uj::isIN1(agg, "nor", "one", "any", "two", "all"))
    uj::errs_if_nots(ok.x         , "[x] must be a populated atomic vec (?atm_vec)."          ,
                   ok.y         , "[y] must be a populated atomic vec (?atm_vec)."          ,
                   uj::isTF1(na), "[na] must be TRUE or FALSE."                            ,
                   ok.agg       , "[agg] must be NA, 'nor', 'one', 'any', 'two', or 'all'.",
                   uj::isTF1(not), "[not] must be TRUE or FALSE."                           ,
                   ok.xy        , "[x] and [y] are not of compatible modes."               ,    PKG = "uj")
    z <- uj::isIN1(x, y)
    z[x[uj::na(x)]] <- na
    n <- uj::NW(z)
    uj::f0(agg == "nor", !base::any(z), uj::f0(agg == "any", base::any(z), uj::f0(agg == "all", base::all(z), uj::f0(agg == "one", n == 1, uj::f0(agg == "two", n >= 2, z)))))
  } else {F}
}

#' @rdname evals
#' @export
ismf <- function(x, y, na = FALSE, agg = NA) {!uj::isIN(x, y, na = na, agg = agg, not = T)}

#' @rdname evals
#' @export
has <- function(x, y, na = TRUE, agg = NA) {uj::isIN(y, x, na = na, agg = agg, not = F)}

#' @rdname evals
#' @export
lacks <- function(x, y, na = TRUE, agg = NA) {uj::isMF(y, x, na = na, agg = agg)}
