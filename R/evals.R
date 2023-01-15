.conform <- function(...) {
  x <- base::list(...)
  ns <- base::lengths(x)
  if (base::all(base::sapply(x, uj::atm_vec))) {base::all(ns %in% base::c(1, base::max(ns)))}
  else if (base::all(base::sapply(x, base::is.atomic))) {base::length(base::unique(base::sapply(base::lapply(x, base::dim), base::paste0, collapse = "."))) == 1}
}

#' @name evals
#' @encoding UTF-8
#' @family extensions
#' @family logicals
#' @title Enhancements of `base` logical functions
#' @description The following are extensions of base logical functions:
#' \tabular{rl}{
#'        **Function usage** \tab   **Extends**
#'   \cr `not_in(x, y, ...)` \tab   `!(x %in% y)`
#'   \cr `is_out(x, y, ...)` \tab   `!(x %in% y)`
#'   \cr  `isout(x, y, ...)` \tab   `!(x %in% y)`
#'   \cr  `notin(x, y, ...)` \tab   `!(x %in% y)`
#'   \cr  `lacks(x, y, ...)` \tab   `!(y %in% x)`
#'   \cr  `is_in(x, y, ...)` \tab   `x %in% y`
#'   \cr   `isin(x, y, ...)` \tab   `x %in% y`
#'   \cr    `has(x, y, ...)` \tab   `y %in% x`
#'   \cr    `nor(x, y, ...)` \tab   `!(x | y)`
#'   \cr    `one(x, y, ...)` \tab   `xor(x, y)`
#'   \cr    `and(x, y, ...)` \tab   `x & y`
#'   \cr     `or(x, y, ...)` \tab   `x | y`
#'   \cr      `TEST(x, ...)` \tab   `isTRUE(x)`
#'   \cr       `not(x, ...)` \tab   `!x`
#' }
#' The next set of functions evaluate an arbitrary number of `...` arguments expected to be logical scalars for whether:
#' \tabular{rl}{
#'       `ANY` \tab   Any `...` argument is scalar `TRUE`.
#'   \cr `ALL` \tab   All `...` argument are scalar `TRUE`.
#'   \cr `NOR` \tab   `0 ...` arguments are scalar `TRUE`.
#'   \cr `ONE` \tab   `1 ...` argument is scalar `TRUE`.
#'   \cr `TWO` \tab   `2+ ...` arguments are scalar `TRUE`.
#' }
#' The remainder of functions in this family sweep across or within `...` arguments, as specified by the arguments `across` and `within`:
#' \tabular{rl}{
#'      `tests` \tab   Logically indexes `TRUE` values.
#'   \cr    `w` \tab   Numerically indexes `TRUE` values.
#'   \cr `anys` \tab   Checks for any `TRUE` values.
#'   \cr `alls` \tab   Checks for only `TRUE` values.
#'   \cr `nors` \tab   Checks for `0 TRUE` values.
#'   \cr `ones` \tab   Checks for exactly `1 TRUE` value.
#'   \cr `twos` \tab   Checks for `2+ TRUE` values.
#' }
#' @param x An \link[=atm_lgl]{atomic logical object} for all functions other than `is_in`, `not_in`, `has`, and `lacks`. Otherwise, an atomic object.
#' @param y An atomic logical object. for all functions other than `is_in`, `not_in`, `has`, and `lacks`. Otherwise, an atomic object \link[=compatible]{compatible} with `x`.
#' @param ... An arbitrary number of \link[=lgl_vec]{logical vecs} to be processed.
#' @param na A non-`NA` logical scalar indicating what value should replace `NA` values.
#' @param err A non-`NA` logical scalar. `err = TRUE` indicates `TRUE` should be substituted for non-logical values, `err = FALSE` indicates `FALSE` should be substituted for non-logical values, `err = NA` indicates an error should be thrown if a non-logical value is encountered.
#' @param a A non-`NA` logical scalar indicating whether to atomize `...` before processing. This creates a single atomic vector of all atomic elements contained in all `...` arguments and effectively changes the behavior of `or` to `any`, `and` to `all`, and `not` to `!any`.
#' @param not A non-`NA` logical scalar indicating whether to negate values in arguments supplied in `...` before processing.
#' @param agg A \link[=cmp_chr_scl]{complete character scalar} in `c('nor', 'one', 'any', 'two', 'all')` used to specify, respectively, that 0, 1, any, 2 or more, and all values must be `TRUE`.
#' @param sweep \link[=cmp_chr_scl]{Character scalar}. `NA`, `across`, `within`, and `both` indicate no sweeping, sweeping corresponding elements across arguments, sweeping within argument, and sweeping across and within all arguments, respectively.
#' @examples
#' s1. <- TRUE
#' s2. <- TRUE
#' s3. <- FALSE
#' v1. <- sample(c(TRUE, FALSE), 5, replace = TRUE)
#' v2. <- sample(c(TRUE, FALSE), 5, replace = TRUE)
#' v3. <- sample(c(TRUE, FALSE), 5, replace = TRUE)
#'
#' v1.
#' v2.
#' v3.
#'
#' not(v3.)
#' or(v2., v3.)
#' and(v2., v3.)
#' nor(v2., v3.)
#' one(v2., v3.)
#'
#' TEST(s1.)
#' TEST(NA, na = FALSE)
#' TEST("seven", err = FALSE)
#'
#' ANY(s1., s2., s3.)
#' ALL(s1., s2., s3.)
#' NOR(s1., s2., s3.)
#' ONE(s1., s2., s3.)
#' TWO(s1., s2., s3.)
#'
#' tests(v1.)
#' tests(v1., v2., v3., sweep = "across", agg = "any")
#' tests(v1., v2., v3., sweep = "within", agg = "nor")
#' tests(v1., v2., v3., sweep = "both", agg = "two")
#'
#' ones(v1., v2., v3., sweep = "across")
#' twos(v1., v2., v3., sweep = "within")
#' nors(v1., v2., v3., sweep = "both")
#' anys(v1., v2., v3., sweep = "within")
#' alls(v1., v2., v3., sweep = "across")
#'
#' is_in("a", letters)
#' is_in(c("a", "b", "c"), letters, agg = "all")
#'
#' not_in(c("a", "b", "1"), letters, agg = "two")
#'
#' has(letters, "1")
#' has(letters, "a")
#'
#' lacks(letters, "1")
#' lacks(letters, "a")
#' @export
not <- function(x, na = 'err') {
  errs <- base::c(uj::f0(uj::pop_lgl(x)                    , NULL, "[x] must be a populated logical object (?cmp_lgl)."),
                  uj::f0(uj::isLG(na) | uj::isEQ(na, 'err'), NULL, "[na] must be TRUE, FALSE, NA, or 'err'."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  if (uj::isTF(na)) {x[base::is.na(x)] <- na}
  !x
}

#' @rdname evals
#' @export
and <- function(x, y, na = 'err') {
  errs <- base::c(uj::f0(uj::pop_lgl(x)                    , NULL, "[x] must be a populated logical object (?cmp_lgl)."),
                  uj::f0(uj::pop_lgl(y)                    , NULL, "[x] must be a populated logical object (?cmp_lgl)."),
                  uj::f0(uj:::.conform(x, y)               , NULL, "[x] and y must be conformable."),
                  uj::f0(uj::isLG(na) | uj::isEQ(na, 'err'), NULL, "[na] must be TRUE, FALSE, NA, or 'err'."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  if (uj::isTF(na)) {x[base::is.na(x)] <- na; y[base::is.na(x)] <- na}
  x & y
}

#' @rdname evals
#' @export
or <- function(x, y, na = 'err') {
  errs <- base::c(uj::f0(uj::pop_lgl(x)                    , NULL, "[x] must be a populated logical object (?cmp_lgl)."),
                  uj::f0(uj::pop_lgl(y)                    , NULL, "[x] must be a populated logical object (?cmp_lgl)."),
                  uj::f0(uj:::.conform(x, y)               , NULL, "[x] and y must be conformable."),
                  uj::f0(uj::isLG(na) | uj::isEQ(na, 'err'), NULL, "[na] must be TRUE, FALSE, NA, or 'err'."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  if (uj::isTF(na)) {x[base::is.na(x)] <- na; y[base::is.na(x)] <- na}
  x | y
}

#' @rdname evals
#' @export
nor <- function(x, y, na = 'err') {
  errs <- base::c(uj::f0(uj::pop_lgl(x)                    , NULL, "[x] must be a populated logical object (?cmp_lgl)."),
                  uj::f0(uj::pop_lgl(y)                    , NULL, "[x] must be a populated logical object (?cmp_lgl)."),
                  uj::f0(uj:::.conform(x, y)               , NULL, "[x] and y must be conformable."),
                  uj::f0(uj::isLG(na) | uj::isEQ(na, 'err'), NULL, "[na] must be TRUE, FALSE, NA, or 'err'."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  if (uj::isTF(na)) {x[base::is.na(x)] <- na; y[base::is.na(x)] <- na}
  !(x | y)
}

#' @rdname evals
#' @export
one <- function(x, y, na = 'err') {
  errs <- base::c(uj::f0(uj::pop_lgl(x)                    , NULL, "[x] must be a populated logical object (?cmp_lgl)."),
                  uj::f0(uj::pop_lgl(y)                    , NULL, "[x] must be a populated logical object (?cmp_lgl)."),
                  uj::f0(uj:::.conform(x, y)               , NULL, "[x] and [y] must be conformable."),
                  uj::f0(uj::isLG(na) | uj::isEQ(na, 'err'), NULL, "[na] must be TRUE, FALSE, NA, or 'err'."))
  if (!base::is.null(errs)) {stop(uj::.errs(errs))}
  if (uj::isTF(na)) {x[base::is.na(x)] <- na; y[base::is.na(x)] <- na}
  base::xor(x, y)
}

#' @rdname evals
#' @export
TEST <- function(x, na = FALSE, err = NA) {
  errs <- base::c(uj::f0(uj::isTF(na)                        , NULL, "[na] must be TRUE or FALSE."),
                  uj::f0(uj::isLG(err) | uj::isEQ(err, 'err'), NULL, "[err] must be TRUE, FALSE, NA, or 'err'."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  if (!uj::lgl_scl(x) & !uj::isLG(err)) {stop(uj::format_errs(pkg = "uj", "[x] is not a logical scalar."))} else if (!uj::lgl_scl(x)) {err} else if (base::is.na(x)) {na} else {x}
}

#' @rdname evals
#' @export
ANY <- function(..., err = NA) {
  errs <- base::c(uj::f0(base::all(base::sapply(base::list(...), uj::lgl_scl)), NULL, "All arguments in [...] must be logical scalars (?lgl_scl)."),
                  uj::f0(uj::isLG(err) | uj::isEQ(err, 'na')                  , NULL, "[err] must be TRUE, FALSE, NA, or 'na'."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  for (i in 1:base::...length()) {
    x <- tryCatch(base::...elt(i) == T, error = function(e) NA, finally = NULL)
    vx <- uj::cmp_lgl_scl(x)
    if (!vx) {
      if (uj::isTF(err)) {x <- err}
      else {stop(uj::format_errs(pkg = "uj", base::paste0("[..", i, "] did not resolve to TRUE or FALSE.")))}
    }
    if (x) {return(T)}
  }
  F
}

#' @rdname evals
#' @export
ALL <- function(..., err = NA) {
  errs <- base::c(uj::f0(base::all(base::sapply(base::list(...), uj::lgl_scl)), NULL, "All arguments in [...] must be logical scalars (?lgl_scl)."),
                  uj::f0(uj::isLG(err) | uj::isEQ(err, 'na')                  , NULL, "[err] must be TRUE, FALSE, NA, or 'na'."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  for (i in 1:base::...length()) {
    x <- tryCatch(base::...elt(i) == T, error = function(e) NA, finally = NULL)
    vx <- uj::cmp_lgl_scl(x)
    if (!vx) {if (uj::isTF(err)) {x <- err} else {stop(uj::format_errs(pkg = "uj", base::paste0("[..", i, "] did not resolve to TRUE or FALSE.")))}}
    if (!x) {return(F)}
  }
  T
}

#' @rdname evals
#' @export
NOR <- function(..., err = NA) {
  errs <- base::c(uj::f0(base::all(base::sapply(base::list(...), uj::lgl_scl)), NULL, "All arguments in [...] must be logical scalars (?lgl_scl)."),
                  uj::f0(uj::isLG(err) | uj::isEQ(err, 'na')                  , NULL, "[err] must be TRUE, FALSE, NA, or 'na'."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  for (i in 1:base::...length()) {
    x <- tryCatch(base::...elt(i) == T, error = function(e) NA, finally = NULL)
    vx <- uj::cmp_lgl_scl(x)
    if (!vx) {
      if (uj::isTF(err)) {x <- err}
      else {stop(uj::format_errs(pkg = "uj", base::paste0("[..", i, "] did not resolve to TRUE or FALSE.")))}
    }
    if (x) {return(F)}
  }
  T
}

#' @rdname evals
#' @export
ONE <- function(..., err = NA) {
  errs <- base::c(uj::f0(base::all(base::sapply(base::list(...), uj::lgl_scl)), NULL, "All arguments in [...] must be logical scalars (?lgl_scl)."),
                  uj::f0(uj::isLG(err) | uj::isEQ(err, 'na')                  , NULL, "[err] must be TRUE, FALSE, NA, or 'na'."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  n <- 0
  for (i in 1:base::...length()) {
    x <- tryCatch(base::...elt(i) == T, error = function(e) NA, finally = NULL)
    vx <- uj::cmp_lgl_scl(x)
    if (!vx) {
      if (uj::isTF(err)) {x <- err}
      else {stop(uj::format_errs(pkg = "uj", base::paste0("[..", i, "] did not resolve to TRUE or FALSE.")))}
    }
    if (x) {n <- n + 1}
    if (n > 1) {return(F)}
  }
  (n == 1)
}

#' @rdname evals
#' @export
TWO <- function(..., err = NA) {
  errs <- base::c(uj::f0(base::all(base::sapply(base::list(...), uj::lgl_scl)), NULL, "All arguments in [...] must be logical scalar (?lgl_scl)."),
                  uj::f0(isLG(err) | uj::isEQ(err, 'na')                      , NULL, "[err] must be TRUE, FALSE, NA, or 'na'."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  n <- 0
  for (i in 1:base::...length()) {
    x <- tryCatch(base::...elt(i) == T, error = function(e.) NA, finally = NULL)
    vx <- uj::cmp_lgl_scl(x)
    if (!vx) {
      if (uj::isTF(err)) {x <- err}
      else {stop(uj::format_errs(pkg = "uj", base::paste0("[..", i, "] did not resolve to TRUE or FALSE.")))}
    }
    if (x) {n <- n + 1}
    if (n > 1) {return(T)}
  }
  F
}

#' @rdname evals
#' @export
tests <- function(..., a = FALSE, na = FALSE, not = FALSE, agg = NA, sweep = NA) {
  x <- uj::f0(uj::isT(a), base::list(uj::av(...)), base::list(...))                                      # if atomizing is requested, store the atomized values in a 1 element list, otherwise, extract ... as a list
  n <- base::length(x)                                                                 # number of args in {...}
  ns <- uj::f0(n == 0, NULL, base::lengths(x))                                             # lengths of args in {...}
  ok.s <- uj::f0(uj::isNAS(sweep), T, sweep %in% c("across", "within", "both"))
  ok.a <- uj::f0(uj::isNAS(agg), T, agg %in% c("nor", "one", "any", "two", "all"))
  ok.sa <- (uj::isNAS(agg) & uj::isNAS(sweep)) | (uj::isOKS(agg) & uj::isOKS(sweep))
  ok.r <- uj::f0(n < 2, T, base::all(base::round(base::max(ns) / ns) == base::max(ns) / ns))                 # whether arguments are recyclable
  errs <- base::c(uj::f0(uj::isTF(a)  , NULL, "[a] must be TRUE or FALSE."),
                  uj::f0(uj::isTF(na) , NULL, "[na] must be TRUE or FALSE."),
                  uj::f0(uj::isTF(not), NULL, "[not] must be TRUE or FALSE."),
                  uj::f0(ok.r         , NULL, "[...] arguments are not recyclable (?recyclable)."),
                  uj::f0(ok.s         , NULL, "[sweep] must be NA, 'across', 'within', or 'both'."),
                  uj::f0(ok.a         , NULL, "[agg] must be NA, 'nor', 'one', 'any', 'two', 'all'."),
                  uj::f0(ok.sa        , NULL, "[agg] and [sweep] must both be NA or must both be non-NA."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  for (i in 1:base::length(x)) {
    arg <- x[[i]]
    arg[base::is.na(arg)] <- na
    if (not) {arg <- !arg}
    x[[i]] <- arg
  }
  if (uj::isNAS(sweep) & uj::isNAS(agg)) {return(x)}
  maxn <- base::max(base::lengths(x))
  reps <- maxn / base::lengths(x)
  if (sweep != "both") {
    args <- base::matrix(NA, nrow = maxn, ncol = base::length(x))
    for (i in 1:base::length(x)) {args[, i] <- base::rep.int(x[[i]], reps[i])}
    x <- args
    nw <- uj::f0(sweep == "within", base::colSums(1 * x), base::rowSums(1 * x))
  } else {nw <- base::sum(1 * uj::av(x))}
  uj::f0(agg == "nor", nw == 0, uj::f0(agg == "any", nw >= 1, uj::f0(agg == "all", nw == base::length(x), uj::f0(agg == "one", nw == 1, nw > 1))))
}

#' @rdname evals
#' @export
nors <- function(..., a = FALSE, na = FALSE, sweep = "across") {
  if (uj::isEQ(sweep, "across") | uj::isEQ(sweep, "both") | uj::isEQ(sweep, "within")) {uj::tests(..., a = a, na = na, agg = "nor", sweep = sweep)}
  else {stop(uj::format_errs(pkg = "uj", "[sweep] must be 'across', 'within', or 'both'."))}
}

#' @rdname evals
#' @export
anys <- function(..., a = FALSE, na = FALSE, sweep = "across") {
  if (uj::isEQ(sweep, "across") | uj::isEQ(sweep, "both") | uj::isEQ(sweep, "within")) {uj::tests(..., a = a, na = na, agg = "any", sweep = sweep)}
  else {stop(uj::format_errs(pkg = "uj", "[sweep] must be 'across', 'within', or 'both'."))}
}

#' @rdname evals
#' @export
alls <- function(..., a = FALSE, na = FALSE, sweep = "across") {
  if (uj::isEQ(sweep, "across") | uj::isEQ(sweep, "both") | uj::isEQ(sweep, "within")) {uj::tests(..., a = a, na = na, agg = "all", sweep = sweep)}
  else {stop(uj::format_errs(pkg = "uj", "[sweep] must be 'across', 'within', or 'both'."))}
}

#' @rdname evals
#' @export
ones <- function(..., a = FALSE, na = FALSE, sweep = "across") {
  if (uj::isEQ(sweep, "across") | uj::isEQ(sweep, "both") | uj::isEQ(sweep, "within")) {uj::tests(..., a = a, na = na, agg = "one", sweep = sweep)}
  else {stop(uj::format_errs(pkg = "uj", "[sweep] must be 'across', 'within', or 'both'."))}
}

#' @rdname evals
#' @export
twos <- function(..., a = FALSE, na = FALSE, sweep = "across") {
  if (uj::isEQ(sweep, "across") | uj::isEQ(sweep, "both") | uj::isEQ(sweep, "within")) {uj::tests(..., a = a, na = na, agg = "two", sweep = sweep)}
  else {stop(uj::format_errs(pkg = "uj", "[sweep] must be 'across', 'within', or 'both'."))}
}

#' @rdname evals
#' @export
isin <- function(x, y, na = FALSE, agg = NA, not = FALSE) {
  if (base::is.null(x) | base::is.null(y)) {return(F)}
  ok.x <- uj::pop_vec(x) & uj::atm_vec(x)
  ok.y <- uj::pop_vec(y) & uj::atm_vec(y)
  ok.xy <- uj::f0(!ok.x | !ok.y, T, uj::compatible(x, y))
  ok.agg <- uj::f0(uj::isNAS(agg), T, agg %in% c("nor", "one", "any", "two", "all"))
  errs <- base::c(uj::f0(ok.x         , NULL, "[x] must be a populated atomic vec (?pop_vec)."),
                  uj::f0(ok.y         , NULL, "[y] must be a populated atomic vec (?pop_vec)."),
                  uj::f0(uj::isTF(na) , NULL, "[na] must be TRUE or FALSE."),
                  uj::f0(ok.agg       , NULL, "[agg] must be NA, 'nor', 'one', 'any', 'two', or 'all'."),
                  uj::f0(uj::isTF(not), NULL, "[not] must be TRUE or FALSE."),
                  uj::f0(ok.xy        , NULL, "[x] and [y] are not of compatible modes."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  out <- x %in% y
  out[x[base::is.na(x)]] <- na
  n <- base::length(base::which(out))
  uj::f0(agg == "nor", !base::any(out), uj::f0(agg == "any", base::any(out), uj::f0(agg == "all", base::all(out), uj::f0(agg == "one", n == 1, uj::f0(agg == "two", n >= 2, out)))))
}

#' @rdname evals
#' @export
is_in <- isin

#' @rdname evals
#' @export
isout <- function(x, y, na = FALSE, agg = NA) {!uj::isin(x, y, na = na, agg = agg, not = T)}

#' @rdname evals
#' @export
is_out <- isout

#' @rdname evals
#' @export
not_in <- isout

#' @rdname evals
#' @export
notin <- isout

#' @rdname evals
#' @export
has <- function(x, y, na = TRUE, agg = NA) {uj::isin(y, x, na = na, agg = agg, not = F)}

#' @rdname evals
#' @export
lacks <- function(x, y, na = TRUE, agg = NA) {!uj::isin(y, x, na = na, agg = agg, not = T)}
