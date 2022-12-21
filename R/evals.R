.conform <- function(...) {
  x <- list(...)
  ns <- lengths(x)
  if (all(sapply(x, atm_vec))) {all(ns %in% c(1, max(ns)))}
  else if (all(sapply(x, is.atomic))) {length(unique(sapply(lapply(x, dim), paste0, collapse = "."))) == 1}
}

#' @name evals
#' @encoding UTF-8
#' @family extensions
#' @family logicals
#' @title Enhancements of `base` logical functions
#' @description The following are extensions of base logical functions:
#' \tabular{rl}{
#'        **Function usage** \tab   **Extends**
#'   \cr `not_in(x, y, ...)` \tab   `!(x %in% y)`
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
  errs <- c(f0(pop_lgl(x)                , NULL, "[x] must be a populated logical object (?cmp_lgl)."),
            f0(isLG(na) | isEQ(na, 'err'), NULL, "[na] must be TRUE, FALSE, NA, or 'err'."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (isTF(na)) {x[is.na(x)] <- na}
  !x
}

#' @rdname evals
#' @export
and <- function(x, y, na = 'err') {
  errs <- c(f0(pop_lgl(x)                , NULL, "[x] must be a populated logical object (?cmp_lgl)."),
            f0(pop_lgl(y)                , NULL, "[x] must be a populated logical object (?cmp_lgl)."),
            f0(.conform(x, y)            , NULL, "[x] and y must be conformable."),
            f0(isLG(na) | isEQ(na, 'err'), NULL, "[na] must be TRUE, FALSE, NA, or 'err'."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (isTF(na)) {x[is.na(x)] <- na; y[is.na(x)] <- na}
  x & y
}

#' @rdname evals
#' @export
or <- function(x, y, na = 'err') {
  errs <- c(f0(pop_lgl(x)                , NULL, "[x] must be a populated logical object (?cmp_lgl)."),
            f0(pop_lgl(y)                , NULL, "[x] must be a populated logical object (?cmp_lgl)."),
            f0(.conform(x, y)            , NULL, "[x] and y must be conformable."),
            f0(isLG(na) | isEQ(na, 'err'), NULL, "[na] must be TRUE, FALSE, NA, or 'err'."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (isTF(na)) {x[is.na(x)] <- na; y[is.na(x)] <- na}
  x | y
}

#' @rdname evals
#' @export
nor <- function(x, y, na = 'err') {
  errs <- c(f0(pop_lgl(x)                , NULL, "[x] must be a populated logical object (?cmp_lgl)."),
            f0(pop_lgl(y)                , NULL, "[x] must be a populated logical object (?cmp_lgl)."),
            f0(.conform(x, y)            , NULL, "[x] and y must be conformable."),
            f0(isLG(na) | isEQ(na, 'err'), NULL, "[na] must be TRUE, FALSE, NA, or 'err'."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (isTF(na)) {x[is.na(x)] <- na; y[is.na(x)] <- na}
  !(x | y)
}

#' @rdname evals
#' @export
one <- function(x, y, na = 'err') {
  errs <- c(f0(pop_lgl(x)                , NULL, "[x] must be a populated logical object (?cmp_lgl)."),
            f0(pop_lgl(y)                , NULL, "[x] must be a populated logical object (?cmp_lgl)."),
            f0(.conform(x, y)            , NULL, "[x] and [y] must be conformable."),
            f0(isLG(na) | isEQ(na, 'err'), NULL, "[na] must be TRUE, FALSE, NA, or 'err'."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (isTF(na)) {x[is.na(x)] <- na; y[is.na(x)] <- na}
  xor(x, y)
}

#' @rdname evals
#' @export
TEST <- function(x, na = FALSE, err = NA) {
  errs <- c(f0(isTF(na)                    , NULL, "[na] must be TRUE or FALSE."),
            f0(isLG(err) | isEQ(err, 'err'), NULL, "[err] must be TRUE, FALSE, NA, or 'err'."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (!lgl_scl(x) & !isLG(err)) {stop(.errs("[x] is not a logical scalar."))} else if (!lgl_scl(x)) {err} else if (is.na(x)) {na} else {x}
}

#' @rdname evals
#' @export
ANY <- function(..., err = NA) {
  errs <- c(f0(all(sapply(list(...), lgl_scl)), NULL, "All arguments in [...] must be logical scalars (?lgl_scl)."),
            f0(isLG(err) | isEQ(err, 'na')    , NULL, "[err] must be TRUE, FALSE, NA, or 'na'."))
  if (!is.null(errs)) {stop(.errs(errs))}
  for (i in 1:...length()) {
    x <- tryCatch(...elt(i) == T, error = function(e) NA, finally = NULL)
    vx <- cmp_lgl_scl(x)
    if (!vx) {
      if (isTF(err)) {x <- err}
      else {stop(.errs(paste0("[..", i, "] did not resolve to TRUE or FALSE.")))}
    }
    if (x) {return(T)}
  }
  F
}

#' @rdname evals
#' @export
ALL <- function(..., err = NA) {
  errs <- c(f0(all(sapply(list(...), lgl_scl)), NULL, "All arguments in [...] must be logical scalars (?lgl_scl)."),
            f0(isLG(err) | isEQ(err, 'na')    , NULL, "[err] must be TRUE, FALSE, NA, or 'na'."))
  if (!is.null(errs)) {stop(.errs(errs))}
  for (i in 1:...length()) {
    x <- tryCatch(...elt(i) == T, error = function(e) NA, finally = NULL)
    vx <- cmp_lgl_scl(x)
    if (!vx) {if (isTF(err)) {x <- err} else {stop(.errs(paste0("[..", i, "] did not resolve to TRUE or FALSE.")))}}
    if (!x) {return(F)}
  }
  T
}

#' @rdname evals
#' @export
NOR <- function(..., err = NA) {
  errs <- c(f0(all(sapply(list(...), lgl_scl)), NULL, "All arguments in [...] must be logical scalars (?lgl_scl)."),
            f0(isLG(err) | isEQ(err, 'na')    , NULL, "[err] must be TRUE, FALSE, NA, or 'na'."))
  if (!is.null(errs)) {stop(.errs(errs))}
  for (i in 1:...length()) {
    x <- tryCatch(...elt(i) == T, error = function(e) NA, finally = NULL)
    vx <- cmp_lgl_scl(x)
    if (!vx) {
      if (isTF(err)) {x <- err}
      else {stop(.errs(paste0("[..", i, "] did not resolve to TRUE or FALSE.")))}
    }
    if (x) {return(F)}
  }
  T
}

#' @rdname evals
#' @export
ONE <- function(..., err = NA) {
  errs <- c(f0(all(sapply(list(...), lgl_scl)), NULL, "All arguments in [...] must be logical scalars (?lgl_scl)."),
            f0(isLG(err) | isEQ(err, 'na')    , NULL, "[err] must be TRUE, FALSE, NA, or 'na'."))
  if (!is.null(errs)) {stop(.errs(errs))}
  n <- 0
  for (i in 1:...length()) {
    x <- tryCatch(...elt(i) == T, error = function(e) NA, finally = NULL)
    vx <- cmp_lgl_scl(x)
    if (!vx) {
      if (isTF(err)) {x <- err}
      else {stop(.errs(paste0("[..", i, "] did not resolve to TRUE or FALSE.")))}
    }
    if (x) {n <- n + 1}
    if (n > 1) {return(F)}
  }
  (n == 1)
}

#' @rdname evals
#' @export
TWO <- function(..., err = NA) {
  errs <- c(f0(all(sapply(list(...), lgl_scl)), NULL, "All arguments in [...] must be logical scalar (?lgl_scl)."),
            f0(isLG(err) | isEQ(err, 'na')    , NULL, "[err] must be TRUE, FALSE, NA, or 'na'."))
  if (!is.null(errs)) {stop(.errs(errs))}
  n <- 0
  for (i in 1:...length()) {
    x <- tryCatch(...elt(i) == T, error = function(e.) NA, finally = NULL)
    vx <- cmp_lgl_scl(x)
    if (!vx) {
      if (isTF(err)) {x <- err}
      else {stop(.errs(paste0("[..", i, "] did not resolve to TRUE or FALSE.")))}
    }
    if (x) {n <- n + 1}
    if (n > 1) {return(T)}
  }
  F
}

#' @rdname evals
#' @export
tests <- function(..., a = FALSE, na = FALSE, not = FALSE, agg = NA, sweep = NA) {
  x <- f0(isT(a), list(av(...)), list(...))                                      # if atomizing is requested, store the atomized values in a 1 element list, otherwise, extract ... as a list
  n <- length(x)                                                                 # number of args in {...}
  ns <- f0(n == 0, NULL, lengths(x))                                             # lengths of args in {...}
  ok.s <- f0(isNAS(sweep), T, sweep %in% c("across", "within", "both"))
  ok.a <- f0(isNAS(agg), T, agg %in% c("nor", "one", "any", "two", "all"))
  ok.sa <- (isNAS(agg) & isNAS(sweep)) | (isOKS(agg) & isOKS(sweep))
  ok.r <- f0(n < 2, T, all(round(max(ns) / ns) == max(ns) / ns))                 # whether arguments are recyclable
  errs <- c(f0(isTF(a)  , NULL, "[a] must be TRUE or FALSE."),
            f0(isTF(na) , NULL, "[na] must be TRUE or FALSE."),
            f0(isTF(not), NULL, "[not] must be TRUE or FALSE."),
            f0(ok.r     , NULL, "[...] arguments are not recyclable (?recyclable)."),
            f0(ok.s     , NULL, "[sweep] must be NA, 'across', 'within', or 'both'."),
            f0(ok.a     , NULL, "[agg] must be NA, 'nor', 'one', 'any', 'two', 'all'."),
            f0(ok.sa    , NULL, "[agg] and [sweep] must both be NA or must both be non-NA."))
  if (!is.null(errs)) {stop(.errs(errs))}
  for (i in 1:length(x)) {
    arg <- x[[i]]
    arg[is.na(arg)] <- na
    if (not) {arg <- !arg}
    x[[i]] <- arg
  }
  if (isNAS(sweep) & isNAS(agg)) {return(x)}
  maxn <- max(lengths(x))
  reps <- maxn / lengths(x)
  if (sweep != "both") {
    args <- matrix(NA, nrow = maxn, ncol = length(x))
    for (i in 1:length(x)) {args[, i] <- rep.int(x[[i]], reps[i])}
    x <- args
    nw <- f0(sweep == "within", colSums(1 * x), rowSums(1 * x))
  } else {nw <- sum(1 * av(x))}
  f0(agg == "nor", nw == 0, f0(agg == "any", nw >= 1, f0(agg == "all", nw == length(x), f0(agg == "one", nw == 1, nw > 1))))
}

#' @rdname evals
#' @export
nors <- function(..., a = FALSE, na = FALSE, sweep = "across") {
  if (isEQ(sweep, "across") | isEQ(sweep, "both") | isEQ(sweep, "within")) {tests(..., a = a, na = na, agg = "nor", sweep = sweep)}
  else {stop(.errs("[sweep] must be 'across', 'within', or 'both'."))}
}

#' @rdname evals
#' @export
anys <- function(..., a = FALSE, na = FALSE, sweep = "across") {
  if (isEQ(sweep, "across") | isEQ(sweep, "both") | isEQ(sweep, "within")) {tests(..., a = a, na = na, agg = "any", sweep = sweep)}
  else {stop(.errs("[sweep] must be 'across', 'within', or 'both'."))}
}

#' @rdname evals
#' @export
alls <- function(..., a = FALSE, na = FALSE, sweep = "across") {
  if (isEQ(sweep, "across") | isEQ(sweep, "both") | isEQ(sweep, "within")) {tests(..., a = a, na = na, agg = "all", sweep = sweep)}
  else {stop(.errs("[sweep] must be 'across', 'within', or 'both'."))}
}

#' @rdname evals
#' @export
ones <- function(..., a = FALSE, na = FALSE, sweep = "across") {
  if (isEQ(sweep, "across") | isEQ(sweep, "both") | isEQ(sweep, "within")) {tests(..., a = a, na = na, agg = "one", sweep = sweep)}
  else {stop(.errs("[sweep] must be 'across', 'within', or 'both'."))}
}

#' @rdname evals
#' @export
twos <- function(..., a = FALSE, na = FALSE, sweep = "across") {
  if (isEQ(sweep, "across") | isEQ(sweep, "both") | isEQ(sweep, "within")) {tests(..., a = a, na = na, agg = "two", sweep = sweep)}
  else {stop(.errs("[sweep] must be 'across', 'within', or 'both'."))}
}

#' @rdname evals
#' @export
isin <- function(x, y, na = FALSE, agg = NA, not = FALSE) {
  ok.x <- pop_vec(x) & atm_vec(x)
  ok.y <- pop_vec(y) & atm_vec(y)
  ok.xy <- f0(!ok.x | !ok.y, T, compatible(x, y))
  ok.agg <- f0(isNAS(agg), T, agg %in% c("nor", "one", "any", "two", "all"))
  errs <- c(f0(ok.x     , NULL, "[x] must be a populated atomic vec (?pop_vec)."),
            f0(ok.y     , NULL, "[y] must be a populated atomic vec (?pop_vec)."),
            f0(isTF(na) , NULL, "[na] must be TRUE or FALSE."),
            f0(ok.agg   , NULL, "[agg] must be NA, 'nor', 'one', 'any', 'two', or 'all'."),
            f0(isTF(not), NULL, "[not] must be TRUE or FALSE."),
            f0(ok.xy    , NULL, "[x] and [y] are not of compatible modes."))
  if (!is.null(errs)) {stop(.errs(errs))}
  out <- x %in% y
  out[x[is.na(x)]] <- na
  n <- length(which(out))
  f0(agg == "nor", !any(out), f0(agg == "any", any(out), f0(agg == "all", all(out), f0(agg == "one", n == 1, f0(agg == "two", n >= 2, out)))))
}

#' @rdname evals
#' @export
is_in <- isin

#' @rdname evals
#' @export
isout <- function(x, y, na = FALSE, agg = NA) {!isin(x, y, na = na, agg = agg, not = T)}

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
has <- function(x, y, na = TRUE, agg = NA) {isin(y, x, na = na, agg = agg, not = F)}

#' @rdname evals
#' @export
lacks <- function(x, y, na = TRUE, agg = NA) {!isin(y, x, na = na, agg = agg, not = T)}
