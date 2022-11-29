.conform <- function(...) {
  x <- list(...)
  ns <- lengths(x)
  if (all(sapply(x, atm_vec))) {all(ns %in% c(1, max(ns)))}
  else if (all(sapply(x, is.atomic))) {length(unique(sapply(lapply(x, dim), paste0, collapse = "."))) == 1}
}

#' @name uj_logicals
#' @family extensions
#' @family logicals
#' @title Enhancements of `base` Logical Functions
#' @description \itemize{
#'   \item **`TEST(x, ...)`**: is an extended analog of `isTRUE(x)`.
#'   \item **`is_in(x, y, ...)`**: is an extended analog of `x %in% y`.
#'   \item **`not_in(x, y, ...)`**: is an extended analog of `!(x %in% y)`.
#'   \item **`has(x, y, ...)`**: is an extended analog of `y %in% x`.
#'   \item **`lacks(x, y, ...)`**: is an extended analog of `!(y %in% x)`.
#'   \item **`not(x, ...)`**: is an extended analog of `!x`.
#'   \item **`and(x, y, ...)`**: is an extended analog of `x & y`.
#'   \item **`or(x, y, ...)`**: is an extended analog of `x | y`.
#'   \item **`nor(x, y, ...)`**: is an extended analog of `!(x | y)`.
#'   \item **`one(x, y, ...)`**: is an extended analog of `xor(x, y)`.
#'   \item **`ANY(.)`**: evaluates whether any `...` argument is `TRUE`.
#'   \item **`ALL(.)`**: evaluates whether all `...` arguments are `TRUE`.
#'   \item **`NOR(.)`**: evaluates whether `0` `...` arguments are `TRUE`.
#'   \item **`ONE(.)`**: evaluates whether exactly `1` `...` argument is `TRUE`.
#'   \item **`TWO(.)`**: evaluates whether `2+` `...` arguments are `TRUE`.
#'   \item **`tests(.)`**: logically indexes `TRUE` values in each sweep.
#'   \item **`w(.)`**: numerically indexes `TRUE` values in each sweep.
#'   \item **`nors(.)`**: evaluates each sweep for 0 `TRUE` values.
#'   \item **`anys(.)`**: evaluates each sweep for any `TRUE` values.
#'   \item **`alls(.)`**: evaluates each sweep for only `TRUE` values.
#'   \item **`ones(.)`**: evaluates each sweep for exactly `1` `TRUE` value.
#'   \item **`twos(.)`**: evaluates each sweep for `2+` `TRUE` values.
#' }
#' @param x An \link[=atm_lgl]{atomic logical object} for all functions other than `is_in`, `not_in`, `has`, and `lacks`. Otherwise, an atomic object.
#' @param y An atomic logical object. for all functions other than `is_in`, `not_in`, `has`, and `lacks`. Otherwise, an atomic object \link[=compatible]{compatible} with `x`.
#' @param na A non-`NA` logical scalar indicating what value should replace `NA` values.
#' @param err A non-`NA` logical scalar. `err = TRUE` indicates `TRUE` should be substituted for non-logical values, `err = FALSE` indicates `FALSE` should be substituted for non-logical values, `err = NA` indicates an error should be thrown if a non-logical value is encountered.
#' @param ... An arbitrary number of \link[=lgl_vec]{logical vecs} to be processed.
#' @param agg A \link[=cmp_chr_scl]{complete character scalar} in `c('nor', 'one', 'any', 'two', 'all')`**: used to specify, respectively, that 0, 1, any, 2 or more, and all arguments must be `TRUE`.
#' @param a A non-`NA` logical scalar indicating whether to atomize `...` before processing. This creates a single atomic vector of all atomic elements contained in all `...` arguments and effectively changes the behavior of `or` to `any`, `and` to `all`, and `not` to `!any`.
#' @param not A non-`NA` logical scalar indicating whether to negate values in arguments supplied in `...` before processing.
#' @param across,within \link[=cmp_chr_scl]{Character scalars}. `NA` indicates not summarizing across or within `...` arguments. Values in `c('nor', 'one', 'any', 'two', 'all')`**: indicate zero, exactly one, any, two or more, or all values are `TRUE` when looking across corresponding elements of `...` arguments vs. within each `...` argument.
#' @export
not <- function(x, na = 'err') {
  errs <- c(f0(pop_lgl(x)                , NULL, "\n \u2022 [x] must be a populated logical object (?cmp_lgl)."),
            f0(isLG(na) | isEQ(na, 'err'), NULL, "\n \u2022 [na] must be TRUE, FALSE, NA, or 'err'."))
  if (!is.null(errs)) {stop(errs)}
  if (isTF(na)) {x[is.na(x)] <- na}
  !x
}

#' @rdname uj_logicals
#' @export
and <- function(x, y, na = 'err') {
  errs <- c(f0(pop_lgl(x)                , NULL, "\n \u2022 [x] must be a populated logical object (?cmp_lgl)."),
            f0(pop_lgl(y)                , NULL, "\n \u2022 [x] must be a populated logical object (?cmp_lgl)."),
            f0(.conform(x, y)            , NULL, "\n \u2022 [x] and y must be conformable."),
            f0(isLG(na) | isEQ(na, 'err'), NULL, "\n \u2022 [na] must be TRUE, FALSE, NA, or 'err'."))
  if (!is.null(errs)) {stop(errs)}
  if (isTF(na)) {x[is.na(x)] <- na; y[is.na(x)] <- na}
  x & y
}

#' @rdname uj_logicals
#' @export
or <- function(x, y, na = 'err') {
  errs <- c(f0(pop_lgl(x)                , NULL, "\n \u2022 [x] must be a populated logical object (?cmp_lgl)."),
            f0(pop_lgl(y)                , NULL, "\n \u2022 [x] must be a populated logical object (?cmp_lgl)."),
            f0(.conform(x, y)            , NULL, "\n \u2022 [x] and y must be conformable."),
            f0(isLG(na) | isEQ(na, 'err'), NULL, "\n \u2022 [na] must be TRUE, FALSE, NA, or 'err'."))
  if (!is.null(errs)) {stop(errs)}
  if (isTF(na)) {x[is.na(x)] <- na; y[is.na(x)] <- na}
  x | y
}

#' @rdname uj_logicals
#' @export
nor <- function(x, y, na = 'err') {
  errs <- c(f0(pop_lgl(x)                , NULL, "\n \u2022 [x] must be a populated logical object (?cmp_lgl)."),
            f0(pop_lgl(y)                , NULL, "\n \u2022 [x] must be a populated logical object (?cmp_lgl)."),
            f0(.conform(x, y)            , NULL, "\n \u2022 [x] and y must be conformable."),
            f0(isLG(na) | isEQ(na, 'err'), NULL, "\n \u2022 [na] must be TRUE, FALSE, NA, or 'err'."))
  if (!is.null(errs)) {stop(errs)}
  if (isTF(na)) {x[is.na(x)] <- na; y[is.na(x)] <- na}
  !(x | y)
}

#' @rdname uj_logicals
#' @export
one <- function(x, y, na = 'err') {
  errs <- c(f0(pop_lgl(x)                , NULL, "\n \u2022 [x] must be a populated logical object (?cmp_lgl)."),
            f0(pop_lgl(y)                , NULL, "\n \u2022 [x] must be a populated logical object (?cmp_lgl)."),
            f0(.conform(x, y)            , NULL, "\n \u2022 [x] and y must be conformable."),
            f0(isLG(na) | isEQ(na, 'err'), NULL, "\n \u2022 [na] must be TRUE, FALSE, NA, or 'err'."))
  if (!is.null(errs)) {stop(errs)}
  if (isTF(na)) {x[is.na(x)] <- na; y[is.na(x)] <- na}
  xor(x, y)
}

#' @rdname uj_logicals
#' @export
TEST <- function(x, na = FALSE, err = NA) {
  errs <- c(f0(isTF(na)                    , NULL, "\n \u2022 [na] must be TRUE or FALSE."),
            f0(isLG(err) | isEQ(err, 'err'), NULL, "\n \u2022 [err] must be TRUE, FALSE, NA, or 'err'."))
  if (!is.null(errs)) {stop(errs)}
  if (!lgl_scl(x) & !isLG(err)) {stop("\n \u2022 [x] is not a logical scalar.")} else if (!lgl_scl(x)) {err} else if (is.na(x)) {na} else {x}
}

#' @rdname uj_logicals
#' @export
ANY <- function(..., err = NA) {
  errs <- c(f0(all(sapply(list(...), pop_lgl)), NULL, "\n \u2022 All arguments in [...] must be complete logical objects (?cmp_lgl)."),
            f0(.conform(...)                  , NULL, "\n \u2022 All arguments in [...] must be conformable."),
            f0(isLG(err) | isEQ(err, 'na')    , NULL, "\n \u2022 [err] must be TRUE, FALSE, NA, or 'na'."))
  if (!is.null(errs)) {stop(errs)}
  for (i in 1:...length()) {
    x <- tryCatch(...elt(i) == T, error = function(e) NA, finally = NULL)
    vx <- cmp_lgl_scl(x)
    if (!vx) {
      if (isTF(err)) {x <- err}
      else {stop("\n \u2022 [..", i, "] did not resolve to TRUE or FALSE.")}
    }
    if (x) {return(T)}
  }
  F
}

#' @rdname uj_logicals
#' @export
ALL <- function(..., err = NA) {
  errs <- c(f0(all(sapply(list(...), pop_lgl)), NULL, "\n \u2022 All arguments in [...] must be complete logical objects (?cmp_lgl)."),
            f0(.conform(...)              , NULL, "\n \u2022 All arguments in [...] must be conformable."),
            f0(isLG(err) | isEQ(err, 'na'), NULL, "\n \u2022 [err] must be TRUE, FALSE, NA, or 'na'."))
  if (!is.null(errs)) {stop(errs)}
  for (i in 1:...length()) {
    x <- tryCatch(...elt(i) == T, error = function(e) NA, finally = NULL)
    vx <- cmp_lgl_scl(x)
    if (!vx) {if (isTF(err)) {x <- err} else {stop("\n \u2022 [..", i, "] did not resolve to TRUE or FALSE.")}}
    if (!x) {return(F)}
  }
  T
}

#' @rdname uj_logicals
#' @export
NOR <- function(..., err = NA) {
  errs <- c(f0(all(sapply(list(...), pop_lgl)), NULL, "\n \u2022 All arguments in [...] must be complete logical objects (?cmp_lgl)."),
            f0(.conform(...)              , NULL, "\n \u2022 All arguments in [...] must be conformable."),
            f0(isLG(err) | isEQ(err, 'na'), NULL, "\n \u2022 [err] must be TRUE, FALSE, NA, or 'na'."))
  if (!is.null(errs)) {stop(errs)}
  for (i in 1:...length()) {
    x <- tryCatch(...elt(i) == T, error = function(e) NA, finally = NULL)
    vx <- cmp_lgl_scl(x)
    if (!vx) {
      if (isTF(err)) {x <- err}
      else {stop("\n \u2022 [..", i, "] did not resolve to TRUE or FALSE.")}
    }
    if (x) {return(F)}
  }
  T
}

#' @rdname uj_logicals
#' @export
ONE <- function(..., err = NA) {
  errs <- c(f0(all(sapply(list(...), pop_lgl)), NULL, "\n \u2022 All arguments in [...] must be complete logical objects (?cmp_lgl)."),
            f0(.conform(...)              , NULL, "\n \u2022 All arguments in [...] must be conformable."),
            f0(isLG(err) | isEQ(err, 'na'), NULL, "\n \u2022 [err] must be TRUE, FALSE, NA, or 'na'."))
  if (!is.null(errs)) {stop(errs)}
  n <- 0
  for (i in 1:...length()) {
    x <- tryCatch(...elt(i) == T, error = function(e) NA, finally = NULL)
    vx <- cmp_lgl_scl(x)
    if (!vx) {
      if (isTF(err)) {x <- err}
      else {stop("\n \u2022 [..", i, "] did not resolve to TRUE or FALSE.")}
    }
    if (x) {n <- n + 1}
    if (n > 1) {return(F)}
  }
  (n == 1)
}

#' @rdname uj_logicals
#' @export
TWO <- function(..., err = NA) {
  errs <- c(f0(all(sapply(list(...), pop_lgl)), NULL, "\n \u2022 All arguments in [...] must be complete logical objects (?cmp_lgl)."),
            f0(.conform(...)                  , NULL, "\n \u2022 All arguments in [...] must be conformable."),
            f0(isLG(err) | isEQ(err, 'na')    , NULL, "\n \u2022 [err] must be TRUE, FALSE, NA, or 'na'."))
  if (!is.null(errs)) {stop(errs)}
  n <- 0
  for (i in 1:...length()) {
    x <- tryCatch(...elt(i) == T, error = function(e.) NA, finally = NULL)
    vx <- cmp_lgl_scl(x)
    if (!vx) {
      if (isTF(err)) {x <- err}
      else {stop("\n \u2022 [..", i, "] did not resolve to TRUE or FALSE.")}
    }
    if (x) {n <- n + 1}
    if (n > 1) {return(T)}
  }
  F
}

#' @rdname uj_logicals
#' @export
tests <- function(..., na = F, a = F, not = F, across = NA, within = NA) {
  av <- av(...)                                                                  # atomize arguments in ...
  x <- f0(isT(a), list(av), list(...))                                           # if atomizing is requested, store the atomized values in a 1 element list, otherwise, extract ... as a list
  n <- length(x)                                                                 # number of args in {...}
  ns <- f0(n == 0, NULL, lengths(x))                                             # lengths of args in {...}
  aw <- list(c(NA, 'nor', 'one', 'any', 'two', 'all'))                           # possible values of {across} and {within}
  ok.reps <- f0(n < 2, T, all(round(max(ns) / ns) == max(ns) / ns))              # whether arguments are recyclable
  errs <- c(f0(isTF(na)        , NULL, "\n \u2022 [na] must be TRUE or FALSE."),
            f0(isTF(a)         , NULL, "\n \u2022 [a] must be TRUE or FALSE."),
            f0(isTF(not)       , NULL, "\n \u2022 [not] must be TRUE or FALSE."),
            f0(isIN(across, aw), NULL, "\n \u2022 [across] must be NA, 'nor', 'one', 'any', 'two', 'all'."),
            f0(isIN(within, aw), NULL, "\n \u2022 [within] must be NA, 'nor', 'one', 'any', 'two', 'all'."),
            f0(ok.reps         , NULL, "\n \u2022 Arguments in [...] are not recyclable (?recyclable)."))
  if (!is.null(errs)) {stop(errs)}
  for (i in 1:n) {                                                               # for each argument
    x[[i]][is.na(x[[i]])] <- na                                                  # > replace any NA values with [na]
    if (not) {x[[i]] <- !x[[i]]}                                                 # > and negative if specified in [not]
  }
  if (isOk(within)) {                                                            # if within-argument aggregation is specified
    for (i in 1:length(x)) {                                                     # > for each argument
      xi <- x[[i]]                                                               # >> extract it
      nx <- length(xi)                                                           # >> get its length
      nw <- length(which(xi))                                                    # >> and the number of TRUE values
      x[[i]] <- f0(within == "nor", nw == 0, f0(within == "any", nw >= 1, f0(within == "all", nw == nx, f0(within == "one", nw == 1, nw > 1))))
    }}
  if (isOk(across)) {                                                            # if across-argument aggregation is specified
    nx <- length(x)                                                              # > get the number arguments
    out <- 0                                                                     # > initialize the result counter vector
    for (i in 1:nx) {out <- out + 1 * x[[i]]}                                    # > for each argument, add its TRUE results to the counter vector
    x <- as.vector(x)                                                            # > convert the resulting list to a vector
    nx <- length(x)                                                              # > get its length
    nw <- length(which(x))                                                       # > and its number of TRUE values
    x <- f0(across == "nor", nw == 0, f0(across == "any", nw >= 1, f0(across == "all", nw == nx, f0(across == "one", nw == 1, nw > 1))))
  }
  if (isOk(across) | isOk(within)) {as.vector(x)} else {x}                       # return the result
}

#' @rdname uj_logicals
#' @export
w <- function(..., not = F, na = F, a = F) {tests(..., not = not, na = na, a = a)}

#' @rdname uj_logicals
#' @export
nors <- function(..., na = F, a = F, across = T, within = F) {
  errs <- c(f0(isTF(across), NULL, "\n \u2022 [across] must be TRUE or FALSE."),
            f0(isTF(within), NULL, "\n \u2022 [within] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(errs)}
  if (isT(across)) {across <- "not"} else {across <- NA}
  if (isT(within)) {within <- "not"} else {within <- NA}
  tests(..., na = na, a = a, across = across, within = within)
}

#' @rdname uj_logicals
#' @export
anys <- function(..., na = F, a = F, across = T, within = F) {
  errs <- c(f0(isTF(across), NULL, "\n \u2022 [across] must be TRUE or FALSE."),
            f0(isTF(within), NULL, "\n \u2022 [within] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(errs)}
  if (isT(across)) {across <- "any"} else {across <- NA}
  if (isT(within)) {within <- "any"} else {within <- NA}
  tests(..., na = na, a = a, across = across, within = within)
}

#' @rdname uj_logicals
#' @export
alls <- function(..., na = F, a = F, across = T, within = F) {
  errs <- c(f0(isTF(across), NULL, "\n \u2022 [across] must be TRUE or FALSE."),
            f0(isTF(within), NULL, "\n \u2022 [within] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(errs)}
  if (isT(across)) {across <- "all"} else {across <- NA}
  if (isT(within)) {within <- "all"} else {within <- NA}
  tests(..., na = na, a = a, across = across, within = within)
}

#' @rdname uj_logicals
#' @export
ones <- function(..., na = F, a = F, across = T, within = F) {
  errs <- c(f0(isTF(across), NULL, "\n \u2022 [across] must be TRUE or FALSE."),
            f0(isTF(within), NULL, "\n \u2022 [within] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(errs)}
  if (isT(across)) {across <- "one"} else {across <- NA}
  if (isT(within)) {within <- "one"} else {within <- NA}
  tests(..., na = na, a = a, across = across, within = within)
}

#' @rdname uj_logicals
#' @export
twos <- function(..., na = F, a = F, across = T, within = F) {
  errs <- c(f0(isTF(across), NULL, "\n \u2022 [across] must be TRUE or FALSE."),
            f0(isTF(within), NULL, "\n \u2022 [within] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(errs)}
  if (isT(across)) {across <- "two"} else {across <- NA}
  if (isT(within)) {within <- "two"} else {within <- NA}
  tests(..., na = na, a = a, across = across, within = within)
}

#' @rdname uj_logicals
#' @export
is_in <- function(x, y, not = F, na = F, agg = NA) {
  types <- c(NA, 'nor', 'one', 'any', 'two', 'all')
  ok.x <- pop_vec(x) & atm_vec(x)
  ok.y <- pop_vec(y) & atm_vec(y)
  ok.xy <- f0(!ok.x | !ok.y, T, compatible(x, y))
  errs <- c(f0(ok.x            , NULL, "\n \u2022 [x] must be a populated atomic vec (?pop_vec)."),
            f0(ok.y            , NULL, "\n \u2022 [y] must be a populated atomic vec (?pop_vec)."),
            f0(isTF(not)       , NULL, "\n \u2022 [not] must be TRUE or FALSE."),
            f0(isTF(na)        , NULL, "\n \u2022 [na] must be TRUE or FALSE."),
            f0(isIN(agg, types), NULL, "\n \u2022 [agg] must be NA, 'nor', 'one', 'any', 'two', or 'all'."),
            f0(ok.xy           , NULL, "\n \u2022 [x] and [y] are not pf compatible modes."))
  if (!is.null(errs)) {stop(errs)}
  out <- x %in% y
  out[x[is.na(x)]] <- na
  n <- length(which(out))
  f0(agg == "nor", !any(out), f0(agg == "any", any(out), f0(agg == "all", all(out), f0(agg == "one", n == 1, f0(agg == "two", n >= 2, out)))))
}

#' @rdname uj_logicals
#' @export
not_in <- function(x, y, na = F, agg = NA) {!is_in(x, y, T, na, agg)}

#' @rdname uj_logicals
#' @export
has <- function(x, y, na = T, agg = NA) {is_in(y, x, F, na, agg)}

#' @rdname uj_logicals
#' @export
lacks <- function(x, y, na = T, agg = NA) {!is_in(y, x, T, na, agg)}
