.conform <- function(...) {
  x <- list(...)
  ns <- lengths(x)
  if (all(sapply(x, atm_vec))) {all(ns %in% c(1, max(ns)))}
  else if (all(sapply(x, is.atomic))) {length(unique(sapply(lapply(x, dim), paste0, collapse = "."))) == 1}
}

.cmp_lgl <- function(...) {all(sapply(list(...), cmp_lgl))}

#' @name logicals.
#' @family logicals
#' @family extensions
#' @family logicals
#' @title Extended, error-checked logicals
#' @param x \link[ilgl]{Atomic logical object} for all functions other than
#'   \code{is_in}, \code{not_in}, \code{has}, and \code{lacks}. Otherwise, an
#'   atomic object.
#' @param y \link[ilgl]{Atomic logical object} for all functions other than
#'   \code{is_in}, \code{not_in}, \code{has}, and \code{lacks}. Otherwise, an
#'   atomic object \link[compatible]{compatible} with \code{x}.
#' @param na \link[cmp_lgl_scl]{Complete logical scalar} indicating what value
#'   should replace \code{NA} values.
#' @param err \link[lgl_scl]{Logical scalar}. \code{err = TRUE}, indicates
#'   \code{TRUE} should be substituted for non-logical values, \code{err =
#'   FALSE} indicates \code{FALSE} should be substituted for non-logical values,
#'   \code{err = NA} indicates an error should be thrown if a non-logical value
#'   is encountered.
#' @param ... Arbitrary number of \link[lgl_vec]{logical vecs} to be processed.
#' @param agg \link[cmp_chr_scl]{Complete character scalar} in \code{c('nor',
#'   'one', 'any', 'two', 'all')} used to specify, respectively, that 0, 1, any,
#'   2 or more, and all arguments must be \code{TRUE}.
#' @param a \link[cmp_lgl_scl]{Complete logical scalar} indicating whether to
#'   atomize \code{...} before processing. This creates a single atomic vector
#'   of all atomic elements contained in all arguments in \code{...} and
#'   effectively changes the behavior of \code{or} to \code{any}, \code{and} to
#'   \code{all}, \code{not} to \code{!any}, and \code{some} and \code{most} to
#'   count the total number of \code{TRUE} values rather than the number of
#'   \code{TRUE} values in the same location of recycled arguments from
#'   \code{...} (but with error checking and flexible \code{NA} value handling.)
#' @param not \link[cmp_lgl_scl]{Complete logical scalar} indicating whether to
#'   negate values in arguments supplied in \code{...} before processing.
#' @param across,within \link[cmp_chr_scl]{Complete character scalar} in
#'   \code{c('', 'none', 'one', 'any', 'some', 'all')} indicating no across
#'   or within argument counting of \code{TRUE} values.
#' @export
logicals. <- function() {help("logicals.", package = "uj")}

#' @describeIn logicals. Wrapper for \code{!x} with error checking.
#' @export
not <- function(x, na = 'err') {
  errs <- c(f0(cmp_lgl(x), NULL, "\n \u2022 [x] must be a complete logical object (?cmp_lgl)."),
            f0(isLG(na) | isEQ(na, 'err'), NULL, "\n \u2022 [na] must be TRUE, FALSE, NA, or 'err'."))
  if (idef(errs)) {stop(errs)}
  if (isTF(na)) {x[is.na(x)] <- na}
  !x
}

#' @describeIn logicals. Wrapper for \code{x & y} with error checking.
#' @export
and <- function(x, y, na = 'err') {
  errs <- c(f0(cmp_lgl(x), NULL, "\n \u2022 [x] must be a complete logical object (?cmp_lgl)."),
            f0(cmp_lgl(y), NULL, "\n \u2022 [y] must be a complete logical object (?cmp_lgl)."),
            f0(.conform(x, y), NULL, "\n \u2022 [x] and y must be conformable."),
            f0(isLG(na) | isEQ(na, 'err'), NULL, "\n \u2022 [na] must be TRUE, FALSE, NA, or 'err'."))
  if (idef(errs)) {stop(errs)}
  if (isTF(na)) {x[is.na(x)] <- na; y[is.na(x)] <- na}
  x & y
}

#' @describeIn logicals. Wrapper for \code{x | y} with error checking.
#' @export
or <- function(x, y, na = 'err') {
  errs <- c(f0(cmp_lgl(x), NULL, "\n \u2022 [x] must be a complete logical object (?cmp_lgl)."),
            f0(cmp_lgl(y), NULL, "\n \u2022 [y] must be a complete logical object (?cmp_lgl)."),
            f0(.conform(x, y), NULL, "\n \u2022 [x] and y must be conformable."),
            f0(isLG(na) | isEQ(na, 'err'), NULL, "\n \u2022 [na] must be TRUE, FALSE, NA, or 'err'."))
  if (idef(errs)) {stop(errs)}
  if (isTF(na)) {x[is.na(x)] <- na; y[is.na(x)] <- na}
  x | y
}

#' @describeIn logicals. Returns \code{FALSE} as soon as a second \code{TRUE}
#'   argument is encountered. Otherwise, after evaluating all arguments
#'   evaluates whether the number of \code{TRUE} arguments is 1.
#' @export
one <- function(x, y, na = 'err') {
  errs <- c(f0(cmp_lgl(x), NULL, "\n \u2022 [x] must be a complete logical object (?cmp_lgl)."),
            f0(cmp_lgl(y), NULL, "\n \u2022 [y] must be a complete logical object (?cmp_lgl)."),
            f0(.conform(x, y), NULL, "\n \u2022 [x] and y must be conformable."),
            f0(isLG(na) | isEQ(na, 'err'), NULL, "\n \u2022 [na] must be TRUE, FALSE, NA, or 'err'."))
  if (idef(errs)) {stop(errs)}
  if (isTF(na)) {x[is.na(x)] <- na; y[is.na(x)] <- na}
  xor(x, y)
}

#' @describeIn logicals. Tests \code{x} and returns either \code{TRUE} or
#'   \code{FALSE}. If \code{x} is not a logical scalar, throw an error if
#'   \code{err = 'err'}; otherwise return the value of \code{err}. Returns the
#'   value of \code{na} if the value of \code{x} is \code{NA}.
#' @export
TEST <- function(x, na = FALSE, err = NA) {
  errs <- c(f0(isTF(na), NULL, "\n \u2022 [na] must be TRUE or FALSE."),
            f0(isLG(err) | isEQ(err, 'err'), NULL, "\n \u2022 [err] must be TRUE, FALSE, NA, or 'err'."))
  if (idef(errs)) {stop(errs)}
  if (!lgl_scl(x) & !isLG(err)) {stop("\n \u2022 [x] is not a logical scalar.")} else if (!lgl_scl(x)) {err} else if (is.na(x)) {na} else {x}
}

#' @describeIn logicals. Returns \code{TRUE} as soon as a \code{TRUE} argument
#'   is encountered. If none are encountered, return \code{FALSE}.
#' @export
ANY <- function(..., err = NA) {
  errs <- c(f0(.cmp_lgl(...), NULL, "\n \u2022 All arguments in [...] must be complete logical objects (?cmp_lgl)."),
            f0(.conform(...), NULL, "\n \u2022 All arguments in [...] must be conformable."),
            f0(isLG(err) | isEQ(err, 'na'), NULL, "\n \u2022 [err] must be TRUE, FALSE, NA, or 'na'."))
  if (idef(errs)) {stop(errs)}
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

#' @describeIn logicals. Returns \code{FALSE} as soon as a \code{FALSE}
#'   argument is encountered. If none are encountered, return \code{TRUE}.
#' @export
ALL <- function(..., err = NA) {
  errs <- c(f0(.cmp_lgl(...), NULL, "\n \u2022 All arguments in [...] must be complete logical objects (?cmp_lgl)."),
            f0(.conform(...), NULL, "\n \u2022 All arguments in [...] must be conformable."),
            f0(isLG(err) | isEQ(err, 'na'), NULL, "\n \u2022 [err] must be TRUE, FALSE, NA, or 'na'."))
  if (idef(errs)) {stop(errs)}
  for (i in 1:...length()) {
    x <- tryCatch(...elt(i) == T, error = function(e) NA, finally = NULL)
    vx <- cmp_lgl_scl(x)
    if (!vx) {if (isTF(err)) {x <- err} else {stop("\n \u2022 [..", i, "] did not resolve to TRUE or FALSE.")}}
    if (!x) {return(F)}
  }
  T
}

#' @describeIn logicals. Returns \code{FALSE} as soon as a \code{TRUE}
#'   argument is encountered. If none are encountered, return \code{TRUE}.
#' @export
NOR <- function(..., err = NA) {
  errs <- c(f0(.cmp_lgl(...), NULL, "\n \u2022 All arguments in [...] must be complete logical objects (?cmp_lgl)."),
            f0(.conform(...), NULL, "\n \u2022 All arguments in [...] must be conformable."),
            f0(isLG(err) | isEQ(err, 'na'), NULL, "\n \u2022 [err] must be TRUE, FALSE, NA, or 'na'."))
  if (idef(errs)) {stop(errs)}
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

#' @describeIn logicals. Generalized \code{xor} with error checking.
#' @export
ONE <- function(..., err = NA) {
  errs <- c(f0(.cmp_lgl(...), NULL, "\n \u2022 All arguments in [...] must be complete logical objects (?cmp_lgl)."),
            f0(.conform(...), NULL, "\n \u2022 All arguments in [...] must be conformable."),
            f0(isLG(err) | isEQ(err, 'na'), NULL, "\n \u2022 [err] must be TRUE, FALSE, NA, or 'na'."))
  if (idef(errs)) {stop(errs)}
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

#' @describeIn logicals. Returns \code{TRUE} as soon as a second \code{TRUE}
#'   argument is encountered. Otherwise, after evaluating all arguments, check
#'   whether the number of \code{TRUE} arguments is 1.
#' @export
TWO <- function(..., err = NA) {
  errs <- c(f0(.cmp_lgl(...), NULL, "\n \u2022 All arguments in [...] must be complete logical objects (?cmp_lgl)."),
            f0(.conform(...), NULL, "\n \u2022 All arguments in [...] must be conformable."),
            f0(isLG(err) | isEQ(err, 'na'), NULL, "\n \u2022 [err] must be TRUE, FALSE, NA, or 'na'."))
  if (idef(errs)) {stop(errs)}
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

#' @describeIn logicals. Tests an arbitrary number of arguments with a wide
#'   array of options
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

  if (idef(errs)) {stop(errs)}
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

#' @describeIn logicals. Indexes \code{TRUE} or \code{FALSE} values.
#' @export
w <- function(..., not = F, na = F, a = F) {tests(..., not = not, na = na, a = a)}

#' @describeIn logicals. Check for across and/or within argument counts of
#'   \code{TRUE} or \code{FALSE} values equal to zero.
#' @export
nors <- function(..., na = F, a = F, across = T, within = F) {
  errs <- c(f0(isTF(across), NULL, "\n \u2022 [across] must be TRUE or FALSE."),
            f0(isTF(within), NULL, "\n \u2022 [within] must be TRUE or FALSE."))
  if (idef(errs)) {stop(errs)}
  if (isT(across)) {across <- "not"} else {across <- NA}
  if (isT(within)) {within <- "not"} else {within <- NA}
  tests(..., na = na, a = a, across = across, within = within)
}

#' @describeIn logicals. Check for across and/or within argument counts of
#'   \code{TRUE} or \code{FALSE} values equal to one or more.
#' @export
anys <- function(..., na = F, a = F, across = T, within = F) {
  errs <- c(f0(isTF(across), NULL, "\n \u2022 [across] must be TRUE or FALSE."),
            f0(isTF(within), NULL, "\n \u2022 [within] must be TRUE or FALSE."))
  if (idef(errs)) {stop(errs)}
  if (isT(across)) {across <- "any"} else {across <- NA}
  if (isT(within)) {within <- "any"} else {within <- NA}
  tests(..., na = na, a = a, across = across, within = within)
}

#' @describeIn logicals. Check for across and/or within argument counts of
#'   \code{TRUE} or \code{FALSE} values equal to the number of elements.
#' @export
alls <- function(..., na = F, a = F, across = T, within = F) {
  errs <- c(f0(isTF(across), NULL, "\n \u2022 [across] must be TRUE or FALSE."),
            f0(isTF(within), NULL, "\n \u2022 [within] must be TRUE or FALSE."))
  if (idef(errs)) {stop(errs)}
  if (isT(across)) {across <- "all"} else {across <- NA}
  if (isT(within)) {within <- "all"} else {within <- NA}
  tests(..., na = na, a = a, across = across, within = within)
}

#' @describeIn logicals. Check for across and/or within argument counts of
#'   \code{TRUE} or \code{FALSE} values equal to one.
#' @export
ones <- function(..., na = F, a = F, across = T, within = F) {
  errs <- c(f0(isTF(across), NULL, "\n \u2022 [across] must be TRUE or FALSE."),
            f0(isTF(within), NULL, "\n \u2022 [within] must be TRUE or FALSE."))
  if (idef(errs)) {stop(errs)}
  if (isT(across)) {across <- "one"} else {across <- NA}
  if (isT(within)) {within <- "one"} else {within <- NA}
  tests(..., na = na, a = a, across = across, within = within)
}

#' @describeIn logicals. Check for across and/or within argument counts of
#'   \code{TRUE} or \code{FALSE} values equal to two ore more.
#' @export
twos <- function(..., na = F, a = F, across = T, within = F) {
  errs <- c(f0(isTF(across), NULL, "\n \u2022 [across] must be TRUE or FALSE."),
            f0(isTF(within), NULL, "\n \u2022 [within] must be TRUE or FALSE."))
  if (idef(errs)) {stop(errs)}
  if (isT(across)) {across <- "two"} else {across <- NA}
  if (isT(within)) {within <- "two"} else {within <- NA}
  tests(..., na = na, a = a, across = across, within = within)
}

#' @describeIn logicals. Whether elements of \code{x} are contained in
#'   \code{y}.
#' @export
is_in <- function(x, y, not = F, na = F, agg = NA) {
  types <- c(NA, 'nor', 'one', 'any', 'two', 'all')
  ok.x <- pop_vec(x) & atm_vec(x)
  ok.y <- pop_vec(y) & atm_vec(y)
  ok.xy <- f0(!ok.x | !ok.y, T, compatible(x, y))
  errs <- c(f0(ok.x, NULL, "\n \u2022 [x] must be a populated atomic vec (?pop_vec)."),
            f0(ok.y, NULL, "\n \u2022 [y] must be a populated atomic vec (?pop_vec)."),
            f0(isTF(not), NULL, "\n \u2022 [not] must be TRUE or FALSE."),
            f0(isTF(na), NULL, "\n \u2022 [na] must be TRUE or FALSE."),
            f0(isIN(agg, types), NULL, "\n \u2022 [agg] must be NA, 'nor', 'one', 'any', 'two', or 'all'."),
            f0(ok.xy, NULL, "\n \u2022 [x] and [y] are not pf compatible modes."))
  if (idef(errs)) {stop(errs)}
  out <- x %in% y
  out[x[is.na(x)]] <- na
  n <- length(which(out))
  f0(agg == "nor", !any(out),
     f0(agg == "any", any(out),
        f0(agg == "all", all(out),
           f0(agg == "one", n == 1,
              f0(agg == "two", n >= 2, out)))))
}

#' @describeIn logicals. Whether elements of \code{x} are not contained in
#'   \code{y}.
#' @export
not_in <- function(x, y, na = F, agg = NA) {!is_in(x, y, T, na, agg)}

#' @describeIn logicals. Whether \code{x} has/contains elements of \code{y}.
#' @export
has <- function(x, y, na = T, agg = NA) {is_in(y, x, F, na, agg)}

#' @describeIn logicals. Whether \code{x} lacks elements of \code{y}.
#' @export
lacks <- function(x, y, na = T, agg = NA) {!is_in(y, x, T, na, agg)}
