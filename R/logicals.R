#' @name logicals
#' @family is_functions
#' @title Extended Logical Functions with Error Checking
#' @param x. A logical object for all functions other than \code{is_in},
#'   \code{not_in}, \code{has}, and \code{lacks}. Otherwise, an atomic object.
#' @param y. A logical object for all functions other than \code{is_in},
#'   \code{not_in}, \code{has}, and \code{lacks}. Otherwise, an atomic object
#'   \link[=compatible]{compatible} with \code{x}.
#' @param na. \code{TRUE} or \code{FALSE} indicating what value should
#'   replace \code{NA} values.
#' @param err. \code{TRUE}, \code{FALSE}, or \code{NA}, indicating that,
#'   respectively, \code{TRUE} should be substituted for a non-logical value,
#'   \code{FALSE} should be substituted for a non-logical value, or an error
#'   should thrown if a non-logical value is encountered.
#' @param ... Arbitrary number of logical scalars or vectors to be processed.
#' @param agg. Character scalar in \code{c('nor', 'one', 'any', 'two', 'all')}
#'   used to specify, respectively, that 0, 1, any, 2 or more, and all arguments
#'   must be \code{TRUE}.
#' @param a. Logical scalar indicating whether to atomize \code{...} before
#'   processing. This creates a single atomic vector of all atomic elements
#'   contained in all arguments in \code{...} and effectively changes the
#'   behavior of \code{or} to \code{any}, \code{and} to \code{all}, \code{not}
#'   to \code{!any}, and \code{some} and \code{most} to count the total number
#'   of \code{TRUE} values rather than the number of \code{TRUE} values in the
#'   same location of recycled arguments from \code{...} (but with error
#'   checking and flexible \code{NA} value handling.)
#' @param not. Logical scalar indicating whether to negate values in arguments
#'   supplied in \code{...} before processing.
#' @param across.,within. Character scalar in \code{c('', 'none', 'one', 'any',
#'   'some', 'all')} indicating no aggregation
#' @return A logical scalar (any function), vector (\code{}), matrix, or array.
#' @export
validate_lgl <- function(x., xxx. = 'lgl', gens. = 0, last. = F) {
  VX <- eval(parse(text = paste0(xxx, "(x)")))
  if (!VX) {bank_err("[x.] must be ", define_xxx_combo(xxx.), ".", gens. = gens. + 1)}
  if (last.) {err_check(gens. + 1)}
  NULL
}

#' @describeIn logicals Wrapper for \code{!x} with error checking.
#' @export
not <- function(x., na. = 'err') {
  vn. <- f0(isLG(na.), T, isEQ(na., 'err'))
  if (!vn.) {stop("\n • [na.] must be TRUE, FALSE, NA, or 'err'.")}
  xxx. <- f0(isEQ(na., 'err'), "cmp_lgl", 'lgl')
  validate_lgl(x., xxx., last. = T)
  if (isTF(na.)) {x.[is.na(x.)] <- na.}
  !x.
}

#' @describeIn logicals Wrapper for \code{x & y} with error checking.
#' @export
and <- function(x., y., na. = 'err') {
  vn. <- f0(isLG(na.), T, isEQ(na., 'err'))
  if (!vn.) {stop("\n • [na.] must be TRUE, FALSE, NA, or 'err'.")}
  xxx. <- f0(isEQ(na., 'err'), "cmp_lgl", 'lgl')
  validate_lgl(x., xxx.)
  validate_lgl(y., xxx., last. = T)
  if (isTF(na.)) {
    x.[is.na(x.)] <- na.
    y.[is.na(x.)] <- na.
  }
  x. & y.
}

#' @describeIn logicals Wrapper for \code{x | y} with error checking.
#' @export
or <- function(x., y., na. = 'err') {
  vn. <- f0(isLG(na.), T, isEQ(na., 'err'))
  if (!vn.) {stop("\n • [na.] must be TRUE, FALSE, NA, or 'err'.")}
  xxx. <- f0(isEQ(na., 'err'), "cmp_lgl", 'lgl')
  validate_lgl(x., xxx.)
  validate_lgl(y., xxx., last. = T)
  if (isTF(na.)) {
    x.[is.na(x.)] <- na.
    y.[is.na(x.)] <- na.
  }
  x. | y.
}

#' @describeIn logicals Returns \code{FALSE} as soon as a second \code{TRUE}
#'   argument is encountered. Otherwise, after evaluating all arguments
#'   evaluates whether the number of \code{TRUE} arguments is 1.
#' @export
one <- function(x., y., na. = 'err') {
  vn. <- f0(isLG(na.), T, isEQ(na., 'err'))
  if (!vn.) {stop("\n • [na.] must be TRUE, FALSE, NA, or 'err'.")}
  xxx. <- f0(isEQ(na., 'err'), "cmp_lgl", 'lgl')
  validate_lgl(x., xxx.)
  validate_lgl(y., xxx., last. = T)
  if (isTF(na.)) {
    x.[is.na(x.)] <- na.
    y.[is.na(x.)] <- na.
  }
  xor(x., y.)
}

#' @describeIn logicals Tests \code{x} and returns either \code{TRUE} or
#'   \code{FALSE}. If \code{x.} is not a logical scalar, throw an error if
#'   \code{err. = 'err'}; otherwise return the value of \code{err.}. Returns the
#'   value of \code{na.} if the value of \code{x} is \code{NA}.
#' @export
TEST <- function(x., na. = FALSE, err. = NA) {
  vn. <- isTF(na.)
  ve. <- f0(isLG(err.), T, isEQ(err., 'err'))
  err. <- NULL
  if (!vn.) {err. <- c(err., "\n • [na.] must be TRUE or FALSE.")}
  if (!ve.) {err. <- c(err., "\n • [err.] must be TRUE, FALSE, NA, or 'err'.")}
  if (idef(err.)) {stop(err.)}
  vx. <- lgl_scl(x.)
  if      (!vx. & !isLG(err.)) {stop("\n • [x.] is not a logical scalar.")}
  else if (!vx.              ) {err.}
  else if (is.na(x.)         ) {na.}
  else                         {x.}
}

#' @describeIn logicals Returns \code{TRUE} as soon as a \code{TRUE} argument is
#'   encountered. If none are encountered, return \code{FALSE}.
#' @export
ANY <- function(..., err. = NA) {
  if (!isLG(err.)) {stop("\n • [err.] must be TRUE, FALSE, or NA.")}
  for (i. in 1:...length()) {
    x. <- tryCatch(...elt(i.) == T, error = function(e) NA, finally = NULL)
    vx. <- cmp_lgl_scl(x.)
    if (!vx.) {
      if (isTF(err.)) {x. <- err.}
      else {stop("\n • [..", i., "] did not resolve to TRUE or FALSE.")}
    }
    if (x.) {return(T)}
  }
  F
}

#' @describeIn logicals Returns \code{FALSE} as soon as a \code{FALSE} argument
#'   is encountered. If none are encountered, return \code{TRUE}.
#' @export
ALL <- function(..., err. = NA) {
  if (!isLG(err.)) {stop("\n • [err.] must be TRUE, FALSE, or NA.")}
  for (i. in 1:...length()) {
    x. <- tryCatch(...elt(i.) == T, error = function(e) NA, finally = NULL)
    vx. <- cmp_lgl_scl(x.)
    if (!vx.) {
      if (isTF(err.)) {x. <- err.}
      else {stop("\n • [..", i., "] did not resolve to TRUE or FALSE.")}
    }
    if (!x.) {return(F)}
  }
  T
}

#' @describeIn logicals Returns \code{FALSE} as soon as a \code{TRUE} argument
#'   is encountered. If none are encountered, return \code{TRUE}.
#' @export
NOR <- function(..., err. = NA) {
  if (!isLG(err.)) {stop("\n • [err.] must be TRUE, FALSE, or NA.")}
  for (i. in 1:...length()) {
    x. <- tryCatch(...elt(i.) == T, error = function(e) NA, finally = NULL)
    vx. <- cmp_lgl_scl(x.)
    if (!vx.) {
      if (isTF(err.)) {x. <- err.}
      else {stop("\n • [..", i., "] did not resolve to TRUE or FALSE.")}
    }
    if (x.) {return(F)}
  }
  T
}

#' @describeIn logicals Generalized \code{xor} with error checking.
#' @export
ONE <- function(..., err. = NA) {
  n. <- 0
  if (!isLG(err.)) {stop("\n • [err.] must be TRUE, FALSE, or NA.")}
  for (i. in 1:...length()) {
    x. <- tryCatch(...elt(i.) == T, error = function(e) NA, finally = NULL)
    vx. <- cmp_lgl_scl(x.)
    if (!vx.) {
      if (isTF(err.)) {x. <- err.}
      else {stop("\n • [..", i., "] did not resolve to TRUE or FALSE.")}
    }
    if (x.) {n. <- n. + 1}
    if (n. > 1) {return(F)}
  }
  (n. == 1)
}

#' @describeIn logicals Returns \code{TRUE} as soon as a second \code{TRUE}
#'   argument is encountered. Otherwise, after evaluating all arguments, check
#'   whether the number of \code{TRUE} arguments is 1.
#' @export
#' @export
TWO <- function(..., err. = NA) {
  n. <- 0
  if (!isLG(err.)) {stop("\n • [err.] must be TRUE, FALSE, or NA.")}
  for (i. in 1:...length()) {
    x. <- tryCatch(...elt(i.) == T, error = function(e.) NA, finally = NULL)
    vx. <- cmp_lgl_scl(x.)
    if (!vx.) {
      if (isTF(err.)) {x. <- err.}
      else {stop("\n • [..", i., "] did not resolve to TRUE or FALSE.")}
    }
    if (x.) {n. <- n. + 1}
    if (n. > 1) {return(T)}
  }
  F
}

#' @describeIn logicals Tests an arbitrary number of arguments with a wide array
#'   of options.
#' @export
tests <- function(..., na. = F, a. = F, not. = F, across. = NA, within. = NA) {
  av. <- av(...)                                                                 # atomize arguments in ...
  x. <- f0(isT(a.), list(av.), list(...))                                        # if atomizing is requested, store the atomized values in a 1 element list, otherwise, extract ... as a list
  n. <- length(x.)                                                               # number of args in {...}
  ns. <- f0(n. == 0, NULL, lengths(x.))                                          # lengths of args in {...}
  aw. <- list(c(NA, 'nor', 'one', 'any', 'two', 'all'))                          # possible values of {across} and {within}
  vna. <- isTF(na.)
  va. <- isTF(a.)
  vnt. <- isTF(not.)
  vac. <- isIN(across., aw.)
  vwi. <- isIN(within., aw.)
  vr. <- f0(n. < 2, T, all(round(max(ns.) / ns.) == max(ns.) / ns.))             # whether arguments are recyclable
  err. <- NULL
  if (!vna.) {err. <- c(err., "\n • [na.] must be TRUE or FALSE.")}
  if (!va. ) {err. <- c(err., "\n • [a.] must be TRUE or FALSE.")}
  if (!vnt.) {err. <- c(err., "\n • [not.] must be TRUE or FALSE.")}
  if (!vac.) {err. <- c(err., "\n • [across.] must be NA, 'nor', 'one', 'any', 'two', 'all'.")}
  if (!vwi.) {err. <- c(err., "\n • [within.] must be NA, 'nor', 'one', 'any', 'two', 'all'.")}
  if (!vr. ) {err. <- c(err., "\n • Arguments in [...] are not recyclable.")}
  if (idef(err.)) {stop(err.)}
  for (i. in 1:n.) {                                                             # for each argument
    x.[[i.]][is.na(x.[[i.]])] <- na.                                             # > replace any NA values with [na]
    if (not.) {x.[[i.]] <- !x.[[i.]]}                                            # > and negative if specified in [not]
  }
  if (isOk(within.)) {                                                           # if within-argument aggregation is specified
    for (i. in 1:length(x.)) {                                                   # > for each argument
      xi. <- x.[[i.]]                                                            # >> extract it
      nx. <- length(xi.)                                                         # >> get its length
      nw. <- length(which(xi.))                                                  # >> and the number of TRUE values
      x.[[i.]] <- f0(within. == "nor", nw. == 0,                                 # >> whether the number of TRUE values meets requirements
                  f0(within. == "any", nw. >= 1,
                  f0(within. == "all", nw. == nx.,
                  f0(within. == "one", nw. == 1, nw. > 1))))
    }}
  if (isOk(across.)) {                                                           # if across-argument aggregation is specified
    nx. <- length(x.)                                                            # > get the number arguments
    out. <- 0                                                                    # > initialize the result counter vector
    for (i. in 1:nx.) {out. <- out. + 1 * x.[[i.]]}                              # > for each argument, add its TRUE results to the counter vector
    x. <- as.vector(x.)                                                          # > convert the resulting list to a vector
    nx. <- length(x.)                                                            # > get its length
    nw. <- length(which(x.))                                                     # > and its number of TRUE values
    x. <- f0(across. == "nor", nw. == 0,                                         # > whether the number of TRUE values meets requirements
          f0(across. == "any", nw. >= 1,
          f0(across. == "all", nw. == nx,
          f0(across. == "one", nw. == 1, nw. > 1))))
  }
  if (isOk(across.) | isOk(within.)) {as.vector(x.)} else {x.}                   # return the result
}

#' @describeIn logicals Indexes \code{TRUE} or \code{FALSE} values.
#' @export
w <- function(..., not. = F, na. = F, a. = F) {tests(..., not. = not., na. = na., a. = a.)}

#' @describeIn logicals Check for across and/or within argument counts of
#'   \code{TRUE} or \code{FALSE} values equal to zero.
#' @export
nors <- function(..., na. = F, a. = F, across. = T, within. = F) {
  va. <- isTF(across.)
  vw. <- isTF(within.)
  err. <- NULL
  if (!va.) {err. <- c(err., "\n • [across.] must be TRUE or FALSE.")}
  if (!vw.) {err. <- c(err., "\n • [within.] must be TRUE or FALSE.")}
  if (idef(err.)) {stop(err.)}
  if (isT(across.)) {across. <- "not"} else {across. <- NA}
  if (isT(within.)) {within. <- "not"} else {within. <- NA}
  tests(..., na. = na., a. = a., across. = across., within. = within.)
}

#' @describeIn logicals Check for across and/or within argument counts of
#'   \code{TRUE} or \code{FALSE} values equal to one or more.
#' @export
anys <- function(..., na. = F, a. = F, across. = T, within. = F) {
  va. <- isTF(across.)
  vw. <- isTF(within.)
  err. <- NULL
  if (!va.) {err. <- c(err., "\n • [across.] must be TRUE or FALSE.")}
  if (!vw.) {err. <- c(err., "\n • [within.] must be TRUE or FALSE.")}
  if (idef(err.)) {stop(err.)}
  if (isT(across.)) {across. <- "any"} else {across. <- NA}
  if (isT(within.)) {within. <- "any"} else {within. <- NA}
  tests(..., na. = na., a = a., across. = across., within. = within.)
}

#' @describeIn logicals Check for across and/or within argument counts of
#'   \code{TRUE} or \code{FALSE} values equal to the number of elements.
#' @export
alls <- function(..., na. = F, a. = F, across. = T, within. = F) {
  va. <- isTF(across.)
  vw. <- isTF(within.)
  err. <- NULL
  if (!va.) {err. <- c(err., "\n • [across.] must be TRUE or FALSE.")}
  if (!vw.) {err. <- c(err., "\n • [within.] must be TRUE or FALSE.")}
  if (idef(err.)) {stop(err.)}
  if (isT(across.)) {across. <- "all"} else {across. <- NA}
  if (isT(within.)) {within. <- "all"} else {within. <- NA}
  tests(..., na. = na., a. = a., across. = across., within. = within.)
}

#' @describeIn logicals Check for across and/or within argument counts of
#'   \code{TRUE} or \code{FALSE} values equal to one.
#' @export
ones <- function(..., na. = F, a. = F, across. = T, within. = F) {
  va. <- isTF(across.)
  vw. <- isTF(within.)
  err. <- NULL
  if (!va.) {err. <- c(err., "\n • [across.] must be TRUE or FALSE.")}
  if (!vw.) {err. <- c(err., "\n • [within.] must be TRUE or FALSE.")}
  if (idef(err.)) {stop(err.)}
  if (isT(across.)) {across. <- "one"} else {across. <- NA}
  if (isT(within.)) {within. <- "one"} else {within. <- NA}
  tests(..., na. = na., a. = a., across. = across., within. = within.)
}

#' @describeIn logicals Check for across and/or within argument counts of
#'   \code{TRUE} or \code{FALSE} values equal to two ore more.
#' @export
twos <- function(..., na. = F, a. = F, across. = T, within. = F) {
  va. <- isTF(across.)
  vw. <- isTF(within.)
  err. <- NULL
  if (!va.) {err. <- c(err., "\n • [across.] must be TRUE or FALSE.")}
  if (!vw.) {err. <- c(err., "\n • [within.] must be TRUE or FALSE.")}
  if (idef(err.)) {stop(err.)}
  if (isT(across.)) {across. <- "two"} else {across. <- NA}
  if (isT(within.)) {within. <- "two"} else {within. <- NA}
  tests(..., na. = na., a. = a., across. = across., within. = within.)
}

#' @describeIn logicals Whether elements of \code{x} are contained in \code{y}.
#' @export
is_in <- function(x., y., not. = F, na. = F, agg. = NA) {
  types. <- c(NA, 'nor', 'one', 'any', 'two', 'all')
  vx. <- ivec(x.)
  vy. <- ivec(y.)
  vnt. <- isTF(not.)
  vna. <- isTF(na.)
  vag. <- isIN(agg., types.)
  vxy. <- f0(!vx. | !vy., T, compatible(x., y.))
  err. <- NULL
  if (!vx.) {err. <- c(err., "\n • [x] must be a populated atomic vector.")}
  if (!vy.) {err. <- c(err., "\n • [y] must be a populated atomic vector.")}
  if (!vnt.) {err. <- c(err., "\n • [not.] must be TRUE or FALSE.")}
  if (!vna.) {err. <- c(err., "\n • [na.] must be TRUE or FALSE.")}
  if (!vag.) {err. <- c(err., "\n • [agg.] must be NA, 'nor', 'one', 'any', 'two', or 'all'.")}
  if (!vxy.) {err. <- c(err., "\n • [x] and [y] are not fo compatible modes.")}
  if (idef(err.)) {stop(err.)}
  out. <- x. %in% y.                                                             # get the initial result
  out.[x.[is.na(x.)]] <- na.                                                     # replace any NA values with [na.]
  n. <- length(which(out.))                                                      # get the number of TRUE values
  if      (agg. == "nor") {!any(out.)}                                           # apply the appropriate aggregation, if any
  else if (agg. == "any") {any(out.)}
  else if (agg. == "all") {all(out.)}
  else if (agg. == "one") {n. == 1}
  else if (agg. == "two") {n. >= 2}
  else {out.}
}

#' @describeIn logicals Whether elements of \code{x.} are not contained in
#'   \code{y.}.
#' @export
not_in <- function(x., y., na. = F, agg. = NA) {!is_in(x., y., T, na., agg.)}

#' @describeIn logicals Whether \code{x.} has/contains elements of \code{y.}.
#' @export
has <- function(x., y., na. = T, agg. = NA) {is_in(y., x., F, na., agg.)}

#' @describeIn logicals Whether \code{x.} lacks elements of \code{y.}.
#' @export
lacks <- function(x., y., na. = T, agg. = NA) {!is_in(y., x., T, na., agg.)}
