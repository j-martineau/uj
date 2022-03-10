#' @name logicals
#' @family is_functions
#' @title Extended Logical Functions
#' @details \strong{\code{not}, \code{and}, \code{or}, and \code{nor}}
#'   \cr Error checked \code{!}, \code{&}, \code{|}, and \code{xor} functions.
#'   \cr\cr
#'   \strong{\code{TEST}}
#'   \cr Tests \code{x} and returns either \code{TRUE} or \code{FALSE}. If
#'   \code{x} is not a logical scalar, throw an error if \code{err = 'err'};
#'   otherwise return the value of \code{err}. Returns the value of \code{na} if
#'   the value of \code{x} is \code{NA}.
#'   \cr\cr
#'   \strong{\code{NOR}}
#'   \cr Returns \code{FALSE} as soon as a \code{TRUE} argument is encountered.
#'   If none are encountered, return \code{TRUE}.
#'   \cr\cr
#'   \strong{\code{ANY}}
#'   \cr Returns \code{TRUE} as soon as a \code{TRUE} argument is encountered.
#'   If none are encountered, return \code{FALSE}.
#'   \cr\cr
#'   \strong{\code{ALL}}
#'   \cr Returns \code{FALSE} as soon as a \code{FALSE} argument is encountered.
#'   If none are encountered, return \code{TRUE}.
#'   \cr\cr
#'   \strong{\code{ONE}}
#'   \cr Returns \code{FALSE} as soon as a second \code{TRUE} argument is
#'   encountered. Otherwise, after evaluating all arguments evaluates whether
#'   the number of \code{TRUE} arguments is 1.
#'   \cr\cr
#'   \strong{\code{TWO}}
#'   \cr Returns \code{TRUE} as soon as a second \code{TRUE} argument is
#'   encountered. Otherwise, after evaluating all arguments, check whether the
#'   number of \code{TRUE} arguments is 1.
#'   \cr\cr
#'   \strong{\code{test}}
#'   \cr Tests an arbitrary number of arguments with a wide array of options.
#'   \cr\cr
#'   \strong{\code{w}}
#'   \cr Indexes \code{TRUE} or \code{FALSE} values.
#'   \cr\cr
#'   \strong{\code{nors}, \code{anys}, \code{alls}, \code{ones}, and
#'   \code{twos}}
#'   \cr Check for across and/or within argument counts of \code{TRUE} or
#'   \code{FALSE} values equal to zero, one or more, all, one, or two or more,
#'   respectively.
#'   \cr\cr
#'   \strong{\code{is_in}, \code{not_in}, \code{has}, and \code{lacks}}
#'   \cr Check whether each element of \code{x} is contained by, contains values
#'   of, is not contained by, and does not contain values of \code{y},
#'   respectively.
#' @param x A logical object for all functions other than \code{is_in},
#'   \code{not_in}, \code{has}, and \code{lacks}. Otherwise, an atomic object.
#' @param y A logical object for all functions other than \code{is_in},
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
logicals <- NULL

validate_lgc <- function(x, prop = 'lgc', gens = 0, last = F) {
  VX <- eval(parse(text = paste0(prop, "(x)")))
  if (!VX) {bank_err("[x] must be ", define_combo(prop), ".", gens = gens + 1)}
  if (last) {err_check(gens + 1)}
  NULL
}

#' @rdname logicals
#' @export
not <- function(x, na. = 'err') {
  VN <- f0(isLG(na.), T, isEQ(na., 'err'))
  if (!VN) {stop("\n  * [na.] must be TRUE, FALSE, NA, or 'err'.")}
  prop <- f0(isEQ(na, 'err'), "cmp_lgc", 'lgc')
  validate_lgc(x, prop, last = T)
  if (isTF(na.)) {x[is.na(x)] <- na.}
  !x
}

#' @rdname logicals
#' @export
and <- function(x, y, na. = 'err') {
  VN <- f0(isLG(na.), T, isEQ(na., 'err'))
  if (!VN) {stop("\n  * [na.] must be TRUE, FALSE, NA, or 'err'.")}
  prop <- f0(isEQ(na, 'err'), "cmp_lgc", 'lgc')
  validate_lgc(x, prop)
  validate_lgc(y, prop, last = T)
  if (isTF(na.)) {
    x[is.na(x)] <- na.
    y[is.na(x)] <- na.
  }
  x & y
}

#' @rdname logicals
#' @export
or <- function(x, y, na. = 'err') {
  VN <- f0(isLG(na.), T, isEQ(na., 'err'))
  if (!VN) {stop("\n  * [na.] must be TRUE, FALSE, NA, or 'err'.")}
  prop <- f0(isEQ(na., 'err'), "cmp_lgc", 'lgc')
  validate_lgc(x, prop)
  validate_lgc(y, prop, last = T)
  if (isTF(na.)) {
    x[is.na(x)] <- na.
    y[is.na(x)] <- na.
  }
  x | y
}

#' @rdname logicals
#' @export
one <- function(x, y, na. = 'err') {
  VN <- f0(isLG(na.), T, isEQ(na., 'err'))
  if (!VN) {stop("\n  * [na.] must be TRUE, FALSE, NA, or 'err'.")}
  prop <- f0(isEQ(na., 'err'), "cmp_lgc", 'lgc')
  validate_lgc(x, prop)
  validate_lgc(y, prop, last = T)
  if (isTF(na)) {
    x[is.na(x)] <- na.
    y[is.na(x)] <- na.
  }
  xor(x, y)
}

#' @rdname logicals
#' @export
TEST <- function(x, na. = FALSE, err. = NA) {
  VN <- isTF(na.)
  VE <- f0(isLG(err.), T, isEQ(err., 'err'))
  E <- NULL
  if (!VN) {E <- c(E, "\n  * [na.] must be TRUE or FALSE.")}
  if (!VE) {E <- c(E, "\n  * [err.] must be TRUE, FALSE, NA, or 'err'.")}
  if (xdef(E)) {stop(E)}
  VX <- lgc_scl(x)
  if      (!VX & !isLG(err.)) {stop("\n  * [x] is not a logical scalar.")}
  else if (!VX              ) {err.}
  else if (is.na(x)         ) {na.}
  else                        {x}
}

#' @rdname logicals
#' @export
ANY <- function(..., err. = NA) {
  if (!isLG(err.)) {stop("\n  * [err.] must be TRUE, FALSE, or NA.")}
  for (i in 1:...length()) {
    x <- tryCatch(...elt(i) == T, error = function(e) NA, finally = NULL)
    VX <- cmp_lgc_scl(x)
    if (!VX) {
      if (isTF(err.)) {x <- err.}
      else {stop("\n  * [..", i, "] did not resolve to TRUE or FALSE.")}
    }
    if (x) {return(T)}
  }
  F
}

#' @rdname logicals
#' @export
ALL <- function(..., err. = NA) {
  if (!isLG(err.)) {stop("\n  * [err.] must be TRUE, FALSE, or NA.")}
  for (i in 1:...length()) {
    x <- tryCatch(...elt(i) == T, error = function(e) NA, finally = NULL)
    VX <- cmp_lgc_scl(x)
    if (!VX) {
      if (isTF(err.)) {x <- err.}
      else {stop("\n  * [..", i, "] did not resolve to TRUE or FALSE.")}
    }
    if (!x) {return(F)}
  }
  T
}

#' @rdname logicals
#' @export
NOR <- function(..., err. = NA) {
  if (!isLG(err.)) {stop("\n  * [err.] must be TRUE, FALSE, or NA.")}
  for (i in 1:...length()) {
    x <- tryCatch(...elt(i) == T, error = function(e) NA, finally = NULL)
    VX <- cmp_lgc_scl(x)
    if (!VX) {
      if (isTF(err.)) {x <- err.}
      else {stop("\n  * [..", i, "] did not resolve to TRUE or FALSE.")}
    }
    if (x) {return(F)}
  }
  T
}

#' @rdname logicals
#' @export
ONE <- function(..., err. = NA) {
  N <- 0
  if (!isLG(err.)) {stop("\n  * [err.] must be TRUE, FALSE, or NA.")}
  for (i in 1:...length()) {
    x <- tryCatch(...elt(i) == T, error = function(e) NA, finally = NULL)
    VX <- cmp_lgc_scl(x)
    if (!VX) {
      if (isTF(err.)) {x <- err.}
      else {stop("\n  * [..", i, "] did not resolve to TRUE or FALSE.")}
    }
    if (x) {N <- N + 1}
    if (N > 1) {return(F)}
  }
  (N == 1)
}

#' @rdname logicals
#' @export
TWO <- function(..., err. = NA) {
  N <- 0
  if (!isLG(err.)) {stop("\n  * [err.] must be TRUE, FALSE, or NA.")}
  for (i in 1:...length()) {
    x <- tryCatch(...elt(i) == T, error = function(e) NA, finally = NULL)
    VX <- cmp_lgc_scl(x)
    if (!VX) {
      if (isTF(err.)) {x <- err.}
      else {stop("\n  * [..", i, "] did not resolve to TRUE or FALSE.")}
    }
    if (x) {N <- N + 1}
    if (N > 1) {return(T)}
  }
  F
}

#' @rdname logicals
#' @export
tests <- function(..., na. = F, a. = F, not. = F, across. = NA, within. = NA) {
  A   <- av(...)                                                                 # atomize arguments in ...
  x   <- f0(isT(a.), list(A), list(...))                                         # if atomizing is requested, store the atomized values in a 1 element list, otherwise, extract ... as a list
  N   <- length(x)                                                               # number of args in {...}
  L   <- f0(N == 0, NULL, lengths(x))                                            # lengths of args in {...}
  AW  <- list(c(NA, 'nor', 'one', 'any', 'two', 'all'))                          # possible values of {across} and {within}
  VNA <- isTF(na.); VA <- isTF(a.); VNT <- isTF(not.)
  VAC <- isIN(across., AW)
  VWI <- isIN(within., AW)
  VR  <- f0(N < 2, T, all(round(max(L) / L) == max(L) / L))                      # whether arguments are recyclable
  E   <- NULL
  if (!VNA) {E <- c(E, "\n  * [na.] must be TRUE or FALSE.")}
  if (!VA ) {E <- c(E, "\n  * [a.] must be TRUE or FALSE.")}
  if (!VNT) {E <- c(E, "\n  * [not.] must be TRUE or FALSE.")}
  if (!VAC) {E <- c(E, "\n  * [across.] must be NA, 'nor', 'one', 'any', 'two', 'all'.")}
  if (!VWI) {E <- c(E, "\n  * [within.] must be NA, 'nor', 'one', 'any', 'two', 'all'.")}
  if (!VR ) {E <- c(E, "\n  * Arguments in [...] are not recyclable.")}
  if (xdef(E)) {stop(E)}
  for (i in 1:N) {                                                               # for each argument
    x[[i]][is.na(x[[i]])] <- na.                                                 # > replace any NA values with [na]
    if (not.) {x[[i]] <- !x[[i]]}                                                # > and negative if specified in [not]
  }
  if (isOk(within.)) {                                                           # if within-argument aggregation is specified
    for (i in 1:length(x)) {                                                     # > for each argument
      X <- x[[i]]                                                                # >> extract it
      N <- length(X)                                                             # >> get its length
      W <- length(which(X))                                                      # >> and the number of TRUE values
      x[[i]] <- f0(within. == "nor", W == 0,                                 # >> whether the number of TRUE values meets requirements
                f0(within. == "any", W >= 1,
                f0(within. == "all", W == N,
                f0(within. == "one", W == 1, W > 1))))
    }}
  if (isOk(across.)) {                                                           # if across-argument aggregation is specified
    N <- length(x)                                                               # > get the number arguments
    R <- 0                                                                       # > initialize the result counter vector
    for (i in 1:N) {R <- R + 1 * x[[i]]}                                         # > for each argument, add its TRUE results to the counter vector
    x <- as.vector(x)                                                            # > convert the resulting list to a vector
    N <- length(x)                                                               # > get its length
    W <- length(which(x))                                                        # > and its number of TRUE values
    x <- f0(across. == "nor", W == 0,                                        # > whether the number of TRUE values meets requirements
         f0(across. == "any", W >= 1,
         f0(across. == "all", W == N,
         f0(across. == "one", W == 1, W > 1))))
  }
  if (isOk(across.) | isOk(within.)) {as.vector(x)} else {x}                     # return the result
}

#' @rdname logicals
#' @export
w <- function(..., not. = F, na. = F, a. = F) {tests(..., not. = not., na. = na., a. = a.)}

#' @rdname logicals
#' @export
nors <- function(..., na. = F, a. = F, across. = T, within. = F) {
  VA <- isTF(across.); VW <- isTF(within.)
  E  <- NULL
  if (!VA) {E <- c(E, "\n  * [across.] must be TRUE or FALSE.")}
  if (!VW) {E <- c(E, "\n  * [within.] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  if (isT(across.)) {across. <- "not"} else {across. <- NA}
  if (isT(within.)) {within. <- "not"} else {within. <- NA}
  tests(..., na. = na., a. = a., across. = across., within. = within.)
}

#' @rdname logicals
#' @export
anys <- function(..., na. = F, a. = F, across. = T, within. = F) {
  VA <- isTF(across.); VW <- isTF(within.)
  E  <- NULL
  if (!VA) {E <- c(E, "\n  * [across.] must be TRUE or FALSE.")}
  if (!VW) {E <- c(E, "\n  * [within.] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  if (isT(across.)) {across. <- "any"} else {across. <- NA}
  if (isT(within.)) {within. <- "any"} else {within. <- NA}
  tests(..., na. = na., a = a., across. = across., within. = within.)
}

#' @rdname logicals
#' @export
alls <- function(..., na. = F, a. = F, across. = T, within. = F) {
  VA <- isTF(across.); VW <- isTF(within.)
  E  <- NULL
  if (!VA) {E <- c(E, "\n  * [across.] must be TRUE or FALSE.")}
  if (!VW) {E <- c(E, "\n  * [within.] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  if (isT(across.)) {across. <- "all"} else {across. <- NA}
  if (isT(within.)) {within. <- "all"} else {within. <- NA}
  tests(..., na. = na., a. = a., across. = across., within. = within.)
}

#' @rdname logicals
#' @export
ones <- function(..., na. = F, a. = F, across. = T, within. = F) {
  VA <- isTF(across.); VW <- isTF(within.)
  E  <- NULL
  if (!VA) {E <- c(E, "\n  * [across.] must be TRUE or FALSE.")}
  if (!VW) {E <- c(E, "\n  * [within.] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  if (isT(across.)) {across. <- "one"} else {across. <- NA}
  if (isT(within.)) {within. <- "one"} else {within. <- NA}
  tests(..., na. = na., a. = a., across. = across., within. = within.)
}

#' @rdname logicals
#' @export
twos <- function(..., na. = F, a. = F, across. = T, within. = F) {
  VA <- isTF(across.); VW <- isTF(within.)
  E  <- NULL
  if (!VA) {E <- c(E, "\n  * [across.] must be TRUE or FALSE.")}
  if (!VW) {E <- c(E, "\n  * [within.] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  if (isT(across.)) {across. <- "two"} else {across. <- NA}
  if (isT(within.)) {within. <- "two"} else {within. <- NA}
  tests(..., na. = na., a. = a., across. = across., within. = within.)
}

#' @rdname logicals
#' @export
is_in <- function(x, y, not. = F, na. = F, agg. = NA) {
  Agg <- c(NA, 'nor', 'one', 'any', 'two', 'all')
  VXX <- xvec(x)
  VYY <- xvec(y)
  VNT <- isTF(not.)
  VNA <- isTF(na.)
  VAG <- isIN(agg., Agg)
  VXY <- f0(!VXX | !VYY, T, compatible(x, y))
  E <- NULL
  if (!VXX) {E <- c(E, "\n  * [x] must be a populated atomic vector.")}
  if (!VYY) {E <- c(E, "\n  * [y] must be a populated atomic vector.")}
  if (!VNT) {E <- c(E, "\n  * [not.] must be TRUE or FALSE.")}
  if (!VNA) {E <- c(E, "\n  * [na.] must be TRUE or FALSE.")}
  if (!VAG) {E <- c(E, "\n  * [agg.] must be NA, 'nor', 'one', 'any', 'two', or 'all'.")}
  if (!VXY) {E <- c(E, "\n  * [x] and [y] are not fo compatible modes.")}
  if (xdef(E)) {stop(E)}
  R <- x %in% y                                                                  # get the initial result
  R[x[is.na(x)]] <- na.                                                          # replace any NA values with [na.]
  N <- length(which(R))                                                          # get the number of TRUE values
  if      (agg. == "nor") {!any(R)}                                              # apply the appropriate aggregation, if any
  else if (agg. == "any") { any(R)}
  else if (agg. == "all") { all(R)}
  else if (agg. == "one") { N == 1}
  else if (agg. == "two") { N >= 2}
  else {R}
}

#' @rdname logicals
#' @export
not_in <- function(x, y, na. = F, agg. = NA) {!is_in(x, y, T, na., agg.)}

#' @rdname logicals
#' @export
has <- function(x, y, na. = T, agg. = NA) {is_in(y, x, F, na., agg.)}

#' @rdname logicals
#' @export
lacks <- function(x, y, na. = T, agg. = NA) {!is_in(y, x, T, na., agg.)}
