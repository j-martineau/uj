#' @name naming
#' @family extensions
#' @title Get Names, Set Names, Determine if Named
#' @description \tabular{ll}{
#'   `namev`    \tab Name the elements of a vector or \link[=ivls]{vlist}.   \cr
#'   `namer`    \tab Name the rows of a matrix or data.frame.                \cr
#'   `namec`    \tab Name the columns of a matrix or data.frame.             \cr
#'   `namel`    \tab Create and name a 1-element list.                       \cr
#'   `namerc`   \tab Name rows and columns of a matrix or data.frame.        \cr
#'   `named`    \tab Determine whether an object's elements, rows, and/or
#'                   columns are named, accounting for whether names must be
#'                   unique, whether the object may contain empty elements, and
#'                   whether names may be blank strings.                     \cr
#'   `enamed`   \tab Are elements of `x` named?                              \cr
#'   `rnamed`   \tab Are rows of `x` named?                                  \cr
#'   `cnamed`   \tab Are columns of `x` named?                               \cr
#'   `dnamed`   \tab Are dot arguments (`...`) named?                        \cr
#'   `getnames` \tab Get element, row, and/or column names with optional
#'                   restrictions.                                           \cr
#'   `en`       \tab Get element names.                                      \cr
#'   `rn`       \tab Get row names.                                          \cr
#'   `cn`       \tab Get column names.                                       \cr
#'   `rcn`      \tab Get row and column names as a 2-element list.             }
#'
#' @param x Vector or list for \code{namev} and \code{vn}; matrix or data.frame
#'   for \code{rn}, \code{namer}, \code{cn}, \code{namec}, \code{namerc}; or any
#'   object for \code{namel}.
#' @param ... Arbitrary number of arguments to be inspected for names.
#' @param err \link[=cmp_lgl_scl]{Complete logical} scalar indicating whether
#'   names must be non-missing.
#' @param d \link[=cmp_psw_scl]{Complete positive whole-number scalar} giving
#'   the dimension(s) to name: `0` for elements (of atomic or list vectors
#'   only), `1` for rows of matrices or tibbles, `2` for columns of
#'   matrices or data.frames, or `12` for rows and columns of matrices or
#'   data.frames.
#' @param u \link[=cmp_lgl_scl]{Complete logical scalar} indicating whether
#'   names must be unique.
#' @param blank \link[=cmp_lgl_scl]{Complete logical scalar} indicating whether
#'   blank strings (`""`) are allowed as names.
#' @param ln \link[=cmp_chr_scl]{Complete character scalar} element name.
#' @param en \link[=cmp_chr_vec]{Complete character vec} of element names.
#' @param rn,cn \link[=cmp_chr_vec]{Complete character vecs} of row or column
#'   names, respectively.
#' @return Character vector or a named/renamed version of `x`.
#' @export
namev <- function(x, en) {
  ok.en <- cmp_vec(en) & length(x) == length(en)
  errs <- c(f0(imvc(x), NULL, " \u2022 [x] must be an multivec (?imvc)."),
            f0(ok.en  , NULL, " \u2022 [en] must be a complete atomic vec (?cmp_vec) of the same length as [x]."))
  if (!is.null(errs)) {stop(errs)}
  names(x) <- en
  x
}

#' @rdname naming
#' @export
namer <- function(x, rn) {
  ok.rn <- cmp_vec(rn) & nrow(x) == nrow(en)
  errs <- c(f0(imvc(x), NULL, " \u2022 [x] must be an multivec (?imvc)."),
            f0(ok.rn  , NULL, " \u2022 [rn] must be a complete atomic vec (?cmp_vec) of length equal to the number of rows in [x]."))
  if (!is.null(errs)) {stop(errs)}
  rownames(x) <- rn
  x
}

#' @rdname naming
#' @export
namec <- function(x, cn) {
  ok.cn <- cmp_vec(rn) & ncol(x) == ncol(en)
  errs <- c(f0(imvc(x), NULL, " \u2022 [x] must be an multivec (?imvc)."),
            f0(ok.cn  , NULL, " \u2022 [cn] must be a complete atomic vec (?cmp_vec) of length equal to the number of columns in [x]."))
  if (!is.null(errs)) {stop(errs)}
  colnames(x) <- cn
  x
}

#' @rdname naming
#' @export
namel <- function(x, ln) {
  if (!cmp_scl(ln)) {stop(" \u2022 [ln] must be a complete atomic scalar (?cmp_scl).")}
  names(x) <- ln
  x
}

#' @rdname naming
#' @export
namerc <- function(x, rn, cn) {namer(namec(x, cn), rn)}

#' @rdname naming
#' @export
named <- function(x, d = 0, u = T, blank = F) {
  ok.x <- ivec(x) | pop_vls(x) | (is.array(x) & length(dim(x)) == 1)
  ok.d <- isIN(d, c(0, 1, 2, 12))
  ok.d1D <- ANY(isEQ(d, 0), id1D(x))
  ok.d2D <- ANY(isIN(d, c(1, 2, 12)), id2D(x))
  errs <- c(f0(ok.x       , NULL, "\n \u2022 [x] must be a vector, populated vlist (?pop_vls), matrix, or data.frame."),
            f0(ok.d       , NULL, "\n \u2022 [d] must be 0, 1, 2, or 12."),
            f0(ok.d1D     , NULL, "\n \u2022 [d] must be 0 when [x] is a vector, vlist (?is_vls), or 1D array."),
            f0(ok.d2D     , NULL, "\n \u2022 [d] must be 1, 2, or 12 when [x] is a matrix or data.frame."),
            f0(isTF(u)    , NULL, "\n \u2022 [u] must be TRUE or FALSE."),
            f0(isTF(blank), NULL, "\n \u2022 [blank] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(errs)}
  leOK <- ueOK <- beOK <- lrOK <- urOK <- brOK <- lcOK <- ucOK <- bcOK <- T      # initialize result scalars
  if (length(x) == 0) {return(F)}                                                # length 0 is unnamed by default
  if (d == 0) {                                                                  # if inspecting for element names
    elabs <- names(x)                                                            # > get element names
    leOK <- length(elabs) > 0                                                    # > are elements named?
    ueOK <- f0(leOK & u, isEQ(elabs, uv(elabs)), F)                              # > do names meet uniqueness specification?
    beOK <- f0(leOK & blank, !any(elabs == ""), F)                               # > do names meet blankness specification?
  }
  if (d %in% c(1, 12)) {
    rlabs <- rownames(x)
    lrOK <- length(rlabs) > 0
    urOK <- f0(lrOK & u, isEQ(rlabs, uv(rlabs)), F)
    brOK <- f0(lrOK & blank, !any(rlabs == ""), F)
  }
  if (d %in% c(2, 12)) {
    clabs <- colnames(x)
    lcOK <- length(clabs) > 0
    ucOK <- f0(lcOK & u, isEQ(clabs, uv(clabs)), F)
    bcOK <- f0(lcOK & blank, !any(clabs == ""), F)
  }
  leOK & ueOK & beOK & lrOK & urOK & brOK & lcOK & ucOK & bcOK
}

#' @rdname naming
#' @export
enamed <- function(x, u = T, blank = F) {named(x, 0, u, blank)}

#' @rdname naming
#' @export
rnamed <- function(x, u = T, blank = F) {named(x, 1, u, blank)}

#' @rdname naming
#' @export
cnamed <- function(x, u = T, blank = F) {named(x, 2, u, blank)}

#' @rdname naming
#' @export
rcnamed <- function(x, u = T, blank = F) {named(x, 12, u, blank)}

#' @rdname naming
#' @export
dnamed <- function(..., u = T, blank = F) {named(list(...), 0, u, blank)}

#' @rdname naming
#' @export
getnames <- function(x, d = 0, u = T, err = F) {
  errs <- c(f0(ipop(x)         , NULL, "\n \u2022 [x] must be populated (?ipop)."),
            f0(isIN(d, 0:2, 12), NULL, "\n \u2022 [d] must be 0, 1, 2, or 12."),
            f0(isTF(u)         , NULL, "\n \u2022 [u] must be TRUE or FALSE."),
            f0(isTF(err)       , NULL, "\n \u2022 [err] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(errs)}
  if (d == 0) {
    en <- names(x)
    if (err & length(en) > 0) {stop("\n \u2022 elements of [x] are not named.")}
    if (u & !is_unq(en))      {stop("\n \u2022 element names of [x] are not unique.")}
    return(en)
  }
  if (!id2D(x)) {stop("\n \u2022 [x] must be a matrix or a dtf (?is_dtf)")}
  if (d %in% c(1, 12)) {
    rn <- rownames(x)
    if (err & length(rn) > 0) {stop("\n \u2022 row of [x] are not named.")}
    if (u & !is_unq(rn))      {stop("\n \u2022 row names of [x] are not unique.")}
    if (d == 1) {return(rn)}
  }
  if (d %in% c(2, 12)) {
    cn <- colnames(x)
    if (err & length(cn) > 0) {stop("\n \u2022 column of [x] are not named.")}
    if (u & !is_unq(cn))      {stop("\n \u2022 column names of [x] are not unique.")}
    if (d == 2) {return(cn)}
  }
  list(r = rn, c = cn)
}

#' @rdname naming
#' @export
en <- function(x, u = T, err = F) {getnames(x, 0, u, err)}

#' @rdname naming
#' @export
rn <- function(x, u = T, err = F) {getnames(x, 1, u, err)}

#' @rdname naming
#' @export
cn <- function(x, u = T, err = F) {getnames(x, 2, u, err)}

#' @rdname naming
#' @export
rcn <- function(x, u = T, err = F) {getnames(x, 12, u, err)}
