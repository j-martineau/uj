#' @name naming
#' @title Get names, set names, determine whether named
#' @description \tabular{rl}{
#'      `getnames`   \tab Gets element, row, and/or column names with optional restrictions.
#'   \cr             \tab   
#'   \cr   `named`   \tab Evaluates whether an object's elements, rows, and/or columns are named, accounting for whether names must be unique, whether the object may contain empty elements, and whether names may be blank strings.
#'   \cr             \tab   
#'   \cr `rcnamed`   \tab Are both rows and columns named?
#'   \cr  `enamed`   \tab Are elements of `x` named?
#'   \cr  `rnamed`   \tab Are rows of `x` named?
#'   \cr  `cnamed`   \tab Are columns of `x` named?
#'   \cr  `dnamed`   \tab Are `...` arguments named?
#'   \cr             \tab   
#'   \cr  `namerc`   \tab Names rows and columns of `x`.
#'   \cr   `namer`   \tab Names rows of `x`.
#'   \cr   `namec`   \tab Names columns of `x`.
#'   \cr   `namev`   \tab Names elements of a `vector` or \code{\link[=ivls]{vlist}}.
#'   \cr   `namel`   \tab Creates and names a `1`-element `list`.
#'   \cr             \tab   
#'   \cr     `rcn`   \tab Gets row and column names in a `list`.
#'   \cr      `en`   \tab Gets element names.
#'   \cr      `rn`   \tab Gets row names.
#'   \cr      `cn`   \tab Gets column names.
#' }
#' @param x Vector or list for `namev` and `vn`; matrix or data.frame for `rn`, `namer`, `cn`, `namec`, and `namerc`; or any object for `namel`.
#' @param ... Arbitrary number of arguments to be inspected for names.
#' @param err A non-`NA` logical scalar indicating whether names must be non-missing.
#' @param d A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving the dimension(s) to name: `0` for elements (of atomic or list vectors only), `1` for rows of matrices or tibbles, `2` for columns of matrices or data.frames, or `12` for rows and columns of matrices or data.frames.
#' @param u A non-`NA` logical scalar indicating whether names must be unique.
#' @param blank A non-`NA` logical scalar indicating whether blank strings (`""`) are allowed as names.
#' @param ln A \link[=cmp_chr_scl]{complete character scalar} element name.
#' @param en A \link[=cmp_chr_vec]{complete character vec} of element names.
#' @param rn,cn Complete character vecs of row or column names, respectively.
#' @return \tabular{rl}{
#'      `rcnamed` \tab   A logical scalar.
#'   \cr `rnamed` \tab   A logical scalar.
#'   \cr `cnamed` \tab   A logical scalar.
#'   \cr `enamed` \tab   A logical scalar.
#'   \cr `dnamed` \tab   A logical scalar.
#'   \cr ` named` \tab   A logical scalar.
#'   \cr `namerc` \tab   A matrix or data.frame.
#'   \cr  `namer` \tab   A matrix or data.frame.
#'   \cr  `namec` \tab   A matrix or data.frame.
#'   \cr  `namev` \tab   A vector.
#'   \cr  `namel` \tab   A `1`-element list.
#'   \cr    `rcn` \tab   A list of `2` vectors.
#'   \cr     `en` \tab   A character vector.
#'   \cr     `rn` \tab   A character vector.
#'   \cr     `cn` \tab   A character vector.
#' }
#' @examples
#' dots_named. <- function(...) {dnamed(...)}
#'
#' Vec. <- 1:5
#' Vls. <- list(num = 1:5, ae = letters[1:5])
#' Mat. <- matrix(letters[1:25], nrow = 5)
#' Dtf. <- data.frame(ae = letters[1:5], AE = LETTERS[1:5])
#'
#' Vec.
#' Vls.
#' Mat.
#' Dtf.
#'
#' named(Vec.)
#' named(Vls.)
#' named(Mat., d = 12)
#' named(Dtf., d = 2)
#'
#' enamed(Vls.)
#' rnamed(Mat.)
#' cnamed(Dtf.)
#' rcnamed(Dtf.)
#'
#' dots_named.(Vec., Vls.)
#' dots_named.(var1 = Vec., var2 = Vls.)
#'
#' getnames(Vls.)
#' getnames(Mat., d = 1)
#' getnames(Dtf., d = 2)
#'
#' en(Vec.)
#' en(Vls.)
#'
#' rn(Mat.)
#' cn(Dtf.)
#' rcn(Dtf.)
#'
#' namel(Dtf., "a data.frame")
#' namev(Vec., letters[Vec.])
#' namer(Mat., letters[Vec.])
#' namec(Dtf., c("letters", "LETTERS"))
#' namerc(Mat., letters[Vec.], Vec.)
#'
#' @export
namev <- function(x, en) {
  ok.en <- cmp_vec(en) & length(x) == length(en)
  errs <- c(f0(imvc(x), NULL, "[x] must be an multivec (?imvc)."),
            f0(ok.en  , NULL, "[en] must be a complete atomic vec (?cmp_vec) of the same length as [x]."))
  if (!is.null(errs)) {stop(.errs(errs))}
  names(x) <- en
  x
}

#' @rdname naming
#' @export
namer <- function(x, rn) {
  ok.rn <- cmp_vec(rn) & nrow(x) == length(rn)
  errs <- c(f0(id2D(x), NULL, "[x] must be a matrix or data.frame."),
            f0(ok.rn  , NULL, "[rn] must be a complete atomic vec (?cmp_vec) of length equal to the number of rows in [x]."))
  if (!is.null(errs)) {stop(.errs(errs))}
  rownames(x) <- rn
  x
}

#' @rdname naming
#' @export
namec <- function(x, cn) {
  ok.cn <- cmp_vec(cn) & ncol(x) == length(cn)
  errs <- c(f0(id2D(x), NULL, "[x] must be a matrix or data.frame."),
            f0(ok.cn  , NULL, "[cn] must be a complete atomic vec (?cmp_vec) of length equal to the number of columns in [x]."))
  if (!is.null(errs)) {stop(.errs(errs))}
  colnames(x) <- cn
  x
}

#' @rdname naming
#' @export
namel <- function(x, ln) {
  if (!cmp_scl(ln)) {stop(.errs("[ln] must be a complete atomic scalar (?cmp_scl)."))}
  x <- list(x)
  names(x) <- ln
  x
}

#' @rdname naming
#' @export
namerc <- function(x, rn, cn) {namer(namec(x, cn), rn)}

#' @rdname naming
#' @export
named <- function(x, d = 0, u = T, blank = F) {
  ok.x <- pop_vec(x) | pop_vls(x) | pop_mat(x) | pop_dtf(x)
  ok.d <- isIN(d, c(0, 1, 2, 12))
  ok.d1D <- f0(!id1D(x), T, d == 0)
  ok.d2D <- f0(!id2D(x), T, d %in% c(1, 2, 12))
  errs <- c(f0(ok.x       , NULL, "[x] must be a vector, populated vlist (?pop_vls), matrix, or data.frame."),
            f0(ok.d       , NULL, "[d] must be 0, 1, 2, or 12."),
            f0(ok.d1D     , NULL, "[d] must be 0 when [x] is a vector, vlist (?is_vls), or 1D array."),
            f0(ok.d2D     , NULL, "[d] must be 1, 2, or 12 when [x] is a matrix or data.frame."),
            f0(isTF(u)    , NULL, "[u] must be TRUE or FALSE."),
            f0(isTF(blank), NULL, "[blank] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(.errs(errs))}
  leOK <- ueOK <- beOK <- lrOK <- urOK <- brOK <- lcOK <- ucOK <- bcOK <- T      # initialize result scalars
  if (d == 0) {                                                                  # if inspecting for element names
    elabs <- names(x)                                                            # > get element names
    leOK <- length(elabs) > 0                                                    # > are elements named?
    ueOK <- f0(leOK & u, isEQ(elabs, uv(elabs)), T)                              # > do names meet uniqueness specification?
    beOK <- f0(leOK & blank, !any(elabs == ""), T)                               # > do names meet blankness specification?
  }
  if (d %in% c(1, 12)) {
    rlabs <- rownames(x)
    lrOK <- length(rlabs) > 0
    urOK <- f0(lrOK & u, isEQ(rlabs, uv(rlabs)), T)
    brOK <- f0(lrOK & blank, !any(rlabs == ""), T)
  }
  if (d %in% c(2, 12)) {
    clabs <- colnames(x)
    lcOK <- length(clabs) > 0
    ucOK <- f0(lcOK & u, isEQ(clabs, uv(clabs)), T)
    bcOK <- f0(lcOK & blank, !any(clabs == ""), T)
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
  errs <- c(f0(ipop(x)         , NULL, "[x] must be populated (?ipop)."),
            f0(isIN(d, 0:2, 12), NULL, "[d] must be 0, 1, 2, or 12."),
            f0(isTF(u)         , NULL, "[u] must be TRUE or FALSE."),
            f0(isTF(err)       , NULL, "[err] must be TRUE or FALSE."))
  if (!is.null(errs)) {stop(.errs(errs))}
  if (d == 0) {
    en <- names(x)
    if (length(en) == 0 & !err) {return(NULL)}
    else if (err & length(en) == 0) {stop(.errs("elements of [x] are not named."))}
    else if (u & !is_unq(en))       {stop(.errs("element names of [x] are not unique."))}
    en
  } else if (!id2D(x)) {
    stop(.errs("[x] must be a matrix or a data.frame when d is 1, 2, or 12."))
  } else if (d == 12) {
    rn <- rownames(x)
    cn <- colnames(x)
    if      (err & (length(rn) == 0 | length(cn) == 0)) {stop(.errs("rows and/or columns of [x] are not named."))}
    else if (u   & (!is_unq(rn)     | !is_unq(cn)    )) {stop(.errs("row and/or column names of [x] are not unique."))}
    list(rows = rn, cols = cn)
  } else if (d == 1) {
    rn <- rownames(x)
    if (length(rn) == 0 & !err) {return(NULL)}
    else if (err & length(rn) == 0) {stop(.errs("rows of [x] are not named."))}
    else if (u & !is_unq(rn))       {stop(.errs("row names of [x] are not unique."))}
    rn
  } else if (d == 2) {
    cn <- colnames(x)
    if (length(cn) == 0 & !err) {return(NULL)}
    else if (err & length(cn) == 0) {stop(.errs("columns of [x] are not named."))}
    else if (u & !is_unq(cn))       {stop(.errs("column names of [x] are not unique."))}
    cn
  }
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
