#' @name naming
#' @encoding UTF-8
#' @family extensions
#' @title Get names, set names, determine whether named
#' @description \tabular{rl}{
#'      `uj::getnames`   \tab General name retrieval function (with optional restrictions).
#'   \cr     `rcn`   \tab Gets matrix and data.frame row and column names.
#'   \cr      `rn`   \tab Gets matrix and data.frame row names.
#'   \cr      `cn`   \tab Gets matrix and data.frame row names.
#'   \cr      `en`   \tab Gets vector abd \link[=ivls]{vlist} element names.
#'   \cr      `dn`   \tab Gets `...` argument names.
#'   \cr             \tab  
#'   \cr   `named`   \tab General name existence check (with optional restrictions).
#'   \cr `rcnamed`   \tab Are both rows and columns named?
#'   \cr  `rnamed`   \tab Are rows of `x` named?
#'   \cr  `cnamed`   \tab Are columns of `x` named?
#'   \cr  `enamed`   \tab Are elements of `x` named?
#'   \cr  `dnamed`   \tab Are `...` arguments named?
#'   \cr             \tab  
#'   \cr    `name`   \tab General naming function
#'   \cr  `namerc`   \tab Names rows and columns of `x`.
#'   \cr   `namer`   \tab Names rows of `x`.
#'   \cr   `namec`   \tab Names columns of `x`.
#'   \cr   `namev`   \tab Names elements of a vector or \code{\link[=ivls]{vlist}}.
#'   \cr   `namel`   \tab Creates and names a `1`-element list.
#' }
#' @param x Vector or list for `namev` and `vn`; matrix or data.frame for `rn`, `namer`, `cn`, `namec`, and `namerc`; or any object for `namel`.
#' @param ... An arbitrary number of arguments.
#' @param d A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving the dimension(s) to name: `0` for elements (of atomic or list vectors only), `1` for rows of matrices or tibbles, `2` for columns of matrices or data.frames, or `12` for rows and columns of matrices or data.frames.
#' @param u A non-`NA` logical scalar indicating whether names must be unique.
#' @param u. A non-`NA` logical scalar indicating whether `...` arg names must be unique.
#' @param ln A non-`NA` character scalar to name the single elemnt of the resulting \link[=ivls]{vlist}.
#' @param cn A \link[=cmp_chr_vec]{complete character vec} of column names.
#' @param en A complete character vec of element names.
#' @param rn A complete character vec of row names.
#' @param err A non-`NA` logical scalar indicating whether names must exist.
#' @param err. A non-`NA` logical scalar indicating whether `...` args must have names.
#' @param blank A non-`NA` logical scalar indicating whether blank strings (`""`) are allowed as names.
#' @return *A logical scalar*
#'   \cr   `rcnamed, rnamed, cnamed`
#'   \cr   `dnamed,  enamed`
#'   \cr   `named`
#'   \cr
#'   \cr *A matrix or data.frame*
#'   \cr   `namerc, namer, namec`
#'   \cr
#'   \cr *A character vector*
#'   \cr   `rn, cn, en`
#'   \cr
#'   \cr *A \link[=ivec]{vec} or \link[=ivls]{vlist}*
#'   \cr   `namev`
#'   \cr
#'   \cr*A vec, vlist, matrix, or data.frame*
#'   \cr   `name`
#'   \cr
#'   \cr *A `1`-element vlist*
#'   \cr   `namel`
#'   \cr
#'   \cr *A `2`-element vlist*
#'   \cr   `rcn`
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
#' uj::getnames(Vls.)
#' uj::getnames(Mat., d = 1)
#' uj::getnames(Dtf., d = 2)
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
  ok.en <- uj::cmp_vec(en) & base::length(x) == base::length(en)
  errs <- base::c(uj::f0(uj::imvc(x), NULL, "[x] must be an multivec (?imvc)."),
                  uj::f0(ok.en     , NULL, "[en] must be a complete atomic vec (?cmp_vec) of the same base::length as [x]."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  base::names(x) <- en
  x
}

#' @rdname naming
#' @export
namer <- function(x, rn) {
  ok.rn <- uj::cmp_vec(rn) & base::nrow(x) == base::length(rn)
  errs <- base::c(uj::f0(uj::id2D(x), NULL, "[x] must be a matrix or data.frame."),
                  uj::f0(ok.rn      , NULL, "[rn] must be a complete atomic vec (?cmp_vec) of base::length equal to the number of rows in [x]."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  base::rownames(x) <- rn
  x
}

#' @rdname naming
#' @export
namec <- function(x, cn) {
  ok.cn <- uj::cmp_vec(cn) & base::ncol(x) == base::length(cn)
  errs <- base::c(uj::f0(uj::id2D(x), NULL, "[x] must be a matrix or data.frame."),
                  uj::f0(ok.cn      , NULL, "[cn] must be a complete atomic vec (?cmp_vec) of base::length equal to the number of columns in [x]."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  base::colnames(x) <- cn
  x
}

#' @rdname naming
#' @export
namel <- function(x, ln) {
  if (!uj::cmp_scl(ln)) {stop(uj::format_errs(pkg = "uj", "[ln] must be a complete atomic scalar (?cmp_scl)."))}
  x <- base::list(x)
  base::names(x) <- ln
  x
}

#' @rdname naming
#' @export
namerc <- function(x, rn, cn) {uj::namer(uj::namec(x, cn), rn)}

#' @rdname naming
#' @export
name <- function(x, en = NULL, rn = NULL, cn = NULL) {if (uj::id1D(x)) {uj::namev(x, en)} else if (uj::id2D(x)) {uj::namerc(x, rn, cn)} else {stop(uj::format_errs(pkg = "uj", "[x] must be a vec (?ivec), vlist (?ivls), matrix, or data.frame."))}}

#' @rdname naming
#' @export
named <- function(x, d = 0, u = T, blank = F) {
  ok.x <- uj::pop_vec(x) | uj::pop_vls(x) | uj::pop_mat(x) | uj::pop_dtf(x)
  ok.d <- uj::isIN(d, base::c(0, 1, 2, 12))
  ok.d1D <- uj::f0(!uj::id1D(x), T, d == 0)
  ok.d2D <- uj::f0(!uj::id2D(x), T, d %in% c(1, 2, 12))
  errs <- c(uj::f0(ok.x           , NULL, "[x] must be a vector, populated vlist (?pop_vls), matrix, or data.frame."),
            uj::f0(ok.d           , NULL, "[d] must be 0, 1, 2, or 12."),
            uj::f0(ok.d1D         , NULL, "[d] must be 0 when [x] is a vector, vlist (?is_vls), or 1D array."),
            uj::f0(ok.d2D         , NULL, "[d] must be 1, 2, or 12 when [x] is a matrix or data.frame."),
            uj::f0(uj::isTF(u)    , NULL, "[u] must be TRUE or FALSE."),
            uj::f0(uj::isTF(blank), NULL, "[blank] must be TRUE or FALSE."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  leOK <- ueOK <- beOK <- lrOK <- urOK <- brOK <- lcOK <- ucOK <- bcOK <- T      # initialize result scalars
  if (d == 0) {                                                                  # if inspecting for element names
    elabs <- base::names(x)                                                            # > get element names
    leOK <- base::length(elabs) > 0                                                    # > are elements named?
    ueOK <- uj::f0(leOK & u, uj::isEQ(elabs, uj::uv(elabs)), T)                              # > do names meet uniqueness specification?
    beOK <- uj::f0(leOK & blank, !base::any(elabs == ""), T)                               # > do names meet blankness specification?
  }
  if (d %in% c(1, 12)) {
    rlabs <- base::rownames(x)
    lrOK <- base::length(rlabs) > 0
    urOK <- uj::f0(lrOK & u, uj::isEQ(rlabs, uj::uv(rlabs)), T)
    brOK <- uj::f0(lrOK & blank, !base::any(rlabs == ""), T)
  }
  if (d %in% c(2, 12)) {
    clabs <- base::colnames(x)
    lcOK <- base::length(clabs) > 0
    ucOK <- uj::f0(lcOK & u, uj::isEQ(clabs, uj::uv(clabs)), T)
    bcOK <- uj::f0(lcOK & blank, !base::any(clabs == ""), T)
  }
  leOK & ueOK & beOK & lrOK & urOK & brOK & lcOK & ucOK & bcOK
}

#' @rdname naming
#' @export
enamed <- function(x, u = T, blank = F) {uj::named(x, 0, u, blank)}

#' @rdname naming
#' @export
rnamed <- function(x, u = T, blank = F) {uj::named(x, 1, u, blank)}

#' @rdname naming
#' @export
cnamed <- function(x, u = T, blank = F) {uj::named(x, 2, u, blank)}

#' @rdname naming
#' @export
rcnamed <- function(x, u = T, blank = F) {uj::named(x, 12, u, blank)}

#' @rdname naming
#' @export
dnamed <- function(..., u = T, blank = F) {uj::named(base::list(...), 0, u, blank)}

#' @rdname naming
#' @export
getnames <- function(x, d = 0, u = T, err = F) {
  errs <- base::c(uj::f0(uj::ipop(x)         , NULL, "[x] must be populated (?ipop)."),
                  uj::f0(uj::isIN(d, 0:2, 12), NULL, "[d] must be 0, 1, 2, or 12."),
                  uj::f0(uj::isTF(u)         , NULL, "[u] must be TRUE or FALSE."),
                  uj::f0(uj::isTF(err)       , NULL, "[err] must be TRUE or FALSE."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  if (d == 0) {
    en <- base::names(x)
    if (base::length(en) == 0 & !err)     {return(NULL)}
    else if (err & base::length(en) == 0) {stop(uj::format_errs(pkg = "uj", "elements of [x] are not named."))}
    else if (u & !uj::iunq(en))           {stop(uj::format_errs(pkg = "uj", "element names of [x] are not unique."))}
    en
  } else if (!uj::id2D(x)) {
    stop(uj::format_errs(pkg = "uj", "[x] must be a matrix or a data.frame when d is 1, 2, or 12."))
  } else if (d == 12) {
    rn <- base::rownames(x)
    cn <- base::colnames(x)
    if      (err & (base::length(rn) == 0 | base::length(cn) == 0)) {stop(uj::format_errs(pkg = "uj", "rows and/or columns of [x] are not named."))}
    else if (u   & (!uj::iunq(rn)         | !uj::iunq(cn)        )) {stop(uj::format_errs(pkg = "uj", "row and/or column names of [x] are not unique."))}
    base::list(rows = rn, cols = cn)
  } else if (d == 1) {
    rn <- base::rownames(x)
    if (base::length(rn) == 0 & !err)     {return(NULL)}
    else if (err & base::length(rn) == 0) {stop(uj::format_errs(pkg = "uj", "rows of [x] are not named."))}
    else if (u & !uj::iunq(rn))           {stop(uj::format_errs(pkg = "uj", "row names of [x] are not unique."))}
    rn
  } else if (d == 2) {
    cn <- base::colnames(x)
    if (base::length(cn) == 0 & !err)     {return(NULL)}
    else if (err & base::length(cn) == 0) {stop(uj::format_errs(pkg = "uj", "columns of [x] are not named."))}
    else if (u & !uj::iunq(cn))           {stop(uj::format_errs(pkg = "uj", "column names of [x] are not unique."))}
    cn
  }
}

#' @rdname naming
#' @export
dn <- function(..., u. = T, err. = F) {
  if (...base::length() == 0) {stop(uj::format_errs(pkg = "uj", "No [...] args were supplied."))}
  en <- base::...names()
  if (base::length(en) == 0 & !err.)     {return(NULL)}
  else if (err. & base::length(en) == 0) {stop(uj::format_errs(pkg = "uj", "[...] args are not named"))}
  else if (u. & !uj::iunq(en))           {stop(uj::format_errs(pkg = "uj", "[...] arg names are not unique."))}
  en
}

#' @rdname naming
#' @export
en <- function(x, u = T, err = F) {uj::getnames(x, 0, u, err)}

#' @rdname naming
#' @export
rn <- function(x, u = T, err = F) {uj::getnames(x, 1, u, err)}

#' @rdname naming
#' @export
cn <- function(x, u = T, err = F) {uj::getnames(x, 2, u, err)}

#' @rdname naming
#' @export
rcn <- function(x, u = T, err = F) {uj::getnames(x, 12, u, err)}
