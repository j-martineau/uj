#' @name naming
#' @encoding UTF-8
#' @family extensions
#' @title Naming utilities
#' @description Get object names (with optional restrictions), check whether names exist (with optional restrictions), and apply names.
#' @details
#' \tabular{ll}{  `get_names`      \tab Name retrieval                        \cr
#'                `named`          \tab Name existence check                  \cr
#'                `name`           \tab Naming                                \cr   \tab   \cr
#'                `rcnames, rcn`   \tab Row and column names                  \cr
#'                `rnames, rn`     \tab Row names                             \cr
#'                `cnames, cn`     \tab Column names                          \cr
#'                `enames, en`     \tab Element names                         \cr
#'                `dnames, dn`     \tab `...` arg names                       \cr   \tab   \cr
#'                `rcnamed`        \tab Are both rows and columns named?      \cr
#'                `cnamed`         \tab Are columns of `x` named?             \cr
#'                `dnamed`         \tab Are `...` arguments named?            \cr
#'                `enamed`         \tab Are elements of `x` named?            \cr
#'                `rnamed`         \tab Are rows of `x` named?                \cr   \tab   \cr
#'                `namerc`         \tab Name rows and columns of `x`          \cr
#'                `namec`          \tab Name columns of `x`.                  \cr
#'                `namel`          \tab Create and name a list of length `1`. \cr
#'                `namer`          \tab Name rows of `x`.                     \cr
#'                `namee`          \tab Name elements of a `x`.                 }
#' @param x Vector or list for `namee` and `enames / en`; matrix or data.frame for `rnames / rn`, `cnames / cn`, `rcnames / rcn`, `namerc`, `namec`, and `namer`; or any object for `namel`.
#' @param ... An arbitrary number of arguments.
#' @param dim A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving the dimension(s) to name: `0` for elements (of atomic or list vectors only), `1` for rows of matrices or tibbles, `2` for columns of matrices or data.frames, or `12` for rows and columns of matrices or data.frames.
#' @param u,U `TRUE` or `FALSE` indicating whether names must be unique.
#' @param ln A non-`NA` character scalar to name the single elemnt of the resulting \link[=VLS]{vlist}.
#' @param cn A \link[=cmp_chr_vec]{complete character vec} of column names.
#' @param en A complete character vec of element names.
#' @param rn A complete character vec of row names.
#' @param err `TRUE` or `FALSE` indicating whether names must exist.
#' @param ERR `TRUE` or `FALSE` indicating whether `...` args must have names.
#' @param bl,BL `TRUE` or `FALSE` indicating whether blank strings (`""`) are allowed as names.
#' @return **A \link[=VEC]{vec}, \link[=VLS]{vlist}, matrix, or data.frame** \cr\cr `name`
#' \cr\cr  **A vector or a vlist of length** `2`                             \cr\cr `get_names`
#' \cr\cr  **A matrix or data.frame**                                        \cr\cr `namerc, namer,namec`
#' \cr\cr  **A \link[=VLS]{vlist} of length** `2`                            \cr\cr `rcnames, rcn`
#' \cr\cr  **A `1`-element \link[=VLS]{vlist}**                              \cr\cr `namel`
#' \cr\cr  **A character vector**                                            \cr\cr `rnames, cnames, enames`  \cr `rn, cn, en`
#' \cr\cr  **A logical scalar**                                              \cr\cr `rcnamed, rnamed, cnamed` \cr `dnamed, enamed, named`
#' \cr\cr  **A vector or \link[=VLS]{vlist}**                                \cr\cr `namev`
#' @examples
#' egDotsNamed <- function(...) {dnamed(...)}
#'
#' egVec <- 1:5
#' egVls <- list(num = 1:5, ae = letters[1:5])
#' egMat <- matrix(letters[1:25], nrow = 5)
#' egDtf <- data.frame(ae = letters[1:5], AE = LETTERS[1:5])
#'
#' egVec
#' egVls
#' egMat
#' egDtf
#'
#' named(egVec)
#' named(egVls)
#' named(egMat, d = 12)
#' named(egDtf, d = 2)
#'
#' enamed(egVls)
#' rnamed(egMat)
#' cnamed(egDtf)
#' rcnamed(egDtf)
#'
#' dnamed(egVec, egVls)
#' dnamed(var1 = egVec, var2 = egVls)
#'
#' get_names(egVls)
#' get_names(egMat, d = 1)
#' get_names(egDtf, d = 2)
#'
#' en(egVec)
#' en(egVls)
#'
#' rn(egMat)
#' cn(egDtf)
#' rcn(egDtf)
#'
#' namel(egDtf, "a data.frame")
#' namee(egVec, letters[egVec])
#' namer(egMat, letters[egVec])
#'namec(egDtf, c("letters", "LETTERS"))
#' namerc(egMat, letters[egVec], egVec)
#' @export
namee <- function(x, en) {
  uj::errs_if_nots(uj::MVC(x), "[x] must be an multivec (?MVC).", uj::cmp_vec(en) & uj::NEQ(x, en), "[en] must be a complete vec (?cmp_vec) of the same length as [x].", PKG = "uj")
  base::names(x) <- en
  x
}

#' @rdname naming
#' @export
namer <- function(x, rn) {
  uj::errs_if_nots(uj::MVC(x)                              , "[x] must be an multivec (?MVC)."                            ,
                   uj::cmp_vec(rn) & un::NR(x) == uj::N(rn), "[rn] must be a complete vec (?cmp_vec) of length [nrow(x)].", PKG = "uj")
  base::rownames(x) <- rn
  x
}

#' @rdname naming
#' @export
namec <- function(x, cn) {
  uj::errs_if_nots(uj::MVC(x)                              , "[x] must be an multivec (?MVC)."                            ,
                   uj::cmp_vec(cn) & uj::NC(x) == uj::N(cn), "[cn] must be a complete vec (?cmp_vec) of length [ncol(x)].", PKG = "uj")
  base::colnames(x) <- cn
}

#' @rdname naming
#' @export
namel <- function(x, ln) {
  uj::err_if_not(uj::cmp_scl(ln), "[ln] must be a complete atomic scalar (?cmp_scl).", PKG = "uj")
  x <- base::list(x)
  base::names(x) <- ln
  x
}

#' @rdname naming
#' @export
namerc <- function(x, rn, cn) {uj::namer(uj::namec(x, cn), rn)}

#' @rdname naming
#' @export
name <- function(x, en = NULL, rn = NULL, cn = NULL) {uj::f0(uj::D1D(x), uj::namee(x, en), uj::f0(uj::D2D(x), uj::namerc(x, rn, cn), uj::stopperr("[x] must be a vec (?VEC), vlist (?VLS), matrix, or data.frame.", PKG = "uj")))}

#' @rdname naming
#' @export
named <- function(x, dim = 0, u = T, bl = F) {
  ok.x <- uj::pop_vec(x) | uj::pop_vls(x) | uj::pop_mat(x) | uj::pop_dtf(x)
  ok.d <- uj::isIN1(dim, 0, 1, 2, 12)
  ok.d1D <- uj::f0(!uj::D1D(x), T, dim == 0)
  ok.d2D <- uj::f0(!uj::D2D(x), T, uj::isIN1(dim, 1, 2, 12))
  uj::errs_if_nots(ok.x         , "[x] must be a populated vector, vlist, matrix, or data.frame (?pop_vec, ?pop_vls, ?pop_mat, ?pop_dtf).",
                   ok.d         , "[d] must be 0, 1, 2, or 12."                                                                           ,
                   ok.d1D       , "[d] must be 0 when [x] is a vector, vlist (?VLS), or 1D array."                                        ,
                   ok.d2D       , "[d] must be 1, 2, or 12 when [x] is a matrix or data.frame."                                           ,
                   uj::isTF1(u) , "[u] must be TRUE or FALSE."                                                                            ,
                   uj::isTF1(bl), "[blank] must be TRUE or FALSE."                                                                        , PKG = "uj")
  leOK <- ueOK <- beOK <- lrOK <- urOK <- brOK <- lcOK <- ucOK <- bcOK <- T      # initialize result scalars
  if (dim == 0) {                                                                # if inspecting for element names
    elabs <- base::names(x)                                                           # > get element names
    leOK <- uj::N1P(elabs)                                                       # > are elements named?
    ueOK <- uj::f0(leOK & u, uj::isVEQ(elabs, uj::UV(elabs)), T)                 # > do names meet uniqueness specification?
    beOK <- uj::f0(leOK & bl, uj::noneBL(elabs), T)                              # > do names meet blankness specification?
  }
  if (uj::isIN1(dim, 1, 12)) {
    rlabs <- base::rownames(x)
    lrOK <- uj::N1P(rlabs)
    urOK <- uj::f0(lrOK & u, uj::isVEQ(rlabs, uj::UV(rlabs)), T)
    brOK <- uj::f0(lrOK & bl, uj::noneBL(rlabs), T)
  }
  if (uj::isIN1(dim, 2, 12)) {
    clabs <- base::colnames(x)
    lcOK <- uj::N1P(clabs)
    ucOK <- uj::f0(lcOK & u, uj::EQ(clabs, uj::UV(clabs)), T)
    bcOK <- uj::f0(lcOK & bl, uj::noneBL(clabs), T)
  }
  leOK & ueOK & beOK & lrOK & urOK & brOK & lcOK & ucOK & bcOK
}

#' @rdname naming
#' @export
enamed <- function(x, u = T, bl = F) {uj::named(x, 0, u, bl)}

#' @rdname naming
#' @export
rnamed <- function(x, u = T, bl = F) {uj::named(x, 1, u, bl)}

#' @rdname naming
#' @export
cnamed <- function(x, u = T, bl = F) {uj::named(x, 2, u, bl)}

#' @rdname naming
#' @export
rcnamed <- function(x, u = T, bl = F) {uj::named(x, 12, u, bl)}

#' @rdname naming
#' @export
dnamed <- function(..., U = T, BL = F) {uj::named(base::list(...), 0, U, BL)}

#' @rdname naming
#' @export
get_names <- function(x, d = 0, u = T, err = F) {
  uj::errs_if_nots(uj::POP(x)           , "[x] must be populated (?POP).",
                   uj::isIN1(d, 0:2, 12), "[d] must be 0, 1, 2, or 12."  ,
                   uj::isTF1(u)         , "[u] must be TRUE or FALSE."   ,
                   uj::isTF1(err)       , "[err] must be TRUE or FALSE." , PKG = "uj")
  if (d == 0) {
    en <- base::names(x)
    if (uj::N0(en) & !err) {return(NULL)}
    uj::err_if(err & uj::N0(en) , "elements of [x] are not named.", PKG = "uj")
    uj::err_if(u & notUNQ(en), "element names of [x] are not unique.", PKG = "uj")
    en
  } else if (!uj::D2D(x)) {uj::stopperr("[x] must be a matrix or a data.frame when d is 1, 2, or 12.", PKG = "uj")} else if (d == 12) {
    rn <- base::rownames(x)
    cn <- base::colnames(x)
    if (err & (uj::N0(rn) | uj::N0(cn))) {uj::stopperr("rows and/or columns of [x] are not named.", PKG = "uj")}
    else if (u & (uj::notUNQ(rn) | uj::notUNQ(cn))) {uj::stopperr("row and/or column names of [x] are not unique.", PKG = "uj")}
    base::list(rows = rn, cols = cn)
  } else if (d == 1) {
    rn <- base::rownames(x)
    if (uj::N0(rn) & !err) {return(NULL)}
    else if (err & uj::N0(rn)) {uj::stopperr("rows of [x] are not named.", PKG = "uj")}
    else if (u & notUNQ(rn)) {uj::stopperr("row names of [x] are not unique.", PKG = "uj")}
    rn
  } else if (d == 2) {
    cn <- base::colnames(x)
    if (uj::N0(cn) & !err) {return(NULL)}
    else if (err & uj::N0(cn)) {uj::stopperr("columns of [x] are not named.", PKG = "uj")}
    else if (u & notUNQ(cn)) {uj::stopperr("column names of [x] are not unique.", PKG = "uj")}
    cn
  }
}

#' @rdname naming
#' @export
dnames <- function(..., U = T, ERR = F) {
  uj::err_if(uj::ND0(), "No [...] args were supplied.", PKG = "uj")
  EN <- base::...names()
  if (uj::N0(EN) & !ERR) {return(NULL)}
  uj::err_if(ERR & uj::N0(EN), "[...] args are not named", PKG = "uj")
  uj::err_if(U & notUNQ(EN), "[...] arg names are not unique.", PKG = "uj")
  EN
}

#' @rdname naming
#' @export
dn <- dnames

#' @rdname naming
#' @export
enames <- function(x, u = T, err = F) {uj::get_names(x, 0, u, err)}

#' @rdname naming
#' @export
en <- enames

#' @rdname naming
#' @export
rnames <- function(x, u = T, err = F) {uj::get_names(x, 1, u, err)}

#' @rdname naming
#' @export
rn <- rnames

#' @rdname naming
#' @export
cnames <- function(x, u = T, err = F) {uj::get_names(x, 2, u, err)}

#' @rdname naming
#' @export
cn <- cnames

#' @rdname naming
#' @export
rcnames <- function(x, u = T, err = F) {uj::get_names(x, 12, u, err)}

#' @rdname naming
#' @export
rcn <- rcnames
