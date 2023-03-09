#' @name naming
#' @encoding UTF-8
#' @family extensions
#' @title Naming utilities
#' @description Get object names (with optional restrictions), check whether names exist (with optional restrictions), and apply names.
#' @details
#' \tabular{ll}{  `get_names`        \tab Name retrieval                      \cr
#'                `rcnames, rcn`     \tab Row and column names                \cr
#'                `rnames, rn`       \tab Row names                           \cr
#'                `cnames, cn`       \tab Column names                        \cr
#'                `vnames, vn`       \tab Value/element names                 \cr
#'                `dnames, dn`       \tab `...` arg names                     \cr   \tab   \cr
#'                `named`            \tab Name existence check                \cr
#'                `rcs_are_named`    \tab Are both rows and columns named?    \cr
#'                `cols_are_named`   \tab Are columns of `x` named?           \cr
#'                `dots_are_named`   \tab Are `...` arguments named?          \cr
#'                `rows_are_named`   \tab Are rows of `x` named?              \cr   \tab   \cr
#'                `vals_are_named`   \tab Are values/elements of `x` named?   \cr
#'                `name`             \tab Naming                              \cr
#'                `name_rcs`         \tab Name rows and columns of `x`        \cr
#'                `name_cols`        \tab Name columns of `x`.                \cr
#'                `name_rows`        \tab Name rows of `x`.                   \cr
#'                `name_vals`        \tab Name values/elements of a `x`.      \cr   \tab   \cr
#'                `name_scl_vls`     \tab Create and name a scalar \code{\link[=VLS]{vlist}} }
#' @param x Vector or list for `name_vals` and `val_names / vn`; matrix or data.frame for `rnames / rn`, `cnames / cn`, `rcnames / rcn`, `name_rcs`, `name_cols`, and `name_rows`; or any object for `name_scl_vls`.
#' @param ... An arbitrary number of arguments.
#' @param dim A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving the dimension(s) to name: `0` for elements (of atomic or list vectors only), `1` for rows of matrices or tibbles, `2` for columns of matrices or data.frames, or `12` for rows and columns of matrices or data.frames.
#' @param u,U `TRUE` or `FALSE` indicating whether names must be unique.
#' @param lab A non-`NA` character scalar to name the single element of the resulting \link[=VLS]{vlist}.
#' @param labs,rlabs,clabs,vlabs A \link[=cmp_chr_vec]{complete character vec}.
#' @param err `TRUE` or `FALSE` indicating whether names must exist.
#' @param ERR `TRUE` or `FALSE` indicating whether `...` args must have names.
#' @param bl,BL `TRUE` or `FALSE` indicating whether blank strings (`""`) are allowed as names.
#' @return **A \link[=VEC]{vec}, \link[=VLS]{vlist}, matrix, or data.frame** \cr\cr `name`
#' \cr\cr  **A vector or a vlist of length** `2`                             \cr\cr `get_names`
#' \cr\cr  **A matrix or data.frame**                                        \cr\cr `name_rcs, name_rows, name_cols`
#' \cr\cr  **A \link[=VLS]{vlist} of length** `2`                            \cr\cr `rc_names, rcn`
#' \cr\cr  **A `1`-element \link[=VLS]{vlist}**                              \cr\cr `name_scl_vls`
#' \cr\cr  **A character vector**                                            \cr\cr `rnames, cnames, enames`  \cr `rn, cn, en`
#' \cr\cr  **A logical scalar**                                              \cr\cr `rcs_are_named, rows_are_named, cols_are_named` \cr `dots_are_named, vals_are_named, named`
#' \cr\cr  **A vector or \link[=VLS]{vlist}**                                \cr\cr `name_vals`
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
#' vals_are_named(egVls)
#' rows_are_named(egMat)
#' cols_are_named(egDtf)
#' rcs_are_named(egDtf)
#'
#' dots_are_named(egVec, egVls)
#' dots_are_named(var1 = egVec, var2 = egVls)
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
#' name_scl_vls(egDtf, "a data.frame")
#' name_vals(egVec, letters[egVec])
#' name_rows(egMat, letters[egVec])
#' name_cols(egDtf, c("letters", "LETTERS"))
#' name_rcs(egMat, letters[egVec], egVec)
#' @export
name_vals <- function(x, vn) {
  errs <- NULL
  if (!uj:::.VEC(x)) {errs <- base::c(errs, "[x] must be a vec (?VEC).")}
  if (!uj:::.cmp_vec(en) | base::length(x) != base::length(en)) {errs <- base::c(errs, "[en] must be a complete vec (?cmp_vec) of the same length as [x].")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  base::names(x) <- en
  x
}

#' @rdname naming
#' @export
name_rows <- function(x, labs) {
  errs <- NULL
  if (!uj:::.D2D(x)) {errs <- base::c(errs, "[x] must be a matrix or data.frame.")}
  if (!uj:::.cmp_vec(labs) | base::NROW(x) != base::length(labs)) {errs <- base::c(errs, "[labs] must be a complete vec (?cmp_vec) of length as NROW(x).")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  base::rownames(x) <- labs
  x
}

#' @rdname naming
#' @export
name_cols <- function(x, labs) {
  errs <- NULL
  if (!uj:::.D2D(x)) {errs <- base::c(errs, "[x] must be a matrix or data.frame.")}
  if (!uj:::.cmp_vec(labs) | base::NCOL(x) != base::length(labs)) {errs <- base::c(errs, "[labs] must be a complete vec (?cmp_vec) of length as NCOL(x).")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  base::colnames(x) <- labs
}

#' @rdname naming
#' @export
name_scl_vls <- function(x, lab) {
  if (!uj:::.cmp_scl(lab)) {uj::stopperr("[lab] must be a complete atomic scalar (?cmp_scl).", PKG = "uj")}
  x <- base::list(x)
  base::names(x) <- lab
  x
}

#' @rdname naming
#' @export
name_rcs <- function(x, rlabs, clabs) {uj::name_rows(uj::name_cols(x, clabs), rlabs)}

#' @rdname naming
#' @export
name <- function(x, vlabs = NULL, rlabs = NULL, clabs = NULL) {
  if      (uj:::.D1D(x)) {uj::name_vals(x, vlabs)}
  else if (uj:::.D2D(x)) {uj::name_rcs(x, rlabs, clabs)}
  else {uj::stopperr("[x] must be a vec (?VEC), vlist (?VLS), matrix, or data.frame.", PKG = "uj")}
}

#' @rdname naming
#' @export
named <- function(x, dim = 0, u = T, bl = F) {
  ok.x <- uj:::.pop_vec(x) | uj:::.pop_vls(x) | uj:::.pop_mat(x) | uj:::.pop_dtf(x)
  if (uj::cmp_nnw_scl(dim)) {ok.dim <- dim %in% base::c(0, 1, 2, 12)} else {ok.dim <- F}
  ok.d1d <- uj::f0(!uj::D1D(x), T, dim == 0)
  ok.d2d <- uj::f0(!uj::D2D(x) | !ok.dim, T, dim %in% base::c(1, 2, 12))
  errs <- NULL
  if (!ok.x) {errs <- base::c(errs, "[x] must be a populated vector, vlist, matrix, or data.frame (?pop_vec, ?pop_vls, ?pop_mat, ?pop_dtf).")}
  if (!ok.dim) {errs <- base::c(errs, "[d] must be 0, 1, 2, or 12.")}
  if (!ok.d1d) {errs <- base::c(errs, "[d] must be 0 when [x] is a vector, vlist (?VLS), or 1D array.")}
  if (!ok.d2d) {errs <- base::c(errs, "[d] must be 1, 2, or 12 when [x] is a matrix or data.frame.")}
  if (!uj:::.cmp_lgl_scl(u)) {errs <- base::c(errs, "[u] must be TRUE or FALSE.")}
  if (!uj:::.cmp_lgl_scl(bl)) {errs <- base::c(errs, "[blank] must be TRUE or FALSE.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  leOK <- ueOK <- beOK <- lrOK <- urOK <- brOK <- lcOK <- ucOK <- bcOK <- T      # initialize result scalars
  if (dim == 0) {                                                                # if inspecting for element names
    elabs <- base::names(x)                                                      # > get element names
    leOK <- base::length(elabs) > 0                                              # > are elements named?
    ueOK <- uj::f0(leOK & u, base::length(elabs) == base::length(base::unique(elabs)), T)                 # > do names meet uniqueness specification?
    beOK <- uj::f0(leOK & bl, !base::any(elabs == ""), T)                              # > do names meet blankness specification?
  }
  if (dim %in% base::c(1, 12)) {
    rlabs <- base::rownames(x)
    lrOK <- base::length(rlabs) > 0
    urOK <- uj::f0(lrOK & u, base::length(rlabs) == base::length(base::unique(rlabs)), T)
    brOK <- uj::f0(lrOK & bl, !base::any(rlabs == ""), T)
  }
  if (dim %in% base::c(2, 12)) {
    clabs <- base::colnames(x)
    lcOK <- base::length(clabs) > 0
    ucOK <- uj::f0(lcOK & u, base::length(clabs) == base::length(base::unique(clabs)), T)
    bcOK <- uj::f0(lcOK & bl, !base::any(clabs == ""), T)
  }
  leOK & ueOK & beOK & lrOK & urOK & brOK & lcOK & ucOK & bcOK
}

#' @rdname naming
#' @export
named_elts <- function(x, u = T, bl = F) {uj::named(x, 0, u, bl)}

#' @rdname naming
#' @export
named_rows <- function(x, u = T, bl = F) {uj::named(x, 1, u, bl)}

#' @rdname naming
#' @export
named_cols <- function(x, u = T, bl = F) {uj::named(x, 2, u, bl)}

#' @rdname naming
#' @export
named_rcs <- function(x, u = T, bl = F) {uj::named(x, 12, u, bl)}

#' @rdname naming
#' @export
dnamed <- function(..., U = T, BL = F) {uj::named(base::list(...), 0, U, BL)}

#' @rdname naming
#' @export
get_names <- function(x, d = 0, u = T, err = F) {
  errs <- NULL
  if (!uj:::.POP(x)) {errs <- base::c(errs, "[x] must be populated (?POP).")}
  if (!uj:::.cmp_nnw_scl(d, valid = base::c(0:2, 12))) {errs <- base::c(errs, "[d] must be 0, 1, 2, or 12.")}
  if (!uj:::.cmp_lgl_scl(u)) {errs <- base::c(errs, "[u] must be TRUE or FALSE.")}
  if (!uj:::.cmp_lgl_scl(err)) {errs <- base::c(errs, "[err] must be TRUE or FALSE.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  if (d == 0) {
    vn <- base::names(x)
    if (base::length(vn) == 0 & !err) {return(NULL)}
    if (err & base::length(vn) == 0) {uj::stopperr("values of [x] are not named.", PKG = "uj")}
    if (u & base::length(vn) != base::length(base::unique(vn))) {uj::stopperr("value names of [x] are not unique.", PKG = "uj")}
    vn
  } else if (!uj:::.D2D(x)) {uj::stopperr("[x] must be a matrix or a data.frame when d is 1, 2, or 12.", PKG = "uj")} else if (d == 12) {
    rn <- base::rownames(x)
    cn <- base::colnames(x)
    if (err & (base::length(rn) == 0 | base::length(cn) == 0)) {uj::stopperr("rows and/or columns of [x] are not named.", PKG = "uj")}
    else if (u & (base::length(rn) != base::length(base::unique(rn)) | base::length(cn) != base::length(base::unique(cn)))) {uj::stopperr("row and/or column names of [x] are not unique.", PKG = "uj")}
    base::list(rows = rn, cols = cn)
  } else if (d == 1) {
    rn <- base::rownames(x)
    if (base::length(rn) == 0 & !err) {return(NULL)}
    else if (err & base::length(rn) == 0) {uj::stopperr("rows of [x] are not named.", PKG = "uj")}
    else if (u & base::length(rn) != base::length(base::unique(rn))) {uj::stopperr("row names of [x] are not unique.", PKG = "uj")}
    rn
  } else if (d == 2) {
    cn <- base::colnames(x)
    if (base::length(cn) == 0 & !err) {return(NULL)}
    else if (err & base::length(cn) == 0) {uj::stopperr("columns of [x] are not named.", PKG = "uj")}
    else if (u & base::length(cn) != base::length(base::unique(cn))) {uj::stopperr("column names of [x] are not unique.", PKG = "uj")}
    cn
  }
}

#' @rdname naming
#' @export
dnames <- function(..., U = T, ERR = F) {
  if (base::...length() == 0) {uj::stopperr("No [...] args were supplied.", PKG = "uj")}
  dn <- base::...names()
  ndn <- base::length(dn)
  if (ndn == 0 & !ERR) {return(NULL)}
  if (ERR & ndn == 0) {uj::stopperr("[...] args are not named", PKG = "uj")}
  if (U & base::length(dn) != base::length(base::unique(dn))) {uj::stopperr("[...] arg names are not unique.", PKG = "uj")}
  dn
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
