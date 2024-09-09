#' @encoding UTF-8
#' @title Naming utilities
#' @description Get object names (with optional restrictions), check whether names exist (with optional restrictions), and apply names.
#' @param x Vector or list for `name_vals`/`name_elts` and `val_names / elt_names / vn / en`; matrix or data.frame for `rnames / rn`, `cnames / cn`, `rcnames / rcn`, `name_rcs`, `name_cols`, and `name_rows`; or any object for `name_scl_vls`.
#' @param ... An arbitrary number of arguments.
#' @param bl `TRUE` or `FALSE` indicating whether blank strings (`""`) are allowed as names and `...` argument names, respectively.
#' @param d A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving the dimension(s) to name: `0` for elements (of atomic or list vectors only), `1` for rows of matrices or tibbles, `2` for columns of matrices or data.frames, or `12` for rows and columns of matrices or data.frames.
#' @param err `TRUE` or `FALSE` indicating whether names and `...` argument names must exist, respectively.
#' @param u `TRUE` or `FALSE` indicating whether names and `...` argument names must be unique, respectively.
#' @param name A non-`NA` character scalar to name the single element of the resulting \link[=VLS]{vlist}.
#' @param en,vn,rn,cn A \link[=cmp_chr_vec]{complete character vec} element, value, row, and column named, respectively.
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
naming_help <- function() {utils::help("naming_help", package = "uj")}

#' @describeIn naming_help Named the values/elements of `x` as `vn`. Returns `x` with names applied.
#' @export
name_vals <- function(x, vn) {
  errs <- NULL
  if (!uj::.VEC(x)) {errs <- base::c(errs, "[x] must be a vec (?VEC).")}
  if (!uj::.cmp_vec(vn) | base::length(x) != base::length(vn)) {errs <- base::c(errs, "[vn] must be a complete vec (?cmp_vec) of the same length as [x].")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  base::names(x) <- vn
  x
}

#' @describeIn naming_help An alias for `name_vals`.
#' @export
name_vs <- name_vals

#' @describeIn naming_help Name the values/elements of `x` as `en`
#' @export
name_elts <- function(x, en) {uj::name_vals(x, en)}

#' @describeIn naming_help An alias for `name_elts`.
#' @export
name_es <- name_elts

#' @describeIn naming_help Name the rows of `x` as `rn`. Returns `x` with row names applied.
#' @export
name_rows <- function(x, rn) {
  errs <- NULL
  if (!uj::.D2D(x)) {errs <- base::c(errs, "[x] must be a matrix or data.frame.")}
  if (!uj::.cmp_vec(rn) | base::NROW(x) != base::length(rn)) {errs <- base::c(errs, "[rn] must be a complete vec (?cmp_vec) of length as NROW(x).")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  base::rownames(x) <- rn
  x
}

#' @describeIn naming_help An alias for `name_rows`.
#' @export
name_rs <- name_rows

#' @describeIn naming_help Name the columns of `x` as `cn`. Returns `x` with column names applied.
#' @export
name_cols <- function(x, cn) {
  errs <- NULL
  if (!uj::.D2D(x)) {errs <- base::c(errs, "[x] must be a matrix or data.frame.")}
  if (!uj::.cmp_vec(cn) | base::NCOL(x) != base::length(cn)) {errs <- base::c(errs, "[cn] must be a complete vec (?cmp_vec) of length as NCOL(x).")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  base::colnames(x) <- cn
  x
}

#' @describeIn naming_help An alias for `name_cols`.
#' @export
name_cs <- name_cols

#' @describeIn naming_help Store `x` in a single-element list and name that element `en`. Returns a named, single-element list.
#' @export
name_scl_vls <- function(x, en) {
  if (!uj::.cmp_scl(en)) {uj::stopperr("[en] must be a complete atomic scalar (?cmp_scl).")}
  x <- base::list(x)
  base::names(x) <- en
  x
}

#' @describeIn naming_help An alias for `name_scl_vls`.
#' @export
name_sv <- name_scl_vls

#' @describeIn naming_help Name the rows of `x` as `rn` and the columns of `x` as `cn`. Returns `x` with row and column names applied.
#' @export
name_rows_cols <- function(x, rn, cn) {uj::name_rows(uj::name_cols(x, cn), rn)}

#' @describeIn naming_help An alias for `name_rows_cols`.
#' @export
name_rcs <- name_rows_cols

#' @describeIn naming_help Name the values/elements of, rows of, and/or columns of `x`. Returns `x` with specified names applied.
#' @export
name <- function(x, en = NULL, vn = NULL, rn = NULL, cn = NULL) {
  if      (uj::.D1D(x)) {uj::name_vals(x, uj::f0(base::is.null(en), vn, en))}
  else if (uj::.D2D(x)) {uj::name_rcs(x, rn, cn)}
  else {uj::stopperr("[x] must be a vec (?VEC), vlist (?VLS), matrix, or data.frame.")}
}

#' @describeIn naming_help Evaluates whether elements/rows/columns/drawers/... of `x` are named, optionally checking for uniqueness and non-blankness. Returns a logical scalar or throws an error.
#' @export
named <- function(x, d = 0, u = T, bl = F) {
  okX <- uj::.pop_vec(x) | uj::.pop_vls(x) | uj::.pop_mat(x) | uj::.pop_dtf(x)
  if (uj::cmp_nnw_scl(d)) {okD <- d %in% base::c(0, 1, 2, 12)} else {okD <- F}
  okD1D <- uj::f0(!uj::D1D(x), T, d == 0)
  okD2D <- uj::f0(!uj::D2D(x) | !okD, T, dim %in% base::c(1, 2, 12))
  errs <- NULL
  if (!okX) {errs <- base::c(errs, "[x] must be a populated vector, vlist, matrix, or data.frame (?pop_vec, ?pop_vls, ?pop_mat, ?pop_dtf).")}
  if (!okD) {errs <- base::c(errs, "[d] must be 0, 1, 2, or 12.")}
  if (!okD1D) {errs <- base::c(errs, "[d] must be 0 when [x] is a vector, vlist (?VLS), or 1D array.")}
  if (!okD2D) {errs <- base::c(errs, "[d] must be 1, 2, or 12 when [x] is a matrix or data.frame.")}
  if (!uj::.cmp_lgl_scl(u)) {errs <- base::c(errs, "[u] must be TRUE or FALSE.")}
  if (!uj::.cmp_lgl_scl(bl)) {errs <- base::c(errs, "[bl] must be TRUE or FALSE.")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  leOK <- ueOK <- beOK <- lrOK <- urOK <- brOK <- lcOK <- ucOK <- bcOK <- T
  if (d == 0) {
    en <- base::names(x)
    leOK <- base::length(en) > 0
    ueOK <- uj::f0(leOK & u, base::length(en) == base::length(base::unique(en)), T)
    beOK <- uj::f0(leOK & bl, !base::any(en == ""), T)
  }
  if (d %in% base::c(1, 12)) {
    rn <- base::rownames(x)
    lrOK <- base::length(rn) > 0
    urOK <- uj::f0(lrOK & u, base::length(rn) == base::length(base::unique(rn)), T)
    brOK <- uj::f0(lrOK & bl, !base::any(rn == ""), T)
  }
  if (d %in% base::c(2, 12)) {
    cn <- base::colnames(x)
    lcOK <- base::length(cn) > 0
    ucOK <- uj::f0(lcOK & u, base::length(cn) == base::length(base::unique(cn)), T)
    bcOK <- uj::f0(lcOK & bl, !base::any(cn == ""), T)
  }
  leOK & ueOK & beOK & lrOK & urOK & brOK & lcOK & ucOK & bcOK
}

#' @describeIn naming_help Evaluates whether elements/values of `x` are named, optionally checking for uniqueness and non-blankness. Returns a logical scalar or throws an error.
#' @export
named_elts <- function(x, u = T, bl = F) {uj::named(x, 0, u = u, bl = bl)}

#' @describeIn naming_help An alias for `named_elts`.
#' @export
elts_named <- named_elts

#' @describeIn naming_help An alias for `named_elts`.
#' @export
named_es <- named_elts

#' @describeIn naming_help An alias for `named_elts`.
#' @export
es_named <- named_elts

#' @describeIn naming_help An alias for `named_elts`.
#' @export
named_vals <- named_elts

#' @describeIn naming_help An alias for `named_elts`.
#' @export
vals_named <- named_elts

#' @describeIn naming_help An alias for `named_elts`.
#' @export
named_vs <- named_elts

#' @describeIn naming_help An alias for `named_elts`.
#' @export
vs_named <- named_elts

#' @describeIn naming_help Evaluates whether rows of `x` are named, optionally checking for uniqueness and non-blankness. Returns a logical scalar or throws an error.
#' @export
named_rows <- function(x, u = T, bl = F) {uj::named(x, 1, u = u, bl = bl)}

#' @describeIn naming_help An alias for `named_rows`.
#' @export
rows_named <- named_rows

#' @describeIn naming_help An alias for `named_rows`.
#' @export
named_rs <- named_rows

#' @describeIn naming_help An alias for `named_rows`.
#' @export
rs_named <- named_rows

#' @describeIn naming_help Evaluates whether columns of `x` are named, optionally checking for uniqueness and non-blankness. Returns a logical scalar or throws an error.
#' @export
named_cols <- function(x, u = T, bl = F) {uj::named(x, 2, u = u, bl = bl)}

#' @describeIn naming_help An alias for `named_cols`.
#' @export
cols_named <- named_cols

#' @describeIn naming_help An alias for `named_cols`.
#' @export
named_cs <- named_cols

#' @describeIn naming_help An alias for `named_cols`.
#' @export
cs_named <- named_cols

#' @describeIn naming_help Evaluates whether rows and columns of `x` are named, optionally checking for uniqueness and non-blankness. Returns a logical scalar or throws an error.
#' @export
named_rows_cols <- function(x, u = T, bl = F) {uj::named(x, 12, u = u, bl = bl)}

#' @describeIn naming_help An alias for `named_rows_cols`.
#' @export
rows_cols_named <- named_rows_cols

#' @describeIn naming_help An alias for `named_rows_cols`.
#' @export
named_rcs <- named_rows_cols

#' @describeIn naming_help An alias for `named_rows_cols`.
#' @export
rcs_named <- named_rows_cols

#' @describeIn naming_help Evaluates whether all `...` arguments are named. Returns a logical scalar or throws an error.
#' @export
all_dots_named <- function(...) {
  dLabs <- base::...names()
  nDots <- base::...length()
  uj::f0(uj::N(dLabs) == nDots, !base::any(dLabs == ""), F)
}

#' @describeIn naming_help Generalized name retrieval (elements/values/rows/cols/drawers/...), optionally checking for uniqueness. Returns either a character vector of a list of two character vectors, or throws an error.
#' @export
get_names <- function(x, d = 0, u = T, err = F) {
  errs <- NULL
  if (!uj::.POP(x)) {errs <- base::c(errs, "[x] must be populated (?POP).")}
  if (!uj::.cmp_num_scl(d, valid = base::c(0:2, 12))) {errs <- base::c(errs, "[d] must be 0, 1, 2, or 12.")}
  if (!uj::.cmp_lgl_scl(u)) {errs <- base::c(errs, "[u] must be TRUE or FALSE.")}
  if (!uj::.cmp_lgl_scl(err)) {errs <- base::c(errs, "[err] must be TRUE or FALSE.")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  if (d == 0) {
    en <- base::names(x)
    if (base::length(en) == 0 & !err) {return(NULL)}
    if (err & base::length(en) == 0) {uj::stopperr("values of [x] are not named.")}
    if (u & base::length(en) != base::length(base::unique(en))) {uj::stopperr("value names of [x] are not unique.")}
    en
  } else if (!uj::.D2D(x)) {uj::stopperr("[x] must be a matrix or a data.frame when .d is 1, 2, or 12.")}
  else if (d == 12) {
    rn <- base::rownames(x)
    cn <- base::colnames(x)
    if (err & (base::length(rn) == 0 | base::length(cn) == 0)) {uj::stopperr("rows and/or columns of [x] are not named.")}
    else if (u & (base::length(rn) != base::length(base::unique(rn)) | base::length(cn) != base::length(base::unique(cn)))) {uj::stopperr("row and/or column names of [x] are not unique.")}
    base::list(rows = rn, cols = cn)
  } else if (d == 1) {
    rn <- base::rownames(x)
    if (base::length(rn) == 0 & !err) {return(NULL)}
    else if (err & base::length(rn) == 0) {uj::stopperr("rows of [x] are not named.")}
    else if (u & base::length(rn) != base::length(base::unique(rn))) {uj::stopperr("row names of [x] are not unique.")}
    rn
  } else if (d == 2) {
    cn <- base::colnames(x)
    if (base::length(cn) == 0 & !err) {return(NULL)}
    else if (err & base::length(cn) == 0) {uj::stopperr("columns of [x] are not named.")}
    else if (u & base::length(cn) != base::length(base::unique(cn))) {uj::stopperr("column names of [x] are not unique.")}
    cn
  }
}

#' @describeIn naming_help Gets names of the elements/values of `x`, optionally checking for uniqueness. Returns a character vector or throws an error.
#' @export
enames <- function(x, u = T, err = F) {uj::get_names(x, d = 0, u = u, err = err)}

#' @describeIn naming_help An alias for `enames`.
#' @export
en <- base::names

#' @describeIn naming_help An alias for `enames`.
#' @export
vnames <- enames

#' @describeIn naming_help An alias for `enames`.
#' @export
vn <- base::names

#' @describeIn naming_help Gets names of the rows of `x`, optionally checking for uniqueness. Returns a character vector or throws an error.
#' @export
rnames <- function(x, u = T, err = F) {uj::get_names(x, d = 1, u = u, err = err)}

#' @describeIn naming_help An alias for `rnames`.
#' @export
rn <- base::rownames

#' @describeIn naming_help Gets names of the columns of `x`, optionally checking for uniqueness. Returns a character vector or throws an error.
#' @export
cnames <- function(x, u = T, err = F) {uj::get_names(x, d = 2, u = u, err = err)}

#' @describeIn naming_help An alias for `cnames`.
#' @export
cn <- base::colnames

#' @describeIn naming_help Gets names of the rows and columns of `x`, optionally checking for uniqueness. Returns a character vector or throws an error.
#' @export
row_col_names <- function(x, u = T, err = F) {uj::get_names(x, d = 12, u = u, err = err)}

#' @describeIn naming_help An alias for `row_col_names`.
#' @export
rcnames <- row_col_names

#' @describeIn naming_help An alias for `row_col_names`.
#' @export
rcn <- row_col_names
