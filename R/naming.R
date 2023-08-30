#' @name naming
#' @encoding UTF-8
#' @family extensions
#' @title Naming utilities
#' @description Get object names (with optional restrictions), check whether names exist (with optional restrictions), and apply names.
#' @details
#' \tabular{ll}{  `get_names`        \tab name retrieval                      \cr
#'                `rcnames, rcn`     \tab Row and column names                \cr
#'                `rnames, rn`       \tab Row names                           \cr
#'                `cnames, cn`       \tab Column names                        \cr
#'                `vnames, vn`       \tab Value/element names                 \cr
#'                `dnames, dn`       \tab `...` arg names                     \cr   \tab   \cr
#'                `named`            \tab name existence check                \cr
#'                `rcs_are_named`    \tab Are both rows and columns named?    \cr
#'                `cols_are_named`   \tab Are columns of `x` named?           \cr
#'                `dots_are_named`   \tab Are `...` arguments named?          \cr
#'                `rows_are_named`   \tab Are rows of `x` named?              \cr   \tab   \cr
#'                `vals_are_named`   \tab Are values/elements of `x` named?   \cr
#'                `name`             \tab Naming                              \cr
#'                `name_rcs`         \tab name rows and columns of `x`        \cr
#'                `name_cols`        \tab name columns of `x`.                \cr
#'                `name_rows`        \tab name rows of `x`.                   \cr
#'                `name_vals`        \tab name values/elements of a `x`.      \cr   \tab   \cr
#'                `name_scl_vls`     \tab Create and name a scalar \code{\link[=VLS]{vlist}} }
#' @param x Vector or list for `name_vals` and `val_names / vn`; matrix or data.frame for `rnames / rn`, `cnames / cn`, `rcnames / rcn`, `name_rcs`, `name_cols`, and `name_rows`; or any object for `name_scl_vls`.
#' @param ... An arbitrary number of arguments.
#' @param .D A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving the dimension(s) to name: `0` for elements (of atomic or list vectors only), `1` for rows of matrices or tibbles, `2` for columns of matrices or data.frames, or `12` for rows and columns of matrices or data.frames.
#' @param .U `TRUE` or `FALSE` indicating whether names must be unique.
#' @param name A non-`NA` character scalar to name the single element of the resulting \link[=VLS]{vlist}.
#' @param names,rnames,cnames,vnames A \link[=cmp_chr_vec]{complete character vec}.
#' @param .ERR `TRUE` or `FALSE` indicating whether names must exist.
#' @param ERR `TRUE` or `FALSE` indicating whether `...` args must have names.
#' @param .BL,BL `TRUE` or `FALSE` indicating whether blank strings (`""`) are allowed as names.
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
#' named(egMat, .D = 12)
#' named(egDtf, .D = 2)
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
#' get_names(egMat, .D = 1)
#' get_names(egDtf, .D = 2)
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
name_vals <- function(x, names) {
  Errors <- NULL
  if (!uj:::.VEC(x)) {Errors <- base::c(Errors, "[x] must be a vec (?VEC).")}
  if (!uj:::.cmp_vec(names) | base::length(x) != base::length(names)) {Errors <- base::c(Errors, "[names] must be a complete vec (?cmp_vec) of the same length as [x].")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  base::names(x) <- names
  x
}

#' @rdname naming
#' @export
name_rows <- function(x, names) {
  Errors <- NULL
  if (!uj:::.D2D(x)) {Errors <- base::c(Errors, "[x] must be a matrix or data.frame.")}
  if (!uj:::.cmp_vec(names) | base::NROW(x) != base::length(names)) {Errors <- base::c(Errors, "[names] must be a complete vec (?cmp_vec) of length as NROW(x).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  base::rownames(x) <- names
  x
}

#' @rdname naming
#' @export
name_cols <- function(x, names) {
  Errors <- NULL
  if (!uj:::.D2D(x)) {Errors <- base::c(Errors, "[x] must be a matrix or data.frame.")}
  if (!uj:::.cmp_vec(names) | base::NCOL(x) != base::length(names)) {Errors <- base::c(Errors, "[names] must be a complete vec (?cmp_vec) of length as NCOL(x).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  base::colnames(x) <- names
  x
}

#' @rdname naming
#' @export
name_scl_vls <- function(x, name) {
  if (!uj:::.cmp_scl(name)) {uj::stopperr("[name] must be a complete atomic scalar (?cmp_scl).", .PKG = "uj")}
  x <- base::list(x)
  base::names(x) <- name
  x
}

#' @rdname naming
#' @export
name_rcs <- function(x, rnames, cnames) {uj::name_rows(uj::name_cols(x, cnames), rnames)}

#' @rdname naming
#' @export
name <- function(x, vnames = NULL, rnames = NULL, cnames = NULL) {
  if      (uj:::.D1D(x)) {uj::name_vals(x, vnames)}
  else if (uj:::.D2D(x)) {uj::name_rcs(x, rnames, cnames)}
  else {uj::stopperr("[x] must be a vec (?VEC), vlist (?VLS), matrix, or data.frame.", .PKG = "uj")}
}

#' @rdname naming
#' @export
named <- function(x, .D = 0, .U = T, .BL = F) {
  OkX <- uj:::.pop_vec(x) | uj:::.pop_vls(x) | uj:::.pop_mat(x) | uj:::.pop_dtf(x)
  if (uj::cmp_nnw_scl(.D)) {OkD <- .D %in% base::c(0, 1, 2, 12)} else {OkD <- F}
  OkD1D <- uj::f0(!uj::D1D(x), T, .D == 0)
  OkD2D <- uj::f0(!uj::D2D(x) | !OkD, T, dim %in% base::c(1, 2, 12))
  Errors <- NULL
  if (!OkX) {Errors <- base::c(Errors, "[x] must be a populated vector, vlist, matrix, or data.frame (?pop_vec, ?pop_vls, ?pop_mat, ?pop_dtf).")}
  if (!OkD) {Errors <- base::c(Errors, "[.D] must be 0, 1, 2, or 12.")}
  if (!OkD1D) {Errors <- base::c(Errors, "[.D] must be 0 when [x] is a vector, vlist (?VLS), or 1D array.")}
  if (!OkD2D) {Errors <- base::c(Errors, "[.D] must be 1, 2, or 12 when [x] is a matrix or data.frame.")}
  if (!uj:::.cmp_lgl_scl(.U)) {Errors <- base::c(Errors, "[.U] must be TRUE or FALSE.")}
  if (!uj:::.cmp_lgl_scl(.BL)) {Errors <- base::c(Errors, "[.BL] must be TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  leOK <- ueOK <- beOK <- lrOK <- urOK <- brOK <- lcOK <- ucOK <- bcOK <- T      # initialize result scalars
  if (.D == 0) {                                                                # if inspecting for element names
    eNames <- base::names(x)                                                      # > get element names
    leOK <- base::length(eNames) > 0                                              # > are elements named?
    ueOK <- uj::f0(leOK & .U, base::length(eNames) == base::length(base::unique(eNames)), T)                 # > do names meet uniqueness specification?
    beOK <- uj::f0(leOK & .BL, !base::any(eNames == ""), T)                              # > do names meet blankness specification?
  }
  if (.D %in% base::c(1, 12)) {
    rnames <- base::rownames(x)
    lrOK <- base::length(rnames) > 0
    urOK <- uj::f0(lrOK & .U, base::length(rnames) == base::length(base::unique(rnames)), T)
    brOK <- uj::f0(lrOK & .BL, !base::any(rnames == ""), T)
  }
  if (.D %in% base::c(2, 12)) {
    cnames <- base::colnames(x)
    lcOK <- base::length(cnames) > 0
    ucOK <- uj::f0(lcOK & .U, base::length(cnames) == base::length(base::unique(cnames)), T)
    bcOK <- uj::f0(lcOK & .BL, !base::any(cnames == ""), T)
  }
  leOK & ueOK & beOK & lrOK & urOK & brOK & lcOK & ucOK & bcOK
}

#' @rdname naming
#' @export
named_elts <- function(x, .U = T, .BL = F) {uj::named(x, 0, .U, .BL)}

#' @rdname naming
#' @export
named_rows <- function(x, .U = T, .BL = F) {uj::named(x, 1, .U, .BL)}

#' @rdname naming
#' @export
named_cols <- function(x, .U = T, .BL = F) {uj::named(x, 2, .U, .BL)}

#' @rdname naming
#' @export
named_rcs <- function(x, .U = T, .BL = F) {uj::named(x, 12, .U, .BL)}

#' @rdname naming
#' @export
dnamed <- function(..., .U = T, BL = F) {uj::named(base::list(...), 0, .U, BL)}

#' @rdname naming
#' @export
get_names <- function(x, .D = 0, .U = T, .ERR = F) {
  Errors <- NULL
  if (!uj:::.POP(x)) {Errors <- base::c(Errors, "[x] must be populated (?POP).")}
  if (!uj:::.cmp_num_scl(.D, Valid = base::c(0:2, 12))) {Errors <- base::c(Errors, "[.D] must be 0, 1, 2, or 12.")}
  if (!uj:::.cmp_lgl_scl(.U)) {Errors <- base::c(Errors, "[.U] must be TRUE or FALSE.")}
  if (!uj:::.cmp_lgl_scl(.ERR)) {Errors <- base::c(Errors, "[.ERR] must be TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  if (.D == 0) {
    names <- base::names(x)
    if (base::length(names) == 0 & !.ERR) {return(NULL)}
    if (.ERR & base::length(names) == 0) {uj::stopperr("values of [x] are not named.", .PKG = "uj")}
    if (.U & base::length(names) != base::length(base::unique(names))) {uj::stopperr("value names of [x] are not unique.", .PKG = "uj")}
    names
  } else if (!uj:::.D2D(x)) {uj::stopperr("[x] must be a matrix or a data.frame when .D is 1, 2, or 12.", .PKG = "uj")} else if (.D == 12) {
    rnames <- base::rownames(x)
    cnames <- base::colnames(x)
    if (.ERR & (base::length(rnames) == 0 | base::length(cnames) == 0)) {uj::stopperr("rows and/or columns of [x] are not named.", .PKG = "uj")}
    else if (.U & (base::length(rnames) != base::length(base::unique(rnames)) | base::length(cnames) != base::length(base::unique(cnames)))) {uj::stopperr("row and/or column names of [x] are not unique.", .PKG = "uj")}
    base::list(rows = rnames, cols = cnames)
  } else if (.D == 1) {
    rnames <- base::rownames(x)
    if (base::length(rnames) == 0 & !.ERR) {return(NULL)}
    else if (.ERR & base::length(rnames) == 0) {uj::stopperr("rows of [x] are not named.", .PKG = "uj")}
    else if (.U & base::length(rnames) != base::length(base::unique(rnames))) {uj::stopperr("row names of [x] are not unique.", .PKG = "uj")}
    rnames
  } else if (.D == 2) {
    cnames <- base::colnames(x)
    if (base::length(cnames) == 0 & !.ERR) {return(NULL)}
    else if (.ERR & base::length(cnames) == 0) {uj::stopperr("columns of [x] are not named.", .PKG = "uj")}
    else if (.U & base::length(cnames) != base::length(base::unique(cnames))) {uj::stopperr("column names of [x] are not unique.", .PKG = "uj")}
    cnames
  }
}

#' @rdname naming
#' @export
dnames <- function(..., .U = T, .ERR = F) {
  if (base::...length() == 0) {uj::stopperr("No [...] args were supplied.", .PKG = "uj")}
  names <- base::...names()
  nNames <- base::length(names)
  if (nNames == 0 & !.ERR) {return(NULL)}
  if (.ERR & nNames == 0) {uj::stopperr("[...] args are not named", .PKG = "uj")}
  if (.U & nNames != base::length(base::unique(names))) {uj::stopperr("[...] arg names are not unique.", .PKG = "uj")}
  dn
}

#' @rdname naming
#' @export
dn <- dnames

#' @rdname naming
#' @export
enames <- function(x, .U = T, .ERR = F) {uj::get_names(x, 0, .U, .ERR)}

#' @rdname naming
#' @export
en <- enames

#' @rdname naming
#' @export
rnames <- function(x, .U = T, .ERR = F) {uj::get_names(x, 1, .U, .ERR)}

#' @rdname naming
#' @export
rn <- rnames

#' @rdname naming
#' @export
cnames <- function(x, .U = T, .ERR = F) {uj::get_names(x, 2, .U, .ERR)}

#' @rdname naming
#' @export
cn <- cnames

#' @rdname naming
#' @export
rcnames <- function(x, .U = T, .ERR = F) {uj::get_names(x, 12, .U, .ERR)}

#' @rdname naming
#' @export
rcn <- rcnames
