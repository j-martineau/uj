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
#'                `cols_are_named`   \tab Are columns of `X` named?           \cr
#'                `dots_are_named`   \tab Are `...` arguments named?          \cr
#'                `rows_are_named`   \tab Are rows of `X` named?              \cr   \tab   \cr
#'                `vals_are_named`   \tab Are values/elements of `X` named?   \cr
#'                `name`             \tab Naming                              \cr
#'                `name_rcs`         \tab Name rows and columns of `X`        \cr
#'                `name_cols`        \tab Name columns of `X`.                \cr
#'                `name_rows`        \tab Name rows of `X`.                   \cr
#'                `name_vals`        \tab Name values/elements of a `X`.      \cr   \tab   \cr
#'                `name_scl_vls`     \tab Create and name a scalar \code{\link[=VLS]{vlist}} }
#' @param X Vector or list for `name_vals` and `val_names / vn`; matrix or data.frame for `rnames / rn`, `cnames / cn`, `rcnames / rcn`, `name_rcs`, `name_cols`, and `name_rows`; or any object for `name_scl_vls`.
#' @param ... An arbitrary number of arguments.
#' @param D A \link[=cmp_psw_scl]{complete positive whole-number scalar} giving the dimension(s) to name: `0` for elements (of atomic or list vectors only), `1` for rows of matrices or tibbles, `2` for columns of matrices or data.frames, or `12` for rows and columns of matrices or data.frames.
#' @param U `TRUE` or `FALSE` indicating whether names must be unique.
#' @param Lab A non-`NA` character scalar to name the single element of the resulting \link[=VLS]{vlist}.
#' @param Labs,rLabs,cLabs,vLabs A \link[=cmp_chr_vec]{complete character vec}.
#' @param Err `TRUE` or `FALSE` indicating whether names must exist.
#' @param ERR `TRUE` or `FALSE` indicating whether `...` args must have names.
#' @param Bl,BL `TRUE` or `FALSE` indicating whether blank strings (`""`) are allowed as names.
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
#' named(egMat, D = 12)
#' named(egDtf, D = 2)
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
#' get_names(egMat, D = 1)
#' get_names(egDtf, D = 2)
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
name_vals <- function(X, Names) {
  Errors <- NULL
  if (!uj:::.VEC(X)) {Errors <- base::c(Errors, "[X] must be a vec (?VEC).")}
  if (!uj:::.cmp_vec(Names) | base::length(X) != base::length(Names)) {Errors <- base::c(Errors, "[Names] must be a complete vec (?cmp_vec) of the same length as [X].")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  base::names(X) <- Names
  X
}

#' @rdname naming
#' @export
name_rows <- function(X, Names) {
  Errors <- NULL
  if (!uj:::.D2D(X)) {Errors <- base::c(Errors, "[X] must be a matrix or data.frame.")}
  if (!uj:::.cmp_vec(Names) | base::NROW(X) != base::length(Names)) {Errors <- base::c(Errors, "[Names] must be a complete vec (?cmp_vec) of length as NROW(X).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  base::rownames(X) <- Names
  X
}

#' @rdname naming
#' @export
name_cols <- function(X, Names) {
  Errors <- NULL
  if (!uj:::.D2D(X)) {Errors <- base::c(Errors, "[X] must be a matrix or data.frame.")}
  if (!uj:::.cmp_vec(Names) | base::NCOL(X) != base::length(Names)) {Errors <- base::c(Errors, "[Names] must be a complete vec (?cmp_vec) of length as NCOL(X).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  base::colnames(X) <- Names
  X
}

#' @rdname naming
#' @export
name_scl_vls <- function(X, Name) {
  if (!uj:::.cmp_scl(Name)) {uj::stopperr("[Name] must be a complete atomic scalar (?cmp_scl).", PKG = "uj")}
  X <- base::list(X)
  base::names(X) <- Name
  X
}

#' @rdname naming
#' @export
name_rcs <- function(X, rNames, cNames) {uj::name_rows(uj::name_cols(X, cNames), rNames)}

#' @rdname naming
#' @export
name <- function(X, vNames = NULL, rNames = NULL, cNames = NULL) {
  if      (uj:::.D1D(X)) {uj::name_vals(X, vNames)}
  else if (uj:::.D2D(X)) {uj::name_rcs(X, rNames, cNames)}
  else {uj::stopperr("[X] must be a vec (?VEC), vlist (?VLS), matrix, or data.frame.", PKG = "uj")}
}

#' @rdname naming
#' @export
named <- function(X, D = 0, U = T, Bl = F) {
  OkX <- uj:::.pop_vec(X) | uj:::.pop_vls(X) | uj:::.pop_mat(X) | uj:::.pop_dtf(X)
  if (uj::cmp_nnw_scl(D)) {OkD <- D %in% base::c(0, 1, 2, 12)} else {OkD <- F}
  OkD1D <- uj::f0(!uj::D1D(X), T, D == 0)
  OkD2D <- uj::f0(!uj::D2D(X) | !OkD, T, dim %in% base::c(1, 2, 12))
  Errors <- NULL
  if (!OkX) {Errors <- base::c(Errors, "[X] must be a populated vector, vlist, matrix, or data.frame (?pop_vec, ?pop_vls, ?pop_mat, ?pop_dtf).")}
  if (!OkD) {Errors <- base::c(Errors, "[D] must be 0, 1, 2, or 12.")}
  if (!OkD1D) {Errors <- base::c(Errors, "[D] must be 0 when [X] is a vector, vlist (?VLS), or 1D array.")}
  if (!OkD2D) {Errors <- base::c(Errors, "[D] must be 1, 2, or 12 when [X] is a matrix or data.frame.")}
  if (!uj:::.cmp_lgl_scl(U)) {Errors <- base::c(Errors, "[U] must be TRUE or FALSE.")}
  if (!uj:::.cmp_lgl_scl(Bl)) {Errors <- base::c(Errors, "[Bl] must be TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  leOK <- ueOK <- beOK <- lrOK <- urOK <- brOK <- lcOK <- ucOK <- bcOK <- T      # initialize result scalars
  if (D == 0) {                                                                # if inspecting for element names
    eNames <- base::names(X)                                                      # > get element names
    leOK <- base::length(eNames) > 0                                              # > are elements named?
    ueOK <- uj::f0(leOK & U, base::length(eNames) == base::length(base::unique(eNames)), T)                 # > do names meet uniqueness specification?
    beOK <- uj::f0(leOK & Bl, !base::any(eNames == ""), T)                              # > do names meet blankness specification?
  }
  if (D %in% base::c(1, 12)) {
    rNames <- base::rownames(X)
    lrOK <- base::length(rNames) > 0
    urOK <- uj::f0(lrOK & U, base::length(rNames) == base::length(base::unique(rNames)), T)
    brOK <- uj::f0(lrOK & Bl, !base::any(rNames == ""), T)
  }
  if (D %in% base::c(2, 12)) {
    cNames <- base::colnames(X)
    lcOK <- base::length(cNames) > 0
    ucOK <- uj::f0(lcOK & U, base::length(cNames) == base::length(base::unique(cNames)), T)
    bcOK <- uj::f0(lcOK & Bl, !base::any(cNames == ""), T)
  }
  leOK & ueOK & beOK & lrOK & urOK & brOK & lcOK & ucOK & bcOK
}

#' @rdname naming
#' @export
named_elts <- function(X, U = T, Bl = F) {uj::named(X, 0, U, Bl)}

#' @rdname naming
#' @export
named_rows <- function(X, U = T, Bl = F) {uj::named(X, 1, U, Bl)}

#' @rdname naming
#' @export
named_cols <- function(X, U = T, Bl = F) {uj::named(X, 2, U, Bl)}

#' @rdname naming
#' @export
named_rcs <- function(X, U = T, Bl = F) {uj::named(X, 12, U, Bl)}

#' @rdname naming
#' @export
dnamed <- function(..., U = T, BL = F) {uj::named(base::list(...), 0, U, BL)}

#' @rdname naming
#' @export
get_names <- function(X, D = 0, U = T, Err = F) {
  Errors <- NULL
  if (!uj:::.POP(X)) {Errors <- base::c(Errors, "[X] must be populated (?POP).")}
  if (!uj:::.cmp_num_scl(D, Valid = base::c(0:2, 12))) {Errors <- base::c(Errors, "[D] must be 0, 1, 2, or 12.")}
  if (!uj:::.cmp_lgl_scl(U)) {Errors <- base::c(Errors, "[U] must be TRUE or FALSE.")}
  if (!uj:::.cmp_lgl_scl(Err)) {Errors <- base::c(Errors, "[Err] must be TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  if (D == 0) {
    Names <- base::names(X)
    if (base::length(Names) == 0 & !Err) {return(NULL)}
    if (Err & base::length(Names) == 0) {uj::stopperr("values of [X] are not named.", PKG = "uj")}
    if (U & base::length(Names) != base::length(base::unique(Names))) {uj::stopperr("value names of [X] are not unique.", PKG = "uj")}
    Names
  } else if (!uj:::.D2D(X)) {uj::stopperr("[X] must be a matrix or a data.frame when D is 1, 2, or 12.", PKG = "uj")} else if (D == 12) {
    rNames <- base::rownames(X)
    cNames <- base::colnames(X)
    if (Err & (base::length(rNames) == 0 | base::length(cNames) == 0)) {uj::stopperr("rows and/or columns of [X] are not named.", PKG = "uj")}
    else if (U & (base::length(rNames) != base::length(base::unique(rNames)) | base::length(cNames) != base::length(base::unique(cNames)))) {uj::stopperr("row and/or column names of [X] are not unique.", PKG = "uj")}
    base::list(rows = rNames, cols = cNames)
  } else if (D == 1) {
    rNames <- base::rownames(X)
    if (base::length(rNames) == 0 & !Err) {return(NULL)}
    else if (Err & base::length(rNames) == 0) {uj::stopperr("rows of [X] are not named.", PKG = "uj")}
    else if (U & base::length(rNames) != base::length(base::unique(rNames))) {uj::stopperr("row names of [X] are not unique.", PKG = "uj")}
    rNames
  } else if (D == 2) {
    cNames <- base::colnames(X)
    if (base::length(cNames) == 0 & !Err) {return(NULL)}
    else if (Err & base::length(cNames) == 0) {uj::stopperr("columns of [X] are not named.", PKG = "uj")}
    else if (U & base::length(cNames) != base::length(base::unique(cNames))) {uj::stopperr("column names of [X] are not unique.", PKG = "uj")}
    cNames
  }
}

#' @rdname naming
#' @export
dnames <- function(..., U = T, ERR = F) {
  if (base::...length() == 0) {uj::stopperr("No [...] args were supplied.", PKG = "uj")}
  Names <- base::...names()
  nNames <- base::length(Names)
  if (nNames == 0 & !ERR) {return(NULL)}
  if (ERR & nNames == 0) {uj::stopperr("[...] args are not named", PKG = "uj")}
  if (U & nNames != base::length(base::unique(Names))) {uj::stopperr("[...] arg names are not unique.", PKG = "uj")}
  dn
}

#' @rdname naming
#' @export
dn <- dnames

#' @rdname naming
#' @export
enames <- function(X, U = T, Err = F) {uj::get_names(X, 0, U, Err)}

#' @rdname naming
#' @export
en <- enames

#' @rdname naming
#' @export
rnames <- function(X, U = T, Err = F) {uj::get_names(X, 1, U, Err)}

#' @rdname naming
#' @export
rn <- rnames

#' @rdname naming
#' @export
cnames <- function(X, U = T, Err = F) {uj::get_names(X, 2, U, Err)}

#' @rdname naming
#' @export
cn <- cnames

#' @rdname naming
#' @export
rcnames <- function(X, U = T, Err = F) {uj::get_names(X, 12, U, Err)}

#' @rdname naming
#' @export
rcn <- rcnames
