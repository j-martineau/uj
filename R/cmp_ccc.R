#' @encoding UTF-8
#' @family properties
#' @title Completeness + xclass combination properties
#' @description Check for combinations of \link[=CMP]{completeness} and \link[=ccc]{xclass}.
#' @details
#' \tabular{ll}{  `cmp_ccc_funs`   \tab What complete + xclass combination property functions are there?                                                                       \cr   \tab   \cr
#'                `cmp_{ccc}`      \tab Is `x` both complete and a match to the single xclass property `'{ccc}'` where `{ccc}` is a placeholder for any given xclass property? \cr   \tab   \cr
#'                `cmp_ccc`        \tab Is `x` both complete and a match to the single xclass property in `.CCC`?                                                                              }
#' @param x An R object.
#' @param .CCC A character scalar single xclass property from \code{\link{ccc_props}()}.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character vector** \cr\cr `cmp_ccc_funs`
#' \cr\cr  **A logical scalar**   \cr\cr `cmp_{ccc}, cmp_ccc`
#' @examples
#' cmp_ccc_funs()
#' cmp_ccc(letters, "mvc")
#' cmp_ccc(1, "scl")
#' cmp_ccc(NA, "gen")
#' cmp_mvc(letters)
#' cmp_scl(1)
#'
#' @export
cmp_ccc <- function(x, .CCC, ...) {
  cfun <- function(cx) {uj::run("uj:::.", base::toupper(.CCC), "(cx)")}
  afun <- function(ax) {if (!base::is.atomic(ax)) {F} else if (base::length(ax) == 0) {F} else {!base::any(base::is.na(ax))}}
  dfun <- function(dx) {
    if (base::NROW(x) * base::NCOL(x) > 0) {
      for (i in 1:base::NCOL(x)) {if (!afun(dx[[i]])) {return(F)}}
      T
    } else {F}
  }
  vfun <- function(vx) {base::all(base::sapply(vx, afun))}
  if (base::is.character(.CCC)) {.CCC <- base::tolower(.CCC)}
  Errs <- uj:::.meets_errs(x, ...)
  if (!base::is.null(Errs)) {uj::stopperr(Errs, .PKG = "uj")}
  if (!(.CCC %in% uj::v(ccc))) {F}
  else if (!cfun(x)) {F}
  else if (.CCC == "dtf") {dfun(x)}
  else if (.CCC == "vls") {vfun(x)}
  else {afun(x)}
}

#' @rdname cmp_ccc
#' @export
cmp_ccc_funs <- function() {base::paste0('cmp_', uj::v(ccc))}

#' @rdname cmp_ccc
#' @export
cmp_arr <- function(x, ...) {uj::cmp_ccc(x, 'arr', ...)}

#' @rdname cmp_ccc
#' @export
cmp_dtf <- function(x, ...) {uj::cmp_ccc(x, 'dtf', ...)}

#' @rdname cmp_ccc
#' @export
cmp_gen <- function(x, ...) {uj::cmp_ccc(x, 'gen', ...)}

#' @rdname cmp_ccc
#' @export
cmp_mat <- function(x, ...) {uj::cmp_ccc(x, 'mat', ...)}

#' @rdname cmp_ccc
#' @export
cmp_mvc <- function(x, ...) {uj::cmp_ccc(x, 'mvc', ...)}

#' @rdname cmp_ccc
#' @export
cmp_scl <- function(x, ...) {uj::cmp_ccc(x, 'scl', ...)}

#' @rdname cmp_ccc
#' @export
cmp_vec <- function(x, ...) {uj::cmp_ccc(x, 'vec', ...)}

#' @rdname cmp_ccc
#' @export
cmp_vls <- function(x, ...) {uj::cmp_ccc(x, 'vls', ...)}
