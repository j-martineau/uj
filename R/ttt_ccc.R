#' @name ttt_ccc.
#' @family props
#' @title Fundamental type plus extended class properties
#' @description Combinations of \link[ttt]{fundamental type} and
#'   \link[ccc]{extended class}.
#' @param x An object.
#' @param xxx Optional \link[cmp_chr_scl]{Complete character scalar} naming an
#'   extended mode from \code{\link{mmm_vals}()}.
#' @return Logical scalar.
#' @export
ttt_ccc. <- function() {help("ttt_ccc.", package = "uj")}

#' @describeIn ttt_ccc. Get a character vector of all possible fundamental
#'   type plus extended class properties.
#' @export
ttt_ccc_vals <- function() {
  join <- function(x) {paste0(av(x), collapse = "_")}
  sort(av(apply(expand.grid(ttt = ttt_vals(), ccc = ccc_vals()), 2, join)))
}

# atm_ccc ####

#' @describeIn ttt_ccc. Is \code{x} an \link[iatm]{atomic} \link[iarr]{array}?
#' @export
atm_arr <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!pop_arr(x)) {F} else if (!is.atomic(x)) {F} else if (is.null(xxx)) {T} else {run("i", xxx, "(x)")}
}

#' @describeIn ttt_ccc. Is \code{x} an \link[iatm]{atomic} \link[igen]{generic}?
#' @export
atm_gen <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!pop_gen(x)) {F} else if (!is.atomic(x)) {F} else if (is.null(xxx)) {T} else {run("i", xxx, "(x)")}
}

#' @describeIn ttt_ccc. Is \code{x} an \link[iatm]{atomic} \link[imat]{matrix}?
#' @export
atm_mat <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!imat(x)) {F} else if (!is.atomic(x)) {F} else if (is.null(xxx)) {T} else {run("i", xxx, "(x)")}
}

#' @describeIn ttt_ccc. Is \code{x} an \link[iatm]{atomic}
#'   \link[imvc]{multivec}?
#' @export
atm_mvc <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!imvc(x)) {F} else if (!is.atomic(x)) {F} else if (is.null(mmm)) {T} else {run("i", mmm, "(x)")}
}

#' @describeIn ttt_ccc. Is \code{x} an \link[iatm]{atomic} \link[iscl]{scalar}?
#' @export
atm_scl <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!iscl(x)) {F} else if (!is.atomic(x)) {F} else if (is.null(xxx)) {T} else {run("i", xxx, "(x)")}
}

#' @describeIn ttt_ccc. Is \code{x} an \link[iatm]{atomic} \link[idtf]{dtf}?
#' @export
atm_dtf <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (nrow(x) * ncol(x) == 0) {F} else if (!all(apply(x, 2, is.atomic))) {F} else if (is.null(xxx)) {T}
  else {
    code <- paste0("i", xxx, "(x[ , ", 1:length(x), "])")
    code <- paste0(code, collapse = ", ")
    code <- paste0("all(", code, ")")
    run(code)
  }}

#' @describeIn ttt_ccc. Is \code{x} an \link[iatm]{atomic} \link[ivec]{vec}?
#' @export
atm_vec <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!ivec(x)) {F} else if (!is.atomic(x)) {F} else if (is.null(xxx)) {T} else {run("i", xxx, "(x)")}
}

#' @describeIn ttt_ccc. Is \code{x} an \link[iatm]{atomic} \link[ivls]{vlist}?
#' @export
atm_vls <- function(x, xxx = NULL) {
  vxxx <- ifelse(is.null(xxx), T, isIN(xxx, mmm_vals()))
  if (!vxxx) {stop("\n • [xxx] must be NULL or a character scalar value from mmm_vals().")}
  if (!pop_vls(x)) {F} else if (!all(sapply(x, is.atomic))) {F} else if (is.null(xxx)) {T}
  else {
    out <- paste0("i", xxx, "(x[[", 1:length(x), "]])")
    out <- paste0(out, collapse = ", ")
    run(paste0("all(", out, ")"))
  }}

# pop_ccc ####

#' @describeIn ttt_ccc. Is \code{x} a \link[ipop]{populated} \link[iarr]{array}?
#' @export
pop_arr <- function(x, xxx = NULL) {length(x) > 0 & f0(inll(xxx), iarr(x), atm_arr(x, xxx))}

#' @describeIn ttt_ccc. Is \code{x} a \link[ipop]{populated}
#'   \link[igen]{generic}?
#' @export
pop_gen <- function(x, xxx = NULL) {length(x) > 0 & f0(inll(xxx), igen(x), atm_gen(x, xxx))}

#' @describeIn ttt_ccc. Is \code{x} a \link[ipop]{populated}
#'   \link[imat]{matrix}?
#' @export
pop_mat <- function(x, xxx = NULL) {length(x) > 0 & f0(inll(xxx), imat(x), atm_mat(x, xxx))}

#' @describeIn ttt_ccc. Is \code{x} a \link[ipop]{populated}
#'   \link[imvc]{multivec}?
#' @export
pop_mvc <- function(x, xxx = NULL) {length(x) > 0 & f0(inll(xxx), imvc(x), atm_mvc(x, xxx))}

#' @describeIn ttt_ccc. Is \code{x} a \link[ipop]{populated}
#'   \link[iscl]{scalar}?
#' @export
pop_scl <- function(x, xxx = NULL) {length(x) > 0 & f0(inll(xxx), iscl(x), atm_scl(x, xxx))}

#' @describeIn ttt_ccc. Is \code{x} a \link[ipop]{populated} \link[idtf]{dtf}?
#' @export
pop_dtf <- function(x, xxx = NULL) {length(x) > 0 & f0(inll(xxx), idtf(x), atm_dtf(x, xxx))}

#' @describeIn ttt_ccc. Is \code{x} a \link[ipop]{populated} \link[ivec]{vec}?
#' @export
pop_vec <- function(x, xxx = NULL) {length(x) > 0 & f0(inll(xxx), ivec(x), atm_vec(x, xxx))}

#' @describeIn ttt_ccc. Is \code{x} a \link[ipop]{populated} \link[ivls]{vlist}?
#' @export
pop_vls <- function(x, xxx = NULL) {length(x) > 0 & f0(inll(xxx), ivls(x), atm_vls(x, xxx))}

# cmp_ccc ####

#' @describeIn ttt_ccc. Is \code{x} a \link[icmp]{complete} \link[iarr]{array}?
#' @export
cmp_arr <- function(x) {icmp(x) & iarr(x)}

#' @describeIn ttt_ccc. Is \code{x} a \link[icmp]{complete}
#'   \link[igen]{generic}?
#' @export
cmp_gen <- function(x) {icmp(x) & igen(x)}

#' @describeIn ttt_ccc. Is \code{x} a \link[icmp]{complete} \link[imat]{matrix}?
#' @export
cmp_mat <- function(x) {icmp(x) & imat(x)}

#' @describeIn ttt_ccc. Is \code{x} a \link[icmp]{complete}
#'   \link[imvc]{multivec}?
#' @export
cmp_mvc <- function(x) {icmp(x) & imvc(x)}

#' @describeIn ttt_ccc. Is \code{x} a \link[icmp]{complete} \link[iscl]{scalar}?
#' @export
cmp_scl <- function(x) {icmp(x) & iscl(x)}

#' @describeIn ttt_ccc. Is \code{x} a \link[icmp]{complete} \link[idtf]{dtf}?
#' @export
cmp_dtf <- function(x) {icmp(x) & idtf(x)}

#' @describeIn ttt_ccc. Is \code{x} a \link[icmp]{complete} \link[ivec]{vec}?
#' @export
cmp_vec <- function(x) {icmp(x) & ivec(x)}

#' @describeIn ttt_ccc. Is \code{x} a \link[icmp]{complete} \link[ivls]{vlist}?
#' @export
cmp_vls <- function(x) {icmp(x) & ivls(x)}

# emp_ccc ####

#' @describeIn ttt_ccc. Is \code{x} an \link[iemp]{empty} \link[iarr]{array}?
#' @export
emp_arr <- function(x) {iemp(x) & iarr(x)}

#' @describeIn ttt_ccc. Is \code{x} an \link[iemp]{empty} \link[igen]{generic}?
#' @export
emp_gen <- function(x) {iemp(x) & igen(x)}

#' @describeIn ttt_ccc. Is \code{x} an \link[iemp]{empty} \link[imat]{matrix}?
#' @export
emp_mat <- function(x) {iemp(x) & imat(x)}

#' @describeIn ttt_ccc. Is \code{x} an \link[iemp]{empty} \link[imvc]{multivec}?
#' @export
emp_mvc <- function(x) {iemp(x) & imvc(x)}

#' @describeIn ttt_ccc. Is \code{x} an \link[iemp]{empty} \link[iscl]{scalar}?
#' @export
emp_scl <- function(x) {iemp(x) & iscl(x)}

#' @describeIn ttt_ccc. Is \code{x} an \link[iemp]{empty} \link[idtf]{dtf}?
#' @export
emp_dtf <- function(x) {iemp(x) & idtf(x)}

#' @describeIn ttt_ccc. Is \code{x} an \link[iemp]{empty} \link[ivec]{vec}?
#' @export
emp_vec <- function(x) {iemp(x) & ivec(x)}

#' @describeIn ttt_ccc. Is \code{x} an \link[iemp]{empty} \link[ivls]{vlist}?
#' @export
emp_vls <- function(x) {iemp(x) & ivls(x)}

# rcr_ccc ####

#' @describeIn ttt_ccc. Is \code{x} a \link[ircr]{recursive} \link[iarr]{array}?
#' @export
rcr_arr <- function(x) {ircr(x) & iarr(x)}

#' @describeIn ttt_ccc. Is \code{x} a \link[ircr]{recursive}
#'   \link[igen]{generic}?
#' @export
rcr_gen <- function(x) {ircr(x) & igen(x)}

#' @describeIn ttt_ccc. Is \code{x} a \link[ircr]{recursive}
#'   \link[imat]{matrix}?
#' @export
rcr_mat <- function(x) {ircr(x) & imat(x)}

#' @describeIn ttt_ccc. Is \code{x} a \link[ircr]{recursive}
#'   \link[imvc]{multivec}?
#' @export
rcr_mvc <- function(x) {ircr(x) & imvc(x)}

#' @describeIn ttt_ccc. Is \code{x} a \link[ircr]{recursive}
#'   \link[iscl]{scalar}?
#' @export
rcr_scl <- function(x) {ircr(x) & iscl(x)}

#' @describeIn ttt_ccc. Is \code{x} a \link[ircr]{recursive} \link[idtf]{dtf}?
#' @export
rcr_dtf <- function(x) {ircr(x) & idtf(x)}

#' @describeIn ttt_ccc. Is \code{x} a \link[ircr]{recursive} \link[ivec]{vec}?
#' @export
rcr_vec <- function(x) {ircr(x) & ivec(x)}

#' @describeIn ttt_ccc. Is \code{x} a \link[ircr]{recursive} \link[ivls]{vlist}?
#' @export
rcr_vls <- function(x) {ircr(x) & ivls(x)}
