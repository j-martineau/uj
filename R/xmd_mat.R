#' @name xmd_mat
#' @family props
#' @title xmd + Atomic matrix
#' @description See \code{\link{xmd}}, and \code{\link{is_atm_matrix}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
xmd_mat_vals <- function() {
  x <- paste0("", c("atm", xmd_vals()), "_mat")
  names(x) <- rep.int("xmd_mat", length(x))
  x
}

#' @rdname xmd_mat
#' @export
atm_mat <- function(x) {is_atm_matrix(x)}

#' @rdname xmd_mat
#' @export
chr_mat <- function(x) {is_atm_matrix(x, "chr")}

#' @rdname xmd_mat
#' @export
ch1_mat <- function(x) {is_atm_matrix(x, "ch1")}

#' @rdname xmd_mat
#' @export
clr_mat <- function(x) {is_atm_matrix(x, "clr")}

#' @rdname xmd_mat
#' @export
evn_mat <- function(x) {is_atm_matrix(x, "evn")}

#' @rdname xmd_mat
#' @export
fac_mat <- function(x) {is_atm_matrix(x, "fac")}

#' @rdname xmd_mat
#' @export
frc_mat <- function(x) {is_atm_matrix(x, "frc")}

#' @rdname xmd_mat
#' @export
ind_mat <- function(x) {is_atm_matrix(x, "ind")}

#' @rdname xmd_mat
#' @export
lgc_mat <- function(x) {is_atm_matrix(x, "lgc")}

#' @rdname xmd_mat
#' @export
cnb_mat <- function(x) {is_atm_matrix(x, "cnb")}

#' @rdname xmd_mat
#' @export
neg_mat <- function(x) {is_atm_matrix(x, "neg")}

#' @rdname xmd_mat
#' @export
ngw_mat <- function(x) {is_atm_matrix(x, "ngw")}

#' @rdname xmd_mat
#' @export
nng_mat <- function(x) {is_atm_matrix(x, "nng")}

#' @rdname xmd_mat
#' @export
nnw_mat <- function(x) {is_atm_matrix(x, "nnw")}

#' @rdname xmd_mat
#' @export
nps_mat <- function(x) {is_atm_matrix(x, "nps")}

#' @rdname xmd_mat
#' @export
npw_mat <- function(x) {is_atm_matrix(x, "npw")}

#' @rdname xmd_mat
#' @export
nst_mat <- function(x) {is_atm_matrix(x, "nst")}

#' @rdname xmd_mat
#' @export
num_mat <- function(x) {is_atm_matrix(x, "num")}

#' @rdname xmd_mat
#' @export
odd_mat <- function(x) {is_atm_matrix(x, "odd")}

#' @rdname xmd_mat
#' @export
ord_mat <- function(x) {is_atm_matrix(x, "ord")}

#' @rdname xmd_mat
#' @export
pct_mat <- function(x) {is_atm_matrix(x, "pct")}

#' @rdname xmd_mat
#' @export
pos_mat <- function(x) {is_atm_matrix(x, "pos")}

#' @rdname xmd_mat
#' @export
ppn_mat <- function(x) {is_atm_matrix(x, "ppn")}

#' @rdname xmd_mat
#' @export
psw_mat <- function(x) {is_atm_matrix(x, "psw")}

#' @rdname xmd_mat
#' @export
srt_mat <- function(x) {is_atm_matrix(x, "srt")}

#' @rdname xmd_mat
#' @export
str_mat <- function(x) {is_atm_matrix(x, "str")}

#' @rdname xmd_mat
#' @export
uno_mat <- function(x) {is_atm_matrix(x, "uno")}

#' @rdname xmd_mat
#' @export
whl_mat <- function(x) {is_atm_matrix(x, "whl")}
