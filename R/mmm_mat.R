#' @name mmm_mat
#' @family props
#' @title Extended Mode + Atomic matrix (mmm + mat)
#' @description See \code{\link{mmm}}, and \code{\link{is_atm_matrix}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
mmm_mat_vals <- function() {
  x <- paste0("", c("atm", mmm_vals()), "_mat")
  names(x) <- rep.int("mmm_mat", length(x))
  x
}

#' @rdname mmm_mat
#' @export
atm_mat <- function(x) {is_atm_matrix(x)}

#' @rdname mmm_mat
#' @export
chr_mat <- function(x) {is_atm_matrix(x, "chr")}

#' @rdname mmm_mat
#' @export
ch1_mat <- function(x) {is_atm_matrix(x, "ch1")}

#' @rdname mmm_mat
#' @export
clr_mat <- function(x) {is_atm_matrix(x, "clr")}

#' @rdname mmm_mat
#' @export
evn_mat <- function(x) {is_atm_matrix(x, "evn")}

#' @rdname mmm_mat
#' @export
fac_mat <- function(x) {is_atm_matrix(x, "fac")}

#' @rdname mmm_mat
#' @export
frc_mat <- function(x) {is_atm_matrix(x, "frc")}

#' @rdname mmm_mat
#' @export
ind_mat <- function(x) {is_atm_matrix(x, "ind")}

#' @rdname mmm_mat
#' @export
lgc_mat <- function(x) {is_atm_matrix(x, "lgc")}

#' @rdname mmm_mat
#' @export
cnb_mat <- function(x) {is_atm_matrix(x, "cnb")}

#' @rdname mmm_mat
#' @export
neg_mat <- function(x) {is_atm_matrix(x, "neg")}

#' @rdname mmm_mat
#' @export
ngw_mat <- function(x) {is_atm_matrix(x, "ngw")}

#' @rdname mmm_mat
#' @export
nng_mat <- function(x) {is_atm_matrix(x, "nng")}

#' @rdname mmm_mat
#' @export
nnw_mat <- function(x) {is_atm_matrix(x, "nnw")}

#' @rdname mmm_mat
#' @export
nps_mat <- function(x) {is_atm_matrix(x, "nps")}

#' @rdname mmm_mat
#' @export
npw_mat <- function(x) {is_atm_matrix(x, "npw")}

#' @rdname mmm_mat
#' @export
nst_mat <- function(x) {is_atm_matrix(x, "nst")}

#' @rdname mmm_mat
#' @export
num_mat <- function(x) {is_atm_matrix(x, "num")}

#' @rdname mmm_mat
#' @export
odd_mat <- function(x) {is_atm_matrix(x, "odd")}

#' @rdname mmm_mat
#' @export
ord_mat <- function(x) {is_atm_matrix(x, "ord")}

#' @rdname mmm_mat
#' @export
pct_mat <- function(x) {is_atm_matrix(x, "pct")}

#' @rdname mmm_mat
#' @export
pos_mat <- function(x) {is_atm_matrix(x, "pos")}

#' @rdname mmm_mat
#' @export
ppn_mat <- function(x) {is_atm_matrix(x, "ppn")}

#' @rdname mmm_mat
#' @export
psw_mat <- function(x) {is_atm_matrix(x, "psw")}

#' @rdname mmm_mat
#' @export
srt_mat <- function(x) {is_atm_matrix(x, "srt")}

#' @rdname mmm_mat
#' @export
str_mat <- function(x) {is_atm_matrix(x, "str")}

#' @rdname mmm_mat
#' @export
uno_mat <- function(x) {is_atm_matrix(x, "uno")}

#' @rdname mmm_mat
#' @export
whl_mat <- function(x) {is_atm_matrix(x, "whl")}
