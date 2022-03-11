#' @name mmm_avt
#' @family props
#' @title Extended Mode + Atomic vtype (mmm + avt)
#' @description See \code{\link{mmm}}, and \code{\link{is_atm_vtype}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
mmm_avt_vals <- function() {
  x <- paste0("", c("atm", mmm_vals()), "_avt")
  names(x) <- rep.int("mmm_avt", length(x))
  x
}

#' @rdname mmm_avt
#' @export
atm_avt <- function(x) {is_atm_vtype(x)}

#' @rdname mmm_avt
#' @export
chr_avt <- function(x) {is_atm_vtype(x, "chr")}

#' @rdname mmm_avt
#' @export
ch1_avt <- function(x) {is_atm_vtype(x, "ch1")}

#' @rdname mmm_avt
#' @export
clr_avt <- function(x) {is_atm_vtype(x, "clr")}

#' @rdname mmm_avt
#' @export
evn_avt <- function(x) {is_atm_vtype(x, "evn")}

#' @rdname mmm_avt
#' @export
fac_avt <- function(x) {is_atm_vtype(x, "fac")}

#' @rdname mmm_avt
#' @export
frc_avt <- function(x) {is_atm_vtype(x, "frc")}

#' @rdname mmm_avt
#' @export
ind_avt <- function(x) {is_atm_vtype(x, "ind")}

#' @rdname mmm_avt
#' @export
lgc_avt <- function(x) {is_atm_vtype(x, "lgc")}

#' @rdname mmm_avt
#' @export
cnb_avt <- function(x) {is_atm_vtype(x, "cnb")}

#' @rdname mmm_avt
#' @export
neg_avt <- function(x) {is_atm_vtype(x, "neg")}

#' @rdname mmm_avt
#' @export
ngw_avt <- function(x) {is_atm_vtype(x, "ngw")}

#' @rdname mmm_avt
#' @export
nng_avt <- function(x) {is_atm_vtype(x, "nng")}

#' @rdname mmm_avt
#' @export
nnw_avt <- function(x) {is_atm_vtype(x, "nnw")}

#' @rdname mmm_avt
#' @export
nps_avt <- function(x) {is_atm_vtype(x, "nps")}

#' @rdname mmm_avt
#' @export
npw_avt <- function(x) {is_atm_vtype(x, "npw")}

#' @rdname mmm_avt
#' @export
nst_avt <- function(x) {is_atm_vtype(x, "nst")}

#' @rdname mmm_avt
#' @export
num_avt <- function(x) {is_atm_vtype(x, "num")}

#' @rdname mmm_avt
#' @export
odd_avt <- function(x) {is_atm_vtype(x, "odd")}

#' @rdname mmm_avt
#' @export
ord_avt <- function(x) {is_atm_vtype(x, "ord")}

#' @rdname mmm_avt
#' @export
pct_avt <- function(x) {is_atm_vtype(x, "pct")}

#' @rdname mmm_avt
#' @export
pos_avt <- function(x) {is_atm_vtype(x, "pos")}

#' @rdname mmm_avt
#' @export
ppn_avt <- function(x) {is_atm_vtype(x, "ppn")}

#' @rdname mmm_avt
#' @export
psw_avt <- function(x) {is_atm_vtype(x, "psw")}

#' @rdname mmm_avt
#' @export
srt_avt <- function(x) {is_atm_vtype(x, "srt")}

#' @rdname mmm_avt
#' @export
str_avt <- function(x) {is_atm_vtype(x, "str")}

#' @rdname mmm_avt
#' @export
uno_avt <- function(x) {is_atm_vtype(x, "uno")}

#' @rdname mmm_avt
#' @export
whl_avt <- function(x) {is_atm_vtype(x, "whl")}
