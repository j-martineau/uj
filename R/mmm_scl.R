#' @name mmm_scl
#' @family props
#' @title Extended Mode + Atomic scalar (mmm + scl)
#' @description See \code{\link{mmm}}, and \code{\link{is_atm_scalar}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
mmm_scl_vals <- function() {
  x <- paste0("", c("atm", mmm_vals()), "_scl")
  names(x) <- rep.int("mmm_scl", length(x))
  x
}

#' @rdname mmm_scl
#' @export
atm_scl <- function(x) {is_atm_scalar(x)}

#' @rdname mmm_scl
#' @export
chr_scl <- function(x) {is_atm_scalar(x, "chr")}

#' @rdname mmm_scl
#' @export
ch1_scl <- function(x) {is_atm_scalar(x, "ch1")}

#' @rdname mmm_scl
#' @export
clr_scl <- function(x) {is_atm_scalar(x, "clr")}

#' @rdname mmm_scl
#' @export
evn_scl <- function(x) {is_atm_scalar(x, "evn")}

#' @rdname mmm_scl
#' @export
fac_scl <- function(x) {is_atm_scalar(x, "fac")}

#' @rdname mmm_scl
#' @export
frc_scl <- function(x) {is_atm_scalar(x, "frc")}

#' @rdname mmm_scl
#' @export
ind_scl <- function(x) {is_atm_scalar(x, "ind")}

#' @rdname mmm_scl
#' @export
lgc_scl <- function(x) {is_atm_scalar(x, "lgc")}

#' @rdname mmm_scl
#' @export
cnb_scl <- function(x) {is_atm_scalar(x, "cnb")}

#' @rdname mmm_scl
#' @export
neg_scl <- function(x) {is_atm_scalar(x, "neg")}

#' @rdname mmm_scl
#' @export
ngw_scl <- function(x) {is_atm_scalar(x, "ngw")}

#' @rdname mmm_scl
#' @export
nng_scl <- function(x) {is_atm_scalar(x, "nng")}

#' @rdname mmm_scl
#' @export
nnw_scl <- function(x) {is_atm_scalar(x, "nnw")}

#' @rdname mmm_scl
#' @export
nps_scl <- function(x) {is_atm_scalar(x, "nps")}

#' @rdname mmm_scl
#' @export
npw_scl <- function(x) {is_atm_scalar(x, "npw")}

#' @rdname mmm_scl
#' @export
nst_scl <- function(x) {is_atm_scalar(x, "nst")}

#' @rdname mmm_scl
#' @export
num_scl <- function(x) {is_atm_scalar(x, "num")}

#' @rdname mmm_scl
#' @export
odd_scl <- function(x) {is_atm_scalar(x, "odd")}

#' @rdname mmm_scl
#' @export
ord_scl <- function(x) {is_atm_scalar(x, "ord")}

#' @rdname mmm_scl
#' @export
pct_scl <- function(x) {is_atm_scalar(x, "pct")}

#' @rdname mmm_scl
#' @export
pos_scl <- function(x) {is_atm_scalar(x, "pos")}

#' @rdname mmm_scl
#' @export
ppn_scl <- function(x) {is_atm_scalar(x, "ppn")}

#' @rdname mmm_scl
#' @export
psw_scl <- function(x) {is_atm_scalar(x, "psw")}

#' @rdname mmm_scl
#' @export
srt_scl <- function(x) {is_atm_scalar(x, "srt")}

#' @rdname mmm_scl
#' @export
chr_str <- function(x) {is_atm_scalar(x, "str")}

#' @rdname mmm_scl
#' @export
uno_scl <- function(x) {is_atm_scalar(x, "uno")}

#' @rdname mmm_scl
#' @export
whl_scl <- function(x) {is_atm_scalar(x, "whl")}
