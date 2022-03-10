#' @name xmd_scl
#' @family props
#' @title xmd + Atomic scalar
#' @description See \code{\link{xmd}}, and \code{\link{is_atm_scalar}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
xmd_scl_vals <- function() {
  x <- paste0("", c("atm", xmd_vals()), "_scl")
  names(x) <- rep.int("xmd_scl", length(x))
  x
}

#' @rdname xmd_scl
#' @export
atm_scl <- function(x) {is_atm_scalar(x)}

#' @rdname xmd_scl
#' @export
chr_scl <- function(x) {is_atm_scalar(x, "chr")}

#' @rdname xmd_scl
#' @export
ch1_scl <- function(x) {is_atm_scalar(x, "ch1")}

#' @rdname xmd_scl
#' @export
clr_scl <- function(x) {is_atm_scalar(x, "clr")}

#' @rdname xmd_scl
#' @export
evn_scl <- function(x) {is_atm_scalar(x, "evn")}

#' @rdname xmd_scl
#' @export
fac_scl <- function(x) {is_atm_scalar(x, "fac")}

#' @rdname xmd_scl
#' @export
frc_scl <- function(x) {is_atm_scalar(x, "frc")}

#' @rdname xmd_scl
#' @export
ind_scl <- function(x) {is_atm_scalar(x, "ind")}

#' @rdname xmd_scl
#' @export
lgc_scl <- function(x) {is_atm_scalar(x, "lgc")}

#' @rdname xmd_scl
#' @export
cnb_scl <- function(x) {is_atm_scalar(x, "cnb")}

#' @rdname xmd_scl
#' @export
neg_scl <- function(x) {is_atm_scalar(x, "neg")}

#' @rdname xmd_scl
#' @export
ngw_scl <- function(x) {is_atm_scalar(x, "ngw")}

#' @rdname xmd_scl
#' @export
nng_scl <- function(x) {is_atm_scalar(x, "nng")}

#' @rdname xmd_scl
#' @export
nnw_scl <- function(x) {is_atm_scalar(x, "nnw")}

#' @rdname xmd_scl
#' @export
nps_scl <- function(x) {is_atm_scalar(x, "nps")}

#' @rdname xmd_scl
#' @export
npw_scl <- function(x) {is_atm_scalar(x, "npw")}

#' @rdname xmd_scl
#' @export
nst_scl <- function(x) {is_atm_scalar(x, "nst")}

#' @rdname xmd_scl
#' @export
num_scl <- function(x) {is_atm_scalar(x, "num")}

#' @rdname xmd_scl
#' @export
odd_scl <- function(x) {is_atm_scalar(x, "odd")}

#' @rdname xmd_scl
#' @export
ord_scl <- function(x) {is_atm_scalar(x, "ord")}

#' @rdname xmd_scl
#' @export
pct_scl <- function(x) {is_atm_scalar(x, "pct")}

#' @rdname xmd_scl
#' @export
pos_scl <- function(x) {is_atm_scalar(x, "pos")}

#' @rdname xmd_scl
#' @export
ppn_scl <- function(x) {is_atm_scalar(x, "ppn")}

#' @rdname xmd_scl
#' @export
psw_scl <- function(x) {is_atm_scalar(x, "psw")}

#' @rdname xmd_scl
#' @export
srt_scl <- function(x) {is_atm_scalar(x, "srt")}

#' @rdname xmd_scl
#' @export
chr_str <- function(x) {is_atm_scalar(x, "str")}

#' @rdname xmd_scl
#' @export
uno_scl <- function(x) {is_atm_scalar(x, "uno")}

#' @rdname xmd_scl
#' @export
whl_scl <- function(x) {is_atm_scalar(x, "whl")}
