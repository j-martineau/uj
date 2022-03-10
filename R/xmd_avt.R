#' @name xmd_avt
#' @family props
#' @title xmd + atomic vtype
#' @description See \code{\link{xmd}}, and \code{\link{is_atm_vtype}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
xmd_avt_vals <- function() {
  x <- paste0("", c("atm", xmd_vals()), "_avt")
  names(x) <- rep.int("xmd_avt", length(x))
  x
}

#' @rdname xmd_avt
#' @export
atm_avt <- function(x) {is_atm_vtype(x)}

#' @rdname xmd_avt
#' @export
chr_avt <- function(x) {is_atm_vtype(x, "chr")}

#' @rdname xmd_avt
#' @export
ch1_avt <- function(x) {is_atm_vtype(x, "ch1")}

#' @rdname xmd_avt
#' @export
clr_avt <- function(x) {is_atm_vtype(x, "clr")}

#' @rdname xmd_avt
#' @export
evn_avt <- function(x) {is_atm_vtype(x, "evn")}

#' @rdname xmd_avt
#' @export
fac_avt <- function(x) {is_atm_vtype(x, "fac")}

#' @rdname xmd_avt
#' @export
frc_avt <- function(x) {is_atm_vtype(x, "frc")}

#' @rdname xmd_avt
#' @export
ind_avt <- function(x) {is_atm_vtype(x, "ind")}

#' @rdname xmd_avt
#' @export
lgc_avt <- function(x) {is_atm_vtype(x, "lgc")}

#' @rdname xmd_avt
#' @export
cnb_avt <- function(x) {is_atm_vtype(x, "cnb")}

#' @rdname xmd_avt
#' @export
neg_avt <- function(x) {is_atm_vtype(x, "neg")}

#' @rdname xmd_avt
#' @export
ngw_avt <- function(x) {is_atm_vtype(x, "ngw")}

#' @rdname xmd_avt
#' @export
nng_avt <- function(x) {is_atm_vtype(x, "nng")}

#' @rdname xmd_avt
#' @export
nnw_avt <- function(x) {is_atm_vtype(x, "nnw")}

#' @rdname xmd_avt
#' @export
nps_avt <- function(x) {is_atm_vtype(x, "nps")}

#' @rdname xmd_avt
#' @export
npw_avt <- function(x) {is_atm_vtype(x, "npw")}

#' @rdname xmd_avt
#' @export
nst_avt <- function(x) {is_atm_vtype(x, "nst")}

#' @rdname xmd_avt
#' @export
num_avt <- function(x) {is_atm_vtype(x, "num")}

#' @rdname xmd_avt
#' @export
odd_avt <- function(x) {is_atm_vtype(x, "odd")}

#' @rdname xmd_avt
#' @export
ord_avt <- function(x) {is_atm_vtype(x, "ord")}

#' @rdname xmd_avt
#' @export
pct_avt <- function(x) {is_atm_vtype(x, "pct")}

#' @rdname xmd_avt
#' @export
pos_avt <- function(x) {is_atm_vtype(x, "pos")}

#' @rdname xmd_avt
#' @export
ppn_avt <- function(x) {is_atm_vtype(x, "ppn")}

#' @rdname xmd_avt
#' @export
psw_avt <- function(x) {is_atm_vtype(x, "psw")}

#' @rdname xmd_avt
#' @export
srt_avt <- function(x) {is_atm_vtype(x, "srt")}

#' @rdname xmd_avt
#' @export
str_avt <- function(x) {is_atm_vtype(x, "str")}

#' @rdname xmd_avt
#' @export
uno_avt <- function(x) {is_atm_vtype(x, "uno")}

#' @rdname xmd_avt
#' @export
whl_avt <- function(x) {is_atm_vtype(x, "whl")}
