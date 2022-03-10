#' @name xmd_mvc
#' @family props
#' @title xmd + Atomic mvect
#' @description See \code{\link{xmd}}, and \code{\link{is_atm_mvect}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
xmd_mvc_vals <- function() {
  x <- paste0("", c("atm", xmd_vals()), "_mvc")
  names(x) <- rep.int("xmd_mvc", length(x))
  x
}

#' @rdname xmd_mvc
#' @export
atm_mvc <- function(x) {is_atm_mvect(x)}

#' @rdname xmd_mvc
#' @export
chr_mvc <- function(x) {is_atm_mvect(x, "chr")}

#' @rdname xmd_mvc
#' @export
ch1_mvc <- function(x) {is_atm_mvect(x, "ch1")}

#' @rdname xmd_mvc
#' @export
clr_mvc <- function(x) {is_atm_mvect(x, "clr")}

#' @rdname xmd_mvc
#' @export
evn_mvc <- function(x) {is_atm_mvect(x, "evn")}

#' @rdname xmd_mvc
#' @export
fac_mvc <- function(x) {is_atm_mvect(x, "fac")}

#' @rdname xmd_mvc
#' @export
frc_mvc <- function(x) {is_atm_mvect(x, "frc")}

#' @rdname xmd_mvc
#' @export
ind_mvc <- function(x) {is_atm_mvect(x, "ind")}

#' @rdname xmd_mvc
#' @export
lgc_mvc <- function(x) {is_atm_mvect(x, "lgc")}

#' @rdname xmd_mvc
#' @export
cnb_mvc <- function(x) {is_atm_mvect(x, "cnb")}

#' @rdname xmd_mvc
#' @export
neg_mvc <- function(x) {is_atm_mvect(x, "neg")}

#' @rdname xmd_mvc
#' @export
ngw_mvc <- function(x) {is_atm_mvect(x, "ngw")}

#' @rdname xmd_mvc
#' @export
nng_mvc <- function(x) {is_atm_mvect(x, "nng")}

#' @rdname xmd_mvc
#' @export
nnw_mvc <- function(x) {is_atm_mvect(x, "nnw")}

#' @rdname xmd_mvc
#' @export
nps_mvc <- function(x) {is_atm_mvect(x, "nps")}

#' @rdname xmd_mvc
#' @export
npw_mvc <- function(x) {is_atm_mvect(x, "npw")}

#' @rdname xmd_mvc
#' @export
nst_mvc <- function(x) {is_atm_mvect(x, "nst")}

#' @rdname xmd_mvc
#' @export
num_mvc <- function(x) {is_atm_mvect(x, "num")}

#' @rdname xmd_mvc
#' @export
odd_mvc <- function(x) {is_atm_mvect(x, "odd")}

#' @rdname xmd_mvc
#' @export
ord_mvc <- function(x) {is_atm_mvect(x, "ord")}

#' @rdname xmd_mvc
#' @export
pct_mvc <- function(x) {is_atm_mvect(x, "pct")}

#' @rdname xmd_mvc
#' @export
pos_mvc <- function(x) {is_atm_mvect(x, "pos")}

#' @rdname xmd_mvc
#' @export
ppn_mvc <- function(x) {is_atm_mvect(x, "ppn")}

#' @rdname xmd_mvc
#' @export
psw_mvc <- function(x) {is_atm_mvect(x, "psw")}

#' @rdname xmd_mvc
#' @export
srt_mvc <- function(x) {is_atm_mvect(x, "srt")}

#' @rdname xmd_mvc
#' @export
str_mvc <- function(x) {is_atm_mvect(x, "str")}

#' @rdname xmd_mvc
#' @export
uno_mvc <- function(x) {is_atm_mvect(x, "uno")}

#' @rdname xmd_mvc
#' @export
whl_mvc <- function(x) {is_atm_mvect(x, "whl")}
