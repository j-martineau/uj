#' @name mmm_avt
#' @family props
#' @title Extended Mode + Atomic vtype (mmm + avt)
#' @description Get all possible extended mode + atomic vtype properties.
#' @details See \code{\link{mmm}}, and \code{\link{atm_vtp}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
mmm_avt_vals <- function() {
  x <- paste0("", c("atm", mmm_vals()), "_avt")
  names(x) <- rep.int("mmm_avt", length(x))
  x
}

#' @describeIn mmm_avt Is \code{x} a character atomic vtype?
#' @export
chr_avt <- function(x) {atm_vtp(x, "chr")}

#' @describeIn mmm_avt Is \code{x} a onechar atomic vtype?
#' @export
ch1_avt <- function(x) {atm_vtp(x, "ch1")}

#' @describeIn mmm_avt Is \code{x} a color atomic vtype?
#' @export
clr_avt <- function(x) {atm_vtp(x, "clr")}

#' @describeIn mmm_avt Is \code{x} an even-numeric atomic vtype?
#' @export
evn_avt <- function(x) {atm_vtp(x, "evn")}

#' @describeIn mmm_avt Is \code{x} a factor atomic vtype?
#' @export
fac_avt <- function(x) {atm_vtp(x, "fac")}

#' @describeIn mmm_avt Is \code{x} a fractional-numeric atomic vtype?
#' @export
frc_avt <- function(x) {atm_vtp(x, "frc")}

#' @describeIn mmm_avt Is \code{x} an indexer atomic vtype?
#' @export
ind_avt <- function(x) {atm_vtp(x, "ind")}

#' @describeIn mmm_avt Is \code{x} a logical atomic vtype?
#' @export
lgl_avt <- function(x) {atm_vtp(x, "lgl")}

#' @describeIn mmm_avt Is \code{x} a negative-numeric atomic vtype?
#' @export
neg_avt <- function(x) {atm_vtp(x, "neg")}

#' @describeIn mmm_avt Is \code{x} a non-negative-whole-numeric atomic vtype?
#' @export
ngw_avt <- function(x) {atm_vtp(x, "ngw")}

#' @describeIn mmm_avt Is \code{x} a non-negative-numeric atomic vtype?
#' @export
nng_avt <- function(x) {atm_vtp(x, "nng")}

#' @describeIn mmm_avt Is \code{x} a non-negative-whole-numeric atomic vtype?
#' @export
nnw_avt <- function(x) {atm_vtp(x, "nnw")}

#' @describeIn mmm_avt Is \code{x} a non-positive-numeric atomic vtype?
#' @export
nps_avt <- function(x) {atm_vtp(x, "nps")}

#' @describeIn mmm_avt Is \code{x} a non-positive-whole-numeric atomic vtype?
#' @export
npw_avt <- function(x) {atm_vtp(x, "npw")}

#' @describeIn mmm_avt Is \code{x} a non-sortable atomic atomic vtype?
#' @export
nst_avt <- function(x) {atm_vtp(x, "nst")}

#' @describeIn mmm_avt Is \code{x} a numeric atomic vtype?
#' @export
num_avt <- function(x) {atm_vtp(x, "num")}

#' @describeIn mmm_avt Is \code{x} an odd-numeric atomic vtype?
#' @export
odd_avt <- function(x) {atm_vtp(x, "odd")}

#' @describeIn mmm_avt Is \code{x} an ordered-factor atomic vtype?
#' @export
ord_avt <- function(x) {atm_vtp(x, "ord")}

#' @describeIn mmm_avt Is \code{x} a percent-numeric atomic vtype?
#' @export
pct_avt <- function(x) {atm_vtp(x, "pct")}

#' @describeIn mmm_avt Is \code{x} a positive-numeric atomic vtype?
#' @export
pos_avt <- function(x) {atm_vtp(x, "pos")}

#' @describeIn mmm_avt Is \code{x} a proportion-numeric atomic vtype?
#' @export
ppn_avt <- function(x) {atm_vtp(x, "ppn")}

#' @describeIn mmm_avt Is \code{x} a positive-whole-numeric atomic vtype?
#' @export
psw_avt <- function(x) {atm_vtp(x, "psw")}

#' @describeIn mmm_avt Is \code{x} a sortable atomic atomic vtype?
#' @export
srt_avt <- function(x) {atm_vtp(x, "srt")}

#' @describeIn mmm_avt Is \code{x} a string atomic vtype?
#' @export
str_avt <- function(x) {atm_vtp(x, "str")}

#' @describeIn mmm_avt Is \code{x} an unordered-factor atomic vtype?
#' @export
uno_avt <- function(x) {atm_vtp(x, "uno")}

#' @describeIn mmm_avt Is \code{x} a whole-numeric atomic vtype?
#' @export
whl_avt <- function(x) {atm_vtp(x, "whl")}
