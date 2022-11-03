#' @name mmm_arr
#' @family props
#' @title Extended Mode + Atomic array (mmm + arr)
#' @description Get all possible extended mode + array properties.
#' @details See \code{\link{mmm}} and \code{\link{is_atm_arr}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
mmm_arr_vals <- function() {
  x <- paste0(c("atm", mmm_vals()), "_arr")
  names(x) <- rep.int("mmm_arr", length(x))
  x
}

#' @describeIn mmm_arr Is \code{x} a character atomic array?
#' @export
chr_arr <- function(x) {atm_arr(x, "chr")}

#' @describeIn mmm_arr Is \code{x} a onechar atomic array?
#' @export
ch1_arr <- function(x) {atm_arr(x, "ch1")}

#' @describeIn mmm_arr Is \code{x} a color atomic array?
#' @export
clr_arr <- function(x) {atm_arr(x, "clr")}

#' @describeIn mmm_arr Is \code{x} an even-numeric atomic array?
#' @export
evn_arr <- function(x) {atm_arr(x, "evn")}

#' @describeIn mmm_arr Is \code{x} a factor atomic array?
#' @export
fac_arr <- function(x) {atm_arr(x, "fac")}

#' @describeIn mmm_arr Is \code{x} a fractional-numeric atomic array?
#' @export
frc_arr <- function(x) {atm_arr(x, "frc")}

#' @describeIn mmm_arr Is \code{x} an indexer atomic array?
#' @export
ind_arr <- function(x) {atm_arr(x, "ind")}

#' @describeIn mmm_arr Is \code{x} a logical atomic array?
#' @export
lgl_arr <- function(x) {atm_arr(x, "lgl")}

#' @describeIn mmm_arr Is \code{x} a negative-numeric atomic array?
#' @export
neg_arr <- function(x) {atm_arr(x, "neg")}

#' @describeIn mmm_arr Is \code{x} a negative-whole-numeric atomic array?
#' @export
ngw_arr <- function(x) {atm_arr(x, "ngw")}

#' @describeIn mmm_arr Is \code{x} a non-negative-numeric atomic array?
#' @export
nng_arr <- function(x) {atm_arr(x, "nng")}

#' @describeIn mmm_arr Is \code{x} a non-negative-whole atomic array?
#' @export
nnw_arr <- function(x) {atm_arr(x, "nnw")}

#' @describeIn mmm_arr Is \code{x} a non-positive-numeric atomic array?
#' @export
nps_arr <- function(x) {atm_arr(x, "nps")}

#' @describeIn mmm_arr Is \code{x} a non-positive-whole-numeric atomic array?
#' @export
npw_arr <- function(x) {atm_arr(x, "npw")}

#' @describeIn mmm_arr Is \code{x} a non-sortable atomic array?
#' @export
nst_arr <- function(x) {atm_arr(x, "nst")}

#' @describeIn mmm_arr Is \code{x} a numeric atomic array?
#' @export
num_arr <- function(x) {atm_arr(x, "num")}

#' @describeIn mmm_arr Is \code{x} an odd-numeric atomic array?
#' @export
odd_arr <- function(x) {atm_arr(x, "odd")}

#' @describeIn mmm_arr Is \code{x} an ordered-factor atomic array?
#' @export
ord_arr <- function(x) {atm_arr(x, "ord")}

#' @describeIn mmm_arr Is \code{x} a percent-numeric atomic array?
#' @export
pct_arr <- function(x) {atm_arr(x, "pct")}

#' @describeIn mmm_arr Is \code{x} a positive-numeric atomic array?
#' @export
pos_arr <- function(x) {atm_arr(x, "pos")}

#' @describeIn mmm_arr Is \code{x} a positive-whole-numeric atomic array?
#' @export
psw_arr <- function(x) {atm_arr(x, "psw")}

#' @describeIn mmm_arr Is \code{x} a proportion-numeric atomic array?
#' @export
ppn_arr <- function(x) {atm_arr(x, "ppn")}

#' @describeIn mmm_arr Is \code{x} a sortable atomic array?
#' @export
srt_arr <- function(x) {atm_arr(x, "srt")}

#' @describeIn mmm_arr Is \code{x} a string atomic array?
#' @export
str_arr <- function(x) {atm_arr(x, "str")}

#' @describeIn mmm_arr Is \code{x} a unordered-factor atomic array?
#' @export
uno_arr <- function(x) {atm_arr(x, "uno")}

#' @describeIn mmm_arr Is \code{x} a whole-numeric atomic array?
#' @export
whl_arr <- function(x) {atm_arr(x, "whl")}
