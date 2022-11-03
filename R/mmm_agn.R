#' @name mmm_agn
#' @family props
#' @title Extended Mode + Atomic Generic (mmm + agn)
#' @description Get all possible extended mode + atomic generic properties.
#' @details See \code{\link{mmm}}, and \code{\link{atm_gen}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
mmm_agn_vals <- function() {
  x <- paste0("", c("atm", mmm_vals()), "_agn")
  names(x) <- rep.int("mmm_agn", length(x))
  x
}

#' @describeIn mmm_agn Is \code{x} a character atomic generic?
#' @export
chr_agn <- function(x) {atm_gen(x, "chr")}

#' @describeIn mmm_agn Is \code{x} a onechar atomic generic?
#' @export
ch1_agn <- function(x) {atm_gen(x, "ch1")}

#' @describeIn mmm_agn Is \code{x} a color atomic
#' @export
clr_agn <- function(x) {atm_gen(x, "clr")}

#' @describeIn mmm_agn Is \code{x} an even-numeric atomic generic?
#' @export
evn_agn <- function(x) {atm_gen(x, "evn")}

#' @describeIn mmm_agn Is \code{x} a factor atomic generic?
#' @export
fac_agn <- function(x) {atm_gen(x, "fac")}

#' @describeIn mmm_agn Is \code{x} a fractional-numeric atomic generic?
#' @export
frc_agn <- function(x) {atm_gen(x, "frc")}

#' @describeIn mmm_agn Is \code{x} an indexer atomic generic?
#' @export
ind_agn <- function(x) {atm_gen(x, "ind")}

#' @describeIn mmm_agn Is \code{x} a logical atomic generic?
#' @export
lgl_agn <- function(x) {atm_gen(x, "lgl")}

#' @describeIn mmm_agn Is \code{x} a negative-numeric atomic generic?
#' @export
neg_agn <- function(x) {atm_gen(x, "neg")}

#' @describeIn mmm_agn Is \code{x} a negative-whole-numeric atomic generic?
#' @export
ngw_agn <- function(x) {atm_gen(x, "ngw")}

#' @describeIn mmm_agn Is \code{x} a non-negative-numeric atomic generic?
#' @export
nng_agn <- function(x) {atm_gen(x, "nng")}

#' @describeIn mmm_agn Is \code{x} a non-negative-whole-numeric atomic generic?
#' @export
nnw_agn <- function(x) {atm_gen(x, "nnw")}

#' @describeIn mmm_agn Is \code{x} a non-positive-numeric atomic generic?
#' @export
nps_agn <- function(x) {atm_gen(x, "nps")}

#' @describeIn mmm_agn Is \code{x} a non-positive-whole-numeric atomic generic?
#' @export
npw_agn <- function(x) {atm_gen(x, "npw")}

#' @describeIn mmm_agn Is \code{x} a non-sortable atomic generic?
#' @export
nst_agn <- function(x) {atm_gen(x, "nst")}

#' @describeIn mmm_agn Is \code{x} a numeric atomic generic?
#' @export
num_agn <- function(x) {atm_gen(x, "num")}

#' @describeIn mmm_agn Is \code{x} an odd-numeric atomic generic?
#' @export
odd_agn <- function(x) {atm_gen(x, "odd")}

#' @describeIn mmm_agn Is \code{x} an ordered-factor atomic generic?
#' @export
ord_agn <- function(x) {atm_gen(x, "ord")}

#' @describeIn mmm_agn Is \code{x} a percent-numeric atomic generic?
#' @export
pct_agn <- function(x) {atm_gen(x, "pct")}

#' @describeIn mmm_agn Is \code{x} a positive-numeric atomic generic?
#' @export
pos_agn <- function(x) {atm_gen(x, "pos")}

#' @describeIn mmm_agn Is \code{x} a proportion-numeric atomic generic?
#' @export
ppn_agn <- function(x) {atm_gen(x, "ppn")}

#' @describeIn mmm_agn Is \code{x} a positive-whole-numeric atomic generic?
#' @export
psw_agn <- function(x) {atm_gen(x, "psw")}

#' @describeIn mmm_agn Is \code{x} a sortable atomic generic?
#' @export
srt_agn <- function(x) {atm_gen(x, "srt")}

#' @describeIn mmm_agn Is \code{x} a string atomic generic?
#' @export
str_agn <- function(x) {atm_gen(x, "str")}

#' @describeIn mmm_agn Is \code{x} an unordered-factor atomic generic?
#' @export
uno_agn <- function(x) {atm_gen(x, "uno")}

#' @describeIn mmm_agn Is \code{x} a whole-numeric atomic generic?
#' @export
whl_agn <- function(x) {atm_gen(x, "whl")}
