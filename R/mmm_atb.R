#' @name mmm_atb
#' @family props
#' @title Extended Mode + Atomic tibble (mmm + atb)
#' @description Get all possible extended mode + atomic tibble properties.
#' @details See \code{\link{mmm}} and \code{\link{atm_tbl}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
mmm_atb_vals <- function() {
  x <- paste0(c("atm", mmm_vals()), "_atb")
  names(x) <- rep.int("mmm_atb", length(x))
  x
}

#' @describeIn mmm_atb Is \code{x} a character atomic tibble?
#' @export
chr_atb <- function(x) {atm_tbl(x, "chr")}

#' @describeIn mmm_atb Is \code{x} a onechar atomic tibble?
#' @export
ch1_atb <- function(x) {atm_tbl(x, "ch1")}

#' @describeIn mmm_atb Is \code{x} a color atomic tibble?
#' @export
clr_atb <- function(x) {atm_tbl(x, "clr")}

#' @describeIn mmm_atb Is \code{x} a even-numeric atomic tibble?
#' @export
evn_atb <- function(x) {atm_tbl(x, "evn")}

#' @describeIn mmm_atb Is \code{x} a factor atomic tibble?
#' @export
fac_atb <- function(x) {atm_tbl(x, "fac")}

#' @describeIn mmm_atb Is \code{x} a fractional-numeric atomic tibble?
#' @export
frc_atb <- function(x) {atm_tbl(x, "frc")}

#' @describeIn mmm_atb Is \code{x} an indexer atomic tibble?
#' @export
ind_atb <- function(x) {atm_tbl(x, "ind")}

#' @describeIn mmm_atb Is \code{x} a logical atomic tibble?
#' @export
lgl_atb <- function(x) {atm_tbl(x, "lgl")}

#' @describeIn mmm_atb Is \code{x} a negative-numeric atomic tibble?
#' @export
neg_atb <- function(x) {atm_tbl(x, "neg")}

#' @describeIn mmm_atb Is \code{x} a non-negative-whole-numeric atomic tibble?
#' @export
ngw_atb <- function(x) {atm_tbl(x, "ngw")}

#' @describeIn mmm_atb Is \code{x} a non-negative-numeric atomic tibble?
#' @export
nng_atb <- function(x) {atm_tbl(x, "nng")}

#' @describeIn mmm_atb Is \code{x} a non-negative-whole-numeric atomic tibble?
#' @export
nnw_atb <- function(x) {atm_tbl(x, "nnw")}

#' @describeIn mmm_atb Is \code{x} a non-positive-numeric atomic tibble?
#' @export
nps_atb <- function(x) {atm_tbl(x, "nps")}

#' @describeIn mmm_atb Is \code{x} a non-positive-whole-numeric atomic tibble?
#' @export
npw_atb <- function(x) {atm_tbl(x, "npw")}

#' @describeIn mmm_atb Is \code{x} a non-sortable atomic tibble?
#' @export
nst_atb <- function(x) {atm_tbl(x, "nst")}

#' @describeIn mmm_atb Is \code{x} a numeric atomic tibble?
#' @export
num_atb <- function(x) {atm_tbl(x, "num")}

#' @describeIn mmm_atb Is \code{x} an odd-numeric atomic tibble?
#' @export
odd_atb <- function(x) {atm_tbl(x, "odd")}

#' @describeIn mmm_atb Is \code{x} an ordered-factor atomic tibble?
#' @export
ord_atb <- function(x) {atm_tbl(x, "ord")}

#' @describeIn mmm_atb Is \code{x} a percent-numeric atomic tibble?
#' @export
pct_atb <- function(x) {atm_tbl(x, "pct")}

#' @describeIn mmm_atb Is \code{x} a positive-numeric atomic tibble?
#' @export
pos_atb <- function(x) {atm_tbl(x, "pos")}

#' @describeIn mmm_atb Is \code{x} a positive-whole-numeric atomic tibble?
#' @export
psw_atb <- function(x) {atm_tbl(x, "psw")}

#' @describeIn mmm_atb Is \code{x} a proportion-numeric atomic tibble?
#' @export
ppn_atb <- function(x) {atm_tbl(x, "ppn")}

#' @describeIn mmm_atb Is \code{x} a sortable atomic tibble?
#' @export
srt_atb <- function(x) {atm_tbl(x, "srt")}

#' @describeIn mmm_atb Is \code{x} a string atomic tibble?
#' @export
str_atb <- function(x) {atm_tbl(x, "str")}

#' @describeIn mmm_atb Is \code{x} an unordered-factor atomic tibble?
#' @export
uno_atb <- function(x) {atm_tbl(x, "uno")}

#' @describeIn mmm_atb Is \code{x} a whole-numeric atomic tibble?
#' @export
whl_atb <- function(x) {atm_tbl(x, "whl")}
