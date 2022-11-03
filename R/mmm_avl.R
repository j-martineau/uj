#' @name mmm_avl
#' @family props
#' @title Extended Mode + Atomic vlist (mmm + avl)
#' @description Get all possible extended mode + atomic vlist properties.
#' @details See \code{\link{mmm}}, and \code{\link{atm_vls}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}
#' @export
mmm_avl_vals <- function() {
  x <- paste0("", c("atm", mmm_vals()), "_avl")
  names(x) <- rep.int("mmm_avl", length(x))
  x
}

#' @describeIn mmm_avl Is \code{x} a character atomic vlist?
#' @export
chr_avl <- function(x) {atm_vls(x, "chr")}

#' @describeIn mmm_avl Is \code{x} a onechar atomic vlist?
#' @export
ch1_avl <- function(x) {atm_vls(x, "ch1")}

#' @describeIn mmm_avl Is \code{x} a color atomic vlist?
#' @export
clr_avl <- function(x) {atm_vls(x, "clr")}

#' @describeIn mmm_avl Is \code{x} an even-numeric atomic vlist?
#' @export
evn_avl <- function(x) {atm_vls(x, "evn")}

#' @describeIn mmm_avl Is \code{x} a factor atomic vlist?
#' @export
fac_avl <- function(x) {atm_vls(x, "fac")}

#' @describeIn mmm_avl Is \code{x} a fractional-numeric atomic vlist?
#' @export
frc_avl <- function(x) {atm_vls(x, "frc")}

#' @describeIn mmm_avl Is \code{x} an indexer atomic vlist?
#' @export
ind_avl <- function(x) {atm_vls(x, "ind")}

#' @describeIn mmm_avl Is \code{x} a logical atomic vlist?
#' @export
lgl_avl <- function(x) {atm_vls(x, "lgl")}

#' @describeIn mmm_avl Is \code{x} a negative-numeric atomic vlist?
#' @export
neg_avl <- function(x) {atm_vls(x, "neg")}

#' @describeIn mmm_avl Is \code{x} a negative-whole-numeric atomic vlist?
#' @export
ngw_avl <- function(x) {atm_vls(x, "ngw")}

#' @describeIn mmm_avl Is \code{x} a non-negative-numeric atomic vlist?
#' @export
nng_avl <- function(x) {atm_vls(x, "nng")}

#' @describeIn mmm_avl Is \code{x} a non-negative-whole-numeric atomic vlist?
#' @export
nnw_avl <- function(x) {atm_vls(x, "nnw")}

#' @describeIn mmm_avl Is \code{x} a non-positive-numeric atomic vlist?
#' @export
nps_avl <- function(x) {atm_vls(x, "nps")}

#' @describeIn mmm_avl Is \code{x} a non-positive-whole-numeric atomic vlist?
#' @export
npw_avl <- function(x) {atm_vls(x, "npw")}

#' @describeIn mmm_avl Is \code{x} a non-sortable atomic vlist?
#' @export
nst_avl <- function(x) {atm_vls(x, "nst")}

#' @describeIn mmm_avl Is \code{x} a numeric atomic vlist?
#' @export
num_avl <- function(x) {atm_vls(x, "num")}

#' @describeIn mmm_avl Is \code{x} an odd-numeric atomic vlist?
#' @export
odd_avl <- function(x) {atm_vls(x, "odd")}

#' @describeIn mmm_avl Is \code{x} an ordered-factor atomic vlist?
#' @export
ord_avl <- function(x) {atm_vls(x, "ord")}

#' @describeIn mmm_avl Is \code{x} a percent-numeric atomic vlist?
#' @export
pct_avl <- function(x) {atm_vls(x, "pct")}

#' @describeIn mmm_avl Is \code{x} a positive-numeric atomic vlist?
#' @export
pos_avl <- function(x) {atm_vls(x, "pos")}

#' @describeIn mmm_avl Is \code{x} a proportion-numeric atomic vlist?
#' @export
ppn_avl <- function(x) {atm_vls(x, "ppn")}

#' @describeIn mmm_avl Is \code{x} a positive-whole-numeric atomic vlist?
#' @export
psw_avl <- function(x) {atm_vls(x, "psw")}

#' @describeIn mmm_avl Is \code{x} a sortable atomic vlist?
#' @export
srt_avl <- function(x) {atm_vls(x, "srt")}

#' @describeIn mmm_avl Is \code{x} a string atomic vlist?
#' @export
str_avl <- function(x) {atm_vls(x, "str")}

#' @describeIn mmm_avl Is \code{x} an unordered-factor atomic vlist?
#' @export
uno_avl <- function(x) {atm_vls(x, "uno")}

#' @describeIn mmm_avl Is \code{x} a whole-numeric atomic vlist?
#' @export
whl_avl <- function(x) {atm_vls(x, "whl")}
