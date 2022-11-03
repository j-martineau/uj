#' @name cmp_mmm_mvc
#' @family props
#' @title Complete + Extended Mode + Atomic Mvect (cmp + mmm + mvc)
#' @description Get all possible complete + extended mode + atomic mvect
#'   properties.
#' @details See \code{\link{mmm}}, and \code{\link{cmp_mvc}}.
#' @param x An object
#' @return \code{TRUE} or \code{FALSE}.
#' @export
cmp_mmm_mvc_vals <- function() {
  x <- paste0("cmp_", c("atm", mmm_vals()), "_mvc")
  names(x) <- rep.int("cmp_mmm_mvc", length(x))
  x
}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete atomic mvect?
#' @export
cmp_atm_mvc <- function(x) {cmp_mvc(x) & iatm(x)}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete character atomic mvect?
#' @export
cmp_chr_mvc <- function(x) {cmp_mvc(x, "chr")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete onechar atomic mvect?
#' @export
cmp_ch1_mvc <- function(x) {cmp_mvc(x, "ch1")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete color atomic mvect?
#' @export
cmp_clr_mvc <- function(x) {cmp_mvc(x, "clr")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete even-number atomic mvect?
#' @export
cmp_evn_mvc <- function(x) {cmp_mvc(x, "evn")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete factor atomic mvect?
#' @export
cmp_fac_mvc <- function(x) {cmp_mvc(x, "fac")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete fractional-number atomic
#'   mvect?
#' @export
cmp_frc_mvc <- function(x) {cmp_mvc(x, "frc")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete indexer atomic mvect?
#' @export
cmp_ind_mvc <- function(x) {cmp_mvc(x, "ind")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete logical atomic mvect?
#' @export
cmp_lgl_mvc <- function(x) {cmp_mvc(x, "lgl")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete negative-number atomic mvect?
#' @export
cmp_neg_mvc <- function(x) {cmp_mvc(x, "neg")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete negative-whole-number atomic
#'   mvect?
#' @export
cmp_ngw_mvc <- function(x) {cmp_mvc(x, "ngw")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete non-negative-number atomic
#'   mvect?
#' @export
cmp_nng_mvc <- function(x) {cmp_mvc(x, "nng")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete non-negative-whole-number
#'   atomic mvect?
#' @export
cmp_nnw_mvc <- function(x) {cmp_mvc(x, "nnw")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete non-positive-number atomic
#'   mvect?
#' @export
cmp_nps_mvc <- function(x) {cmp_mvc(x, "nps")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete non-positive-whole-number
#'   atomic mvect?
#' @export
cmp_npw_mvc <- function(x) {cmp_mvc(x, "npw")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete non-sortable atomic mvect?
#' @export
cmp_nst_mvc <- function(x) {cmp_mvc(x, "nst")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete number atomic mvect?
#' @export
cmp_num_mvc <- function(x) {cmp_mvc(x, "num")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete odd-number atomic mvect?
#' @export
cmp_odd_mvc <- function(x) {cmp_mvc(x, "odd")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete ordered-factor atomic mvect?
#' @export
cmp_ord_mvc <- function(x) {cmp_mvc(x, "ord")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete percent-number atomic mvect?
#' @export
cmp_pct_mvc <- function(x) {cmp_mvc(x, "pct")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete positive-number atomic mvect?
#' @export
cmp_pos_mvc <- function(x) {cmp_mvc(x, "pos")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete proportion-number atomic
#'   mvect?
#' @export
cmp_ppn_mvc <- function(x) {cmp_mvc(x, "ppn")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete positive-whole-number atomic
#'   mvect?
#' @export
cmp_psw_mvc <- function(x) {cmp_mvc(x, "psw")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete sortable atomic mvect?
#' @export
cmp_srt_mvc <- function(x) {cmp_mvc(x, "srt")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete string atomic mvect?
#' @export
cmp_str_mvc <- function(x) {cmp_mvc(x, "str")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete unordered-factor atomic mvect?
#' @export
cmp_uno_mvc <- function(x) {cmp_mvc(x, "uno")}

#' @describeIn cmp_mmm_mvc Is \code{x} a complete whole-number atomic mvect?
#' @export
cmp_whl_mvc <- function(x) {cmp_mvc(x, "whl")}
