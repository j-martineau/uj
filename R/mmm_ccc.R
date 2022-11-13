#' @name mmm_ccc.
#' @family props
#' @title Extended mode plus extended class properties
#' @description Combinations of \link[mmm]{extended mode} and
#'   \link[ccc]{extended class}. Note that this set of functions defines
#'   atomic-mode data.frames (\code{mmm_dtf} functions) and atomic-mode vlists
#'   (\code{mmm_vls} functions). Atomic data frames are data frames without any
#'   recursive cells. Atomic vlists are vlists that do not themselves contain
#'   any elements that are recursive.
#' @param x An ℝ object.
#' @return A \link[ilgl]{logical} scalar.
#' @export
mmm_ccc. <- function() {help("mmm_ccc.", package = "uj")}

#' @describeIn mmm_ccc. Get a \link[ichr]{character} vector of all possible extended mode
#'   plus extended class properties.
#' @export
mmm_ccc_vals <- function() {
  x <- expand.grid(mmm = mmm_vals(), ccc = ccc_vals())
  x <- av(apply(x, 1, paste0, collapse = "_"))
  sort(x)
}

#' @describeIn mmm_ccc. Is \code{x} a \link[ich1]{onechar} array?
#' @export
ch1_arr <- function(x) {!iarr(x) & ich1(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ichr]{character} array?
#' @export
chr_arr <- function(x) {!iarr(x) & ichr(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[iclr]{color} array?
#' @export
clr_arr <- function(x) {!iarr(x) & iclr(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[ievn]{even whole-number} array?
#' @export
evn_arr <- function(x) {!iarr(x) & ievn(x)}

#' @describeIn mmm_ccc. Is \code{x}  \link[ifac]{factor} array?
#' @export
fac_arr <- function(x) {!iarr(x) & ifac(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifrc]{fractional numeric} array?
#' @export
frc_arr <- function(x) {!iarr(x) & ifrc(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iind]{indexer} array?
#' @export
ind_arr <- function(x) {!iarr(x) & iind(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ilgl]{logical} array?
#' @export
lgl_arr <- function(x) {!iarr(x) & ilgl(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ineg]{negative numeric} array?
#' @export
neg_arr <- function(x) {!iarr(x) & ineg(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ingw]{negative whole-number} array?
#' @export
ngw_arr <- function(x) {!iarr(x) & ingw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inng]{non-negative numeric} array?
#' @export
nng_arr <- function(x) {!iarr(x) & inng(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[innw]{non-negative whole-number} array?
#' @export
nnw_arr <- function(x) {!iarr(x) & innw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inps]{non-positive numeric} array?
#' @export
nps_arr <- function(x) {!iarr(x) & inps(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inpw]{non-positive whole-number} array?
#' @export
npw_arr <- function(x) {!iarr(x) & inpw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inst]{non-sortable} array?
#' @export
nst_arr <- function(x) {!iarr(x) & inst(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inum]{numeric} array?
#' @export
num_arr <- function(x) {!iarr(x) & inum(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iodd]{odd whole-number} array?
#' @export
odd_arr <- function(x) {!iarr(x) & iodd(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iord]{ordered-factor} array?
#' @export
ord_arr <- function(x) {!iarr(x) & iord(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipct]{percentage numeric (0-100)} array?
#' @export
pct_arr <- function(x) {!iarr(x) & ipct(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipos]{positive numeric} array?
#' @export
pos_arr <- function(x) {!iarr(x) & ipos(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ippn]{proportional numeric (0-1)} array?
#' @export
ppn_arr <- function(x) {!iarr(x) & ippn(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipsw]{positive whole-number} array?
#' @export
psw_arr <- function(x) {!iarr(x) & ipsw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[isrt]{sortable} array?
#' @export
srt_arr <- function(x) {!iarr(x) & isrt(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[istr]{string} array?
#' @export
str_arr <- function(x) {!iarr(x) & istr(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iuno]{unordered-factor} array?
#' @export
uno_arr <- function(x) {!iarr(x) & iuno(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[iwhl]{whole-number} array?
#' @export
whl_arr <- function(x) {!iarr(x) & iwhl(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ich1]{onechar} data.frame?
#' @export
ch1_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ich1(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[ichr]{character} data.frame?
#' @export
chr_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ichr(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[iclr]{color} data.frame?
#' @export
clr_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, iclr(x)))}}

#' @describeIn mmm_ccc. Is \code{x} an \link[ievn]{even whole-number} data.frame?
#' @export
evn_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ievn(x)))}}

#' @describeIn mmm_ccc. Is \code{x}  \link[ifac]{factor} data.frame?
#' @export
fac_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ifac(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifrc]{fractional numeric} data.frame?
#' @export
frc_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ifrc(x)))}}

#' @describeIn mmm_ccc. Is \code{x} an \link[iind]{indexer} data.frame?
#' @export
ind_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, iind(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[ilgl]{logical} data.frame?
#' @export
lgl_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ilgl(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[ineg]{negative numeric} data.frame?
#' @export
neg_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ineg(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[ingw]{negative whole-number} data.frame?
#' @export
ngw_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ingw(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[inng]{non-negative numeric} data.frame?
#' @export
nng_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, inng(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[innw]{non-negative whole-number} data.frame?
#' @export
nnw_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, innw(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[inps]{non-positive numeric} data.frame?
#' @export
nps_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, inps(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[inpw]{non-positive whole-number} data.frame?
#' @export
npw_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, inpw(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[inst]{non-sortable} data.frame?
#' @export
nst_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, inst(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[inum]{numeric} data.frame?
#' @export
num_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, inum(x)))}}

#' @describeIn mmm_ccc. Is \code{x} an \link[iodd]{odd whole-number} data.frame?
#' @export
odd_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, iodd(x)))}}

#' @describeIn mmm_ccc. Is \code{x} an \link[iord]{ordered-factor} data.frame?
#' @export
ord_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, iord(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipct]{percentage numeric (0-100)} data.frame?
#' @export
pct_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ipct(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipos]{positive numeric} data.frame?
#' @export
pos_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ipos(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[ippn]{proportional numeric (0-1)} data.frame?
#' @export
ppn_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ippn(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipsw]{positive whole-number} data.frame?
#' @export
psw_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ipsw(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[isrt]{sortable} data.frame?
#' @export
srt_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, isrt(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[istr]{string} data.frame?
#' @export
str_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, istr(x)))}}

#' @describeIn mmm_ccc. Is \code{x} an \link[iuno]{unordered-factor} data.frame?
#' @export
uno_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, iuno(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[iwhl]{whole-number} data.frame?
#' @export
whl_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, iwhl(x)))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[ich1]{onechar} generic?
#' @export
ch1_gen <- function(x) {!igen(x) & ich1(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ichr]{character} generic?
#' @export
chr_gen <- function(x) {!igen(x) & ichr(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[iclr]{color} generic?
#' @export
clr_gen <- function(x) {!igen(x) & iclr(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[ievn]{even whole-number} generic?
#' @export
evn_gen <- function(x) {!igen(x) & ievn(x)}

#' @describeIn mmm_ccc. Is \code{x}  \link[ifac]{factor} generic?
#' @export
fac_gen <- function(x) {!igen(x) & ifac(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifrc]{fractional numeric} generic?
#' @export
frc_gen <- function(x) {!igen(x) & ifrc(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iind]{indexer} generic?
#' @export
ind_gen <- function(x) {!igen(x) & iind(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ilgl]{logical} generic?
#' @export
lgl_gen <- function(x) {!igen(x) & ilgl(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ineg]{negative numeric} generic?
#' @export
neg_gen <- function(x) {!igen(x) & ineg(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ingw]{negative whole-number} generic?
#' @export
ngw_gen <- function(x) {!igen(x) & ingw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inng]{non-negative numeric} generic?
#' @export
nng_gen <- function(x) {!igen(x) & inng(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[innw]{non-negative whole-number} generic?
#' @export
nnw_gen <- function(x) {!igen(x) & innw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inps]{non-positive numeric} generic?
#' @export
nps_gen <- function(x) {!igen(x) & inps(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inpw]{non-positive whole-number} generic?
#' @export
npw_gen <- function(x) {!igen(x) & inpw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inst]{non-sortable} generic?
#' @export
nst_gen <- function(x) {!igen(x) & inst(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inum]{numeric} generic?
#' @export
num_gen <- function(x) {!igen(x) & inum(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iodd]{odd whole-number} generic?
#' @export
odd_gen <- function(x) {!igen(x) & iodd(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iord]{ordered-factor} generic?
#' @export
ord_gen <- function(x) {!igen(x) & iord(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipct]{percentage numeric (0-100)} generic?
#' @export
pct_gen <- function(x) {!igen(x) & ipct(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipos]{positive numeric} generic?
#' @export
pos_gen <- function(x) {!igen(x) & ipos(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ippn]{proportional numeric (0-1)} generic?
#' @export
ppn_gen <- function(x) {!igen(x) & ippn(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipsw]{positive whole-number} generic?
#' @export
psw_gen <- function(x) {!igen(x) & ipsw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[isrt]{sortable} generic?
#' @export
srt_gen <- function(x) {!igen(x) & isrt(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[istr]{string} generic?
#' @export
str_gen <- function(x) {!igen(x) & istr(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iuno]{unordered-factor} generic?
#' @export
uno_gen <- function(x) {!igen(x) & iuno(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[iwhl]{whole-number} generic?
#' @export
whl_gen <- function(x) {!igen(x) & iwhl(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ich1]{onechar} matrix?
#' @export
ch1_mat <- function(x) {!imat(x) & ich1(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ichr]{character} matrix?
#' @export
chr_mat <- function(x) {!imat(x) & ichr(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[iclr]{color} matrix?
#' @export
clr_mat <- function(x) {!imat(x) & iclr(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[ievn]{even whole-number} matrix?
#' @export
evn_mat <- function(x) {!imat(x) & ievn(x)}

#' @describeIn mmm_ccc. Is \code{x}  \link[ifac]{factor} matrix?
#' @export
fac_mat <- function(x) {!imat(x) & ifac(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifrc]{fractional numeric} matrix?
#' @export
frc_mat <- function(x) {!imat(x) & ifrc(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iind]{indexer} matrix?
#' @export
ind_mat <- function(x) {!imat(x) & iind(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ilgl]{logical} matrix?
#' @export
lgl_mat <- function(x) {!imat(x) & ilgl(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ineg]{negative numeric} matrix?
#' @export
neg_mat <- function(x) {!imat(x) & ineg(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ingw]{negative whole-number} matrix?
#' @export
ngw_mat <- function(x) {!imat(x) & ingw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inng]{non-negative numeric} matrix?
#' @export
nng_mat <- function(x) {!imat(x) & inng(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[innw]{non-negative whole-number} matrix?
#' @export
nnw_mat <- function(x) {!imat(x) & innw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inps]{non-positive numeric} matrix?
#' @export
nps_mat <- function(x) {!imat(x) & inps(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inpw]{non-positive whole-number} matrix?
#' @export
npw_mat <- function(x) {!imat(x) & inpw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inst]{non-sortable} matrix?
#' @export
nst_mat <- function(x) {!imat(x) & inst(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inum]{numeric} matrix?
#' @export
num_mat <- function(x) {!imat(x) & inum(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iodd]{odd whole-number} matrix?
#' @export
odd_mat <- function(x) {!imat(x) & iodd(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iord]{ordered-factor} matrix?
#' @export
ord_mat <- function(x) {!imat(x) & iord(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipct]{percentage numeric (0-100)} matrix?
#' @export
pct_mat <- function(x) {!imat(x) & ipct(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipos]{positive numeric} matrix?
#' @export
pos_mat <- function(x) {!imat(x) & ipos(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ippn]{proportional numeric (0-1)} matrix?
#' @export
ppn_mat <- function(x) {!imat(x) & ippn(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipsw]{positive whole-number} matrix?
#' @export
psw_mat <- function(x) {!imat(x) & ipsw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[isrt]{sortable} matrix?
#' @export
srt_mat <- function(x) {!imat(x) & isrt(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[istr]{string} matrix?
#' @export
str_mat <- function(x) {!imat(x) & istr(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iuno]{unordered-factor} matrix?
#' @export
uno_mat <- function(x) {!imat(x) & iuno(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[iwhl]{whole-number} matrix?
#' @export
whl_mat <- function(x) {!imat(x) & iwhl(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ich1]{onechar} multivec?
#' @export
ch1_mvc <- function(x) {!imvc(x) & ich1(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ichr]{character} multivec?
#' @export
chr_mvc <- function(x) {!imvc(x) & ichr(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[iclr]{color} multivec?
#' @export
clr_mvc <- function(x) {!imvc(x) & iclr(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[ievn]{even whole-number} multivec?
#' @export
evn_mvc <- function(x) {!imvc(x) & ievn(x)}

#' @describeIn mmm_ccc. Is \code{x}  \link[ifac]{factor} multivec?
#' @export
fac_mvc <- function(x) {!imvc(x) & ifac(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifrc]{fractional numeric} multivec?
#' @export
frc_mvc <- function(x) {!imvc(x) & ifrc(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iind]{indexer} multivec?
#' @export
ind_mvc <- function(x) {!imvc(x) & iind(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ilgl]{logical} multivec?
#' @export
lgl_mvc <- function(x) {!imvc(x) & ilgl(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ineg]{negative numeric} multivec?
#' @export
neg_mvc <- function(x) {!imvc(x) & ineg(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ingw]{negative whole-number} multivec?
#' @export
ngw_mvc <- function(x) {!imvc(x) & ingw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inng]{non-negative numeric} multivec?
#' @export
nng_mvc <- function(x) {!imvc(x) & inng(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[innw]{non-negative whole-number} multivec?
#' @export
nnw_mvc <- function(x) {!imvc(x) & innw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inps]{non-positive numeric} multivec?
#' @export
nps_mvc <- function(x) {!imvc(x) & inps(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inpw]{non-positive whole-number} multivec?
#' @export
npw_mvc <- function(x) {!imvc(x) & inpw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inst]{non-sortable} multivec?
#' @export
nst_mvc <- function(x) {!imvc(x) & inst(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inum]{numeric} multivec?
#' @export
num_mvc <- function(x) {!imvc(x) & inum(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iodd]{odd whole-number} multivec?
#' @export
odd_mvc <- function(x) {!imvc(x) & iodd(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iord]{ordered-factor} multivec?
#' @export
ord_mvc <- function(x) {!imvc(x) & iord(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipct]{percentage numeric (0-100)} multivec?
#' @export
pct_mvc <- function(x) {!imvc(x) & ipct(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipos]{positive numeric} multivec?
#' @export
pos_mvc <- function(x) {!imvc(x) & ipos(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ippn]{proportional numeric (0-1)} multivec?
#' @export
ppn_mvc <- function(x) {!imvc(x) & ippn(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipsw]{positive whole-number} multivec?
#' @export
psw_mvc <- function(x) {!imvc(x) & ipsw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[isrt]{sortable} multivec?
#' @export
srt_mvc <- function(x) {!imvc(x) & isrt(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[istr]{string} multivec?
#' @export
str_mvc <- function(x) {!imvc(x) & istr(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iuno]{unordered-factor} multivec?
#' @export
uno_mvc <- function(x) {!imvc(x) & iuno(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[iwhl]{whole-number} multivec?
#' @export
whl_mvc <- function(x) {!imvc(x) & iwhl(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ich1]{onechar} scalr?
#' @export
ch1_scl <- function(x) {!iscl(x) & ich1(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ichr]{character} scalr?
#' @export
chr_scl <- function(x) {!iscl(x) & ichr(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[iclr]{color} scalr?
#' @export
clr_scl <- function(x) {!iscl(x) & iclr(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[ievn]{even whole-number} scalr?
#' @export
evn_scl <- function(x) {!iscl(x) & ievn(x)}

#' @describeIn mmm_ccc. Is \code{x}  \link[ifac]{factor} scalr?
#' @export
fac_scl <- function(x) {!iscl(x) & ifac(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifrc]{fractional numeric} scalr?
#' @export
frc_scl <- function(x) {!iscl(x) & ifrc(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iind]{indexer} scalr?
#' @export
ind_scl <- function(x) {!iscl(x) & iind(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ilgl]{logical} scalr?
#' @export
lgl_scl <- function(x) {!iscl(x) & ilgl(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ineg]{negative numeric} scalr?
#' @export
neg_scl <- function(x) {!iscl(x) & ineg(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ingw]{negative whole-number} scalr?
#' @export
ngw_scl <- function(x) {!iscl(x) & ingw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inng]{non-negative numeric} scalr?
#' @export
nng_scl <- function(x) {!iscl(x) & inng(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[innw]{non-negative whole-number} scalr?
#' @export
nnw_scl <- function(x) {!iscl(x) & innw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inps]{non-positive numeric} scalr?
#' @export
nps_scl <- function(x) {!iscl(x) & inps(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inpw]{non-positive whole-number} scalr?
#' @export
npw_scl <- function(x) {!iscl(x) & inpw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inst]{non-sortable} scalr?
#' @export
nst_scl <- function(x) {!iscl(x) & inst(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inum]{numeric} scalr?
#' @export
num_scl <- function(x) {!iscl(x) & inum(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iodd]{odd whole-number} scalr?
#' @export
odd_scl <- function(x) {!iscl(x) & iodd(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iord]{ordered-factor} scalr?
#' @export
ord_scl <- function(x) {!iscl(x) & iord(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipct]{percentage numeric (0-100)} scalr?
#' @export
pct_scl <- function(x) {!iscl(x) & ipct(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipos]{positive numeric} scalr?
#' @export
pos_scl <- function(x) {!iscl(x) & ipos(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ippn]{proportional numeric (0-1)} scalr?
#' @export
ppn_scl <- function(x) {!iscl(x) & ippn(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipsw]{positive whole-number} scalr?
#' @export
psw_scl <- function(x) {!iscl(x) & ipsw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[isrt]{sortable} scalr?
#' @export
srt_scl <- function(x) {!iscl(x) & isrt(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[istr]{string} scalr?
#' @export
str_scl <- function(x) {!iscl(x) & istr(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iuno]{unordered-factor} scalr?
#' @export
uno_scl <- function(x) {!iscl(x) & iuno(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[iwhl]{whole-number} scalr?
#' @export
whl_scl <- function(x) {!iscl(x) & iwhl(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ich1]{onechar} vec?
#' @export
ch1_vec <- function(x) {!ivec(x) & ich1(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ichr]{character} vec?
#' @export
chr_vec <- function(x) {!ivec(x) & ichr(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[iclr]{color} vec?
#' @export
clr_vec <- function(x) {!ivec(x) & iclr(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[ievn]{even whole-number} vec?
#' @export
evn_vec <- function(x) {!ivec(x) & ievn(x)}

#' @describeIn mmm_ccc. Is \code{x}  \link[ifac]{factor} vec?
#' @export
fac_vec <- function(x) {!ivec(x) & ifac(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifrc]{fractional numeric} vec?
#' @export
frc_vec <- function(x) {!ivec(x) & ifrc(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iind]{indexer} vec?
#' @export
ind_vec <- function(x) {!ivec(x) & iind(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ilgl]{logical} vec?
#' @export
lgl_vec <- function(x) {!ivec(x) & ilgl(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ineg]{negative numeric} vec?
#' @export
neg_vec <- function(x) {!ivec(x) & ineg(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ingw]{negative whole-number} vec?
#' @export
ngw_vec <- function(x) {!ivec(x) & ingw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inng]{non-negative numeric} vec?
#' @export
nng_vec <- function(x) {!ivec(x) & inng(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[innw]{non-negative whole-number} vec?
#' @export
nnw_vec <- function(x) {!ivec(x) & innw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inps]{non-positive numeric} vec?
#' @export
nps_vec <- function(x) {!ivec(x) & inps(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inpw]{non-positive whole-number} vec?
#' @export
npw_vec <- function(x) {!ivec(x) & inpw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inst]{non-sortable} vec?
#' @export
nst_vec <- function(x) {!ivec(x) & inst(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[inum]{numeric} vec?
#' @export
num_vec <- function(x) {!ivec(x) & inum(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iodd]{odd whole-number} vec?
#' @export
odd_vec <- function(x) {!ivec(x) & iodd(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iord]{ordered-factor} vec?
#' @export
ord_vec <- function(x) {!ivec(x) & iord(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipct]{percentage numeric (0-100)} vec?
#' @export
pct_vec <- function(x) {!ivec(x) & ipct(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipos]{positive numeric} vec?
#' @export
pos_vec <- function(x) {!ivec(x) & ipos(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ippn]{proportional numeric (0-1)} vec?
#' @export
ppn_vec <- function(x) {!ivec(x) & ippn(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipsw]{positive whole-number} vec?
#' @export
psw_vec <- function(x) {!ivec(x) & ipsw(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[isrt]{sortable} vec?
#' @export
srt_vec <- function(x) {!ivec(x) & isrt(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[istr]{string} vec?
#' @export
str_vec <- function(x) {!ivec(x) & istr(x)}

#' @describeIn mmm_ccc. Is \code{x} an \link[iuno]{unordered-factor} vec?
#' @export
uno_vec <- function(x) {!ivec(x) & iuno(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[iwhl]{whole-number} vec?
#' @export
whl_vec <- function(x) {!ivec(x) & iwhl(x)}

#' @describeIn mmm_ccc. Is \code{x} a \link[ich1]{onechar} vlist?
#' @export
ch1_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ich1))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[ichr]{character} vlist?
#' @export
chr_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ichr))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[iclr]{color} vlist?
#' @export
clr_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, iclr))}}

#' @describeIn mmm_ccc. Is \code{x} an \link[ievn]{even whole-number} vlist?
#' @export
evn_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ievn))}}

#' @describeIn mmm_ccc. Is \code{x}  \link[ifac]{factor} vlist?
#' @export
fac_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ifac))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[ifrc]{fractional numeric} vlist?
#' @export
frc_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ifrc))}}

#' @describeIn mmm_ccc. Is \code{x} an \link[iind]{indexer} vlist?
#' @export
ind_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, iind))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[ilgl]{logical} vlist?
#' @export
lgl_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ilgl))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[ineg]{negative numeric} vlist?
#' @export
neg_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ineg))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[ingw]{negative whole-number} vlist?
#' @export
ngw_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ingw))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[inng]{non-negative numeric} vlist?
#' @export
nng_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, inng))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[innw]{non-negative whole-number} vlist?
#' @export
nnw_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, innw))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[inps]{non-positive numeric} vlist?
#' @export
nps_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, inps))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[inpw]{non-positive whole-number} vlist?
#' @export
npw_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, inpw))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[inst]{non-sortable} vlist?
#' @export
nst_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, inst))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[inum]{numeric} vlist?
#' @export
num_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, inum))}}

#' @describeIn mmm_ccc. Is \code{x} an \link[iodd]{odd whole-number} vlist?
#' @export
odd_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, iodd))}}

#' @describeIn mmm_ccc. Is \code{x} an \link[iord]{ordered-factor} vlist?
#' @export
ord_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, iord))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipct]{percentage numeric (0-100)} vlist?
#' @export
pct_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ipct))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipos]{positive numeric} vlist?
#' @export
pos_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ipos))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[ippn]{proportional numeric (0-1)} vlist?
#' @export
ppn_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ippn))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[ipsw]{positive whole-number} vlist?
#' @export
psw_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ipsw))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[isrt]{sortable} vlist?
#' @export
srt_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, isrt))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[istr]{string} vlist?
#' @export
str_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, istr))}}

#' @describeIn mmm_ccc. Is \code{x} an \link[iuno]{unordered-factor} vlist?
#' @export
uno_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, iuno))}}

#' @describeIn mmm_ccc. Is \code{x} a \link[iwhl]{whole-number} vlist?
#' @export
whl_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, iwhl))}}
