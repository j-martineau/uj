#' @name mmm_ccc.
#' @family props
#' @title Extended Mode + Extended Class Properties
#' @description \tabular{ll}{
#'   \code{mmm_ccc_vals}   \tab Gets a character vector of all possible
#'                              \link[=mmm]{extended mode} +
#'                              \link[=ccc]{extended class} properties.      \cr
#'   \code{[mmm]_[ccc]}    \tab Evaluates whether \code{x} is of the
#'                              \link[=mmm]{extended mode} represented by the
#'                              placeholder \code{[mmm]} and of the
#'                              \link[=ccc]{extended class} represented by the
#'                              placeholder \code{[ccc]}.                      }
#' @param x An R object.
#' @return \tabular{ll}{
#'   \code{mmm_ccc_vals}   \tab A character vector.                          \cr
#'   \code{[mmm]_[ccc]}    \tab A logical scalar.                              }
#' @export
mmm_ccc. <- function() {help("mmm_ccc.", package = "uj")}

#' @rdname mmm_ccc.
#' @export
mmm_ccc_vals <- function() {
  x <- expand.grid(mmm = mmm_vals(), ccc = ccc_vals())
  x <- av(apply(x, 1, paste0, collapse = "_"))
  sort(x)
}

#' @rdname mmm_ccc.
#' @export
ch1_arr <- function(x) {iarr(x) & ich1(x)}

#' @rdname mmm_ccc.
#' @export
chr_arr <- function(x) {iarr(x) & ichr(x)}

#' @rdname mmm_ccc.
#' @export
clr_arr <- function(x) {iarr(x) & iclr(x)}

#' @rdname mmm_ccc.
#' @export
evn_arr <- function(x) {iarr(x) & ievn(x)}

#' @rdname mmm_ccc.
#' @export
fac_arr <- function(x) {iarr(x) & ifac(x)}

#' @rdname mmm_ccc.
#' @export
frc_arr <- function(x) {iarr(x) & ifrc(x)}

#' @rdname mmm_ccc.
#' @export
ind_arr <- function(x) {iarr(x) & iind(x)}

#' @rdname mmm_ccc.
#' @export
lgl_arr <- function(x) {iarr(x) & ilgl(x)}

#' @rdname mmm_ccc.
#' @export
neg_arr <- function(x) {iarr(x) & ineg(x)}

#' @rdname mmm_ccc.
#' @export
ngw_arr <- function(x) {iarr(x) & ingw(x)}

#' @rdname mmm_ccc.
#' @export
nng_arr <- function(x) {iarr(x) & inng(x)}

#' @rdname mmm_ccc.
#' @export
nnw_arr <- function(x) {iarr(x) & innw(x)}

#' @rdname mmm_ccc.
#' @export
nps_arr <- function(x) {iarr(x) & inps(x)}

#' @rdname mmm_ccc.
#' @export
npw_arr <- function(x) {iarr(x) & inpw(x)}

#' @rdname mmm_ccc.
#' @export
nst_arr <- function(x) {iarr(x) & inst(x)}

#' @rdname mmm_ccc.
#' @export
num_arr <- function(x) {iarr(x) & inum(x)}

#' @rdname mmm_ccc.
#' @export
odd_arr <- function(x) {iarr(x) & iodd(x)}

#' @rdname mmm_ccc.
#' @export
ord_arr <- function(x) {iarr(x) & iord(x)}

#' @rdname mmm_ccc.
#' @export
pct_arr <- function(x) {iarr(x) & ipct(x)}

#' @rdname mmm_ccc.
#' @export
pos_arr <- function(x) {iarr(x) & ipos(x)}

#' @rdname mmm_ccc.
#' @export
ppn_arr <- function(x) {iarr(x) & ippn(x)}

#' @rdname mmm_ccc.
#' @export
psw_arr <- function(x) {iarr(x) & ipsw(x)}

#' @rdname mmm_ccc.
#' @export
srt_arr <- function(x) {iarr(x) & isrt(x)}

#' @rdname mmm_ccc.
#' @export
str_arr <- function(x) {iarr(x) & istr(x)}

#' @rdname mmm_ccc.
#' @export
uno_arr <- function(x) {iarr(x) & iuno(x)}

#' @rdname mmm_ccc.
#' @export
whl_arr <- function(x) {iarr(x) & iwhl(x)}

#' @rdname mmm_ccc.
#' @export
ch1_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ich1(x)))}}

#' @rdname mmm_ccc.
#' @export
chr_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ichr(x)))}}

#' @rdname mmm_ccc.
#' @export
clr_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, iclr(x)))}}

#' @rdname mmm_ccc.
#' @export
evn_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ievn(x)))}}

#' @rdname mmm_ccc.
#' @export
fac_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ifac(x)))}}

#' @rdname mmm_ccc.
#' @export
frc_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ifrc(x)))}}

#' @rdname mmm_ccc.
#' @export
ind_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, iind(x)))}}

#' @rdname mmm_ccc.
#' @export
lgl_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ilgl(x)))}}

#' @rdname mmm_ccc.
#' @export
neg_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ineg(x)))}}

#' @rdname mmm_ccc.
#' @export
ngw_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ingw(x)))}}

#' @rdname mmm_ccc.
#' @export
nng_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, inng(x)))}}

#' @rdname mmm_ccc.
#' @export
nnw_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, innw(x)))}}

#' @rdname mmm_ccc.
#' @export
nps_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, inps(x)))}}

#' @rdname mmm_ccc.
#' @export
npw_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, inpw(x)))}}

#' @rdname mmm_ccc.
#' @export
nst_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, inst(x)))}}

#' @rdname mmm_ccc.
#' @export
num_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, inum(x)))}}

#' @rdname mmm_ccc.
#' @export
odd_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, iodd(x)))}}

#' @rdname mmm_ccc.
#' @export
ord_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, iord(x)))}}

#' @rdname mmm_ccc.
#' @export
pct_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ipct(x)))}}

#' @rdname mmm_ccc.
#' @export
pos_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ipos(x)))}}

#' @rdname mmm_ccc.
#' @export
ppn_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ippn(x)))}}

#' @rdname mmm_ccc.
#' @export
psw_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, ipsw(x)))}}

#' @rdname mmm_ccc.
#' @export
srt_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, isrt(x)))}}

#' @rdname mmm_ccc.
#' @export
str_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, istr(x)))}}

#' @rdname mmm_ccc.
#' @export
uno_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, iuno(x)))}}

#' @rdname mmm_ccc.
#' @export
whl_dtf <- function(x) {if (!idtf(x)) {F} else if (length(x) == 0) {T} else {all(apply(x, 2, iwhl(x)))}}

#' @rdname mmm_ccc.
#' @export
ch1_gen <- function(x) {igen(x) & ich1(x)}

#' @rdname mmm_ccc.
#' @export
chr_gen <- function(x) {igen(x) & ichr(x)}

#' @rdname mmm_ccc.
#' @export
clr_gen <- function(x) {igen(x) & iclr(x)}

#' @rdname mmm_ccc.
#' @export
evn_gen <- function(x) {igen(x) & ievn(x)}

#' @rdname mmm_ccc.
#' @export
fac_gen <- function(x) {igen(x) & ifac(x)}

#' @rdname mmm_ccc.
#' @export
frc_gen <- function(x) {igen(x) & ifrc(x)}

#' @rdname mmm_ccc.
#' @export
ind_gen <- function(x) {igen(x) & iind(x)}

#' @rdname mmm_ccc.
#' @export
lgl_gen <- function(x) {igen(x) & ilgl(x)}

#' @rdname mmm_ccc.
#' @export
neg_gen <- function(x) {igen(x) & ineg(x)}

#' @rdname mmm_ccc.
#' @export
ngw_gen <- function(x) {igen(x) & ingw(x)}

#' @rdname mmm_ccc.
#' @export
nng_gen <- function(x) {igen(x) & inng(x)}

#' @rdname mmm_ccc.
#' @export
nnw_gen <- function(x) {igen(x) & innw(x)}

#' @rdname mmm_ccc.
#' @export
nps_gen <- function(x) {igen(x) & inps(x)}

#' @rdname mmm_ccc.
#' @export
npw_gen <- function(x) {igen(x) & inpw(x)}

#' @rdname mmm_ccc.
#' @export
nst_gen <- function(x) {igen(x) & inst(x)}

#' @rdname mmm_ccc.
#' @export
num_gen <- function(x) {igen(x) & inum(x)}

#' @rdname mmm_ccc.
#' @export
odd_gen <- function(x) {igen(x) & iodd(x)}

#' @rdname mmm_ccc.
#' @export
ord_gen <- function(x) {igen(x) & iord(x)}

#' @rdname mmm_ccc.
#' @export
pct_gen <- function(x) {igen(x) & ipct(x)}

#' @rdname mmm_ccc.
#' @export
pos_gen <- function(x) {igen(x) & ipos(x)}

#' @rdname mmm_ccc.
#' @export
ppn_gen <- function(x) {igen(x) & ippn(x)}

#' @rdname mmm_ccc.
#' @export
psw_gen <- function(x) {igen(x) & ipsw(x)}

#' @rdname mmm_ccc.
#' @export
srt_gen <- function(x) {igen(x) & isrt(x)}

#' @rdname mmm_ccc.
#' @export
str_gen <- function(x) {igen(x) & istr(x)}

#' @rdname mmm_ccc.
#' @export
uno_gen <- function(x) {igen(x) & iuno(x)}

#' @rdname mmm_ccc.
#' @export
whl_gen <- function(x) {igen(x) & iwhl(x)}

#' @rdname mmm_ccc.
#' @export
ch1_mat <- function(x) {imat(x) & ich1(x)}

#' @rdname mmm_ccc.
#' @export
chr_mat <- function(x) {imat(x) & ichr(x)}

#' @rdname mmm_ccc.
#' @export
clr_mat <- function(x) {imat(x) & iclr(x)}

#' @rdname mmm_ccc.
#' @export
evn_mat <- function(x) {imat(x) & ievn(x)}

#' @rdname mmm_ccc.
#' @export
fac_mat <- function(x) {imat(x) & ifac(x)}

#' @rdname mmm_ccc.
#' @export
frc_mat <- function(x) {imat(x) & ifrc(x)}

#' @rdname mmm_ccc.
#' @export
ind_mat <- function(x) {imat(x) & iind(x)}

#' @rdname mmm_ccc.
#' @export
lgl_mat <- function(x) {imat(x) & ilgl(x)}

#' @rdname mmm_ccc.
#' @export
neg_mat <- function(x) {imat(x) & ineg(x)}

#' @rdname mmm_ccc.
#' @export
ngw_mat <- function(x) {imat(x) & ingw(x)}

#' @rdname mmm_ccc.
#' @export
nng_mat <- function(x) {imat(x) & inng(x)}

#' @rdname mmm_ccc.
#' @export
nnw_mat <- function(x) {imat(x) & innw(x)}

#' @rdname mmm_ccc.
#' @export
nps_mat <- function(x) {imat(x) & inps(x)}

#' @rdname mmm_ccc.
#' @export
npw_mat <- function(x) {imat(x) & inpw(x)}

#' @rdname mmm_ccc.
#' @export
nst_mat <- function(x) {imat(x) & inst(x)}

#' @rdname mmm_ccc.
#' @export
num_mat <- function(x) {imat(x) & inum(x)}

#' @rdname mmm_ccc.
#' @export
odd_mat <- function(x) {imat(x) & iodd(x)}

#' @rdname mmm_ccc.
#' @export
ord_mat <- function(x) {imat(x) & iord(x)}

#' @rdname mmm_ccc.
#' @export
pct_mat <- function(x) {imat(x) & ipct(x)}

#' @rdname mmm_ccc.
#' @export
pos_mat <- function(x) {imat(x) & ipos(x)}

#' @rdname mmm_ccc.
#' @export
ppn_mat <- function(x) {imat(x) & ippn(x)}

#' @rdname mmm_ccc.
#' @export
psw_mat <- function(x) {imat(x) & ipsw(x)}

#' @rdname mmm_ccc.
#' @export
srt_mat <- function(x) {imat(x) & isrt(x)}

#' @rdname mmm_ccc.
#' @export
str_mat <- function(x) {imat(x) & istr(x)}

#' @rdname mmm_ccc.
#' @export
uno_mat <- function(x) {imat(x) & iuno(x)}

#' @rdname mmm_ccc.
#' @export
whl_mat <- function(x) {imat(x) & iwhl(x)}

#' @rdname mmm_ccc.
#' @export
ch1_mvc <- function(x) {imvc(x) & ich1(x)}

#' @rdname mmm_ccc.
#' @export
chr_mvc <- function(x) {imvc(x) & ichr(x)}

#' @rdname mmm_ccc.
#' @export
clr_mvc <- function(x) {imvc(x) & iclr(x)}

#' @rdname mmm_ccc.
#' @export
evn_mvc <- function(x) {imvc(x) & ievn(x)}

#' @rdname mmm_ccc.
#' @export
fac_mvc <- function(x) {imvc(x) & ifac(x)}

#' @rdname mmm_ccc.
#' @export
frc_mvc <- function(x) {imvc(x) & ifrc(x)}

#' @rdname mmm_ccc.
#' @export
ind_mvc <- function(x) {imvc(x) & iind(x)}

#' @rdname mmm_ccc.
#' @export
lgl_mvc <- function(x) {imvc(x) & ilgl(x)}

#' @rdname mmm_ccc.
#' @export
neg_mvc <- function(x) {imvc(x) & ineg(x)}

#' @rdname mmm_ccc.
#' @export
ngw_mvc <- function(x) {imvc(x) & ingw(x)}

#' @rdname mmm_ccc.
#' @export
nng_mvc <- function(x) {imvc(x) & inng(x)}

#' @rdname mmm_ccc.
#' @export
nnw_mvc <- function(x) {imvc(x) & innw(x)}

#' @rdname mmm_ccc.
#' @export
nps_mvc <- function(x) {imvc(x) & inps(x)}

#' @rdname mmm_ccc.
#' @export
npw_mvc <- function(x) {imvc(x) & inpw(x)}

#' @rdname mmm_ccc.
#' @export
nst_mvc <- function(x) {imvc(x) & inst(x)}

#' @rdname mmm_ccc.
#' @export
num_mvc <- function(x) {imvc(x) & inum(x)}

#' @rdname mmm_ccc.
#' @export
odd_mvc <- function(x) {imvc(x) & iodd(x)}

#' @rdname mmm_ccc.
#' @export
ord_mvc <- function(x) {imvc(x) & iord(x)}

#' @rdname mmm_ccc.
#' @export
pct_mvc <- function(x) {imvc(x) & ipct(x)}

#' @rdname mmm_ccc.
#' @export
pos_mvc <- function(x) {imvc(x) & ipos(x)}

#' @rdname mmm_ccc.
#' @export
ppn_mvc <- function(x) {imvc(x) & ippn(x)}

#' @rdname mmm_ccc.
#' @export
psw_mvc <- function(x) {imvc(x) & ipsw(x)}

#' @rdname mmm_ccc.
#' @export
srt_mvc <- function(x) {imvc(x) & isrt(x)}

#' @rdname mmm_ccc.
#' @export
str_mvc <- function(x) {imvc(x) & istr(x)}

#' @rdname mmm_ccc.
#' @export
uno_mvc <- function(x) {imvc(x) & iuno(x)}

#' @rdname mmm_ccc.
#' @export
whl_mvc <- function(x) {imvc(x) & iwhl(x)}

#' @rdname mmm_ccc.
#' @export
ch1_scl <- function(x) {iscl(x) & ich1(x)}

#' @rdname mmm_ccc.
#' @export
chr_scl <- function(x) {iscl(x) & ichr(x)}

#' @rdname mmm_ccc.
#' @export
clr_scl <- function(x) {iscl(x) & iclr(x)}

#' @rdname mmm_ccc.
#' @export
evn_scl <- function(x) {iscl(x) & ievn(x)}

#' @rdname mmm_ccc.
#' @export
fac_scl <- function(x) {iscl(x) & ifac(x)}

#' @rdname mmm_ccc.
#' @export
frc_scl <- function(x) {iscl(x) & ifrc(x)}

#' @rdname mmm_ccc.
#' @export
ind_scl <- function(x) {iscl(x) & iind(x)}

#' @rdname mmm_ccc.
#' @export
lgl_scl <- function(x) {iscl(x) & ilgl(x)}

#' @rdname mmm_ccc.
#' @export
neg_scl <- function(x) {iscl(x) & ineg(x)}

#' @rdname mmm_ccc.
#' @export
ngw_scl <- function(x) {iscl(x) & ingw(x)}

#' @rdname mmm_ccc.
#' @export
nng_scl <- function(x) {iscl(x) & inng(x)}

#' @rdname mmm_ccc.
#' @export
nnw_scl <- function(x) {iscl(x) & innw(x)}

#' @rdname mmm_ccc.
#' @export
nps_scl <- function(x) {iscl(x) & inps(x)}

#' @rdname mmm_ccc.
#' @export
npw_scl <- function(x) {iscl(x) & inpw(x)}

#' @rdname mmm_ccc.
#' @export
nst_scl <- function(x) {iscl(x) & inst(x)}

#' @rdname mmm_ccc.
#' @export
num_scl <- function(x) {iscl(x) & inum(x)}

#' @rdname mmm_ccc.
#' @export
odd_scl <- function(x) {iscl(x) & iodd(x)}

#' @rdname mmm_ccc.
#' @export
ord_scl <- function(x) {iscl(x) & iord(x)}

#' @rdname mmm_ccc.
#' @export
pct_scl <- function(x) {iscl(x) & ipct(x)}

#' @rdname mmm_ccc.
#' @export
pos_scl <- function(x) {iscl(x) & ipos(x)}

#' @rdname mmm_ccc.
#' @export
ppn_scl <- function(x) {iscl(x) & ippn(x)}

#' @rdname mmm_ccc.
#' @export
psw_scl <- function(x) {iscl(x) & ipsw(x)}

#' @rdname mmm_ccc.
#' @export
srt_scl <- function(x) {iscl(x) & isrt(x)}

#' @rdname mmm_ccc.
#' @export
str_scl <- function(x) {iscl(x) & istr(x)}

#' @rdname mmm_ccc.
#' @export
uno_scl <- function(x) {iscl(x) & iuno(x)}

#' @rdname mmm_ccc.
#' @export
whl_scl <- function(x) {iscl(x) & iwhl(x)}

#' @rdname mmm_ccc.
#' @export
ch1_vec <- function(x) {ivec(x) & ich1(x)}

#' @rdname mmm_ccc.
#' @export
chr_vec <- function(x) {ivec(x) & ichr(x)}

#' @rdname mmm_ccc.
#' @export
clr_vec <- function(x) {ivec(x) & iclr(x)}

#' @rdname mmm_ccc.
#' @export
evn_vec <- function(x) {ivec(x) & ievn(x)}

#' @rdname mmm_ccc.
#' @export
fac_vec <- function(x) {ivec(x) & ifac(x)}

#' @rdname mmm_ccc.
#' @export
frc_vec <- function(x) {ivec(x) & ifrc(x)}

#' @rdname mmm_ccc.
#' @export
ind_vec <- function(x) {ivec(x) & iind(x)}

#' @rdname mmm_ccc.
#' @export
lgl_vec <- function(x) {ivec(x) & ilgl(x)}

#' @rdname mmm_ccc.
#' @export
neg_vec <- function(x) {ivec(x) & ineg(x)}

#' @rdname mmm_ccc.
#' @export
ngw_vec <- function(x) {ivec(x) & ingw(x)}

#' @rdname mmm_ccc.
#' @export
nng_vec <- function(x) {ivec(x) & inng(x)}

#' @rdname mmm_ccc.
#' @export
nnw_vec <- function(x) {ivec(x) & innw(x)}

#' @rdname mmm_ccc.
#' @export
nps_vec <- function(x) {ivec(x) & inps(x)}

#' @rdname mmm_ccc.
#' @export
npw_vec <- function(x) {ivec(x) & inpw(x)}

#' @rdname mmm_ccc.
#' @export
nst_vec <- function(x) {ivec(x) & inst(x)}

#' @rdname mmm_ccc.
#' @export
num_vec <- function(x) {ivec(x) & inum(x)}

#' @rdname mmm_ccc.
#' @export
odd_vec <- function(x) {ivec(x) & iodd(x)}

#' @rdname mmm_ccc.
#' @export
ord_vec <- function(x) {ivec(x) & iord(x)}

#' @rdname mmm_ccc.
#' @export
pct_vec <- function(x) {ivec(x) & ipct(x)}

#' @rdname mmm_ccc.
#' @export
pos_vec <- function(x) {ivec(x) & ipos(x)}

#' @rdname mmm_ccc.
#' @export
ppn_vec <- function(x) {ivec(x) & ippn(x)}

#' @rdname mmm_ccc.
#' @export
psw_vec <- function(x) {ivec(x) & ipsw(x)}

#' @rdname mmm_ccc.
#' @export
srt_vec <- function(x) {ivec(x) & isrt(x)}

#' @rdname mmm_ccc.
#' @export
str_vec <- function(x) {ivec(x) & istr(x)}

#' @rdname mmm_ccc.
#' @export
uno_vec <- function(x) {ivec(x) & iuno(x)}

#' @rdname mmm_ccc.
#' @export
whl_vec <- function(x) {ivec(x) & iwhl(x)}

#' @rdname mmm_ccc.
#' @export
ch1_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ich1))}}

#' @rdname mmm_ccc.
#' @export
chr_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ichr))}}

#' @rdname mmm_ccc.
#' @export
clr_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, iclr))}}

#' @rdname mmm_ccc.
#' @export
evn_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ievn))}}

#' @rdname mmm_ccc.
#' @export
fac_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ifac))}}

#' @rdname mmm_ccc.
#' @export
frc_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ifrc))}}

#' @rdname mmm_ccc.
#' @export
ind_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, iind))}}

#' @rdname mmm_ccc.
#' @export
lgl_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ilgl))}}

#' @rdname mmm_ccc.
#' @export
neg_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ineg))}}

#' @rdname mmm_ccc.
#' @export
ngw_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ingw))}}

#' @rdname mmm_ccc.
#' @export
nng_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, inng))}}

#' @rdname mmm_ccc.
#' @export
nnw_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, innw))}}

#' @rdname mmm_ccc.
#' @export
nps_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, inps))}}

#' @rdname mmm_ccc.
#' @export
npw_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, inpw))}}

#' @rdname mmm_ccc.
#' @export
nst_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, inst))}}

#' @rdname mmm_ccc.
#' @export
num_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, inum))}}

#' @rdname mmm_ccc.
#' @export
odd_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, iodd))}}

#' @rdname mmm_ccc.
#' @export
ord_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, iord))}}

#' @rdname mmm_ccc.
#' @export
pct_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ipct))}}

#' @rdname mmm_ccc.
#' @export
pos_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ipos))}}

#' @rdname mmm_ccc.
#' @export
ppn_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ippn))}}

#' @rdname mmm_ccc.
#' @export
psw_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, ipsw))}}

#' @rdname mmm_ccc.
#' @export
srt_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, isrt))}}

#' @rdname mmm_ccc.
#' @export
str_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, istr))}}

#' @rdname mmm_ccc.
#' @export
uno_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, iuno))}}

#' @rdname mmm_ccc.
#' @export
whl_vls <- function(x) {if (!ivls(x)) {F} else if (length(x) == 0) {T} else {all(sapply(x, iwhl))}}
