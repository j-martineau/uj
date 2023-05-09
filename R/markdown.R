#' @name markdown
#' @encoding UTF-8
#' @family strings
#' @family colors
#' @family plots
#' @title Build markdown expressions for styled + colored `ggtext` strings
#' @description Style markdown text with subscripts, superscripts, and colors.
#' @details **Subscript/superscript** functions are:
#' \tabular{ll}{  `subs`   \tab subscript   \cr
#'                `sups`   \tab superscript   }
#' \cr\cr **All remaining functions** in this family start with a prefix character as follows:
#' \tabular{ll}{  prefix = `b`   \tab bold   \cr
#'                prefix = `i`   \tab italic \cr
#'                prefix = `p`   \tab plain    }
#' \cr\cr **Black, white, and invisible text** functions suffixes are:
#' \tabular{ll}{  `blk`   \tab black   \cr
#'                `wht`   \tab white   \cr
#'                `inv`   \tab invisible }
#' \cr For example, the function `bwht` styles markdown text as bold and white.
#' \cr\cr **Greyscale** text suffixes take the form:
#' \tabular{ll}{  `g05`   \tab ` 5:95` white:black ratio \cr
#'                `g10`   \tab `10:90` white:black ratio \cr
#'                `...`   \tab `...`                     \cr
#'                `g90`   \tab `90:10` white:black ratio \cr
#'                `g95`   \tab `95:5 ` white:black ratio   }
#' \cr For example, the function `pg45` styles markdown text as plain and grey with a `45:55` white:black ratio.
#' \cr\cr **Other common-color text** function names append an infix and a suffix to the selected prefix, as follows:
#' \tabular{ll}{  infix = `f`      \tab full intensity    \cr
#'                infix = `l`      \tab light             \cr
#'                infix = `m`      \tab medium            \cr
#'                infix = `d`      \tab dark \cr   \tab   \cr
#'                suffix = `blu`   \tab blue              \cr
#'                suffix = `cyn`   \tab cyan              \cr
#'                suffix = `grn`   \tab green             \cr
#'                suffix = `mag`   \tab magenta           \cr
#'                suffix = `orn`   \tab orange            \cr
#'                suffix = `red`   \tab red               \cr
#'                suffix = `vlt`   \tab violet            \cr
#'                suffix = `ylw`   \tab yellow              }
#'
#' \cr For example, the function `pfred` styles markdown text as plain and full-intensity red.
#' @param X A \link[=cmp_chr_vec]{character vector} of text to be styled.
#' @return A markdown-styled character vector.
#' @export
markdown <- function() {utils::help("markdown", package = "uj")}

#' @rdname markdown
#' @export
subs <- function(X) {base::paste0(uj::v(sub1), X, uj::v(sub2))}

#' @rdname markdown
#' @export
sups <- function(X) {base::paste0(uj::v(sup1), X, uj::v(sup2))}

#' @rdname markdown
#' @export
bfblu <- function(X) {base::paste0(uj::v(bfblu), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
blblu <- function(X) {base::paste0(uj::v(blblu), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bmblu <- function(X) {base::paste0(uj::v(bmblu), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bdblu <- function(X) {base::paste0(uj::v(bdblu), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bfcyn <- function(X) {base::paste0(uj::v(bfcyn), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
blcyn <- function(X) {base::paste0(uj::v(blcyn), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bmcyn <- function(X) {base::paste0(uj::v(bmcyn), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bdcyn <- function(X) {base::paste0(uj::v(bdcyn), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bfgrn <- function(X) {base::paste0(uj::v(bfgrn), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
blgrn <- function(X) {base::paste0(uj::v(blgrn), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bmgrn <- function(X) {base::paste0(uj::v(bmgrn), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bdgrn <- function(X) {base::paste0(uj::v(bdgrn), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bfmag <- function(X) {base::paste0(uj::v(bfmag), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
blmag <- function(X) {base::paste0(uj::v(blmag), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bmmag <- function(X) {base::paste0(uj::v(bmmag), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bdmag <- function(X) {base::paste0(uj::v(bdmag), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bforn <- function(X) {base::paste0(uj::v(bforn), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
blorn <- function(X) {base::paste0(uj::v(blorn), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bmorn <- function(X) {base::paste0(uj::v(bmorn), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bdorn <- function(X) {base::paste0(uj::v(bdorn), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bfred <- function(X) {base::paste0(uj::v(bfred), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
blred <- function(X) {base::paste0(uj::v(blred), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bmred <- function(X) {base::paste0(uj::v(bmred), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bdred <- function(X) {base::paste0(uj::v(bdred), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bfvlt <- function(X) {base::paste0(uj::v(bfvlt), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
blvlt <- function(X) {base::paste0(uj::v(blvlt), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bmvlt <- function(X) {base::paste0(uj::v(bmvlt), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bdvlt <- function(X) {base::paste0(uj::v(bdvlt), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bfylw <- function(X) {base::paste0(uj::v(bfylw), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
blylw <- function(X) {base::paste0(uj::v(blylw), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bmylw <- function(X) {base::paste0(uj::v(bmylw), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bdylw <- function(X) {base::paste0(uj::v(blylw), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bwht <- function(X) {base::paste0(uj::v(bwht), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bg95 <- function(X) {base::paste0(uj::v(bg95), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bg90 <- function(X) {base::paste0(uj::v(bg90), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bg85 <- function(X) {base::paste0(uj::v(bg85), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bg80 <- function(X) {base::paste0(uj::v(bg80), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bg75 <- function(X) {base::paste0(uj::v(bg75), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bg70 <- function(X) {base::paste0(uj::v(bg70), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bg65 <- function(X) {base::paste0(uj::v(bg65), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bg60 <- function(X) {base::paste0(uj::v(bg60), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bg55 <- function(X) {base::paste0(uj::v(bg55), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bg50 <- function(X) {base::paste0(uj::v(bg50), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bg45 <- function(X) {base::paste0(uj::v(bg45), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bg40 <- function(X) {base::paste0(uj::v(bg40), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bg35 <- function(X) {base::paste0(uj::v(bg35), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bg30 <- function(X) {base::paste0(uj::v(bg30), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bg25 <- function(X) {base::paste0(uj::v(bg25), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bg20 <- function(X) {base::paste0(uj::v(bg20), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bg15 <- function(X) {base::paste0(uj::v(bg15), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bg10 <- function(X) {base::paste0(uj::v(bg10), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bg05 <- function(X) {base::paste0(uj::v(bg05), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
bblk <- function(X) {base::paste0(uj::v(bblk), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
binv <- function(X) {base::paste0(uj::v(binv), X,  uj::v(bclose))}

#' @rdname markdown
#' @export
pfblu <- function(X) {base::paste0(uj::v(pfblu), X, uj::v(pclose))}

#' @rdname markdown
#' @export
plblu <- function(X) {base::paste0(uj::v(plblu), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pmblu <- function(X) {base::paste0(uj::v(pmblu), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pdblu <- function(X) {base::paste0(uj::v(pdblu), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pfcyn <- function(X) {base::paste0(uj::v(pfcyn), X, uj::v(pclose))}

#' @rdname markdown
#' @export
plcyn <- function(X) {base::paste0(uj::v(plcyn), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pmcyn <- function(X) {base::paste0(uj::v(pmcyn), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pdcyn <- function(X) {base::paste0(uj::v(pdcyn), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pfgrn <- function(X) {base::paste0(uj::v(pfgrn), X, uj::v(pclose))}

#' @rdname markdown
#' @export
plgrn <- function(X) {base::paste0(uj::v(plgrn), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pmgrn <- function(X) {base::paste0(uj::v(pmgrn), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pdgrn <- function(X) {base::paste0(uj::v(pdgrn), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pfmag <- function(X) {base::paste0(uj::v(pfmag), X, uj::v(pclose))}

#' @rdname markdown
#' @export
plmag <- function(X) {base::paste0(uj::v(plmag), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pmmag <- function(X) {base::paste0(uj::v(pmmag), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pdmag <- function(X) {base::paste0(uj::v(pdmag), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pforn <- function(X) {base::paste0(uj::v(pforn), X, uj::v(pclose))}

#' @rdname markdown
#' @export
plorn <- function(X) {base::paste0(uj::v(plorn), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pmorn <- function(X) {base::paste0(uj::v(pmorn), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pdorn <- function(X) {base::paste0(uj::v(pdorn), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pfred <- function(X) {base::paste0(uj::v(pfred), X, uj::v(pclose))}

#' @rdname markdown
#' @export
plred <- function(X) {base::paste0(uj::v(plred), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pmred <- function(X) {base::paste0(uj::v(pmred), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pdred <- function(X) {base::paste0(uj::v(pdred), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pfvlt <- function(X) {base::paste0(uj::v(pfvlt), X, uj::v(pclose))}

#' @rdname markdown
#' @export
plvlt <- function(X) {base::paste0(uj::v(plvlt), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pmvlt <- function(X) {base::paste0(uj::v(pmvlt), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pdvlt <- function(X) {base::paste0(uj::v(pdvlt), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pfylw <- function(X) {base::paste0(uj::v(pfylw), X, uj::v(pclose))}

#' @rdname markdown
#' @export
plylw <- function(X) {base::paste0(uj::v(plylw), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pmylw <- function(X) {base::paste0(uj::v(pmylw), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pdylw <- function(X) {base::paste0(uj::v(pdylw), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pwht <- function(X) {base::paste0(uj::v(pwht), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pg95 <- function(X) {base::paste0(uj::v(pg95), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pg90 <- function(X) {base::paste0(uj::v(pg90), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pg85 <- function(X) {base::paste0(uj::v(pg85), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pg80 <- function(X) {base::paste0(uj::v(pg80), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pg75 <- function(X) {base::paste0(uj::v(pg75), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pg70 <- function(X) {base::paste0(uj::v(pg70), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pg65 <- function(X) {base::paste0(uj::v(pg65), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pg60 <- function(X) {base::paste0(uj::v(pg60), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pg55 <- function(X) {base::paste0(uj::v(pg55), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pg50 <- function(X) {base::paste0(uj::v(pg50), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pg45 <- function(X) {base::paste0(uj::v(pg45), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pg40 <- function(X) {base::paste0(uj::v(pg40), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pg35 <- function(X) {base::paste0(uj::v(pg35), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pg30 <- function(X) {base::paste0(uj::v(pg30), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pg25 <- function(X) {base::paste0(uj::v(pg25), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pg20 <- function(X) {base::paste0(uj::v(pg20), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pg15 <- function(X) {base::paste0(uj::v(pg15), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pg10 <- function(X) {base::paste0(uj::v(pg10), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pg05 <- function(X) {base::paste0(uj::v(pg05), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pblk <- function(X) {base::paste0(uj::v(pblk), X, uj::v(pclose))}

#' @rdname markdown
#' @export
pinv <- function(X) {base::paste0(uj::v(pinv), X, uj::v(pclose))}

#' @rdname markdown
#' @export
ifblu <- function(X) {base::paste0(uj::v(ifblu), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ilblu <- function(X) {base::paste0(uj::v(ilblu), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
imblu <- function(X) {base::paste0(uj::v(imblu), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
idblu <- function(X) {base::paste0(uj::v(idblu), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ifcyn <- function(X) {base::paste0(uj::v(ifcyn), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ilcyn <- function(X) {base::paste0(uj::v(ilcyn), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
imcyn <- function(X) {base::paste0(uj::v(imcyn), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
idcyn <- function(X) {base::paste0(uj::v(idcyn), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ifgrn <- function(X) {base::paste0(uj::v(ifgrn), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ilgrn <- function(X) {base::paste0(uj::v(ilgrn), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
imgrn <- function(X) {base::paste0(uj::v(imgrn), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
idgrn <- function(X) {base::paste0(uj::v(idgrn), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ifmag <- function(X) {base::paste0(uj::v(ifmag), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ilmag <- function(X) {base::paste0(uj::v(ilmag), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
immag <- function(X) {base::paste0(uj::v(immag), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
idmag <- function(X) {base::paste0(uj::v(idmag), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
iforn <- function(X) {base::paste0(uj::v(iforn), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ilorn <- function(X) {base::paste0(uj::v(ilorn), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
imorn <- function(X) {base::paste0(uj::v(imorn), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
idorn <- function(X) {base::paste0(uj::v(idorn), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ifred <- function(X) {base::paste0(uj::v(ifred), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ilred <- function(X) {base::paste0(uj::v(ilred), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
imred <- function(X) {base::paste0(uj::v(imred), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
idred <- function(X) {base::paste0(uj::v(idred), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ifvlt <- function(X) {base::paste0(uj::v(ifvlt), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ilvlt <- function(X) {base::paste0(uj::v(ilvlt), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
imvlt <- function(X) {base::paste0(uj::v(imvlt), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
idvlt <- function(X) {base::paste0(uj::v(idvlt), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ifylw <- function(X) {base::paste0(uj::v(ifylw), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ilylw <- function(X) {base::paste0(uj::v(ilylw), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
imylw <- function(X) {base::paste0(uj::v(imylw), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
idylw <- function(X) {base::paste0(uj::v(idylw), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
iwht <- function(X) {base::paste0(uj::v(iwht), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ig95 <- function(X) {base::paste0(uj::v(ig95), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ig90 <- function(X) {base::paste0(uj::v(ig90), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ig85 <- function(X) {base::paste0(uj::v(ig85), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ig80 <- function(X) {base::paste0(uj::v(ig80), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ig75 <- function(X) {base::paste0(uj::v(ig75), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ig70 <- function(X) {base::paste0(uj::v(ig70), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ig65 <- function(X) {base::paste0(uj::v(ig65), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ig60 <- function(X) {base::paste0(uj::v(ig60), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ig55 <- function(X) {base::paste0(uj::v(ig55), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ig50 <- function(X) {base::paste0(uj::v(ig50), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ig45 <- function(X) {base::paste0(uj::v(ig45), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ig40 <- function(X) {base::paste0(uj::v(ig40), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ig35 <- function(X) {base::paste0(uj::v(ig35), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ig30 <- function(X) {base::paste0(uj::v(ig30), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ig25 <- function(X) {base::paste0(uj::v(ig25), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ig20 <- function(X) {base::paste0(uj::v(ig20), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ig15 <- function(X) {base::paste0(uj::v(ig15), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ig10 <- function(X) {base::paste0(uj::v(ig10), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
ig05 <- function(X) {base::paste0(uj::v(ig05), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
iblk <- function(X) {base::paste0(uj::v(iblk), X,  uj::v(iclose))}

#' @rdname markdown
#' @export
iinv <- function(X) {base::paste0(uj::v(iinv), X,  uj::v(iclose))}
