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
#' For example, the function `pfred` styles markdown text as plain and full-intensity red.
#' @param x A \link[=cmp_chr_vec]{character vector} of text to be styled.
#' @return A markdown-styled character vector.
#' @export
markdown_help <- function() {utils::help("markdown_help", package = "uj")}

#' @rdname markdown_help
#' @export
subs <- function(x) {base::paste0(uj::v(sub1), x, uj::v(sub2))}

#' @rdname markdown_help
#' @export
sups <- function(x) {base::paste0(uj::v(sup1), x, uj::v(sup2))}

#' @rdname markdown_help
#' @export
bfblu <- function(x) {base::paste0(uj::v(bfblu), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
blblu <- function(x) {base::paste0(uj::v(blblu), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bmblu <- function(x) {base::paste0(uj::v(bmblu), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bdblu <- function(x) {base::paste0(uj::v(bdblu), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bfcyn <- function(x) {base::paste0(uj::v(bfcyn), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
blcyn <- function(x) {base::paste0(uj::v(blcyn), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bmcyn <- function(x) {base::paste0(uj::v(bmcyn), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bdcyn <- function(x) {base::paste0(uj::v(bdcyn), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bfgrn <- function(x) {base::paste0(uj::v(bfgrn), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
blgrn <- function(x) {base::paste0(uj::v(blgrn), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bmgrn <- function(x) {base::paste0(uj::v(bmgrn), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bdgrn <- function(x) {base::paste0(uj::v(bdgrn), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bfmag <- function(x) {base::paste0(uj::v(bfmag), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
blmag <- function(x) {base::paste0(uj::v(blmag), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bmmag <- function(x) {base::paste0(uj::v(bmmag), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bdmag <- function(x) {base::paste0(uj::v(bdmag), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bforn <- function(x) {base::paste0(uj::v(bforn), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
blorn <- function(x) {base::paste0(uj::v(blorn), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bmorn <- function(x) {base::paste0(uj::v(bmorn), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bdorn <- function(x) {base::paste0(uj::v(bdorn), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bfred <- function(x) {base::paste0(uj::v(bfred), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
blred <- function(x) {base::paste0(uj::v(blred), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bmred <- function(x) {base::paste0(uj::v(bmred), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bdred <- function(x) {base::paste0(uj::v(bdred), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bfvlt <- function(x) {base::paste0(uj::v(bfvlt), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
blvlt <- function(x) {base::paste0(uj::v(blvlt), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bmvlt <- function(x) {base::paste0(uj::v(bmvlt), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bdvlt <- function(x) {base::paste0(uj::v(bdvlt), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bfylw <- function(x) {base::paste0(uj::v(bfylw), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
blylw <- function(x) {base::paste0(uj::v(blylw), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bmylw <- function(x) {base::paste0(uj::v(bmylw), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bdylw <- function(x) {base::paste0(uj::v(blylw), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bwht <- function(x) {base::paste0(uj::v(bwht), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bg95 <- function(x) {base::paste0(uj::v(bg95), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bg90 <- function(x) {base::paste0(uj::v(bg90), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bg85 <- function(x) {base::paste0(uj::v(bg85), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bg80 <- function(x) {base::paste0(uj::v(bg80), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bg75 <- function(x) {base::paste0(uj::v(bg75), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bg70 <- function(x) {base::paste0(uj::v(bg70), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bg65 <- function(x) {base::paste0(uj::v(bg65), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bg60 <- function(x) {base::paste0(uj::v(bg60), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bg55 <- function(x) {base::paste0(uj::v(bg55), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bg50 <- function(x) {base::paste0(uj::v(bg50), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bg45 <- function(x) {base::paste0(uj::v(bg45), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bg40 <- function(x) {base::paste0(uj::v(bg40), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bg35 <- function(x) {base::paste0(uj::v(bg35), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bg30 <- function(x) {base::paste0(uj::v(bg30), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bg25 <- function(x) {base::paste0(uj::v(bg25), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bg20 <- function(x) {base::paste0(uj::v(bg20), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bg15 <- function(x) {base::paste0(uj::v(bg15), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bg10 <- function(x) {base::paste0(uj::v(bg10), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bg05 <- function(x) {base::paste0(uj::v(bg05), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
bblk <- function(x) {base::paste0(uj::v(bblk), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
binv <- function(x) {base::paste0(uj::v(binv), x,  uj::v(bclose))}

#' @rdname markdown_help
#' @export
pfblu <- function(x) {base::paste0(uj::v(pfblu), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
plblu <- function(x) {base::paste0(uj::v(plblu), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pmblu <- function(x) {base::paste0(uj::v(pmblu), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pdblu <- function(x) {base::paste0(uj::v(pdblu), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pfcyn <- function(x) {base::paste0(uj::v(pfcyn), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
plcyn <- function(x) {base::paste0(uj::v(plcyn), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pmcyn <- function(x) {base::paste0(uj::v(pmcyn), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pdcyn <- function(x) {base::paste0(uj::v(pdcyn), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pfgrn <- function(x) {base::paste0(uj::v(pfgrn), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
plgrn <- function(x) {base::paste0(uj::v(plgrn), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pmgrn <- function(x) {base::paste0(uj::v(pmgrn), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pdgrn <- function(x) {base::paste0(uj::v(pdgrn), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pfmag <- function(x) {base::paste0(uj::v(pfmag), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
plmag <- function(x) {base::paste0(uj::v(plmag), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pmmag <- function(x) {base::paste0(uj::v(pmmag), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pdmag <- function(x) {base::paste0(uj::v(pdmag), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pforn <- function(x) {base::paste0(uj::v(pforn), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
plorn <- function(x) {base::paste0(uj::v(plorn), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pmorn <- function(x) {base::paste0(uj::v(pmorn), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pdorn <- function(x) {base::paste0(uj::v(pdorn), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pfred <- function(x) {base::paste0(uj::v(pfred), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
plred <- function(x) {base::paste0(uj::v(plred), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pmred <- function(x) {base::paste0(uj::v(pmred), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pdred <- function(x) {base::paste0(uj::v(pdred), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pfvlt <- function(x) {base::paste0(uj::v(pfvlt), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
plvlt <- function(x) {base::paste0(uj::v(plvlt), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pmvlt <- function(x) {base::paste0(uj::v(pmvlt), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pdvlt <- function(x) {base::paste0(uj::v(pdvlt), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pfylw <- function(x) {base::paste0(uj::v(pfylw), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
plylw <- function(x) {base::paste0(uj::v(plylw), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pmylw <- function(x) {base::paste0(uj::v(pmylw), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pdylw <- function(x) {base::paste0(uj::v(pdylw), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pwht <- function(x) {base::paste0(uj::v(pwht), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pg95 <- function(x) {base::paste0(uj::v(pg95), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pg90 <- function(x) {base::paste0(uj::v(pg90), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pg85 <- function(x) {base::paste0(uj::v(pg85), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pg80 <- function(x) {base::paste0(uj::v(pg80), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pg75 <- function(x) {base::paste0(uj::v(pg75), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pg70 <- function(x) {base::paste0(uj::v(pg70), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pg65 <- function(x) {base::paste0(uj::v(pg65), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pg60 <- function(x) {base::paste0(uj::v(pg60), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pg55 <- function(x) {base::paste0(uj::v(pg55), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pg50 <- function(x) {base::paste0(uj::v(pg50), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pg45 <- function(x) {base::paste0(uj::v(pg45), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pg40 <- function(x) {base::paste0(uj::v(pg40), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pg35 <- function(x) {base::paste0(uj::v(pg35), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pg30 <- function(x) {base::paste0(uj::v(pg30), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pg25 <- function(x) {base::paste0(uj::v(pg25), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pg20 <- function(x) {base::paste0(uj::v(pg20), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pg15 <- function(x) {base::paste0(uj::v(pg15), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pg10 <- function(x) {base::paste0(uj::v(pg10), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pg05 <- function(x) {base::paste0(uj::v(pg05), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pblk <- function(x) {base::paste0(uj::v(pblk), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
pinv <- function(x) {base::paste0(uj::v(pinv), x, uj::v(pclose))}

#' @rdname markdown_help
#' @export
ifblu <- function(x) {base::paste0(uj::v(ifblu), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ilblu <- function(x) {base::paste0(uj::v(ilblu), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
imblu <- function(x) {base::paste0(uj::v(imblu), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
idblu <- function(x) {base::paste0(uj::v(idblu), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ifcyn <- function(x) {base::paste0(uj::v(ifcyn), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ilcyn <- function(x) {base::paste0(uj::v(ilcyn), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
imcyn <- function(x) {base::paste0(uj::v(imcyn), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
idcyn <- function(x) {base::paste0(uj::v(idcyn), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ifgrn <- function(x) {base::paste0(uj::v(ifgrn), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ilgrn <- function(x) {base::paste0(uj::v(ilgrn), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
imgrn <- function(x) {base::paste0(uj::v(imgrn), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
idgrn <- function(x) {base::paste0(uj::v(idgrn), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ifmag <- function(x) {base::paste0(uj::v(ifmag), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ilmag <- function(x) {base::paste0(uj::v(ilmag), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
immag <- function(x) {base::paste0(uj::v(immag), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
idmag <- function(x) {base::paste0(uj::v(idmag), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
iforn <- function(x) {base::paste0(uj::v(iforn), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ilorn <- function(x) {base::paste0(uj::v(ilorn), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
imorn <- function(x) {base::paste0(uj::v(imorn), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
idorn <- function(x) {base::paste0(uj::v(idorn), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ifred <- function(x) {base::paste0(uj::v(ifred), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ilred <- function(x) {base::paste0(uj::v(ilred), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
imred <- function(x) {base::paste0(uj::v(imred), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
idred <- function(x) {base::paste0(uj::v(idred), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ifvlt <- function(x) {base::paste0(uj::v(ifvlt), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ilvlt <- function(x) {base::paste0(uj::v(ilvlt), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
imvlt <- function(x) {base::paste0(uj::v(imvlt), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
idvlt <- function(x) {base::paste0(uj::v(idvlt), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ifylw <- function(x) {base::paste0(uj::v(ifylw), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ilylw <- function(x) {base::paste0(uj::v(ilylw), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
imylw <- function(x) {base::paste0(uj::v(imylw), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
idylw <- function(x) {base::paste0(uj::v(idylw), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
iwht <- function(x) {base::paste0(uj::v(iwht), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ig95 <- function(x) {base::paste0(uj::v(ig95), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ig90 <- function(x) {base::paste0(uj::v(ig90), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ig85 <- function(x) {base::paste0(uj::v(ig85), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ig80 <- function(x) {base::paste0(uj::v(ig80), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ig75 <- function(x) {base::paste0(uj::v(ig75), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ig70 <- function(x) {base::paste0(uj::v(ig70), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ig65 <- function(x) {base::paste0(uj::v(ig65), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ig60 <- function(x) {base::paste0(uj::v(ig60), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ig55 <- function(x) {base::paste0(uj::v(ig55), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ig50 <- function(x) {base::paste0(uj::v(ig50), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ig45 <- function(x) {base::paste0(uj::v(ig45), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ig40 <- function(x) {base::paste0(uj::v(ig40), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ig35 <- function(x) {base::paste0(uj::v(ig35), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ig30 <- function(x) {base::paste0(uj::v(ig30), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ig25 <- function(x) {base::paste0(uj::v(ig25), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ig20 <- function(x) {base::paste0(uj::v(ig20), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ig15 <- function(x) {base::paste0(uj::v(ig15), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ig10 <- function(x) {base::paste0(uj::v(ig10), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
ig05 <- function(x) {base::paste0(uj::v(ig05), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
iblk <- function(x) {base::paste0(uj::v(iblk), x,  uj::v(iclose))}

#' @rdname markdown_help
#' @export
iinv <- function(x) {base::paste0(uj::v(iinv), x,  uj::v(iclose))}
