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
#' \tabular{ll}{  prefix = `b`   \tab bold  \cr
#'                prefix = `p`   \tab plain   }
#' \cr\cr **Black, white, and invisible text** functions suffixes are:
#' \tabular{ll}{  `BLK`   \tab black        \cr
#'                `WHT`   \tab white        \cr
#'                `INV`   \tab invisible      }
#' \cr For example, the function `bwht` styles markdown text as bold and white.
#' \cr\cr **Greyscale** text suffixes take the form:
#' \tabular{ll}{  `G05`   \tab ` 5:95` white:black ratio \cr
#'                `G10`   \tab `10:90` white:black ratio \cr
#'                `...`   \tab `...`                     \cr
#'                `G90`   \tab `90:10` white:black ratio \cr
#'                `G95`   \tab `95:5 ` white:black ratio   }
#' \cr For example, the function `pg45` styles markdown text as plain and grey with a `45:55` white:black ratio.
#' \cr\cr **Other common-color text** function names append an infix and a suffix to the selected prefix, as follows:
#' \tabular{ll}{  suffix = `blu`   \tab blue                        \cr
#'                suffix = `cyn`   \tab cyan                        \cr
#'                suffix = `grn`   \tab green                       \cr
#'                suffix = `mag`   \tab magenta                     \cr
#'                suffix = `orn`   \tab orange                      \cr
#'                suffix = `red`   \tab red                         \cr
#'                suffix = `vlt`   \tab violet                      \cr
#'                suffix = `ylw`   \tab yellow         \cr   \tab     }
#' \tabular{ll}{  infix = `f`      \tab full intensity              \cr
#'                infix = `l`      \tab light                       \cr
#'                infix = `m`      \tab medium                      \cr
#'                infix = `d`      \tab dark                          }
#' \cr For example, the function `pfred` styles markdown text as plain and full-intensity red.
#' @param x A \link[=cmp_chr_vec]{character vector} of text to be styled.
#' @return A markdown-styled character vector.
#' @export
markdown <- function() {utils::help("markdown", package = "uj")}

#' @rdname markdown
#' @export
subs <- function(x) {uj::p0(uj::v(sub1), x, uj::v(sub2))}

#' @rdname markdown
#' @export
sups <- function(x) {uj::p0(uj::v(sup1), x, uj::v(sup2))}

#' @rdname markdown
#' @export
bfblu <- function(x) {uj::p0(uj::v(bblu), x, uj::v(bold2))}

#' @rdname markdown
#' @export
blblu <- function(x) {uj::p0(uj::v(blblu), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bmblu <- function(x) {uj::p0(uj::v(bmblu), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bdblu <- function(x) {uj::p0(uj::v(bdblu), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bfcyn <- function(x) {uj::p0(uj::v(bcyn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
blcyn <- function(x) {uj::p0(uj::v(blcyn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bmcyn <- function(x) {uj::p0(uj::v(bmcyn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bdcyn <- function(x) {uj::p0(uj::v(bdcyn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bfgrn <- function(x) {uj::p0(uj::v(bgrn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
blgrn <- function(x) {uj::p0(uj::v(blgrn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bmgrn <- function(x) {uj::p0(uj::v(bmgrn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bdgrn <- function(x) {uj::p0(uj::v(bdgrn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bfmag <- function(x) {uj::p0(uj::v(bmag), x, uj::v(bold2))}

#' @rdname markdown
#' @export
blmag <- function(x) {uj::p0(uj::v(blmag), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bmmag <- function(x) {uj::p0(uj::v(bmmag), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bdmag <- function(x) {uj::p0(uj::v(bdmag), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bforn <- function(x) {uj::p0(uj::v(born), x, uj::v(bold2))}

#' @rdname markdown
#' @export
blorn <- function(x) {uj::p0(uj::v(blorn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bmorn <- function(x) {uj::p0(uj::v(bmorn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bdorn <- function(x) {uj::p0(uj::v(bdorn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bfred <- function(x) {uj::p0(uj::v(bred), x, uj::v(bold2))}

#' @rdname markdown
#' @export
blred <- function(x) {uj::p0(uj::v(blred), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bmred <- function(x) {uj::p0(uj::v(bmred), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bdred <- function(x) {uj::p0(uj::v(bdred), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bfvlt <- function(x) {uj::p0(uj::v(bvlt), x, uj::v(bold2))}

#' @rdname markdown
#' @export
blvlt <- function(x) {uj::p0(uj::v(blvlt), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bmvlt <- function(x) {uj::p0(uj::v(bmvlt), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bdvlt <- function(x) {uj::p0(uj::v(bdvlt), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bfylw <- function(x) {uj::p0(uj::v(bylw), x, uj::v(bold2))}

#' @rdname markdown
#' @export
blylw <- function(x) {uj::p0(uj::v(blylw), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bmylw <- function(x) {uj::p0(uj::v(bmylww), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bdylw <- function(x) {uj::p0(uj::v(bdylw), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bWHT <- function(x) {uj::p0(uj::v(bWHT), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg95 <- function(x) {uj::p0(uj::v(bg95), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg90 <- function(x) {uj::p0(uj::v(bg90), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg85 <- function(x) {uj::p0(uj::v(bg85), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg80 <- function(x) {uj::p0(uj::v(bg80), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg75 <- function(x) {uj::p0(uj::v(bg75), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg70 <- function(x) {uj::p0(uj::v(bg70), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg65 <- function(x) {uj::p0(uj::v(bg65), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg60 <- function(x) {uj::p0(uj::v(bg60), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg55 <- function(x) {uj::p0(uj::v(bg55), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg50 <- function(x) {uj::p0(uj::v(bg50), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg45 <- function(x) {uj::p0(uj::v(bg45), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg40 <- function(x) {uj::p0(uj::v(bg40), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg35 <- function(x) {uj::p0(uj::v(bg35), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg30 <- function(x) {uj::p0(uj::v(bg30), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg25 <- function(x) {uj::p0(uj::v(bg25), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg20 <- function(x) {uj::p0(uj::v(bg20), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg15 <- function(x) {uj::p0(uj::v(bg15), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg10 <- function(x) {uj::p0(uj::v(bg10), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg05 <- function(x) {uj::p0(uj::v(bg05), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bBLK <- function(x) {uj::p0(uj::v(bBLK), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bINV <- function(x) {uj::p0(uj::v(bINV), x, uj::v(bold2))}

#' @rdname markdown
#' @export
pfblu <- function(x) {uj::p0(uj::v(pblu), x, uj::v(plain2))}

#' @rdname markdown
#' @export
plblu <- function(x) {uj::p0(uj::v(plblu), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pmblu <- function(x) {uj::p0(uj::v(pmblu), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pdblu <- function(x) {uj::p0(uj::v(pdblu), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pfcyn <- function(x) {uj::p0(uj::v(pcyn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
plcyn <- function(x) {uj::p0(uj::v(plcyn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pmcyn <- function(x) {uj::p0(uj::v(pmcyn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pdcyn <- function(x) {uj::p0(uj::v(pdcyn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pfgrn <- function(x) {uj::p0(uj::v(pfgrn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
plgrn <- function(x) {uj::p0(uj::v(plgrn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pmgrn <- function(x) {uj::p0(uj::v(pmgrn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pdgrn <- function(x) {uj::p0(uj::v(pdgrn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pfmag <- function(x) {uj::p0(uj::v(pmag), x, uj::v(plain2))}

#' @rdname markdown
#' @export
plmag <- function(x) {uj::p0(uj::v(plmag), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pmmag <- function(x) {uj::p0(uj::v(pmmag), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pdmag <- function(x) {uj::p0(uj::v(pdmag), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pforn <- function(x) {uj::p0(uj::v(porn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
plorn <- function(x) {uj::p0(uj::v(plorn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pmorn <- function(x) {uj::p0(uj::v(pmorn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pdorn <- function(x) {uj::p0(uj::v(pdorn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pfred <- function(x) {uj::p0(uj::v(pred), x, uj::v(plain2))}

#' @rdname markdown
#' @export
plred <- function(x) {uj::p0(uj::v(plred), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pmred <- function(x) {uj::p0(uj::v(pmred), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pdred <- function(x) {uj::p0(uj::v(pdred), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pfvlt <- function(x) {uj::p0(uj::v(pvlt), x, uj::v(plain2))}

#' @rdname markdown
#' @export
plvlt <- function(x) {uj::p0(uj::v(plvlt), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pmvlt <- function(x) {uj::p0(uj::v(pmvlt), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pdvlt <- function(x) {uj::p0(uj::v(pdvlt), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pfylw <- function(x) {uj::p0(uj::v(pylw), x, uj::v(plain2))}

#' @rdname markdown
#' @export
plylw <- function(x) {uj::p0(uj::v(plylw), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pmylw <- function(x) {uj::p0(uj::v(pmylw), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pdylw <- function(x) {uj::p0(uj::v(pdylw), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pWHT <- function(x) {uj::p0(uj::v(pWHT), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pg95 <- function(x) {uj::p0(uj::v(pg95), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pg90 <- function(x) {uj::p0(uj::v(pg90), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pg85 <- function(x) {uj::p0(uj::v(pg85), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pg80 <- function(x) {uj::p0(uj::v(pg80), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pg75 <- function(x) {uj::p0(uj::v(pg75), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pg70 <- function(x) {uj::p0(uj::v(pg70), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pg65 <- function(x) {uj::p0(uj::v(pg65), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pg60 <- function(x) {uj::p0(uj::v(pg60), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pg55 <- function(x) {uj::p0(uj::v(pg55), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pg50 <- function(x) {uj::p0(uj::v(pg50), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pg45 <- function(x) {uj::p0(uj::v(pg45), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pg40 <- function(x) {uj::p0(uj::v(pg40), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pg35 <- function(x) {uj::p0(uj::v(pg35), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pg30 <- function(x) {uj::p0(uj::v(pg30), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pg25 <- function(x) {uj::p0(uj::v(pg25), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pg20 <- function(x) {uj::p0(uj::v(pg20), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pg15 <- function(x) {uj::p0(uj::v(pg15), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pg10 <- function(x) {uj::p0(uj::v(pg10), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pg05 <- function(x) {uj::p0(uj::v(pg05), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pBLK <- function(x) {uj::p0(uj::v(pBLK), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pINV <- function(x) {uj::p0(uj::v(pINV), x, uj::v(plain2))}
