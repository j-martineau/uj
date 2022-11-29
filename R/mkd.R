#' @name mkd
#' @family plots
#' @family colors
#' @title Build markdown expressions for styled + colored `ggtext`
#' @description **Subscript/superscript** functions are:
#' \itemize{
#'   \item **`subs`**: subscript.
#'   \item **`sups`**: superscript.
#' }
#' \cr
#' **Black, white, and invisible text** functions are:
#' \itemize{
#'   \item **`bblk` vs. `pblk`** for bold vs. plain black text.
#'   \item **`bwht` vs. `pwht`** for bold vs. plain white text.
#'   \item **`binv` vs. `pinv`** for bold vs. plain invisible text (use to ensure consistent spacing).
#' }
#' \cr
#' **Greyscale** text function names take the following form:
#' \cr **`bgXX` vs. `pgXX`** for bold vs. plain grey text where `XX` runs from `05` to `95` in increments of `5` to indicate what percentage white the color is. For example, `bg45` produces bold grey text with a white:black ratio of `45:55`.
#' \cr\cr
#' **Other common-color** text function names take the following forms:
#' \itemize{
#'   \item **prefix = `b` vs. `p`** for bold vs. plain text.
#'   \item **infix = `f` vs. `l` vs. `m` vs. `d`** for full-intensity, light, medium, and dark versions.
#'   \item **suffix = `blu`** for blue.
#'   \item **suffix = `cyn`** for cyan.
#'   \item **suffix = `grn`** for green.
#'   \item **suffix = `mag`** for magenta.
#'   \item **suffix = `orn`** for orange.
#'   \item **suffix = `red`** for red.
#'   \item **suffix = `vlt`** for violet.
#'   \item **suffix = `ylw`** for yellow.
#' }
#' @param x A \link[=cmp_chr_vec]{character vector} of text to be styled.
#' @return Markdown-styled character vector
#' @export
subs <- function(x) {paste0(v(sub1), x, v(sub2))}

#' @rdname mkd
#' @export
sups <- function(x) {paste0(v(sup1), x, v(sup2))}

#' @rdname mkd
#' @export
bfblu <- function(x) {paste0(v(bblu), x, v(bold2))}

#' @rdname mkd
#' @export
blblu <- function(x) {paste0(v(blblu), x, v(bold2))}

#' @rdname mkd
#' @export
bmblu <- function(x) {paste0(v(bmblu), x, v(bold2))}

#' @rdname mkd
#' @export
bdblu <- function(x) {paste0(v(bdblu), x, v(bold2))}

#' @rdname mkd
#' @export
bfcyn <- function(x) {paste0(v(bcyn), x, v(bold2))}

#' @rdname mkd
#' @export
blcyn <- function(x) {paste0(v(blcyn), x, v(bold2))}

#' @rdname mkd
#' @export
bmcyn <- function(x) {paste0(v(bmcyn), x, v(bold2))}

#' @rdname mkd
#' @export
bdcyn <- function(x) {paste0(v(bdcyn), x, v(bold2))}

#' @rdname mkd
#' @export
bfgrn <- function(x) {paste0(v(bgrn), x, v(bold2))}

#' @rdname mkd
#' @export
blgrn <- function(x) {paste0(v(blgrn), x, v(bold2))}

#' @rdname mkd
#' @export
bmgrn <- function(x) {paste0(v(bmgrn), x, v(bold2))}

#' @rdname mkd
#' @export
bdgrn <- function(x) {paste0(v(bdgrn), x, v(bold2))}

#' @rdname mkd
#' @export
bfmag <- function(x) {paste0(v(bmag), x, v(bold2))}

#' @rdname mkd
#' @export
blmag <- function(x) {paste0(v(blmag), x, v(bold2))}

#' @rdname mkd
#' @export
bmmag <- function(x) {paste0(v(bmmag), x, v(bold2))}

#' @rdname mkd
#' @export
bdmag <- function(x) {paste0(v(bdmag), x, v(bold2))}

#' @rdname mkd
#' @export
bforn <- function(x) {paste0(v(born), x, v(bold2))}

#' @rdname mkd
#' @export
blorn <- function(x) {paste0(v(blorn), x, v(bold2))}

#' @rdname mkd
#' @export
bmorn <- function(x) {paste0(v(bmorn), x, v(bold2))}

#' @rdname mkd
#' @export
bdorn <- function(x) {paste0(v(bdorn), x, v(bold2))}

#' @rdname mkd
#' @export
bfred <- function(x) {paste0(v(bred), x, v(bold2))}

#' @rdname mkd
#' @export
blred <- function(x) {paste0(v(blred), x, v(bold2))}

#' @rdname mkd
#' @export
bmred <- function(x) {paste0(v(bmred), x, v(bold2))}

#' @rdname mkd
#' @export
bdred <- function(x) {paste0(v(bdred), x, v(bold2))}

#' @rdname mkd
#' @export
bfvlt <- function(x) {paste0(v(bvlt), x, v(bold2))}

#' @rdname mkd
#' @export
blvlt <- function(x) {paste0(v(blvlt), x, v(bold2))}

#' @rdname mkd
#' @export
bmvlt <- function(x) {paste0(v(bmvlt), x, v(bold2))}

#' @rdname mkd
#' @export
bdvlt <- function(x) {paste0(v(bdvlt), x, v(bold2))}

#' @rdname mkd
#' @export
bfylw <- function(x) {paste0(v(bylw), x, v(bold2))}

#' @rdname mkd
#' @export
blylw <- function(x) {paste0(v(blylw), x, v(bold2))}

#' @rdname mkd
#' @export
bmylw <- function(x) {paste0(v(bmylw), x, v(bold2))}

#' @rdname mkd
#' @export
bdylw <- function(x) {paste0(v(bdylw), x, v(bold2))}

#' @rdname mkd
#' @export
bwht <- function(x) {paste0(v(bwht), x, v(bold2))}

#' @rdname mkd
#' @export
bg95 <- function(x) {paste0(v(bg95), x, v(bold2))}

#' @rdname mkd
#' @export
bg90 <- function(x) {paste0(v(bg90), x, v(bold2))}

#' @rdname mkd
#' @export
bg85 <- function(x) {paste0(v(bg85), x, v(bold2))}

#' @rdname mkd
#' @export
bg80 <- function(x) {paste0(v(bg80), x, v(bold2))}

#' @rdname mkd
#' @export
bg75 <- function(x) {paste0(v(bg75), x, v(bold2))}

#' @rdname mkd
#' @export
bg70 <- function(x) {paste0(v(bg70), x, v(bold2))}

#' @rdname mkd
#' @export
bg65 <- function(x) {paste0(v(bg65), x, v(bold2))}

#' @rdname mkd
#' @export
bg60 <- function(x) {paste0(v(bg60), x, v(bold2))}

#' @rdname mkd
#' @export
bg55 <- function(x) {paste0(v(bg55), x, v(bold2))}

#' @rdname mkd
#' @export
bg50 <- function(x) {paste0(v(bg50), x, v(bold2))}

#' @rdname mkd
#' @export
bg45 <- function(x) {paste0(v(bg45), x, v(bold2))}

#' @rdname mkd
#' @export
bg40 <- function(x) {paste0(v(bg40), x, v(bold2))}

#' @rdname mkd
#' @export
bg35 <- function(x) {paste0(v(bg35), x, v(bold2))}

#' @rdname mkd
#' @export
bg30 <- function(x) {paste0(v(bg30), x, v(bold2))}

#' @rdname mkd
#' @export
bg25 <- function(x) {paste0(v(bg25), x, v(bold2))}

#' @rdname mkd
#' @export
bg20 <- function(x) {paste0(v(bg20), x, v(bold2))}

#' @rdname mkd
#' @export
bg15 <- function(x) {paste0(v(bg15), x, v(bold2))}

#' @rdname mkd
#' @export
bg10 <- function(x) {paste0(v(bg10), x, v(bold2))}

#' @rdname mkd
#' @export
bg05 <- function(x) {paste0(v(bg05), x, v(bold2))}

#' @rdname mkd
#' @export
bblk <- function(x) {paste0(v(bblk), x, v(bold2))}

#' @rdname mkd
#' @export
binv <- function(x) {paste0(v(binv), x, v(bold2))}

#' @rdname mkd
#' @export
pfblu <- function(x) {paste0(v(pblu), x, v(plain2))}

#' @rdname mkd
#' @export
plblu <- function(x) {paste0(v(plblu), x, v(plain2))}

#' @rdname mkd
#' @export
pmblu <- function(x) {paste0(v(pmblu), x, v(plain2))}

#' @rdname mkd
#' @export
pdblu <- function(x) {paste0(v(pdblu), x, v(plain2))}

#' @rdname mkd
#' @export
pfcyn <- function(x) {paste0(v(pcyn), x, v(plain2))}

#' @rdname mkd
#' @export
plcyn <- function(x) {paste0(v(plcyn), x, v(plain2))}

#' @rdname mkd
#' @export
pmcyn <- function(x) {paste0(v(pmcyn), x, v(plain2))}

#' @rdname mkd
#' @export
pdcyn <- function(x) {paste0(v(pdcyn), x, v(plain2))}

#' @rdname mkd
#' @export
pfgrn <- function(x) {paste0(v(pfgrn), x, v(plain2))}

#' @rdname mkd
#' @export
plgrn <- function(x) {paste0(v(plgrn), x, v(plain2))}

#' @rdname mkd
#' @export
pmgrn <- function(x) {paste0(v(pmgrn), x, v(plain2))}

#' @rdname mkd
#' @export
pdgrn <- function(x) {paste0(v(pdgrn), x, v(plain2))}

#' @rdname mkd
#' @export
pfmag <- function(x) {paste0(v(pmag), x, v(plain2))}

#' @rdname mkd
#' @export
plmag <- function(x) {paste0(v(plmag), x, v(plain2))}

#' @rdname mkd
#' @export
pmmag <- function(x) {paste0(v(pmmag), x, v(plain2))}

#' @rdname mkd
#' @export
pdmag <- function(x) {paste0(v(pdmag), x, v(plain2))}

#' @rdname mkd
#' @export
pforn <- function(x) {paste0(v(porn), x, v(plain2))}

#' @rdname mkd
#' @export
plorn <- function(x) {paste0(v(plorn), x, v(plain2))}

#' @rdname mkd
#' @export
pmorn <- function(x) {paste0(v(pmorn), x, v(plain2))}

#' @rdname mkd
#' @export
pdorn <- function(x) {paste0(v(pdorn), x, v(plain2))}

#' @rdname mkd
#' @export
pfred <- function(x) {paste0(v(pred), x, v(plain2))}

#' @rdname mkd
#' @export
plred <- function(x) {paste0(v(plred), x, v(plain2))}

#' @rdname mkd
#' @export
pmred <- function(x) {paste0(v(pmred), x, v(plain2))}

#' @rdname mkd
#' @export
pdred <- function(x) {paste0(v(pdred), x, v(plain2))}

#' @rdname mkd
#' @export
pfvlt <- function(x) {paste0(v(pvlt), x, v(plain2))}

#' @rdname mkd
#' @export
plvlt <- function(x) {paste0(v(plvlt), x, v(plain2))}

#' @rdname mkd
#' @export
pmvlt <- function(x) {paste0(v(pmvlt), x, v(plain2))}

#' @rdname mkd
#' @export
pdvlt <- function(x) {paste0(v(pdvlt), x, v(plain2))}

#' @rdname mkd
#' @export
pfylw <- function(x) {paste0(v(pylw), x, v(plain2))}

#' @rdname mkd
#' @export
plylw <- function(x) {paste0(v(plylw), x, v(plain2))}

#' @rdname mkd
#' @export
pmylw <- function(x) {paste0(v(pmylw), x, v(plain2))}

#' @rdname mkd
#' @export
pdylw <- function(x) {paste0(v(pdylw), x, v(plain2))}

#' @rdname mkd
#' @export
pwht <- function(x) {paste0(v(pwht), x, v(plain2))}

#' @rdname mkd
#' @export
pg95 <- function(x) {paste0(v(pg95), x, v(plain2))}

#' @rdname mkd
#' @export
pg90 <- function(x) {paste0(v(pg90), x, v(plain2))}

#' @rdname mkd
#' @export
pg85 <- function(x) {paste0(v(pg85), x, v(plain2))}

#' @rdname mkd
#' @export
pg80 <- function(x) {paste0(v(pg80), x, v(plain2))}

#' @rdname mkd
#' @export
pg75 <- function(x) {paste0(v(pg75), x, v(plain2))}

#' @rdname mkd
#' @export
pg70 <- function(x) {paste0(v(pg70), x, v(plain2))}

#' @rdname mkd
#' @export
pg65 <- function(x) {paste0(v(pg65), x, v(plain2))}

#' @rdname mkd
#' @export
pg60 <- function(x) {paste0(v(pg60), x, v(plain2))}

#' @rdname mkd
#' @export
pg55 <- function(x) {paste0(v(pg55), x, v(plain2))}

#' @rdname mkd
#' @export
pg50 <- function(x) {paste0(v(pg50), x, v(plain2))}

#' @rdname mkd
#' @export
pg45 <- function(x) {paste0(v(pg45), x, v(plain2))}

#' @rdname mkd
#' @export
pg40 <- function(x) {paste0(v(pg40), x, v(plain2))}

#' @rdname mkd
#' @export
pg35 <- function(x) {paste0(v(pg35), x, v(plain2))}

#' @rdname mkd
#' @export
pg30 <- function(x) {paste0(v(pg30), x, v(plain2))}

#' @rdname mkd
#' @export
pg25 <- function(x) {paste0(v(pg25), x, v(plain2))}

#' @rdname mkd
#' @export
pg20 <- function(x) {paste0(v(pg20), x, v(plain2))}

#' @rdname mkd
#' @export
pg15 <- function(x) {paste0(v(pg15), x, v(plain2))}

#' @rdname mkd
#' @export
pg10 <- function(x) {paste0(v(pg10), x, v(plain2))}

#' @rdname mkd
#' @export
pg05 <- function(x) {paste0(v(pg05), x, v(plain2))}

#' @rdname mkd
#' @export
pblk <- function(x) {paste0(v(pblk), x, v(plain2))}

#' @rdname mkd
#' @export
pinv <- function(x) {paste0(v(pinv), x, v(plain2))}
