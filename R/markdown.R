#' @name markdown
#' @encoding UTF-8
#' @family strings
#' @family colors
#' @family plots
#' @title Build markdown expressions for styled + colored `ggtext` strings
#' @description *Subscript/superscript* functions are:
#' \tabular{rl}{
#'       `subs` \tab   subscript
#'   \cr `sups` \tab   superscript
#' }
#' *All remaining functions* in this family start with a prefix character as follows:
#' \tabular{rl}{
#'       prefix = `b` \tab   bold
#'   \cr prefix = `p` \tab   plain
#' }
#' *Black, white, and invisible text* functions suffixes are:
#' \tabular{rl}{
#'       `blk` \tab   black
#'   \cr `wht` \tab   white
#'   \cr `inv` \tab   invisible
#' }
#' For example, the function `bwht` styles markdown text as bold and white.
#' \cr\cr *Greyscale* text suffixes take the form:
#' \tabular{rl}{
#'       `g05` \tab   ` 5:95` white:black ratio
#'   \cr `g10` \tab   `10:90` white:black ratio
#'   \cr `...` \tab   `...`
#'   \cr `g90` \tab   `90:10` white:black ratio
#'   \cr `g95` \tab   `95:5 ` white:black ratio
#' }
#' For example, the function `pg45` styles markdown text as plain and grey with a `45:55` white:black ratio.
#' \cr\cr *Other common-color text* function names append an infix and a suffix to the selected prefix, as follows:
#' \tabular{rl}{
#'       suffix = `blu` \tab   blue
#'   \cr suffix = `cyn` \tab   cyan
#'   \cr suffix = `grn` \tab   green
#'   \cr suffix = `mag` \tab   magenta
#'   \cr suffix = `orn` \tab   orange
#'   \cr suffix = `red` \tab   red
#'   \cr suffix = `vlt` \tab   violet
#'   \cr suffix = `ylw` \tab   yellow
#'   \cr                \tab  
#'   \cr    infix = `f` \tab   full-intensity
#'   \cr    infix = `l` \tab   light
#'   \cr    infix = `m` \tab   medium
#'   \cr    infix = `d` \tab   dark
#' }
#' For example, the function `pfred` styles markdown text as plain and full-intensity red.
#' @param x A \link[=cmp_chr_vec]{character vector} of text to be styled.
#' @return A markdown-styled character vector.
#' @export
subs <- function(x) {paste0(v(sub1), x, v(sub2))}

#' @rdname markdown
#' @export
sups <- function(x) {paste0(v(sup1), x, v(sup2))}

#' @rdname markdown
#' @export
bfblu <- function(x) {paste0(v(bblu), x, v(bold2))}

#' @rdname markdown
#' @export
blblu <- function(x) {paste0(v(blblu), x, v(bold2))}

#' @rdname markdown
#' @export
bmblu <- function(x) {paste0(v(bmblu), x, v(bold2))}

#' @rdname markdown
#' @export
bdblu <- function(x) {paste0(v(bdblu), x, v(bold2))}

#' @rdname markdown
#' @export
bfcyn <- function(x) {paste0(v(bcyn), x, v(bold2))}

#' @rdname markdown
#' @export
blcyn <- function(x) {paste0(v(blcyn), x, v(bold2))}

#' @rdname markdown
#' @export
bmcyn <- function(x) {paste0(v(bmcyn), x, v(bold2))}

#' @rdname markdown
#' @export
bdcyn <- function(x) {paste0(v(bdcyn), x, v(bold2))}

#' @rdname markdown
#' @export
bfgrn <- function(x) {paste0(v(bgrn), x, v(bold2))}

#' @rdname markdown
#' @export
blgrn <- function(x) {paste0(v(blgrn), x, v(bold2))}

#' @rdname markdown
#' @export
bmgrn <- function(x) {paste0(v(bmgrn), x, v(bold2))}

#' @rdname markdown
#' @export
bdgrn <- function(x) {paste0(v(bdgrn), x, v(bold2))}

#' @rdname markdown
#' @export
bfmag <- function(x) {paste0(v(bmag), x, v(bold2))}

#' @rdname markdown
#' @export
blmag <- function(x) {paste0(v(blmag), x, v(bold2))}

#' @rdname markdown
#' @export
bmmag <- function(x) {paste0(v(bmmag), x, v(bold2))}

#' @rdname markdown
#' @export
bdmag <- function(x) {paste0(v(bdmag), x, v(bold2))}

#' @rdname markdown
#' @export
bforn <- function(x) {paste0(v(born), x, v(bold2))}

#' @rdname markdown
#' @export
blorn <- function(x) {paste0(v(blorn), x, v(bold2))}

#' @rdname markdown
#' @export
bmorn <- function(x) {paste0(v(bmorn), x, v(bold2))}

#' @rdname markdown
#' @export
bdorn <- function(x) {paste0(v(bdorn), x, v(bold2))}

#' @rdname markdown
#' @export
bfred <- function(x) {paste0(v(bred), x, v(bold2))}

#' @rdname markdown
#' @export
blred <- function(x) {paste0(v(blred), x, v(bold2))}

#' @rdname markdown
#' @export
bmred <- function(x) {paste0(v(bmred), x, v(bold2))}

#' @rdname markdown
#' @export
bdred <- function(x) {paste0(v(bdred), x, v(bold2))}

#' @rdname markdown
#' @export
bfvlt <- function(x) {paste0(v(bvlt), x, v(bold2))}

#' @rdname markdown
#' @export
blvlt <- function(x) {paste0(v(blvlt), x, v(bold2))}

#' @rdname markdown
#' @export
bmvlt <- function(x) {paste0(v(bmvlt), x, v(bold2))}

#' @rdname markdown
#' @export
bdvlt <- function(x) {paste0(v(bdvlt), x, v(bold2))}

#' @rdname markdown
#' @export
bfylw <- function(x) {paste0(v(bylw), x, v(bold2))}

#' @rdname markdown
#' @export
blylw <- function(x) {paste0(v(blylw), x, v(bold2))}

#' @rdname markdown
#' @export
bmylw <- function(x) {paste0(v(bmylw), x, v(bold2))}

#' @rdname markdown
#' @export
bdylw <- function(x) {paste0(v(bdylw), x, v(bold2))}

#' @rdname markdown
#' @export
bwht <- function(x) {paste0(v(bwht), x, v(bold2))}

#' @rdname markdown
#' @export
bg95 <- function(x) {paste0(v(bg95), x, v(bold2))}

#' @rdname markdown
#' @export
bg90 <- function(x) {paste0(v(bg90), x, v(bold2))}

#' @rdname markdown
#' @export
bg85 <- function(x) {paste0(v(bg85), x, v(bold2))}

#' @rdname markdown
#' @export
bg80 <- function(x) {paste0(v(bg80), x, v(bold2))}

#' @rdname markdown
#' @export
bg75 <- function(x) {paste0(v(bg75), x, v(bold2))}

#' @rdname markdown
#' @export
bg70 <- function(x) {paste0(v(bg70), x, v(bold2))}

#' @rdname markdown
#' @export
bg65 <- function(x) {paste0(v(bg65), x, v(bold2))}

#' @rdname markdown
#' @export
bg60 <- function(x) {paste0(v(bg60), x, v(bold2))}

#' @rdname markdown
#' @export
bg55 <- function(x) {paste0(v(bg55), x, v(bold2))}

#' @rdname markdown
#' @export
bg50 <- function(x) {paste0(v(bg50), x, v(bold2))}

#' @rdname markdown
#' @export
bg45 <- function(x) {paste0(v(bg45), x, v(bold2))}

#' @rdname markdown
#' @export
bg40 <- function(x) {paste0(v(bg40), x, v(bold2))}

#' @rdname markdown
#' @export
bg35 <- function(x) {paste0(v(bg35), x, v(bold2))}

#' @rdname markdown
#' @export
bg30 <- function(x) {paste0(v(bg30), x, v(bold2))}

#' @rdname markdown
#' @export
bg25 <- function(x) {paste0(v(bg25), x, v(bold2))}

#' @rdname markdown
#' @export
bg20 <- function(x) {paste0(v(bg20), x, v(bold2))}

#' @rdname markdown
#' @export
bg15 <- function(x) {paste0(v(bg15), x, v(bold2))}

#' @rdname markdown
#' @export
bg10 <- function(x) {paste0(v(bg10), x, v(bold2))}

#' @rdname markdown
#' @export
bg05 <- function(x) {paste0(v(bg05), x, v(bold2))}

#' @rdname markdown
#' @export
bblk <- function(x) {paste0(v(bblk), x, v(bold2))}

#' @rdname markdown
#' @export
binv <- function(x) {paste0(v(binv), x, v(bold2))}

#' @rdname markdown
#' @export
pfblu <- function(x) {paste0(v(pblu), x, v(plain2))}

#' @rdname markdown
#' @export
plblu <- function(x) {paste0(v(plblu), x, v(plain2))}

#' @rdname markdown
#' @export
pmblu <- function(x) {paste0(v(pmblu), x, v(plain2))}

#' @rdname markdown
#' @export
pdblu <- function(x) {paste0(v(pdblu), x, v(plain2))}

#' @rdname markdown
#' @export
pfcyn <- function(x) {paste0(v(pcyn), x, v(plain2))}

#' @rdname markdown
#' @export
plcyn <- function(x) {paste0(v(plcyn), x, v(plain2))}

#' @rdname markdown
#' @export
pmcyn <- function(x) {paste0(v(pmcyn), x, v(plain2))}

#' @rdname markdown
#' @export
pdcyn <- function(x) {paste0(v(pdcyn), x, v(plain2))}

#' @rdname markdown
#' @export
pfgrn <- function(x) {paste0(v(pfgrn), x, v(plain2))}

#' @rdname markdown
#' @export
plgrn <- function(x) {paste0(v(plgrn), x, v(plain2))}

#' @rdname markdown
#' @export
pmgrn <- function(x) {paste0(v(pmgrn), x, v(plain2))}

#' @rdname markdown
#' @export
pdgrn <- function(x) {paste0(v(pdgrn), x, v(plain2))}

#' @rdname markdown
#' @export
pfmag <- function(x) {paste0(v(pmag), x, v(plain2))}

#' @rdname markdown
#' @export
plmag <- function(x) {paste0(v(plmag), x, v(plain2))}

#' @rdname markdown
#' @export
pmmag <- function(x) {paste0(v(pmmag), x, v(plain2))}

#' @rdname markdown
#' @export
pdmag <- function(x) {paste0(v(pdmag), x, v(plain2))}

#' @rdname markdown
#' @export
pforn <- function(x) {paste0(v(porn), x, v(plain2))}

#' @rdname markdown
#' @export
plorn <- function(x) {paste0(v(plorn), x, v(plain2))}

#' @rdname markdown
#' @export
pmorn <- function(x) {paste0(v(pmorn), x, v(plain2))}

#' @rdname markdown
#' @export
pdorn <- function(x) {paste0(v(pdorn), x, v(plain2))}

#' @rdname markdown
#' @export
pfred <- function(x) {paste0(v(pred), x, v(plain2))}

#' @rdname markdown
#' @export
plred <- function(x) {paste0(v(plred), x, v(plain2))}

#' @rdname markdown
#' @export
pmred <- function(x) {paste0(v(pmred), x, v(plain2))}

#' @rdname markdown
#' @export
pdred <- function(x) {paste0(v(pdred), x, v(plain2))}

#' @rdname markdown
#' @export
pfvlt <- function(x) {paste0(v(pvlt), x, v(plain2))}

#' @rdname markdown
#' @export
plvlt <- function(x) {paste0(v(plvlt), x, v(plain2))}

#' @rdname markdown
#' @export
pmvlt <- function(x) {paste0(v(pmvlt), x, v(plain2))}

#' @rdname markdown
#' @export
pdvlt <- function(x) {paste0(v(pdvlt), x, v(plain2))}

#' @rdname markdown
#' @export
pfylw <- function(x) {paste0(v(pylw), x, v(plain2))}

#' @rdname markdown
#' @export
plylw <- function(x) {paste0(v(plylw), x, v(plain2))}

#' @rdname markdown
#' @export
pmylw <- function(x) {paste0(v(pmylw), x, v(plain2))}

#' @rdname markdown
#' @export
pdylw <- function(x) {paste0(v(pdylw), x, v(plain2))}

#' @rdname markdown
#' @export
pwht <- function(x) {paste0(v(pwht), x, v(plain2))}

#' @rdname markdown
#' @export
pg95 <- function(x) {paste0(v(pg95), x, v(plain2))}

#' @rdname markdown
#' @export
pg90 <- function(x) {paste0(v(pg90), x, v(plain2))}

#' @rdname markdown
#' @export
pg85 <- function(x) {paste0(v(pg85), x, v(plain2))}

#' @rdname markdown
#' @export
pg80 <- function(x) {paste0(v(pg80), x, v(plain2))}

#' @rdname markdown
#' @export
pg75 <- function(x) {paste0(v(pg75), x, v(plain2))}

#' @rdname markdown
#' @export
pg70 <- function(x) {paste0(v(pg70), x, v(plain2))}

#' @rdname markdown
#' @export
pg65 <- function(x) {paste0(v(pg65), x, v(plain2))}

#' @rdname markdown
#' @export
pg60 <- function(x) {paste0(v(pg60), x, v(plain2))}

#' @rdname markdown
#' @export
pg55 <- function(x) {paste0(v(pg55), x, v(plain2))}

#' @rdname markdown
#' @export
pg50 <- function(x) {paste0(v(pg50), x, v(plain2))}

#' @rdname markdown
#' @export
pg45 <- function(x) {paste0(v(pg45), x, v(plain2))}

#' @rdname markdown
#' @export
pg40 <- function(x) {paste0(v(pg40), x, v(plain2))}

#' @rdname markdown
#' @export
pg35 <- function(x) {paste0(v(pg35), x, v(plain2))}

#' @rdname markdown
#' @export
pg30 <- function(x) {paste0(v(pg30), x, v(plain2))}

#' @rdname markdown
#' @export
pg25 <- function(x) {paste0(v(pg25), x, v(plain2))}

#' @rdname markdown
#' @export
pg20 <- function(x) {paste0(v(pg20), x, v(plain2))}

#' @rdname markdown
#' @export
pg15 <- function(x) {paste0(v(pg15), x, v(plain2))}

#' @rdname markdown
#' @export
pg10 <- function(x) {paste0(v(pg10), x, v(plain2))}

#' @rdname markdown
#' @export
pg05 <- function(x) {paste0(v(pg05), x, v(plain2))}

#' @rdname markdown
#' @export
pblk <- function(x) {paste0(v(pblk), x, v(plain2))}

#' @rdname markdown
#' @export
pinv <- function(x) {paste0(v(pinv), x, v(plain2))}
