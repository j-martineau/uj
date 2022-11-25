#' @name mkd
#' @family plots
#' @family colors
#' @title Build Markdown Expressions for Styled and Colored `ggtext`
#' @description Style text as subscript, superscript, bold color, and plain
#'   color. The following markdown styling function names do not follow the
#'   conventions described later:
#'   \tabular{lll}{
#'     TEXT STYLING      \tab REPRESENTATION   \tab NOTE                     \cr
#'     subscript         \tab `subs`           \tab —                        \cr
#'     superscript       \tab `subs`           \tab —                        \cr
#'     bold black        \tab `bblk`           \tab —                        \cr
#'     bold white        \tab `bwht`           \tab —                        \cr
#'     bold invisible    \tab `binv`           \tab 1/256th opaque white.    \cr
#'     plain black       \tab `pblk`           \tab —                        \cr
#'     plain white       \tab `pwht`           \tab —                        \cr
#'     plain invisible   \tab `pinv`           \tab 1/256th opaque white.      }
#'   For all other markdown styling functions, conventions are as
#'   follows:\tabular{lll}{
#'     CONVENTION      \tab VALUE   \tab TEXT STYLING                        \cr
#'     1st character   \tab `p`     \tab Plain.                              \cr
#'     1st character   \tab `b`     \tab Bold.                               \cr
#'     2nd character   \tab `f`     \tab Full intensity version of color.    \cr
#'     2nd character   \tab `l`     \tab Light version of color.             \cr
#'     2nd character   \tab `m`     \tab Medium version of color.            \cr
#'     2nd character   \tab `d`     \tab Dark version of color.              \cr
#'     2nd character   \tab `g`     \tab Grey.                               \cr
#'     suffix          \tab `blu`   \tab Blue.                               \cr
#'     suffix          \tab `cyn`   \tab Cyan.                               \cr
#'     suffix          \tab `grn`   \tab Green.                              \cr
#'     suffix          \tab `mag`   \tab Magenta.                            \cr
#'     suffix          \tab `orn`   \tab Orange.                             \cr
#'     suffix          \tab `red`   \tab Red.                                \cr
#'     suffix          \tab `vlt`   \tab Violet.                             \cr
#'     suffix          \tab `ylw`   \tab Yellow.                             \cr
#'     suffix          \tab `05`    \tab 5% of the way from black to white.  \cr
#'     suffix          \tab `10`    \tab 10% of the way from black to white. \cr
#'     suffix          \tab `15`    \tab 15% of the way from black to white. \cr
#'     suffix          \tab `20`    \tab 20% of the way from black to white. \cr
#'     suffix          \tab `25`    \tab 25% of the way from black to white. \cr
#'     suffix          \tab `30`    \tab 30% of the way from black to white. \cr
#'     suffix          \tab `35`    \tab 35% of the way from black to white. \cr
#'     suffix          \tab `40`    \tab 40% of the way from black to white. \cr
#'     suffix          \tab `45`    \tab 45% of the way from black to white. \cr
#'     suffix          \tab `50`    \tab 50% of the way from black to white. \cr
#'     suffix          \tab `55`    \tab 55% of the way from black to white. \cr
#'     suffix          \tab `60`    \tab 60% of the way from black to white. \cr
#'     suffix          \tab `65`    \tab 65% of the way from black to white. \cr
#'     suffix          \tab `70`    \tab 70% of the way from black to white. \cr
#'     suffix          \tab `75`    \tab 75% of the way from black to white. \cr
#'     suffix          \tab `80`    \tab 80% of the way from black to white. \cr
#'     suffix          \tab `85`    \tab 85% of the way from black to white. \cr
#'     suffix          \tab `90`    \tab 90% of the way from black to white. \cr
#'     suffix          \tab `95`    \tab 95% of the way from black to white.   }
#' @param x A \link[=cmp_chr_vec]{character vector} of text to be styled.
#' @return Markdown-styled character vector
#' @export
mkd <- NULL

#' @rdname mkd
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

#' @describeIn mkd Style text as plain and 15% grey.
#' @export
pg15 <- function(x) {paste0(v(pg15), x, v(plain2))}

#' @describeIn mkd Style text as plain and 10% grey.
#' @export
pg10 <- function(x) {paste0(v(pg10), x, v(plain2))}

#' @describeIn mkd Style text as plain and 10% grey.
#' @export
pg05 <- function(x) {paste0(v(pg05), x, v(plain2))}

#' @describeIn mkd Style text as plain and black.
#' @export
pblk <- function(x) {paste0(v(pblk), x, v(plain2))}

#' @describeIn mkd Style text as plain invisible (1/256th opaque).
#' @export
pinv <- function(x) {paste0(v(pinv), x, v(plain2))}
