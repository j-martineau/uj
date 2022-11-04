#' @name markdown_uj
#' @family plots
#' @family colors
#' @title Build markdown expressions for styled and colored ggtext.
#' @description build_markdown Style text as subscript.
#' @details Style text as subscript, superscript, bold color, and plain
#'   color. The following markdown styling function names do not follow the
#'   conventions described later:
#'   \tabular{lll}{
#'     TEXT STYLING   \tab REPRESENTATION\tab NOTE                           \cr
#'     subscript      \tab \code{subs}   \tab —                              \cr
#'     superscript    \tab \code{subs}   \tab —                              \cr
#'     bold black     \tab \code{bblk}   \tab —                              \cr
#'     bold white     \tab \code{bwht}   \tab —                              \cr
#'     bold invisible \tab \code{binv}   \tab 1/256th opaque white           \cr
#'     plain black    \tab \code{pblk}   \tab —                              \cr
#'     plain white    \tab \code{pwht}   \tab —                              \cr
#'     plain invisible\tab \code{pinv}   \tab 1/256th opaque white             }
#'   For all other markdown styling functions, conventions are as
#'   follows:\tabular{lll}{
#'     CONVENTION   \tab VALUE    \tab TEXT STYLING                          \cr
#'     1st character\tab\code{p}  \tab plain.                                \cr
#'     1st character\tab\code{b}  \tab bold.                                 \cr
#'     2nd character\tab\code{f}  \tab full intensity version of color.      \cr
#'     2nd character\tab\code{l}  \tab light version of color.               \cr
#'     2nd character\tab\code{m}  \tab medium version of color.              \cr
#'     2nd character\tab\code{d}  \tab dark version of color.                \cr
#'     2nd character\tab\code{g}  \tab greyscale.                            \cr
#'     suffix       \tab\code{blu}\tab blue.                                 \cr
#'     suffix       \tab\code{cyn}\tab cyan.                                 \cr
#'     suffix       \tab\code{grn}\tab green.                                \cr
#'     suffix       \tab\code{mag}\tab magenta.                              \cr
#'     suffix       \tab\code{orn}\tab orange.                               \cr
#'     suffix       \tab\code{red}\tab red.                                  \cr
#'     suffix       \tab\code{vlt}\tab violet.                               \cr
#'     suffix       \tab\code{ylw}\tab yellow.                               \cr
#'     suffix       \tab\code{05} \tab 5% of the way from black to white     \cr
#'     suffix       \tab\code{10} \tab 10% of the way from black to white    \cr
#'     suffix       \tab\code{15} \tab 15% of the way from black to white    \cr
#'     suffix       \tab\code{20} \tab 20% of the way from black to white    \cr
#'     suffix       \tab\code{25} \tab 25% of the way from black to white    \cr
#'     suffix       \tab\code{30} \tab 30% of the way from black to white    \cr
#'     suffix       \tab\code{35} \tab 35% of the way from black to white    \cr
#'     suffix       \tab\code{40} \tab 40% of the way from black to white    \cr
#'     suffix       \tab\code{45} \tab 45% of the way from black to white    \cr
#'     suffix       \tab\code{50} \tab 50% of the way from black to white    \cr
#'     suffix       \tab\code{55} \tab 55% of the way from black to white    \cr
#'     suffix       \tab\code{60} \tab 60% of the way from black to white    \cr
#'     suffix       \tab\code{65} \tab 65% of the way from black to white    \cr
#'     suffix       \tab\code{70} \tab 70% of the way from black to white    \cr
#'     suffix       \tab\code{75} \tab 75% of the way from black to white    \cr
#'     suffix       \tab\code{80} \tab 80% of the way from black to white    \cr
#'     suffix       \tab\code{85} \tab 85% of the way from black to white    \cr
#'     suffix       \tab\code{90} \tab 90% of the way from black to white    \cr
#'     suffix       \tab\code{95} \tab 95% of the way from black to white      }
#' @param x Character vector of text to be styled.
#' @return Markdown-styled character vector
#' @export
markdown_uj <- function() {help("markdown_uj", package = "uj")}

#' @describeIn markdown_uj Style text as subscript.
#' @export
subs <- function(x) {paste0(v(sub1), x, v(sub2))}

#' @describeIn markdown_uj Style text as superscript.
#' @export
sups <- function(x) {paste0(v(sup1), x, v(sup2))}

#' @describeIn markdown_uj Style text as bold and full blue.
#' @export
bfblu <- function(x) {paste0(v(bblu), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and light blue.
#' @export
blblu <- function(x) {paste0(v(blblu), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and medium blue.
#' @export
bmblu <- function(x) {paste0(v(bmblu), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and dark blue.
#' @export
bdblu <- function(x) {paste0(v(bdblu), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and full cyan.
#' @export
bfcyn <- function(x) {paste0(v(bcyn), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and light cyan.
#' @export
blcyn <- function(x) {paste0(v(blcyn), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and medium cyan.
#' @export
bmcyn <- function(x) {paste0(v(bmcyn), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and dark cyan.
#' @export
bdcyn <- function(x) {paste0(v(bdcyn), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and full green.
#' @export
bfgrn <- function(x) {paste0(v(bgrn), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and light green.
#' @export
blgrn <- function(x) {paste0(v(blgrn), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and medium green.
#' @export
bmgrn <- function(x) {paste0(v(bmgrn), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and dark green.
#' @export
bdgrn <- function(x) {paste0(v(bdgrn), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and full magenta.
#' @export
bfmag <- function(x) {paste0(v(bmag), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and light magenta.
#' @export
blmag <- function(x) {paste0(v(blmag), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and medium magenta.
#' @export
bmmag <- function(x) {paste0(v(bmmag), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and dark magenta.
#' @export
bdmag <- function(x) {paste0(v(bdmag), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and full orange.
#' @export
bforn <- function(x) {paste0(v(born), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and light orange.
#' @export
blorn <- function(x) {paste0(v(blorn), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and medium orange.
#' @export
bmorn <- function(x) {paste0(v(bmorn), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and dark orange.
#' @export
bdorn <- function(x) {paste0(v(bdorn), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 10% grey.
#' @export
bfred <- function(x) {paste0(v(bred), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and light red.
#' @export
blred <- function(x) {paste0(v(blred), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and medium red.
#' @export
bmred <- function(x) {paste0(v(bmred), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and dark red.
#' @export
bdred <- function(x) {paste0(v(bdred), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and full violet.
#' @export
bfvlt <- function(x) {paste0(v(bvlt), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and light violet.
#' @export
blvlt <- function(x) {paste0(v(blvlt), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and medium violet.
#' @export
bmvlt <- function(x) {paste0(v(bmvlt), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and dark violet.
#' @export
bdvlt <- function(x) {paste0(v(bdvlt), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 10% grey.
#' @export
bfylw <- function(x) {paste0(v(bylw), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and light yellow.
#' @export
blylw <- function(x) {paste0(v(blylw), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and medium yellow.
#' @export
bmylw <- function(x) {paste0(v(bmylw), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and dark yellow.
#' @export
bdylw <- function(x) {paste0(v(bdylw), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 10% grey.
#' @export
bwht <- function(x) {paste0(v(bwht), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 95% grey.
#' @export
bg95 <- function(x) {paste0(v(bg95), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 90% grey.
#' @export
bg90 <- function(x) {paste0(v(bg90), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 85% grey.
#' @export
bg85 <- function(x) {paste0(v(bg85), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 80% grey.
#' @export
bg80 <- function(x) {paste0(v(bg80), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 70% grey.
#' @export
bg75 <- function(x) {paste0(v(bg75), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 70% grey.
#' @export
bg70 <- function(x) {paste0(v(bg70), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 65% grey.
#' @export
bg65 <- function(x) {paste0(v(bg65), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 60% grey.
#' @export
bg60 <- function(x) {paste0(v(bg60), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 55% grey.
#' @export
bg55 <- function(x) {paste0(v(bg55), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 50% grey.
#' @export
bg50 <- function(x) {paste0(v(bg50), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 45% grey.
#' @export
bg45 <- function(x) {paste0(v(bg45), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 40% grey.
#' @export
bg40 <- function(x) {paste0(v(bg40), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 35% grey.
#' @export
bg35 <- function(x) {paste0(v(bg35), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 30% grey.
#' @export
bg30 <- function(x) {paste0(v(bg30), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 25% grey.
#' @export
bg25 <- function(x) {paste0(v(bg25), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 20% grey.
#' @export
bg20 <- function(x) {paste0(v(bg20), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 15% grey.
#' @export
bg15 <- function(x) {paste0(v(bg15), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 10% grey.
#' @export
bg10 <- function(x) {paste0(v(bg10), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and 10% grey.
#' @export
bg05 <- function(x) {paste0(v(bg05), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and black.
#' @export
bblk <- function(x) {paste0(v(bblk), x, v(bold2))}

#' @describeIn markdown_uj Style text as bold and invisible (1/256th opaque).
#' @export
binv <- function(x) {paste0(v(binv), x, v(bold2))}

#' @describeIn markdown_uj Style text as plain and full blue.
#' @export
pfblu <- function(x) {paste0(v(pblu), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and light blue.
#' @export
plblu <- function(x) {paste0(v(plblu), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and medium blue.
#' @export
pmblu <- function(x) {paste0(v(pmblu), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and dark blue.
#' @export
pdblu <- function(x) {paste0(v(pdblu), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and full cyan.
#' @export
pfcyn <- function(x) {paste0(v(pcyn), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and light cyan.
#' @export
plcyn <- function(x) {paste0(v(plcyn), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and medium cyan.
#' @export
pmcyn <- function(x) {paste0(v(pmcyn), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and dark cyan.
#' @export
pdcyn <- function(x) {paste0(v(pdcyn), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and full green.
#' @export
pgrn <- function(x) {paste0(v(pgrn), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and light green.
#' @export
plgrn <- function(x) {paste0(v(plgrn), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and medium green.
#' @export
pmgrn <- function(x) {paste0(v(pmgrn), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and dark green.
#' @export
pdgrn <- function(x) {paste0(v(pdgrn), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and full magenta.
#' @export
pfmag <- function(x) {paste0(v(pmag), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and light magenta.
#' @export
plmag <- function(x) {paste0(v(plmag), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and medium magenta.
#' @export
pmmag <- function(x) {paste0(v(pmmag), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and dark magenta.
#' @export
pdmag <- function(x) {paste0(v(pdmag), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and full orange.
#' @export
pforn <- function(x) {paste0(v(porn), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and light orange.
#' @export
plorn <- function(x) {paste0(v(plorn), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and medium orange.
#' @export
pmorn <- function(x) {paste0(v(pmorn), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and dark orange.
#' @export
pdorn <- function(x) {paste0(v(pdorn), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 10% grey.
#' @export
pfred <- function(x) {paste0(v(pred), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and light red.
#' @export
plred <- function(x) {paste0(v(plred), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and medium red.
#' @export
pmred <- function(x) {paste0(v(pmred), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and dark red.
#' @export
pdred <- function(x) {paste0(v(pdred), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and full violet.
#' @export
pfvlt <- function(x) {paste0(v(pvlt), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and light violet.
#' @export
plvlt <- function(x) {paste0(v(plvlt), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and medium violet.
#' @export
pmvlt <- function(x) {paste0(v(pmvlt), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and dark violet.
#' @export
pdvlt <- function(x) {paste0(v(pdvlt), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 10% grey.
#' @export
pfylw <- function(x) {paste0(v(pylw), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and light yellow.
#' @export
plylw <- function(x) {paste0(v(plylw), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and medium yellow.
#' @export
pmylw <- function(x) {paste0(v(pmylw), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and dark yellow.
#' @export
pdylw <- function(x) {paste0(v(pdylw), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 10% grey.
#' @export
pwht <- function(x) {paste0(v(pwht), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 95% grey.
#' @export
pg95 <- function(x) {paste0(v(pg95), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 90% grey.
#' @export
pg90 <- function(x) {paste0(v(pg90), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 85% grey.
#' @export
pg85 <- function(x) {paste0(v(pg85), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 80% grey.
#' @export
pg80 <- function(x) {paste0(v(pg80), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 70% grey.
#' @export
pg75 <- function(x) {paste0(v(pg75), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 70% grey.
#' @export
pg70 <- function(x) {paste0(v(pg70), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 65% grey.
#' @export
pg65 <- function(x) {paste0(v(pg65), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 60% grey.
#' @export
pg60 <- function(x) {paste0(v(pg60), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 55% grey.
#' @export
pg55 <- function(x) {paste0(v(pg55), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 50% grey.
#' @export
pg50 <- function(x) {paste0(v(pg50), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 45% grey.
#' @export
pg45 <- function(x) {paste0(v(pg45), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 40% grey.
#' @export
pg40 <- function(x) {paste0(v(pg40), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 35% grey.
#' @export
pg35 <- function(x) {paste0(v(pg35), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 30% grey.
#' @export
pg30 <- function(x) {paste0(v(pg30), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 25% grey.
#' @export
pg25 <- function(x) {paste0(v(pg25), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 20% grey.
#' @export
pg20 <- function(x) {paste0(v(pg20), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 15% grey.
#' @export
pg15 <- function(x) {paste0(v(pg15), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 10% grey.
#' @export
pg10 <- function(x) {paste0(v(pg10), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and 10% grey.
#' @export
pg05 <- function(x) {paste0(v(pg05), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain and black.
#' @export
pblk <- function(x) {paste0(v(pblk), x, v(plain2))}

#' @describeIn markdown_uj Style text as plain invisible (1/256th opaque).
#' @export
pinv <- function(x) {paste0(v(pinv), x, v(plain2))}
