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
#' \tabular{ll}{  `blk`   \tab black        \cr
#'                `wht`   \tab white        \cr
#'                `inv`   \tab invisible      }
#' \cr For example, the function `b_wht` styles markdown text as bold and white.
#' \cr\cr **Greyscale** text suffixes take the form:
#' \tabular{ll}{  `g05`   \tab ` 5:95` white:black ratio \cr
#'                `g10`   \tab `10:90` white:black ratio \cr
#'                `...`   \tab `...`                     \cr
#'                `g90`   \tab `90:10` white:black ratio \cr
#'                `g95`   \tab `95:5 ` white:black ratio   }
#' \cr For example, the function `p_g45` styles markdown text as plain and grey with a `45:55` white:black ratio.
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
#' \cr For example, the function `pf_red` styles markdown text as plain and full-intensity red.
#' @param x A \link[=cmp_chr_vec]{character vector} of text to be styled.
#' @return A markdown-styled character vector.
#' @export
markdown <- function() {utils::help("markdown", package = "uj")}

#' @rdname markdown
#' @export
subs <- function(x) {base::paste0(uj::v(sub1), x, uj::v(sub2))}

#' @rdname markdown
#' @export
sups <- function(x) {base::paste0(uj::v(sup1), x, uj::v(sup2))}

#' @rdname markdown
#' @export
bf_blu <- function(x) {base::paste0(uj::v(bf_blu), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bl_blu <- function(x) {base::paste0(uj::v(bl_blu), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bm_blu <- function(x) {base::paste0(uj::v(bm_blu), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bd_blu <- function(x) {base::paste0(uj::v(bd_blu), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bf_cyn <- function(x) {base::paste0(uj::v(bf_cyn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bl_cyn <- function(x) {base::paste0(uj::v(bl_cyn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bm_cyn <- function(x) {base::paste0(uj::v(bm_cyn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bd_cyn <- function(x) {base::paste0(uj::v(bd_cyn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bf_grn <- function(x) {base::paste0(uj::v(bf_grn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bl_grn <- function(x) {base::paste0(uj::v(bl_grn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bm_grn <- function(x) {base::paste0(uj::v(bm_grn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bd_grn <- function(x) {base::paste0(uj::v(bd_grn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bf_mag <- function(x) {base::paste0(uj::v(bf_mag), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bl_mag <- function(x) {base::paste0(uj::v(bl_mag), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bm_mag <- function(x) {base::paste0(uj::v(bm_mag), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bd_mag <- function(x) {base::paste0(uj::v(bd_mag), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bf_orn <- function(x) {base::paste0(uj::v(bf_orn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bl_orn <- function(x) {base::paste0(uj::v(bl_orn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bm_orn <- function(x) {base::paste0(uj::v(bm_orn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bd_orn <- function(x) {base::paste0(uj::v(bd_orn), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bf_red <- function(x) {base::paste0(uj::v(bf_red), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bl_red <- function(x) {base::paste0(uj::v(bl_red), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bm_red <- function(x) {base::paste0(uj::v(bm_red), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bd_red <- function(x) {base::paste0(uj::v(bd_red), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bf_vlt <- function(x) {base::paste0(uj::v(bf_vlt), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bl_vlt <- function(x) {base::paste0(uj::v(bl_vlt), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bm_vlt <- function(x) {base::paste0(uj::v(bm_vlt), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bd_vlt <- function(x) {base::paste0(uj::v(bd_vlt), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bf_ylw <- function(x) {base::paste0(uj::v(bf_ylw), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bl_ylw <- function(x) {base::paste0(uj::v(bl_ylw), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bm_ylw <- function(x) {base::paste0(uj::v(bm_ylw), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bd_ylw <- function(x) {base::paste0(uj::v(bd_ylw), x, uj::v(bold2))}

#' @rdname markdown
#' @export
b_wht <- function(x) {base::paste0(uj::v(b_wht), x, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g95 <- function(x) {base::paste0(uj::v(b_g95), x, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g90 <- function(x) {base::paste0(uj::v(b_g90), x, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g85 <- function(x) {base::paste0(uj::v(b_g85), x, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g80 <- function(x) {base::paste0(uj::v(b_g80), x, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g75 <- function(x) {base::paste0(uj::v(b_g75), x, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g70 <- function(x) {base::paste0(uj::v(b_g70), x, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g65 <- function(x) {base::paste0(uj::v(b_g65), x, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g60 <- function(x) {base::paste0(uj::v(b_g60), x, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g55 <- function(x) {base::paste0(uj::v(b_g55), x, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g50 <- function(x) {base::paste0(uj::v(b_g50), x, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g45 <- function(x) {base::paste0(uj::v(b_g45), x, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g40 <- function(x) {base::paste0(uj::v(b_g40), x, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g35 <- function(x) {base::paste0(uj::v(b_g35), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg_30 <- function(x) {base::paste0(uj::v(b_g30), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg_25 <- function(x) {base::paste0(uj::v(b_g25), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg_20 <- function(x) {base::paste0(uj::v(b_g20), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg_15 <- function(x) {base::paste0(uj::v(b_g15), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg_10 <- function(x) {base::paste0(uj::v(b_g10), x, uj::v(bold2))}

#' @rdname markdown
#' @export
bg_05 <- function(x) {base::paste0(uj::v(b_g05), x, uj::v(bold2))}

#' @rdname markdown
#' @export
b_blk <- function(x) {base::paste0(uj::v(b_blk), x, uj::v(bold2))}

#' @rdname markdown
#' @export
b_inv <- function(x) {base::paste0(uj::v(b_inv), x, uj::v(bold2))}

#' @rdname markdown
#' @export
pf_blu <- function(x) {base::paste0(uj::v(pf_blu), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pl_blu <- function(x) {base::paste0(uj::v(pl_blu), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pm_blu <- function(x) {base::paste0(uj::v(pm_blu), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pd_blu <- function(x) {base::paste0(uj::v(pd_blu), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pf_cyn <- function(x) {base::paste0(uj::v(pf_cyn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pl_cyn <- function(x) {base::paste0(uj::v(pl_cyn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pm_cyn <- function(x) {base::paste0(uj::v(pm_cyn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pd_cyn <- function(x) {base::paste0(uj::v(pd_cyn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pf_grn <- function(x) {base::paste0(uj::v(pf_grn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pl_grn <- function(x) {base::paste0(uj::v(pl_grn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pm_grn <- function(x) {base::paste0(uj::v(pm_grn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pd_grn <- function(x) {base::paste0(uj::v(pd_grn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pf_mag <- function(x) {base::paste0(uj::v(pf_mag), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pl_mag <- function(x) {base::paste0(uj::v(pl_mag), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pm_mag <- function(x) {base::paste0(uj::v(pm_mag), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pd_mag <- function(x) {base::paste0(uj::v(pd_mag), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pf_orn <- function(x) {base::paste0(uj::v(pf_orn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pl_orn <- function(x) {base::paste0(uj::v(pl_orn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pm_orn <- function(x) {base::paste0(uj::v(pm_orn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pd_orn <- function(x) {base::paste0(uj::v(pd_orn), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pf_red <- function(x) {base::paste0(uj::v(pf_red), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pl_red <- function(x) {base::paste0(uj::v(pl_red), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pm_red <- function(x) {base::paste0(uj::v(pm_red), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pd_red <- function(x) {base::paste0(uj::v(pd_red), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pf_vlt <- function(x) {base::paste0(uj::v(pf_vlt), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pl_vlt <- function(x) {base::paste0(uj::v(pl_vlt), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pm_vlt <- function(x) {base::paste0(uj::v(pm_vlt), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pd_vlt <- function(x) {base::paste0(uj::v(pd_vlt), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pf_ylw <- function(x) {base::paste0(uj::v(pf_ylw), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pl_ylw <- function(x) {base::paste0(uj::v(pl_ylw), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pm_ylw <- function(x) {base::paste0(uj::v(pm_ylw), x, uj::v(plain2))}

#' @rdname markdown
#' @export
pd_ylw <- function(x) {base::paste0(uj::v(pd_ylw), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_wht <- function(x) {base::paste0(uj::v(p_wht), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g95 <- function(x) {base::paste0(uj::v(p_g95), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g90 <- function(x) {base::paste0(uj::v(p_g90), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g85 <- function(x) {base::paste0(uj::v(p_g85), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g80 <- function(x) {base::paste0(uj::v(p_g80), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g75 <- function(x) {base::paste0(uj::v(p_g75), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g70 <- function(x) {base::paste0(uj::v(p_g70), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g65 <- function(x) {base::paste0(uj::v(p_g65), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g60 <- function(x) {base::paste0(uj::v(p_g60), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g55 <- function(x) {base::paste0(uj::v(p_g55), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g50 <- function(x) {base::paste0(uj::v(p_g50), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g45 <- function(x) {base::paste0(uj::v(p_g45), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g40 <- function(x) {base::paste0(uj::v(p_g40), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g35 <- function(x) {base::paste0(uj::v(p_g35), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g30 <- function(x) {base::paste0(uj::v(p_g30), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g25 <- function(x) {base::paste0(uj::v(p_g25), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g20 <- function(x) {base::paste0(uj::v(p_g20), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g15 <- function(x) {base::paste0(uj::v(p_g15), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g10 <- function(x) {base::paste0(uj::v(p_g10), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g05 <- function(x) {base::paste0(uj::v(p_g05), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_blk <- function(x) {base::paste0(uj::v(p_blk), x, uj::v(plain2))}

#' @rdname markdown
#' @export
p_inv <- function(x) {base::paste0(uj::v(p_inv), x, uj::v(plain2))}
