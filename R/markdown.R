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
bf_blu <- function(X) {base::paste0(uj::v(bf_blu), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bl_blu <- function(X) {base::paste0(uj::v(bl_blu), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bm_blu <- function(X) {base::paste0(uj::v(bm_blu), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bd_blu <- function(X) {base::paste0(uj::v(bd_blu), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bf_cyn <- function(X) {base::paste0(uj::v(bf_cyn), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bl_cyn <- function(X) {base::paste0(uj::v(bl_cyn), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bm_cyn <- function(X) {base::paste0(uj::v(bm_cyn), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bd_cyn <- function(X) {base::paste0(uj::v(bd_cyn), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bf_grn <- function(X) {base::paste0(uj::v(bf_grn), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bl_grn <- function(X) {base::paste0(uj::v(bl_grn), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bm_grn <- function(X) {base::paste0(uj::v(bm_grn), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bd_grn <- function(X) {base::paste0(uj::v(bd_grn), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bf_mag <- function(X) {base::paste0(uj::v(bf_mag), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bl_mag <- function(X) {base::paste0(uj::v(bl_mag), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bm_mag <- function(X) {base::paste0(uj::v(bm_mag), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bd_mag <- function(X) {base::paste0(uj::v(bd_mag), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bf_orn <- function(X) {base::paste0(uj::v(bf_orn), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bl_orn <- function(X) {base::paste0(uj::v(bl_orn), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bm_orn <- function(X) {base::paste0(uj::v(bm_orn), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bd_orn <- function(X) {base::paste0(uj::v(bd_orn), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bf_red <- function(X) {base::paste0(uj::v(bf_red), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bl_red <- function(X) {base::paste0(uj::v(bl_red), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bm_red <- function(X) {base::paste0(uj::v(bm_red), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bd_red <- function(X) {base::paste0(uj::v(bd_red), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bf_vlt <- function(X) {base::paste0(uj::v(bf_vlt), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bl_vlt <- function(X) {base::paste0(uj::v(bl_vlt), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bm_vlt <- function(X) {base::paste0(uj::v(bm_vlt), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bd_vlt <- function(X) {base::paste0(uj::v(bd_vlt), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bf_ylw <- function(X) {base::paste0(uj::v(bf_ylw), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bl_ylw <- function(X) {base::paste0(uj::v(bl_ylw), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bm_ylw <- function(X) {base::paste0(uj::v(bm_ylw), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bd_ylw <- function(X) {base::paste0(uj::v(bd_ylw), X, uj::v(bold2))}

#' @rdname markdown
#' @export
b_wht <- function(X) {base::paste0(uj::v(b_wht), X, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g95 <- function(X) {base::paste0(uj::v(b_g95), X, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g90 <- function(X) {base::paste0(uj::v(b_g90), X, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g85 <- function(X) {base::paste0(uj::v(b_g85), X, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g80 <- function(X) {base::paste0(uj::v(b_g80), X, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g75 <- function(X) {base::paste0(uj::v(b_g75), X, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g70 <- function(X) {base::paste0(uj::v(b_g70), X, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g65 <- function(X) {base::paste0(uj::v(b_g65), X, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g60 <- function(X) {base::paste0(uj::v(b_g60), X, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g55 <- function(X) {base::paste0(uj::v(b_g55), X, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g50 <- function(X) {base::paste0(uj::v(b_g50), X, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g45 <- function(X) {base::paste0(uj::v(b_g45), X, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g40 <- function(X) {base::paste0(uj::v(b_g40), X, uj::v(bold2))}

#' @rdname markdown
#' @export
b_g35 <- function(X) {base::paste0(uj::v(b_g35), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bg_30 <- function(X) {base::paste0(uj::v(b_g30), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bg_25 <- function(X) {base::paste0(uj::v(b_g25), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bg_20 <- function(X) {base::paste0(uj::v(b_g20), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bg_15 <- function(X) {base::paste0(uj::v(b_g15), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bg_10 <- function(X) {base::paste0(uj::v(b_g10), X, uj::v(bold2))}

#' @rdname markdown
#' @export
bg_05 <- function(X) {base::paste0(uj::v(b_g05), X, uj::v(bold2))}

#' @rdname markdown
#' @export
b_blk <- function(X) {base::paste0(uj::v(b_blk), X, uj::v(bold2))}

#' @rdname markdown
#' @export
b_inv <- function(X) {base::paste0(uj::v(b_inv), X, uj::v(bold2))}

#' @rdname markdown
#' @export
pf_blu <- function(X) {base::paste0(uj::v(pf_blu), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pl_blu <- function(X) {base::paste0(uj::v(pl_blu), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pm_blu <- function(X) {base::paste0(uj::v(pm_blu), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pd_blu <- function(X) {base::paste0(uj::v(pd_blu), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pf_cyn <- function(X) {base::paste0(uj::v(pf_cyn), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pl_cyn <- function(X) {base::paste0(uj::v(pl_cyn), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pm_cyn <- function(X) {base::paste0(uj::v(pm_cyn), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pd_cyn <- function(X) {base::paste0(uj::v(pd_cyn), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pf_grn <- function(X) {base::paste0(uj::v(pf_grn), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pl_grn <- function(X) {base::paste0(uj::v(pl_grn), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pm_grn <- function(X) {base::paste0(uj::v(pm_grn), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pd_grn <- function(X) {base::paste0(uj::v(pd_grn), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pf_mag <- function(X) {base::paste0(uj::v(pf_mag), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pl_mag <- function(X) {base::paste0(uj::v(pl_mag), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pm_mag <- function(X) {base::paste0(uj::v(pm_mag), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pd_mag <- function(X) {base::paste0(uj::v(pd_mag), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pf_orn <- function(X) {base::paste0(uj::v(pf_orn), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pl_orn <- function(X) {base::paste0(uj::v(pl_orn), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pm_orn <- function(X) {base::paste0(uj::v(pm_orn), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pd_orn <- function(X) {base::paste0(uj::v(pd_orn), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pf_red <- function(X) {base::paste0(uj::v(pf_red), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pl_red <- function(X) {base::paste0(uj::v(pl_red), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pm_red <- function(X) {base::paste0(uj::v(pm_red), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pd_red <- function(X) {base::paste0(uj::v(pd_red), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pf_vlt <- function(X) {base::paste0(uj::v(pf_vlt), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pl_vlt <- function(X) {base::paste0(uj::v(pl_vlt), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pm_vlt <- function(X) {base::paste0(uj::v(pm_vlt), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pd_vlt <- function(X) {base::paste0(uj::v(pd_vlt), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pf_ylw <- function(X) {base::paste0(uj::v(pf_ylw), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pl_ylw <- function(X) {base::paste0(uj::v(pl_ylw), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pm_ylw <- function(X) {base::paste0(uj::v(pm_ylw), X, uj::v(plain2))}

#' @rdname markdown
#' @export
pd_ylw <- function(X) {base::paste0(uj::v(pd_ylw), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_wht <- function(X) {base::paste0(uj::v(p_wht), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g95 <- function(X) {base::paste0(uj::v(p_g95), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g90 <- function(X) {base::paste0(uj::v(p_g90), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g85 <- function(X) {base::paste0(uj::v(p_g85), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g80 <- function(X) {base::paste0(uj::v(p_g80), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g75 <- function(X) {base::paste0(uj::v(p_g75), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g70 <- function(X) {base::paste0(uj::v(p_g70), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g65 <- function(X) {base::paste0(uj::v(p_g65), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g60 <- function(X) {base::paste0(uj::v(p_g60), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g55 <- function(X) {base::paste0(uj::v(p_g55), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g50 <- function(X) {base::paste0(uj::v(p_g50), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g45 <- function(X) {base::paste0(uj::v(p_g45), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g40 <- function(X) {base::paste0(uj::v(p_g40), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g35 <- function(X) {base::paste0(uj::v(p_g35), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g30 <- function(X) {base::paste0(uj::v(p_g30), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g25 <- function(X) {base::paste0(uj::v(p_g25), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g20 <- function(X) {base::paste0(uj::v(p_g20), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g15 <- function(X) {base::paste0(uj::v(p_g15), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g10 <- function(X) {base::paste0(uj::v(p_g10), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_g05 <- function(X) {base::paste0(uj::v(p_g05), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_blk <- function(X) {base::paste0(uj::v(p_blk), X, uj::v(plain2))}

#' @rdname markdown
#' @export
p_inv <- function(X) {base::paste0(uj::v(p_inv), X, uj::v(plain2))}
