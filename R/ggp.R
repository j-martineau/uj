#' @name ggp
#' @encoding UTF-8
#' @family plots
#' @family wraps
#' @title Thin wrappers of `ggplot2` and `ggtext` functions
#' @details These functions thinly wrap `ggplot2` functions unless otherwise noted:
#' \tabular{ll}{  `ggp`      \tab \code{\link[ggplot2]{ggplot}}                                   \cr   \tab   \cr
#'                `ggsav`    \tab \code{\link[ggplot2]{ggsave}}                                   \cr   \tab   \cr
#'                `ggaes`    \tab \code{\link[ggplot2]{aes}}                                      \cr
#'                `ggexp`    \tab \code{\link[ggplot2]{expansion}}                                \cr
#'                `ggmar`    \tab \code{\link[ggplot2]{margin}}                                   \cr   \tab   \cr
#'                `ggunit`   \tab \code{\link[ggplot2]{unit}}                                     \cr
#'                `ggvars`   \tab \code{\link[ggplot2]{vars}}                                     \cr   \tab   \cr
#'                `ggdot`    \tab \code{\link[ggplot2]{geom_point}}                               \cr
#'                `gghlin`   \tab \code{\link[ggplot2]{geom_hline}}                               \cr
#'                `gglab`    \tab \code{\link[ggplot2]{geom_label}}                               \cr
#'                `gglin`    \tab \code{\link[ggplot2]{geom_line}}                                \cr
#'                `ggtxt`    \tab \code{\link[ggplot2]{geom_text}}                                \cr
#'                `ggvlin`   \tab \code{\link[ggplot2]{geom_vline}}                               \cr   \tab   \cr
#'                `gglabs`   \tab \code{\link[ggplot2]{labs}}                                     \cr
#'                `ggxlab`   \tab \code{\link[ggplot2]{xlab}}                                     \cr
#'                `ggylab`   \tab \code{\link[ggplot2]{ylab}}                                     \cr   \tab   \cr
#'                `gggrid`   \tab \code{\link[ggplot2]{facet_grid}}                               \cr
#'                `ggwrap`   \tab \code{\link[ggplot2]{facet_wrap}}                               \cr   \tab   \cr
#'                `ggthm`    \tab \code{\link[ggplot2]{theme}}                                    \cr
#'                `ggtmin`   \tab \code{\link[ggplot2]{theme_minimal}}                            \cr   \tab   \cr
#'                `ggsac`    \tab \code{\link[ggplot2]{scale_alpha_continuous}}                   \cr
#'                `ggscm`    \tab \code{\link[ggplot2]{scale_color_manual}}                       \cr
#'                `ggsdm`    \tab \code{\link[ggplot2]{scale_shape_manual}}                       \cr
#'                `ggslm`    \tab \code{\link[ggplot2]{scale_linetype_manual}}                    \cr
#'                `ggsxc`    \tab \code{\link[ggplot2]{scale_x_continuous}}                       \cr
#'                `ggsyc`    \tab \code{\link[ggplot2]{scale_y_continuous}}                       \cr   \tab   \cr
#'                `ggerct`   \tab \code{\link[ggplot2]{element_rect}}                             \cr
#'                `ggetxt`   \tab \code{\link[ggplot2]{element_text}}                             \cr
#'                `ggelin`   \tab \code{\link[ggplot2]{element_line}}                             \cr
#'                `ggemkd`   \tab \code{\link[ggtext:element_markdown]{ggtext::element_markdown}} \cr
#'                `ggenll`   \tab \code{\link[ggplot2]{element_blank}}                              }
#' @export
ggp <- function(...) {uj::run_alias("ggplot2", "ggplot")}

#' @rdname ggp
#' @export
ggaes <- function(...) {uj::run_alias("ggplot2", "aes")}

#' @rdname ggp
#' @export
ggdot <- function(...) {uj::run_alias("ggplot2", "geom_point")}

#' @rdname ggp
#' @export
ggelin <- function(...) {uj::run_alias("ggplot2", "element_line")}

#' @rdname ggp
#' @export
ggemkd <- function(...) {uj::run_alias("ggtext", "element_markdown")}

#' @rdname ggp
#' @export
ggenll <- function(...) {uj::run_alias("ggplot2", "element_blank")}

#' @rdname ggp
#' @export
ggerct <- function(...) {uj::run_alias("ggplot2", "element_rect")}

#' @rdname ggp
#' @export
ggetxt <- function(...) {uj::run_alias("ggplot2", "element_text")}

#' @rdname ggp
#' @export
ggexp <- function(...) {uj::run_alias("ggplot2", "expansion")}

#' @rdname ggp
#' @export
gggrid <- function(...) {uj::run_alias("ggplot2", "facet_grid")}

#' @rdname ggp
#' @export
gghlin <- function(...) {uj::run_alias("ggplot2", "geom_hline")}

#' @rdname ggp
#' @export
gglab <- function(...) {uj::run_alias("ggplot2", "geom_label")}

#' @rdname ggp
#' @export
gglabs <- function(...) {uj::run_alias("ggplot2", "labs")}

#' @rdname ggp
#' @export
gglin <- function(...) {uj::run_alias("ggplot2", "geom_line")}

#' @rdname ggp
#' @export
ggmar <- function(...) {uj::run_alias("ggplot2", "margin")}

#' @rdname ggp
#' @export
ggsac <- function(...) {uj::run_alias("ggplot2", "scale_alpha_continuous")}

#' @rdname ggp
#' @export
ggscm <- function(...) {uj::run_alias("ggplot2", "scale_color_manual")}

#' @rdname ggp
#' @export
ggsdm <- function(...) {uj::run_alias("ggplot2", "scale_shape_manual")}

#' @rdname ggp
#' @export
ggslm <- function(...) {uj::run_alias("ggplot2", "scale_linetype_manual")}

#' @rdname ggp
#' @export
ggsav <- function(...) {uj::run_alias("ggplot2", "ggsave")}

#' @rdname ggp
#' @export
ggsxc <- function(...) {uj::run_alias("ggplot2", "scale_x_continuous")}

#' @rdname ggp
#' @export
ggsyc <- function(...) {uj::run_alias("ggplot2", "scale_y_continuous")}

#' @rdname ggp
#' @export
ggthm <- function(...) {uj::run_alias("ggplot2", "theme")}

#' @rdname ggp
#' @export
ggtmin <- function(...) {uj::run_alias("ggplot2", "theme_minimal")}

#' @rdname ggp
#' @export
ggtxt <- function(...) {uj::run_alias("ggplot2", "geom_text")}

#' @rdname ggp
#' @export
ggunit <- function(...) {uj::run_alias("ggplot2", "unit")}

#' @rdname ggp
#' @export
ggvars <- function(...) {uj::run_alias("ggplot2", "vars")}

#' @rdname ggp
#' @export
ggvlin <- function(...) {uj::run_alias("ggplot2", "geom_vline")}

#' @rdname ggp
#' @export
ggwrap <- function(...) {uj::run_alias("ggplot2", "facet_wrap")}

#' @rdname ggp
#' @export
ggxlab <- function(...) {uj::run_alias("ggplot2", "xlab")}

#' @rdname ggp
#' @export
ggylab <- function(...) {uj::run_alias("ggplot2", "ylab")}
