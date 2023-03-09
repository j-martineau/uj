#' @name ggp
#' @encoding UTF-8
#' @family plots
#' @family wraps
#' @title Thin wrappers of `ggplot2` and `ggtext` functions
#' @details These functions thinly wrap `ggplot2` functions unless otherwise noted:
#' \tabular{ll}{  `ggp`       \tab \code{\link[ggplot2]{ggplot}}                                   \cr   \tab   \cr
#'                `gg_sav`    \tab \code{\link[ggplot2]{ggsave}}                                   \cr   \tab   \cr
#'                `gg_aes`    \tab \code{\link[ggplot2]{aes}}                                      \cr
#'                `gg_exp`    \tab \code{\link[ggplot2]{expansion}}                                \cr
#'                `gg_mar`    \tab \code{\link[ggplot2]{margin}}                                   \cr   \tab   \cr
#'                `gg_unit`   \tab \code{\link[ggplot2]{unit}}                                     \cr
#'                `gg_vars`   \tab \code{\link[ggplot2]{vars}}                                     \cr   \tab   \cr
#'                `gg_dot`    \tab \code{\link[ggplot2]{geom_point}}                               \cr
#'                `gg_hlin`   \tab \code{\link[ggplot2]{geom_hline}}                               \cr
#'                `gg_lab`    \tab \code{\link[ggplot2]{geom_label}}                               \cr
#'                `gg_lin`    \tab \code{\link[ggplot2]{geom_line}}                                \cr
#'                `gg_txt`    \tab \code{\link[ggplot2]{geom_text}}                                \cr
#'                `gg_vlin`   \tab \code{\link[ggplot2]{geom_vline}}                               \cr   \tab   \cr
#'                `gg_labs`   \tab \code{\link[ggplot2]{labs}}                                     \cr
#'                `gg_xlab`   \tab \code{\link[ggplot2]{xlab}}                                     \cr
#'                `gg_ylab`   \tab \code{\link[ggplot2]{ylab}}                                     \cr   \tab   \cr
#'                `gg_grid`   \tab \code{\link[ggplot2]{facet_grid}}                               \cr
#'                `gg_wrap`   \tab \code{\link[ggplot2]{facet_wrap}}                               \cr   \tab   \cr
#'                `gg_thm`    \tab \code{\link[ggplot2]{theme}}                                    \cr
#'                `gg_tmin`   \tab \code{\link[ggplot2]{theme_minimal}}                            \cr   \tab   \cr
#'                `gg_sac`    \tab \code{\link[ggplot2]{scale_alpha_continuous}}                   \cr
#'                `gg_scm`    \tab \code{\link[ggplot2]{scale_color_manual}}                       \cr
#'                `gg_sdm`    \tab \code{\link[ggplot2]{scale_shape_manual}}                       \cr
#'                `gg_slm`    \tab \code{\link[ggplot2]{scale_linetype_manual}}                    \cr
#'                `gg_sxc`    \tab \code{\link[ggplot2]{scale_x_continuous}}                       \cr
#'                `gg_syc`    \tab \code{\link[ggplot2]{scale_y_continuous}}                       \cr   \tab   \cr
#'                `gg_erct`   \tab \code{\link[ggplot2]{element_rect}}                             \cr
#'                `gg_etxt`   \tab \code{\link[ggplot2]{element_text}}                             \cr
#'                `gg_elin`   \tab \code{\link[ggplot2]{element_line}}                             \cr
#'                `gg_emkd`   \tab \code{\link[ggtext:element_markdown]{ggtext::element_markdown}} \cr
#'                `gg_enll`   \tab \code{\link[ggplot2]{element_blank}}                              }
#' @export
ggp <- function(...) {uj::run_alias("ggplot2", "ggplot")}

#' @rdname ggp
#' @export
gg_aes <- function(...) {uj::run_alias("ggplot2", "aes")}

#' @rdname ggp
#' @export
gg_dot <- function(...) {uj::run_alias("ggplot2", "geom_point")}

#' @rdname ggp
#' @export
gg_elin <- function(...) {uj::run_alias("ggplot2", "element_line")}

#' @rdname ggp
#' @export
gg_emkd <- function(...) {uj::run_alias("ggtext", "element_markdown")}

#' @rdname ggp
#' @export
gg_enll <- function(...) {uj::run_alias("ggplot2", "element_blank")}

#' @rdname ggp
#' @export
gg_erct <- function(...) {uj::run_alias("ggplot2", "element_rect")}

#' @rdname ggp
#' @export
gg_etxt <- function(...) {uj::run_alias("ggplot2", "element_text")}

#' @rdname ggp
#' @export
gg_exp <- function(...) {uj::run_alias("ggplot2", "expansion")}

#' @rdname ggp
#' @export
gg_grid <- function(...) {uj::run_alias("ggplot2", "facet_grid")}

#' @rdname ggp
#' @export
gg_hlin <- function(...) {uj::run_alias("ggplot2", "geom_hline")}

#' @rdname ggp
#' @export
gg_lab <- function(...) {uj::run_alias("ggplot2", "geom_label")}

#' @rdname ggp
#' @export
gg_labs <- function(...) {uj::run_alias("ggplot2", "labs")}

#' @rdname ggp
#' @export
gg_lin <- function(...) {uj::run_alias("ggplot2", "geom_line")}

#' @rdname ggp
#' @export
gg_mar <- function(...) {uj::run_alias("ggplot2", "margin")}

#' @rdname ggp
#' @export
gg_sac <- function(...) {uj::run_alias("ggplot2", "scale_alpha_continuous")}

#' @rdname ggp
#' @export
gg_scm <- function(...) {uj::run_alias("ggplot2", "scale_color_manual")}

#' @rdname ggp
#' @export
gg_sdm <- function(...) {uj::run_alias("ggplot2", "scale_shape_manual")}

#' @rdname ggp
#' @export
gg_slm <- function(...) {uj::run_alias("ggplot2", "scale_linetype_manual")}

#' @rdname ggp
#' @export
gg_sav <- function(...) {uj::run_alias("ggplot2", "ggsave")}

#' @rdname ggp
#' @export
gg_sxc <- function(...) {uj::run_alias("ggplot2", "scale_x_continuous")}

#' @rdname ggp
#' @export
gg_syc <- function(...) {uj::run_alias("ggplot2", "scale_y_continuous")}

#' @rdname ggp
#' @export
gg_thm <- function(...) {uj::run_alias("ggplot2", "theme")}

#' @rdname ggp
#' @export
gg_thm0 <- function(...) {uj::run_alias("ggplot2", "theme_minimal")}

#' @rdname ggp
#' @export
gg_txt <- function(...) {uj::run_alias("ggplot2", "geom_text")}

#' @rdname ggp
#' @export
gg_unit <- function(...) {uj::run_alias("ggplot2", "unit")}

#' @rdname ggp
#' @export
gg_vars <- function(...) {uj::run_alias("ggplot2", "vars")}

#' @rdname ggp
#' @export
gg_vlin <- function(...) {uj::run_alias("ggplot2", "geom_vline")}

#' @rdname ggp
#' @export
gg_wrap <- function(...) {uj::run_alias("ggplot2", "facet_wrap")}

#' @rdname ggp
#' @export
gg_xlab <- function(...) {uj::run_alias("ggplot2", "xlab")}

#' @rdname ggp
#' @export
gg_ylab <- function(...) {uj::run_alias("ggplot2", "ylab")}
