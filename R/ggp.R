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
ggp <- ggplot2::ggplot

#' @rdname ggp
#' @export
gg_aes <- ggplot2::aes

#' @rdname ggp
#' @export
gg_dot <- ggplot2::geom_point

#' @rdname ggp
#' @export
gg_elin <- ggplot2::element_line

#' @rdname ggp
#' @export
gg_emkd <- ggtext::element_markdown

#' @rdname ggp
#' @export
gg_enll <- ggplot2::element_blank

#' @rdname ggp
#' @export
gg_erct <- ggplot2::element_rect

#' @rdname ggp
#' @export
gg_etxt <- ggplot2::element_text

#' @rdname ggp
#' @export
gg_exp <- ggplot2::expansion

#' @rdname ggp
#' @export
gg_grid <- ggplot2::facet_grid

#' @rdname ggp
#' @export
gg_hlin <- ggplot2::geom_hline

#' @rdname ggp
#' @export
gg_lab <- ggplot2::geom_label

#' @rdname ggp
#' @export
gg_labs <- ggplot2::labs

#' @rdname ggp
#' @export
gg_lin <- ggplot2::geom_line

#' @rdname ggp
#' @export
gg_mar <- ggplot2::margin

#' @rdname ggp
#' @export
gg_sac <- ggplot2::scale_alpha_continuous

#' @rdname ggp
#' @export
gg_scm <- ggplot2::scale_color_manual

#' @rdname ggp
#' @export
gg_sdm <- ggplot2::scale_shape_manual

#' @rdname ggp
#' @export
gg_slm <- ggplot2::scale_linetype_manual

#' @rdname ggp
#' @export
gg_sav <- ggplot2::ggsave

#' @rdname ggp
#' @export
gg_sxc <- ggplot2::scale_x_continuous

#' @rdname ggp
#' @export
gg_syc <- ggplot2::scale_y_continuous

#' @rdname ggp
#' @export
gg_thm <- ggplot2::theme

#' @rdname ggp
#' @export
gg_thm0 <- ggplot2::theme_minimal

#' @rdname ggp
#' @export
gg_txt <- ggplot2::geom_text

#' @rdname ggp
#' @export
gg_unit <- ggplot2::unit

#' @rdname ggp
#' @export
gg_vars <- ggplot2::vars

#' @rdname ggp
#' @export
gg_vlin <- ggplot2::geom_vline

#' @rdname ggp
#' @export
gg_wrap <- ggplot2::facet_wrap

#' @rdname ggp
#' @export
gg_xlab <- ggplot2::xlab

#' @rdname ggp
#' @export
gg_ylab <- ggplot2::ylab
