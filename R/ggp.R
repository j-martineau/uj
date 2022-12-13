#' @name ggp
#' @family wraps
#' @family plotting
#' @title Thin wrappers of `ggplot2` and `ggtext` functions
#' @description These functions wrap `ggplot2` functions unless otherwise noted:\tabular{rl}{
#'             `ggp`   \tab Thinly wraps \code{\link[ggplot2]{ggplot}}
#'   \cr      `ggsv`   \tab Thinly wraps \code{\link[ggplot2]{ggsave}}
#'   \cr               \tab  
#'   \cr     `ggaes`   \tab Thinly wraps \code{\link[ggplot2]{aes}}
#'   \cr     `ggexp`   \tab Thinly wraps \code{\link[ggplot2]{expansion}}
#'   \cr     `ggmar`   \tab Thinly wraps \code{\link[ggplot2]{margin}}
#'   \cr    `ggunit`   \tab Thinly wraps \code{\link[ggplot2]{unit}}
#'   \cr    `ggvars`   \tab Thinly wraps \code{\link[ggplot2]{vars}}
#'   \cr               \tab  
#'   \cr    `gglabs`   \tab Thinly wraps \code{\link[ggplot2]{labs}}
#'   \cr    `ggxlab`   \tab Thinly wraps \code{\link[ggplot2]{xlab}}
#'   \cr    `ggylab`   \tab Thinly wraps \code{\link[ggplot2]{ylab}}
#'   \cr               \tab  
#'   \cr     `ggdot`   \tab Thinly wraps \code{\link[ggplot2]{geom_point}}
#'   \cr     `gghln`   \tab Thinly wraps \code{\link[ggplot2]{geom_hline}}
#'   \cr     `gglab`   \tab Thinly wraps \code{\link[ggplot2]{geom_label}}
#'   \cr     `gglin`   \tab Thinly wraps \code{\link[ggplot2]{geom_line}}
#'   \cr     `ggtxt`   \tab Thinly wraps \code{\link[ggplot2]{geom_text}}
#'   \cr     `ggvln`   \tab Thinly wraps \code{\link[ggplot2]{geom_vline}}
#'   \cr               \tab  
#'   \cr    `gggrid`   \tab Thinly wraps \code{\link[ggplot2]{facet_grid}}
#'   \cr    `ggwrap`   \tab Thinly wraps \code{\link[ggplot2]{facet_wrap}}
#'   \cr               \tab  
#'   \cr     `ggthm`   \tab Thinly wraps \code{\link[ggplot2]{theme}}
#'   \cr    `ggtmin`   \tab Thinly wraps \code{\link[ggplot2]{theme_minimal}}
#'   \cr               \tab  
#'   \cr     `ggsac`   \tab Thinly wraps \code{\link[ggplot2]{scale_alpha_continuous}}
#'   \cr     `ggscm`   \tab Thinly wraps \code{\link[ggplot2]{scale_color_manual}}
#'   \cr     `ggsdm`   \tab Thinly wraps \code{\link[ggplot2]{scale_shape_manual}}
#'   \cr     `ggslm`   \tab Thinly wraps \code{\link[ggplot2]{scale_linetype_manual}}
#'   \cr     `ggsxc`   \tab Thinly wraps \code{\link[ggplot2]{scale_x_continuous}}
#'   \cr     `ggsyc`   \tab Thinly wraps \code{\link[ggplot2]{scale_y_continuous}}
#'   \cr               \tab  
#'   \cr    `ggerct`   \tab Thinly wraps \code{\link[ggplot2]{element_rect}}
#'   \cr    `ggetxt`   \tab Thinly wraps \code{\link[ggplot2]{element_text}}
#'   \cr    `ggelin`   \tab Thinly wraps \code{\link[ggplot2]{element_line}}
#'   \cr   `ggenull`   \tab Thinly wraps \code{\link[ggplot2]{element_blank}}
#'   \cr               \tab  
#'   \cr     `ggemd`   \tab Thinly wraps \code{\link[ggtext]{element_markdown}}\eqn{^1}
#' }
#' \eqn{1.} A `ggtext` function.
#' @inherit ggplot2::ggplot
#' @export
ggp <- function(data = NULL, mapping = ggplot2::aes(), ..., environment = parent.frame()) {ggplot2::ggplot(data = data, mapping = mapping, ..., environment = environment)}

#' @rdname ggp
#' @inherit ggplot2::aes
#' @export
ggaes <- function(x, y, ...) {ggplot2::aes(x, y, ...)}

#' @rdname ggp
#' @inherit ggplot2::geom_point
#' @export
ggdot <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {ggplot2::geom_point(mapping = mapping, data = data, stat = stat, position = position, ..., na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes)}

#' @rdname ggp
#' @inherit ggplot2::element_line
#' @export
ggelin <- function(colour = NULL, size = NULL, linetype = NULL, lineend = NULL, color = NULL, arrow = NULL, inherit.blank = FALSE) {ggplot2::element_line(colour = colour, size = size, linetype = linetype, lineend = lineend, color = color, arrow = arrow, inherit.blank = inherit.blank)}

#' @rdname ggp
#' @inherit ggtext::element_markdown
#' @export
ggemd <- function(family = NULL, face = NULL, size = NULL, colour = NULL, fill = NULL, box.colour = NULL, linetype = NULL, linewidth = NULL, hjust = NULL, vjust = NULL, halign = NULL, valign = NULL, angle = NULL, lineheight = NULL, margin = NULL, padding = NULL, r = NULL, color = NULL, box.color = NULL, align_widths = NULL, align_heights = NULL, rotate_margins = NULL, debug = FALSE, inherit.blank = FALSE) {ggtext::element_markdown(family = family, face = face, size = size, colour = colour, fill = fill, box.colour = box.colour, linetype = linetype, linewidth = linewidth, hjust = hjust, vjust = vjust, halign = halign, valign = valign, angle = angle, lineheight = lineheight, margin = margin, padding = padding, r = r, color = color, box.color = box.color, align_widths = align_widths, align_heights = align_heights, rotate_margins = rotate_margins, debug = debug, inherit.blank = inherit.blank)}

#' @rdname ggp
#' @inherit ggplot2::element_blank
#' @export
ggenull <- function() {ggplot2::element_blank()}

#' @rdname ggp
#' @inherit ggplot2::element_rect
#' @export
ggerct <- function(fill = NULL, colour = NULL, size = NULL, linetype = NULL, color = NULL, inherit.blank = FALSE) {ggplot2::element_rect(fill = fill, colour = colour, size = size, linetype = linetype, color = color, inherit.blank = inherit.blank)}

#' @rdname ggp
#' @inherit ggplot2::element_text
#' @export
ggetxt <- function(family = NULL, face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE) {ggplot2::element_text(family = family, face = face, colour = colour, size = size, hjust = hjust, vjust = vjust, angle = angle, lineheight = lineheight, color = color, margin = margin, debug = debug, inherit.blank = inherit.blank)}

#' @rdname ggp
#' @inherit ggplot2::expansion
#' @export
ggexp <- function(mult = 0, add = 0) {ggplot2::expansion(mult = mult, add = add)}

#' @rdname ggp
#' @inherit ggplot2::facet_grid
#' @export
gggrid <- function(rows = NULL, cols = NULL, scales = "fixed", space = "fixed", shrink = TRUE, labeller = "label_value", as.table = TRUE, switch = NULL, drop = TRUE, margins = FALSE, facets = NULL) {ggplot2::facet_grid(rows = rows, cols = cols, scales = scales, space = space, shrink = shrink, labeller = labeller, as.table = as.table, switch = switch, drop = drop, margins = margins, facets = facets)}

#' @rdname ggp
#' @inherit ggplot2::geom_hline
#' @export
gghln <- function(mapping = NULL, data = NULL, ..., yintercept, na.rm = FALSE, show.legend = NA) {ggplot2::geom_hline(mapping = mapping, data = data, ..., yintercept = yintercept, na.rm = na.rm, show.legend = show.legend)}

#' @rdname ggp
#' @inherit ggplot2::geom_label
#' @export
gglab <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ..., parse = FALSE, nudge_x = 0, nudge_y = 0, label.padding = ggplot2::unit(0.25, "lines"), label.r = ggplot2::unit(0.15, "lines"), label.size = 0.25, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {ggplot2::geom_label(mapping = mapping, data = data, stat = stat, position = position, ..., parse = parse, nudge_x = nudge_x, nudge_y = nudge_y, label.padding = label.padding, label.r = label.r, label.size = label.size, na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes)}

#' @rdname ggp
#' @inherit ggplot2::labs
#' @export
gglabs <- function(..., title = ggplot2::waiver(), subtitle = ggplot2::waiver(), caption = ggplot2::waiver(), tag = ggplot2::waiver(), alt = ggplot2::waiver(), alt_insight = ggplot2::waiver()) {ggplot2::labs(..., title = title, subtitle = subtitle, caption = caption, tag = tag, alt = alt, alt_insight = alt_insight)}

#' @rdname ggp
#' @inherit ggplot2::geom_line
#' @export
gglin <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", na.rm = FALSE, orientation = NA, show.legend = NA, inherit.aes = TRUE, ...) {ggplot2::geom_line(mapping = mapping, data = data, stat = stat, position = position, na.rm = na.rm, orientation = orientation, show.legend = show.legend, inherit.aes = inherit.aes, ...)}

#' @rdname ggp
#' @inherit ggplot2::margin
#' @export
ggmar <- function(t = 0, r = 0, b = 0, l = 0, unit = "pt") {ggplot2::margin(t = t, r = r, b = b, l = l, unit = unit)}

#' @rdname ggp
#' @inherit ggplot2::scale_alpha_continuous
#' @export
ggsac <- function(..., range = c(0.1, 1)) {ggplot2::scale_alpha_continuous(..., range = range)}

#' @rdname ggp
#' @inherit ggplot2::scale_color_manual
#' @export
ggscm <- function(..., values, aesthetics = "colour", breaks = ggplot2::waiver(), na.value = "grey50") {ggplot2::scale_color_manual(..., values = values, aesthetics = aesthetics, breaks = breaks, na.value = na.value)}

#' @rdname ggp
#' @inherit ggplot2::scale_shape_manual
#' @export
ggsdm <- function(..., values, breaks = ggplot2::waiver(), na.value = NA) {ggplot2::scale_shape_manual(..., values = values, breaks = breaks, na.value = na.value)}

#' @rdname ggp
#' @inherit ggplot2::scale_linetype_manual
#' @export
ggslm <- function(..., values, breaks = ggplot2::waiver(), na.value = "blank") {ggplot2::scale_linetype_manual(..., values = values, breaks = breaks, na.value = na.value)}

#' @rdname ggp
#' @inherit ggplot2::ggsave
#' @export
ggsv <- function(filename, plot = ggplot2::last_plot(), device = NULL, path = NULL, scale = 1, width = NA, height = NA, units = c("in", "cm", "mm", "px"), dpi = 300, limitsize = TRUE, bg = NULL, ...) {ggplot2::ggsave(filename, plot = plot, device = device, path = path, scale = scale, width = width, height = height, units = units, dpi = dpi, limitsize = limitsize, bg = bg, ...)}

#' @rdname ggp
#' @inherit ggplot2::scale_x_continuous
#' @export
ggsxc <- function(name = ggplot2::waiver(), breaks = ggplot2::waiver(), minor_breaks = ggplot2::waiver(), n.breaks = NULL, labels = ggplot2::waiver(), limits = NULL, expand = ggplot2::waiver(), oob = scales::censor, na.value = NA_real_, trans = "identity", guide = ggplot2::waiver(), position = "bottom", sec.axis = ggplot2::waiver()) {ggplot2::scale_x_continuous(name = name, breaks = breaks, minor_breaks = minor_breaks, n.breaks = n.breaks, labels = labels, limits = limits, expand = expand, oob = oob, na.value = na.value, trans = trans, guide = guide, position = position, sec.axis = sec.axis)}

#' @rdname ggp
#' @inherit ggplot2::scale_y_continuous
#' @export
ggsyc <- function(name = ggplot2::waiver(), breaks = ggplot2::waiver(), minor_breaks = ggplot2::waiver(), n.breaks = NULL, labels = ggplot2::waiver(), limits = NULL, expand = ggplot2::waiver(), oob = scales::censor, na.value = NA_real_, trans = "identity", guide = ggplot2::waiver(), position = "left", sec.axis = ggplot2::waiver()) {ggplot2::scale_y_continuous(name = name, breaks = breaks, minor_breaks = minor_breaks, n.breaks = n.breaks, labels = labels, limits = limits, expand = expand, oob = oob, na.value = na.value, trans = trans, guide = guide, position = position, sec.axis = sec.axis)}

#' @rdname ggp
#' @inherit ggplot2::theme
#' @export
ggthm <- function(...) {ggplot2::theme(...)}

#' @rdname ggp
#' @inherit ggplot2::theme_minimal
#' @export
ggtmin <- function(base_size = 11, base_family = "", base_line_size = base_size/22, base_rect_size = base_size/22) {ggplot2::theme_minimal(base_size = base_size, base_family = base_family, base_line_size = base_line_size, base_rect_size = base_rect_size)}

#' @rdname ggp
#' @inherit ggplot2::geom_text
#' @export
ggtxt <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ..., parse = FALSE, nudge_x = 0, nudge_y = 0, check_overlap = FALSE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {ggplot2::geom_text(mapping = mapping, data = data, stat = stat, position = position, ..., parse = parse, nudge_x = nudge_x, nudge_y = nudge_y, check_overlap = check_overlap, na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes)}

#' @rdname ggp
#' @inherit ggplot2::unit
#' @export
ggunit <- function(x, units, data = NULL) {ggplot2::unit(x, units, data = data)}

#' @rdname ggp
#' @inherit ggplot2::vars
#' @export
ggvars <- function(...) {ggplot2::vars(...)}

#' @rdname ggp
#' @inherit ggplot2::geom_vline
#' @export
ggvln <- function(mapping = NULL, data = NULL, ..., xintercept, na.rm = FALSE, show.legend = NA) {ggplot2::geom_vline(mapping = mapping, data = data, ..., xintercept = xintercept, na.rm = na.rm, show.legend = show.legend)}

#' @rdname ggp
#' @inherit ggplot2::facet_wrap
#' @export
ggwrap <- function(facets, nrow = NULL, ncol = NULL, scales = "fixed", shrink = TRUE, labeller = "label_value", as.table = TRUE, switch = NULL, drop = TRUE, dir = "h", strip.position = "top") {ggplot2::facet_wrap(facets, nrow = nrow, ncol = ncol, scales = scales, shrink = shrink, labeller = labeller, as.table = as.table, switch = switch, drop = drop, dir = dir, strip.position = strip.position)}

#' @rdname ggp
#' @inherit ggplot2::xlab
#' @export
ggxlab <- function(label) {ggplot2::xlab(label)}

#' @rdname ggp
#' @inherit ggplot2::ylab
#' @export
ggylab <- function(label) {ggplot2::ylab(label)}