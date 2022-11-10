#' @name simple_colors.
#' @family colors
#' @title Simple color creation
#' @description Simple color creation with a wide variety of options (intensity,
#'   blending, lightening, darkening, opacity) \cr\cr The arguments \code{r},
#'   \code{g}, \code{b}, \code{a}, \code{p}, and \code{s} are recyclable.
#'   Functions returns a vector of colors of length equal to the length of the
#'   longest vector passed in as an parameter. Parameters are recycled until
#'   they are as long as they longest parameter. An error is thrown if each
#'   parameter's length is not a whole-number divisor of the length of the
#'   maximum-length parameter. \cr \cr Red, green, and blue colors that make up
#'   a color are primary (\code{p}). Those that do not are secondary (\code{s})
#'   and function in the following manner:\tabular{ll}{ ARGUMENT VALUES
#'   \tab RESULTING COLOR                              \cr
#'   \code{p = 1, s = 0   }\tab The most intense color.                      \cr
#'   \code{p > s > 0}      \tab A lighter, muted color.                      \cr
#'   \code{s < p < 1}      \tab A darker color.                              \cr
#'   \code{s = 1, p = 0   }\tab The most intense \emph{complementary} color. \cr
#'   \code{s > p > 0}      \tab A lighter, muted \emph{complementary} color. \cr
#'   \code{p < s < 1}      \tab A darker \emph{complementary} color.           }
#' @param x \link[is_vec]{Vec} containing valid ℝ color representations.
#'   Recycled with \code{y}, \code{lighten}, \code{darken}, \code{comp},
#'   \code{a}, \code{p}, \code{s}, \code{wx}, and \code{wy} where these are
#'   valid arguments.
#' @param y \link[is_vec]{Vec} containing valid ℝ color representations.
#' @param na \link[cmp_lgl_scl]{Complete logical scalar} indicating whether
#'   \code{NA} counts as a valid color alpha level.
#' @param lighten \link[cmp_ppn_vec]{Complete proportion-valued numeric vec} of
#'   proportions by which to lighten the colors given in \code{x} toward white.
#' @param darken \link[cmp_ppn_vec]{Complete proportion-valued numeric vec} of
#'   proportions by which to darken the colors given in \code{x} toward black.
#' @param comp \link[cmp_lgl_vec]{Complete logical vec} indicating whether to
#'   return complementary colors.
#' @param r,g,b \link[cmp_ppn_vec]{Complete proportion-valued numeric vecs}
#'   giving intensities of red, blue, and green from the RGB color space.
#'   Recycled with each other and \code{a}.
#' @param h,s,v \link[cmp_ppn_vec]{Complete proportion-valued numeric vecs}
#'   giving hue, saturation, and value/brightness from the HSV color space.
#'   Recycled with each other and \code{a}.
#' @param a \link{cmp_ppn_vec}{Complete proportion-valued numeric vec} of 1 or
#'   more alpha levels (0 = transparent, 0.5 = translucent, 1 = opaque).
#'   \code{NA} values in \code{a} are only applicable with \code{color(.)} and
#'   \code{blend(.)}) to indicate keeping existing alpha values.
#' @param p,s \link[cmp_ppn_vec]{Complete poportion-valued numeric vecs} of 1 or
#'   more primary and secondary color intensities (respectively) in the range
#'   \code{[0, 1]}.
#' @param wx,wy \link[cmp_pos_vec]{Complete positive-valued numeric vectors}
#'   giving weights to apply to \code{x} and \code{y} in color blending.
#' @param nc,ng \link[cmp_psw_scl]{Complete positive-valued whole-numnber
#'   scalars} giving the number of colors and groups, respectively.
#' @return A character vector of 1 or more hexadecimal RGB + alpha color
#'   representations in the form \code{'#RRGGBBAA'}.
#' @export
simple_colors. <- function() {help("simple_colors.", package = "uj")}

#' @describeIn simple_colors. Takes any valid color representation and lightens,
#'   darkens, adds opacity levels, and/or gets the complementary color.
#' @export
color <- function(x, lighten = 0, darken = 0, a = 1, comp = F) {
  vx <- cmp_clr_vec(x)
  vl <- cmp_ppn_vec(lighten)
  vd <- cmp_ppn_vec(darken)
  va <- cmp_ppn_vec(a)
  vc <- isTF(comp)
  if (vx & vl & vd & va) {
    n <- c(length(x), length(lighten), length(darken), length(a))
    r <- max(n) / n
    vr <- all(r == round(r))
  }
  else {vr <- T}
  err <- NULL
  if (!vx) {err <- c(err, "\n • [x] must be a complete color vec (?cmp_clr_vec) [i.e., containing only valid character-mode color values].")}
  if (!vl) {err <- c(err, "\n • [lighten] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].")}
  if (!vd) {err <- c(err, "\n • [darken] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].")}
  if (!va) {err <- c(err, "\n • [a] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].")}
  if (!vc) {err <- c(err, "\n • [comp] must be TRUE or FALSE.")}
  if (!vr) {err <- c(err, "\n • [x], [lighten], [darken], and [a] are not recyclable (?recyclable).")}
  if (idef(err)) {stop(err)}
  if (r[1] > 1) {x       <- rep.int(x      , r[1])}
  if (r[2] > 1) {lighten <- rep.int(lighten, r[2])}
  if (r[3] > 1) {darken  <- rep.int(darken , r[3])}
  if (r[4] > 1) {a       <- rep.int(a      , r[4])}
  if (r[5] > 1) {comp    <- rep.int(comp   , r[5])}
  x <- col2rgb(x, T)
  r <- x[1] / 255                                                                # convert red, green, blue color values from hexadecimal to decimal proportions
  g <- x[2] / 255
  b <- x[3] / 255
  r <- r + (1 - r) * lighten                                                     # lighten colors toward white by the proportion in whiten
  g <- g + (1 - g) * lighten
  b <- b + (1 - b) * lighten
  r <- r * (1 - darken)                                                          # darken colors toward black by the proportion in darken
  g <- g * (1 - darken)
  b <- b * (1 - darken)
  r[comp] <- 1 - r[comp]                                                         # invert colors for colors marked for complementarity
  g[comp] <- 1 - g[comp]
  b[comp] <- 1 - b[comp]
  if (idef(a)) {a <- a}                                                          # if an alpha value was supplied, use it
  else if (nchar(x) == 7) {a <- 1}                                               # ...but if there is no existing alpha value, use a = 1
  else {a <- x[3] / 255}                                                         # ...otherwise, use the existing alpha value
  rgb(r, g, b, a)
}

#' @describeIn simple_colors. Create colors from r(ed), g(reen), b(lue), and
#'   a(lpha) weights in the interval \[0, 1\].
#' @export
rgba <- function(r = 1, g = 1, b = 1, a = 1) {
  vr <- cmp_ppn_vec(r)
  vg <- cmp_ppn_vec(g)
  vb <- cmp_ppn_vec(b)
  va <- cmp_ppn_vec(a)
  if (vr & vg & vb & va) {
    n <- c(length(r), length(g), length(b), length(a))
    vn <- all(round(max(n) / n) == (max(n) / n))
  }
  else {vn <- T}
  err <- NULL
  if (!vr) {err <- c(err, "\n • [r] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].")}
  if (!vg) {err <- c(err, "\n • [g] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].")}
  if (!vb) {err <- c(err, "\n • [b] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].")}
  if (!va) {err <- c(err, "\n • [a] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].")}
  if (!vn) {err <- c(err, "\n • [r], [g], [b], and [a] are not recyclable (?recyclable).")}
  if (idef(err)) {stop(err)}
  rgb(r, g, b, a)
}

#' @describeIn simple_colors. Create colors from h(ue), s(aturation), v(alue),
#'   and a(lpha) weights in the interval \[0, 1\].
#' @export
hsva <- function(h = 1, s = 1, v = 1, a = 1) {
  vh <- cmp_ppn_vec(h)
  vs <- cmp_ppn_vec(s)
  vv <- cmp_ppn_vec(v)
  va <- cmp_ppn_vec(a)
  if (vh & vs & vv & va) {
    n <- c(length(h), length(s), length(v), length(a))
    vn <- all(round(max(n) / n) == (max(n) / n))
  }
  else {vn <- T}
  err <- NULL
  if (!vh) {err <- c(err, "\n • [h] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].")}
  if (!vs) {err <- c(err, "\n • [s] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].")}
  if (!vv) {err <- c(err, "\n • [v] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].")}
  if (!va) {err <- c(err, "\n • [a] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].")}
  if (!vn) {err <- c(err, "\n • [h], [s], [v], and [a] are not recyclable (?recyclable).")}
  if (idef(err)) {stop(err)}
  hsv(h, s, v, a)
}

#' @describeIn simple_colors. Blend colors using weights and optional alpha
#'   values.
#' @export
blend <- function(x, y, wx = 1, wy = 1, a = NA) {
  vx <- cmp_clr_vec( x)
  vy <- cmp_clr_vec( y)
  vwx <- cmp_pos_vec(wx)
  vwy <- cmp_pos_vec(wy)
  if (vx & vy & vwx & vwy) {
    n <- c(length(x), length(y), length(wx), length(wy))
    vn <- all(round(max(n) / n) == (max(n) / n))
  }
  else {vn <- T}
  err <- NULL
  if (!vx) {err <- c(err, "\n • [x] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].")}
  if (!vy) {err <- c(err, "\n • [y] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].")}
  if (!vwx) {err <- c(err, "\n • [wx] must be a complete positive numeric vec (?cmp_pos_vec).")}
  if (!vwy) {err <- c(err, "\n • [wy] must be a complete positive numeric vec (?cmp_pos_vec).")}
  if (!vn) {err <- c(err, "\n • [x], [y], [wx], and [wy] are not recyclable (?recyclable).")}
  if (idef(err)) {stop(err)}
  x  <- as_clr(x)                                                                # convert x and y to hexadecimal RGBA
  y  <- as_clr(y)
  wt <- wx + wy                                                                  # rescale x and y weights to sum to 1
  wx <- wx / wt
  wy <- wy / wt
  xr <- todec(substr(x, 2, 3)) / 255                                             # convert red values of x and y from hexadecimal to decimal proportion
  yr <- todec(substr(y, 2, 3)) / 255
  xg <- todec(substr(x, 4, 5)) / 255                                             # convert green values of x and y from hexadecimal to decimal proportion
  yg <- todec(substr(y, 4, 5)) / 255
  xb <- todec(substr(x, 6, 7)) / 255                                             # convert blue values of x and y from hexadecimal to decimal proportion
  yb <- todec(substr(y, 6, 7)) / 255
  xa <- todec(substr(x, 8, 9)) / 255                                             # convert alpha values of x and y from hexadecimal to decimal proportion
  ya <- todec(substr(y, 8, 9)) / 255
  r <- max(0, min(1, wx * xr + wy * yr))                                        # weight the red, green, blue, and alpha values of x and y
  g <- max(0, min(1, wx * xg + wy * yg))
  b <- max(0, min(1, wx * xb + wy * yb))
  a <- max(0, min(1, wx * xa + wy * ya))
  a[!is.na(a)] <- a[!is.na(a)]                                                   # use any non-NA values in a to replace weighted alpha values
  rgba(r, g, b, a)                                                               # return the blended color in hexadecimal RGBA format
}

#' @describeIn simple_colors. Build red hues by primary color, secondary
#'   color, and alpha weights.
#' @export
red <- function(p = 1, s = 0, a = 1) {rgba(p, s, s, a)}

#' @describeIn simple_colors. Build green hues by primary color, secondary
#'   color, and alpha weights.
#' @export
grn <- function(p = 1, s = 0, a = 1) {rgba(s, p, s, a)}

#' @describeIn simple_colors. Build blu hues by primary color, secondary
#'   color, and alpha weights.
#' @export
blu <- function(p = 1, s = 0, a = 1) {rgba(s, s, p, a)}

#' @describeIn simple_colors. Build cyan hues by primary color, secondary
#'   color, and alpha weights.
#' @export
cyn <- function(p = 1, s = 0, a = 1) {rgba(s, p, p, a)}

#' @describeIn simple_colors. Build magenta hues by primary color, secondary
#'   color, and alpha weights.
#' @export
mag <- function(p = 1, s = 0, a = 1) {rgba(p, s, p, a)}

#' @describeIn simple_colors. Build violet hues by primary color, secondary
#'   color, and alpha weights.
#' @export
vlt <- function(p = 1, s = 0, a = 1) {rgba(p, s, p, a)}

#' @describeIn simple_colors. Build yellow hues by primary color, secondary
#'   color, and alpha weights.
#' @export
ylw <- function(p = 1, s = 0, a = 1) {rgba(p, p, s, a)}

#' @describeIn simple_colors. Build grey hues by primary color and alpha
#'   weights.
#' @export
gry <- function(p = 0.5, a = 1) {rgba(p, p, p, a)}

#' @describeIn simple_colors. Build black hues by alpha weights.
#' @export
blk <- function(a = 1) {rgba(0, 0, 0, a)}

#' @describeIn simple_colors. Build white hues by alpha weights.
#' @export
wht <- function(a = 1) {rgba(1, 1, 1, a)}

#' @describeIn simple_colors. Build purple hues by primary color, secondary
#'   color, and alpha weights.
#' @export
ppl <- function(p = 1, s = 0, a = 1) {blend(red(p, s), blu(p, s), 1, 2, a)}

#' @describeIn simple_colors. Build orange hues by primary color, secondary
#'   color, and alpha weights.
#' @export
orn <- function(p = 1, s = 0, a = 1) {blend(red(p, s), ylw(p, s), 33, 51, a)}

#' @describeIn simple_colors. Build blue-green hues by primary color,
#'   secondary color, and alpha weights.
#' @export
blu_grn <- function(p = 1, s = 0, a = 1) {blend(blu(p, s), grn(p, s), a)}

#' @describeIn simple_colors. Build blue-purple hues by primary color,
#'   secondary color, and alpha weights.
#' @export
blu_ppl <- function(p = 1, s = 0, a = 1) {blend(blu(p, s), ppl(p, s), a)}

#' @describeIn simple_colors. Build blue-violet hues by primary color,
#'   secondary color, and alpha weights.
#' @export
blu_vlt <- function(p = 1, s = 0, a = 1) {blend(blu(p, s), vlt(p, s), a)}

#' @describeIn simple_colors. Build yellow-green hues by primary color,
#'   secondary color, and alpha weights.
#' @export
ylw_grn <- function(p = 1, s = 0, a = 1) {blend(ylw(p, s), grn(p, s), a)}

#' @describeIn simple_colors. Build yellow-orange hues by primary color,
#'   secondary color, and alpha weights.
#' @export
ylw_orn <- function(p = 1, s = 0, a = 1) {blend(ylw(p, s), orn(p, s), a)}

#' @describeIn simple_colors. Build red-orange hues by primary color,
#'   secondary color, and alpha weights.
#' @export
red_orn <- function(p = 1, s = 0, a = 1) {blend(red(p, s), orn(p, s), a)}

#' @describeIn simple_colors. Build red-purple hues by primary color,
#'   secondary color, and alpha weights.
#' @export
red_ppl <- function(p = 1, s = 0, a = 1) {blend(red(p, s), ppl(p, s), a)}

#' @describeIn simple_colors. Build red-violet hues by primary color,
#'   secondary color, and alpha weights.
#' @export
red_vlt <- function(p = 1, s = 0, a = 1) {blend(red(p, s), vlt(p, s), a)}

#' @describeIn simple_colors. Get colors from a flexible colorblind palette with
#'   2 to 20 colors. Colors 1-10 are bright. Colors 11-20 are 50% darkened
#'   versions of colors 1-10. The difference in brightness is maximized to allow
#'   for distinguishing bright from darkened versions, but the differences may
#'   be insufficient for some audiences. Using only colors 1-10 is advisable.
#' @export
pal_cb <- function(ng, nc = NULL, a = 1) {
  vng <- cmp_psw_scl(ng)
  vnc <- f0(inll(nc), T, cmp_psw_scl)
  va <- cmp_ppn_scl(a)
  err <- NULL
  if (!vng) {err <- c(err, "\n • [ng] must be a positive whole-number scalar (?cmp_psw_scl).")}
  if (!vnc) {err <- c(err, "\n • [nc] must be NULL or a positive whole-number scalar (?cmp_psw_scl).")}
  if (!va) {err <- c(err, "\n • [a] must be a proportion scalar (?cmp_ppn_scl). That is, in the interval [0, 1].")}
  if (idef(err)) {stop(err)}
  out <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC", # bright colors
           "#273D54", "#794716", "#712C2D", "#3B5C59", "#2D5128", "#776524", "#583D51", "#804F54", "#4E3B30", "#5D5856") #d darkened 50%
  if (length(nc) == 0) {if (nc < 20) {nc <- nc} else {nc <- 20}}                 # IF [nc] was not supplied THEN IF [nc] is less than 20, use as is ELSE  cap at 20
  if (nc > length(out)) {out <- rep.int(out, ceiling(nc / length(out)))}         # if [nc] is larger than 20, repeat colors
  out <- out[1:nc]                                                               # take the first [nc] colors
  scales::alpha(out, a)                                                          # apply the alpha value
}

#' @describeIn simple_colors. Draws swatches for a palette.
#' @export
pal_swatch <- function(x) {
  if (!cmp_clr_vec(x)) {stop("\n • [x] must be a complete color vec (?cmp_clr_vec) [i.e., containing only valid character-mode color values].")}
  as_clr(x)                                                                      # convert [x] to hexadecimal RGBA format
  Options <- par()                                                               # bank current graphical parameters
  par(mar  = c(0, 0, 0, 0),                                                      # set margins to zero (entire window is the plot region)
      ann  = F,                                                                  # > do not annotate with titles
      xaxs = "i", yaxs = "i",                                                    # > tight axes (no pad for xlim and ylim)
      xaxt = "n", yaxt = "n",                                                    # > do not draw axes
      bty  = "n",                                                                # > no box around the plot region
      col  = "black")                                                            # > default plotting color
  on.exit(par(Options))                                                          # restore the previous graphical parameters when finished
  left <- 0:(length(x) - 1)                                                      # left side of swatch rectangles
  right <- left + 1                                                              # right side of swatch rectangles
  plot(x = c(0, max(right)), y = c(0, 1), col = "white")                         # draw white dots in the bottom left and top right corners
  for (i in 1:max(right)) {rect(left[i], 0, right[i], 1, col = x[i], border = "white")}
}
