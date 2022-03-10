#' @name color
#' @family colors
#' @title Simplified Color Creation
#' @description Simple color creation with a wide variety of options (intensity,
#'   blending, lightening, darkening, opacity), get flexible colorblind
#'   palettes, and plot color swatches.
#' @details \strong{\code{color}}
#'   \cr Takes any valid R color representation and lightens, darkens, adds
#'   opacity levels, and/or gets the complementary color.
#'   \cr \cr
#'   \strong{\code{rgba}}
#'   \cr Creates colors using red (\code{r}), green (\code{g}), blue (\code{b}),
#'   and alpha (\code{a}) intensities between \code{0} and \code{1}, inclusive.
#'   \cr \cr
#'   \strong{\code{hsva}}
#'   \cr Creates colors using hue (\code{h}), staturation (\code{s}),
#'   value/brightness (\code{v}), and alpha (\code{a}) intensities between
#'   \code{0} and \code{1}, inclusive.
#'   \cr \cr
#'   \strong{\code{blend}}
#'   \cr Blends two colors.
#'   \cr \cr
#'   \strong{\code{blk}, \code{wht}, and \code{gry}}
#'   \cr Create variations on black, white, and grey.
#'   \cr \cr
#'   \strong{\code{red}, \code{grn}, and \code{blu}}
#'   \cr Create variations on RGB primary colors red, green, and blue.
#'   \cr \cr
#'   \strong{\code{cyn}, \code{mag}, \code{vlt}, and \code{ylw}}
#'   \cr Create variations on CMYK primary colors cyan, magenta, violet, and
#'   yellow.
#'   \cr \cr
#'   \strong{\code{prp} and \code{orn}}
#'   \cr Create variations on purple (slightly more blue than with violet) and
#'   orange.
#'   \cr \cr
#'   \strong{\code{blu_grn}, \code{blu_prp}, and \code{blu_vlt}}
#'   \cr Create variations on traditional blue blends.
#'   \cr \cr
#'   \strong{\code{ylw_grn} and \code{ylw_orn}}
#'   \cr Create variations on traditional yellow blends.
#'   \cr \cr
#'   \strong{\code{red_orn}, \code{red_prp}, and \code{red_vlt}}
#'   \cr Create variations on traditional red blends.
#'   \cr \cr
#'   \strong{\code{pal_cb}}
#'   \cr Get colors from a flexible colorblind palette with 2 to 20 colors.
#'   Colors 1-10 are bright. Colors 11-20 are 50% darkened versions of colors
#'   1-10. The difference in brightness is maximized to allow for distinguishing
#'   bright from darkened versions, but the differences may be insufficient for
#'   some audiences. Using only colors 1-10 is advisable.
#'   \cr \cr
#'   \strong{\code{pal_swatch}}
#'   \cr Draws swatches for a palette.
#'   \cr\cr
#'   The arguments \code{r}, \code{g}, \code{b}, \code{a}, \code{p}, and
#'   \code{s} are recyclable. Functions returns a vector of colors of length
#'   equal to the length of the longest vector passed in as an parameter.
#'   Parameters are recycled until they are as long as they longest parameter.
#'   An error is thrown if each parameter's length is not a whole-number divisor
#'   of the length of the maximum-length parameter.
#'   \cr \cr
#'   Red, green, and blue colors that make up a color are primary (\code{p}).
#'   Those that do not are secondary (\code{s}) and function in the following
#'   manner:
#'   \tabular{ll}{
#'        \code{p = 1, s = 0}\tab The most intense color
#'     \cr\code{p > s > 0}   \tab A lighter, muted color
#'     \cr\code{p < 1}       \tab A darker color
#'     \cr\code{p = s}       \tab A grey color
#'   }
#'   Treating \code{s} as primary and \code{p} as secondary creates complemtary
#'   colors in the following manner:
#'   \tabular{ll}{
#'        \code{s = 1, p = 0}\tab The most intense complementary color
#'     \cr\code{s > p > 0}   \tab A lighter, muted complementary color
#'     \cr\code{s < 1}       \tab A darker color
#'   }
#' @param x Vector containing valid ℝ color representations. Recycled with
#'   \code{y}, \code{lighten}, \code{darken}, \code{comp}, \code{a}, \code{p},
#'   \code{s}, \code{wx}, and \code{wy} where these are valid arguments.
#' @param y Vector containing valid ℝ color representations.
#' @param na Logical scalar indicating whether \code{NA} counts as a valid color
#'   alpha level.
#' @param lighten A vector of proportions by which to lighten the colors given
#'   in \code{x} toward white.
#' @param darken A vector of proportions by which to darken the colors given in
#'   \code{x} toward black.
#' @param comp A logical vector indicating whether to return complementary
#'   colors.
#' @param r,g,b Numeric vectors of values in the range \code{[0, 1]}
#'   giving intensities of red, blue, and green from the RGB color space.
#'   Recycled with each other and \code{a}.
#' @param h,s,v Numeric vectors of values in the range \code{[0, 1]} giving hue,
#'   saturation, and value/brightness from the HSV colorspace. Recycled with
#'   each other and \code{a}.
#' @param a A numeric vector of 1 or more alpha level in the range \code{[0, 1]}
#'   (0 = transparent, 0.5 = translucent, 1 = opaque). \code{NA} values in
#'   \code{a} are only applicable with \code{color(.)} and \code{blend(.)}) to
#'   indicate keeping existing alpha values.
#' @param p,s Numeric vectors of 1 or more primary and secondary color
#'   intensities (respectively) in the range \code{[0, 1]}.
#' @param wx,wy Weight to apply to \code{x} and \code{y} in color blending.
#' @return A character vector of 1 or more hexadecimal RGB + alpha color
#'   representations in the form \code{'#RRGGBBAA'}.
#' @rdname color
#' @export
color <- function(x, lighten = 0, darken = 0, a = 1, comp = F) {
  VX <- cmp_clr_vec(x)
  VL <- cmp_ppn_vec(lighten)
  VD <- cmp_ppn_vec(darken)
  VA <- cmp_ppn_vec(a)
  VC <- isTF(comp)
  if (VX & VL & VD & VA) {
    N <- c(length(x), length(lighten), length(darken), length(a))
    R <- max(N) / N
    VR <- all(R == round(R))
  }
  else {VR <- T}
  E <- NULL
  if (!VX) {E <- c(E, "\n  * [x] must be a complete character vector of color values.")}
  if (!VL) {E <- c(E, "\n  * [lighten] must be a complete proportion vector (in the interval [0, 1]).")}
  if (!VD) {E <- c(E, "\n  * [darken] must be a complete proportion vector (in the interval [0, 1]).")}
  if (!VA) {E <- c(E, "\n  * [a] must be a complete proportion vector (in the interval [0, 1]).")}
  if (!VC) {E <- c(E, "\n  * [comp] must be TRUE or FALSE.")}
  if (!VR) {E <- c(E, "\n  * [x], [lighten], [darken], and [a] are not recyclable.")}
  if (xdef(E)) {stop(E)}
  if (R[1] > 1) {x       <- rep.int(x      , R[1])}
  if (R[2] > 1) {lighten <- rep.int(lighten, R[2])}
  if (R[3] > 1) {darken  <- rep.int(darken , R[3])}
  if (R[4] > 1) {a       <- rep.int(a      , R[4])}
  if (R[5] > 1) {comp    <- rep.int(comp   , R[5])}
  x <- col2rgb(x, T)
  R <- x[1] / 255                                                                # convert red, green, blue color values from hexadecimal to decimal proportions
  G <- x[2] / 255
  B <- x[3] / 255
  R <- R + (1 - R) * lighten                                                     # lighten colors toward white by the proportion in whiten
  G <- G + (1 - G) * lighten
  B <- B + (1 - B) * lighten
  R <- R * (1 - darken)                                                          # darken colors toward black by the proportion in darken
  G <- G * (1 - darken)
  B <- B * (1 - darken)
  R[comp] <- 1 - R[comp]                                                         # invert colors for colors marked for complementarity
  G[comp] <- 1 - G[comp]
  B[comp] <- 1 - B[comp]
  if (xdef(a)) {A <- a}                                                          # if an alpha value was supplied, use it
  else if (nchar(x) == 7) {A <- 1}                                               # ...but if there is no existing alpha value, use a = 1
  else {A <- x[3] / 255}                                                         # ...otherwise, use the existing alpha value
  rgb(R, G, B, A)
}

#' @rdname color
#' @export
rgba <- function(r = 1, g = 1, b = 1, a = 1) {
  VR <- cmp_ppn_vec(r)
  VG <- cmp_ppn_vec(g)
  VB <- cmp_ppn_vec(b)
  VA <- cmp_ppn_vec(a)
  if (VR & VG & VB & VA) {
    N  <- c(length(r), length(g), length(b), length(a))
    VN <- all(round(max(N) / N) == (max(N) / N))
  }
  else {VN <- T}
  E <- NULL
  if (!VR) {E <- c(E, "\n  * [r] must be a complete proportion vector (in the interval [0, 1]).")}
  if (!VG) {E <- c(E, "\n  * [g] must be a complete proportion vector (in the interval [0, 1]).")}
  if (!VB) {E <- c(E, "\n  * [b] must be a complete proportion vector (in the interval [0, 1]).")}
  if (!VA) {E <- c(E, "\n  * [a] must be a complete proportion vector (in the interval [0, 1]).")}
  if (!VN) {E <- c(E, "\n  * [r], [g], [b], and [a] are not recyclable.")}
  if (xdef(E)) {stop(E)}
  rgb(r, g, b, a)                                                                # return the hexadecimal RGBA representation
}

#' @rdname color
#' @export
hsva <- function(h = 1, s = 1, v = 1, a = 1) {
  VH <- cmp_ppn_vec(h)
  VS <- cmp_ppn_vec(s)
  VV <- cmp_ppn_vec(v)
  VA <- cmp_ppn_vec(a)
  if (VH & VS & VV & VA) {
    N  <- c(length(h), length(s), length(v), length(a))
    VN <- all(round(max(N) / N) == (max(N) / N))
  }
  else {VN <- T}
  E <- NULL
  if (!VH) {E <- c(E, "\n  * [h] must be a complete proportion vector (in the interval [0, 1]).")}
  if (!VS) {E <- c(E, "\n  * [s] must be a complete proportion vector (in the interval [0, 1]).")}
  if (!VV) {E <- c(E, "\n  * [v] must be a complete proportion vector (in the interval [0, 1]).")}
  if (!VA) {E <- c(E, "\n  * [a] must be a complete proportion vector (in the interval [0, 1]).")}
  if (!VN) {E <- c(E, "\n  * [h], [s], [v], and [a] are not recyclable.")}
  if (xdef(E)) {stop(E)}
  hsv(h, s, v, a)                                                                # return the hexadecimal RGBA representation for the specified hsva parameters
}

#' @rdname color
#' @export
blend <- function(x, y, wx = 1, wy = 1, a = NA) { # a = NA indicates blending existing alpha levels
  VXX <- cmp_clr_vec( x)
  VYY <- cmp_clr_vec( y)
  VWX <- cmp_pos_vec(wx)
  VWY <- cmp_pos_vec(wy)
  if (VXX & VYY & VWX & VWY) {
    N  <- c(length(x), length(y), length(wx), length(wy))
    VRC <- all(round(max(N) / N) == (max(N) / N))
  }
  else {VRC <- T}
  E <- NULL
  if (!VXX) {E <- c(E, "\n  * [x] must be a complete proportion vector (in the interval [0, 1]).")}
  if (!VYY) {E <- c(E, "\n  * [y] must be a complete proportion vector (in the interval [0, 1]).")}
  if (!VWX) {E <- c(E, "\n  * [wx] must be a complete positive numeric vector.")}
  if (!VWY) {E <- c(E, "\n  * [wy] must be a complete positive numeric vector.")}
  if (!VRC) {E <- c(E, "\n  * [x], [y], [wx], and [wy] are not recyclable.")}
  if (xdef(E)) {stop(E)}
  x  <- as_color(x)                                                              # convert x and y to hexadecimal RGBA
  y  <- as_color(y)
  wt <- wx + wy                                                                  # rescale x and y weights to sum to 1
  wx <- wx / wt
  wy <- wy / wt
  xR <- todec(substr(x, 2, 3)) / 255                                             # convert red values of x and y from hexadecimal to decimal proportion
  yR <- todec(substr(y, 2, 3)) / 255
  xG <- todec(substr(x, 4, 5)) / 255                                             # convert green values of x and y from hexadecimal to decimal proportion
  yG <- todec(substr(y, 4, 5)) / 255
  xB <- todec(substr(x, 6, 7)) / 255                                             # convert blue values of x and y from hexadecimal to decimal proportion
  yB <- todec(substr(y, 6, 7)) / 255
  xA <- todec(substr(x, 8, 9)) / 255                                             # convert alpha values of x and y from hexadecimal to decimal proportion
  yA <- todec(substr(y, 8, 9)) / 255
  R  <- max(0, min(1, wx * xR + wy * yR))                                        # weight the red, green, blue, and alpha values of x and y
  G  <- max(0, min(1, wx * xG + wy * yG))
  B  <- max(0, min(1, wx * xB + wy * yB))
  A  <- max(0, min(1, wx * xA + wy * yA))
  A[!is.na(a)] <- a[!is.na(a)]                                                   # use any non-NA values in a to replace weighted alpha values
  rgba(R, G, B, A)                                                               # return the blended color in hexadecimal RGBA format
}

#' @name color
#' @export
red <- function(p = 1, s = 0, a = 1) {rgba(p, s, s, a)}

#' @name color
#' @export
grn <- function(p = 1, s = 0, a = 1) {rgba(s, p, s, a)}

#' @name color
#' @export
blu <- function(p = 1, s = 0, a = 1) {rgba(s, s, p, a)}

#' @name color
#' @export
cyn <- function(p = 1, s = 0, a = 1) {rgba(s, p, p, a)}

#' @name color
#' @export
mag <- function(p = 1, s = 0, a = 1) {rgba(p, s, p, a)}

#' @name color
#' @export
vlt <- function(p = 1, s = 0, a = 1) {rgba(p, s, p, a)}

#' @name color
#' @export
ylw <- function(p = 1, s = 0, a = 1) {rgba(p, p, s, a)}

#' @name color
#' @export
gry <- function(p = 0.5, a = 1) {rgba(p, p, p, a)}

#' @name color
#' @export
blk <- function(a = 1) {rgba(0, 0, 0, a)}

#' @name color
#' @export
wht <- function(a = 1) {rgba(1, 1, 1, a)}

#' @name color
#' @export
ppl <- function(p = 1, s = 0, a = 1) {blend(red(p, s), blu(p, s), 1, 2, a)}

#' @name color
#' @export
orn <- function(p = 1, s = 0, a = 1) {blend(red(p, s), ylw(p, s), 33, 51, a)}

#' @name color
#' @export
blu_grn <- function(p = 1, s = 0, a = 1) {blend(blu(p, s), grn(p, s), a)}

#' @name color
#' @export
blu_ppl <- function(p = 1, s = 0, a = 1) {blend(blu(p, s), ppl(p, s), a)}

#' @name color
#' @export
blu_vlt <- function(p = 1, s = 0, a = 1) {blend(blu(p, s), vlt(p, s), a)}

#' @name color
#' @export
ylw_grn <- function(p = 1, s = 0, a = 1) {blend(ylw(p, s), grn(p, s), a)}

#' @name color
#' @export
ylw_orn <- function(p = 1, s = 0, a = 1) {blend(ylw(p, s), orn(p, s), a)}

#' @name color
#' @export
red_orn <- function(p = 1, s = 0, a = 1) {blend(red(p, s), orn(p, s), a)}

#' @name color
#' @export
red_ppl <- function(p = 1, s = 0, a = 1) {blend(red(p, s), ppl(p, s), a)}

#' @name color
#' @export
red_vlt <- function(p = 1, s = 0, a = 1) {blend(red(p, s), vlt(p, s), a)}

#' @name color
#' @export
pal_cb <- function(ngroups, ncolors = NULL, a = 1) {
  VG <- cmp_psw_scl(ngroups)
  VC <- f0(xnll(ncolors), T, cmp_psw_scl)
  VA <- cmp_ppn_scl(a)
  E <- NULL
  if (!VG) {E <- c(E, "\n  * [ngroups] must be a positive whole-number scalar.")}
  if (!VC) {E <- c(E, "\n  * [ncolors] must be NULL or a positive whole-number scalar.")}
  if (!VA) {E <- c(E, "\n  * [a] must be a proportion scalar (in the interval [0, 1]).")}
  if (xdef(E)) {stop(E)}
  R <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",                  # bright colors
         "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC",
         "#273D54", "#794716", "#712C2D", "#3B5C59", "#2D5128",                  # darkened 50%
         "#776524", "#583D51", "#804F54", "#4E3B30", "#5D5856")
  if (length(ncolors) == 0) {                                                    # if [ncolors] was not supplied
    if (ngroups < 20) {ncolors <- ngroups}                                       # > if [ngroups] is less than 20, use as is
    else {ncolors <- 20}                                                         # > otherwise cap at 20
  }
  if (ngroups > length(R)) {R <- rep.int(R, ceiling(ngroups / length(R)))}       # if [ngroups] is larger than 20, repeat colors
  R <- R[1:ngroups]                                                              # take the first [ngroups] colors
  scales::alpha(R, a)                                                            # apply the alpha value
}

#' @name color
#' @export
pal_swatch <- function(x) {
  if (!cmp_clr_vec(x)) {stop("\n  * [x] must be a complete character vector containing only valid color values.")}
  as_color(x)                                                                    # convert [x] to hexadecimal RGBA format
  Options <- par()                                                               # bank current graphical parameters
  par(mar  = c(0, 0, 0, 0),                                                      # set margins to zero (entire window is the plot region)
      ann  = F,                                                                  # > do not annotate with titles
      xaxs = "i", yaxs = "i",                                                    # > tight axes (no pad for xlim and ylim)
      xaxt = "n", yaxt = "n",                                                    # > do not draw axes
      bty  = "n",                                                                # > no box around the plot region
      col  = "black")                                                            # > default plotting color
  on.exit(par(Options))                                                          # restore the previous graphical parameters when finished
  L <- 0:(length(x) - 1)                                                         # left side of swatch rectangles
  R <- L + 1                                                                     # right side of swatch rectangles
  plot(x = c(0, max(R)), y = c(0, 1), col = "white")                             # draw white dots in the bottom left and top right corners
  for (i in 1:max(R)) {rect(L[i], 0, R[i], 1, col = x[i], border = "white")}
}
