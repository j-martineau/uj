#' @name color
#' @family colors
#' @title Simple Color Creation
#' @description Simple color creation with a wide variety of options (intensity,
#'   blending, lightening, darkening, opacity).
#'   \cr\cr
#'   \strong{`color`}: Takes any valid color representation and lightens,
#'   darkens, adds opacity levels, and/or gets the complementary color.
#'   \cr\cr
#'   \strong{`rgba`}: Creates colors from `r` = red, `g` = green, `b` = blue,
#'   and `a` = alpha weights, each in the interval `[0, 1]`.
#'   \cr\cr
#'   \strong{`hsva`}: Creates colors from `h` = hue, `s` = saturation, `v` =
#'   value, and `a` = alpha weights, each in the interval `[0, 1]`.
#'   \cr\cr
#'   \strong{`blend`}: Blends colors `x` and `y` using non-negative numeric
#'   weights `wx` and `wy`, and proportional numeric `a` = alpha values. Each
#'   corresponding pair of values of these arguments is normalized to sum to 1
#'   to give the proportions of the resulting color that should come from `x`
#'   and `y`, respectively.
#'   \cr\cr
#'   \strong{`xxx` and `xxx_yyy`}
#'   \cr\cr
#'   `xxx` functions create versions of common color `xxx` by `p` = primary
#'   intensity, `s` = secondary intensity, and `a` = alpha, each in the interval
#'   `[0, 1]`.
#'   \cr\cr
#'   `xxx_yyy` functions create versions of 50/50 blends of the common colors
#'   `xxx` and `yyy` by `p` = primary intensity, `s` = secondary intensity, and
#'   `a` = alpha, each in the interval `[0, 1]`.
#'   \cr\cr
#'   Three-letter codes for common color are as follows:\tabular{lll}{
#'   3-LETTER   \tab COLOR     \tab RGB CHANNEL VALUES                       \cr
#'   CODE       \tab NAME      \tab OF DEFAULT VERSION                       \cr
#'   `wht`      \tab white     \tab `c(r = 1  , g = 1  , b = 1  )`           \cr
#'   `blk`      \tab black     \tab `c(r = 0  , g = 0  , b = 0  )`           \cr
#'   `gry`      \tab grey      \tab `c(r = 0.5, g = 0.5, b = 0.5)`           \cr
#'   `red`      \tab red       \tab `c(r = 1  , g = 0  , b = 0  )`           \cr
#'   `grn`      \tab green     \tab `c(r = 0  , g = 1  , b = 0  )`           \cr
#'   `blu`      \tab blue      \tab `c(r = 0  , g = 0  , b = 1  )`           \cr
#'   `ylw`      \tab yellow    \tab `c(r = 1  , g = 1  , b = 0  )`           \cr
#'   `cyn`      \tab cyan      \tab `c(r = 0  , g = 1  , b = 1  )`           \cr
#'   `mag`      \tab magenta   \tab `c(r = 1  , g = 0  , b = 1  )`           \cr
#'   `vlt`      \tab violet    \tab `c(r = 1  , g = 0  , b = 1  )`           \cr
#'   `ppl`      \tab purple    \tab A 1:2 blend of red:blue.                 \cr
#'   `orn`      \tab orange    \tab A 33:51 blend of red:yellow.               }
#'   \cr\cr
#'   \strong{`pal_cb`}: Creates colorblind-friendly palettes of 2 to 20 Colors
#'   1-10 vs. 11-20 are bright vs. 50% darkened versions. Bright/darkened
#'   differences may be insufficient for some audiences.
#'   \cr\cr
#'   \strong{`pal_swatch`}: Draws swatches for a collection/palette of colors.
#' @section Recycling: The only arguments not recycled are `na, nc`, and `ng`.
#' @section The Arguments `p` and `s`: `p` is used to indicate the proportion of
#'   the RGB values of the most intense version of a hue should be present in a
#'   resulting color. `s` is used to indicate how much of complement of each RGB
#'   value should be present in a resulting color. The table below gives a
#'   helpful heuristic for setting values of `p` and `s`:\tabular{ll}{
#'   ARGUMENT VALUES      \tab RESULTING COLOR                               \cr
#'   `c(p = 1, s = 0)`    \tab The most intense version of a color.          \cr
#'   `c(s = 1, p = 0)`    \tab The most intense complementary color.         \cr
#'   `c(p > s, s > 0)`    \tab A lighter, muted color.                       \cr
#'   `c(s < p, p < 1)`    \tab A darker color.                               \cr
#'   `c(s > p, p > 0)`    \tab A lighter, muted complementary color.         \cr
#'   `c(p < s, s < 1)`    \tab A darker complementary color.                 \cr
#'   `c(p = 1, s = 1)`    \tab White.                                        \cr
#'   `c(p = 0  , s = 0)`  \tab Black.                                        \cr
#'   `c(p = 0.5, s = 0.5)`\tab Grey.                                           }
#'   A concrete numeric example is that calling
#'   ```
#'        ylw(p = 0.7, s = 0.3)
#'   ```
#'   starts with primary and secondary RGB values of
#'   ```
#'        prm = c(r = 1, g = 1, b = 0)
#'        sec = c(r = 0, g = 0, b = 1)
#'   ```
#'   To get final RGB values, the primary RGB values are multiplied by
#'   `p = 0.7`, secondary RGB values are multiple by `s = 0.3`, and the results
#'   are summed pairwise giving
#'   ```
#'        final = c(r = 0.7, g = 0.7, b = 0.3)
#'   ```
#' @param x An \link[=atm_vec]{atomic vec} containing valid color
#'   representations. Recycled with `y, lighten, darken, comp, a, p`,
#'   `s, wx`, and `wy` where these are valid arguments.
#' @param y An \link[=atm_vec]{atomic vec} containing valid R color
#'   representations.
#' @param na A non=`NA` logical scalar indicating whether `NA` counts as a valid
#'   color alpha level.
#' @param lighten A \link[=cmp_ppn_vec]{complete proportion numeric vec} of
#'   proportions by which to lighten the colors given in `x` toward white.
#' @param darken A \link[=cmp_ppn_vec]{complete proportion numeric vec} of
#'   proportions by which to darken the colors given in `x` toward black.
#' @param comp A \link[=cmp_lgl_vec]{complete logical vec} indicating whether to
#'   return complementary colors.
#' @param r,g,b \link[=cmp_ppn_vec]{Complete proportion numeric vecs} giving
#'   intensities of red, blue, and green from the RGB color space. Recycled with
#'   each other and `a`.
#' @param h,s,v \link[=cmp_ppn_vec]{Complete proportion numeric vecs} giving
#'   hue, saturation, and value/brightness from the HSV color space. Recycled
#'   with each other and `a`.
#' @param a \link[=cmp_ppn_vec]{Complete proportion numeric vec} of 1 or more
#'   alpha levels (`0` = transparent, `0.5` = translucent, `1` = opaque). `NA`
#'   values in `a` are only applicable with `color(.)` and `blend(.)`) to
#'   indicate keeping existing alpha values.
#' @param p,s \link[=cmp_ppn_vec]{Complete poportion numeric vecs} of 1 or more
#'   primary and secondary color intensities (respectively) in the range `[0,
#'   1]`.
#' @param wx,wy \link[=cmp_nnw_vec]{Complete non-negative numeric vecs} giving
#'   weights to apply to `x` and `y` in color blending.
#' @param nc,ng \link[=cmp_psw_scl]{Complete positive whole-number scalars}
#'   giving the number of colors and groups, respectively.
#' @return A character vector of 1 or more hexadecimal RGB + alpha color
#'   representations in the form `'#RRGGBBAA'`.
#' @export
color <- function(x, lighten = 0, darken = 0, a = 1, comp = F) {
  ns <- c(length(x), length(lighten), length(darken), length(a))
  reps <- max(ns) / ns
  errs <- c(f0(cmp_clr_vec(x)          , NULL, "\n \u2022 [x] must be a complete color vec (?cmp_clr_vec) [i.e., containing only valid character-mode color values]."),
            f0(cmp_ppn_vec(lighten)    , NULL, "\n \u2022 [lighten] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1]."),
            f0(cmp_ppn_vec(darken)     , NULL, "\n \u2022 [darken] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1]."),
            f0(cmp_ppn_vec(a)          , NULL, "\n \u2022 [a] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1]."),
            f0(isTF(comp)              , NULL, "\n \u2022 [comp] must be TRUE or FALSE."),
            f0(all(reps == round(reps)), NULL, "\n \u2022 [x], [lighten], [darken], and [a] are not recyclable (?recyclable)."))
  if (!is.null(errs)) {stop(errs)}
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
  else if (nchar(x) == 7) {a <- 1}                                               # ...but if there is no existing alpha value, use a = 1
  else {a <- x[3] / 255}                                                         # ...otherwise, use the existing alpha value
  rgb(r, g, b, a)
}

#' @rdname color
#' @export
rgba <- function(r = 1, g = 1, b = 1, a = 1) {
  ns <- c(length(r), length(g), length(b), length(a))
  reps <- max(ns) / ns
  errs <- c(f0(cmp_ppn_vec(r)          , NULL, "\n \u2022 [r] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1]."),
            f0(cmp_ppn_vec(g)          , NULL, "\n \u2022 [g] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1]."),
            f0(cmp_ppn_vec(b)          , NULL, "\n \u2022 [b] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1]."),
            f0(cmp_ppn_vec(a)          , NULL, "\n \u2022 [a] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1]."),
            f0(all(reps == round(reps)), NULL, "\n \u2022 [r], [g], [b], and [a] are not recyclable (?recyclable)."))
  if (!is.null(errs)) {stop(errs)}
  rgb(r, g, b, a)
}

#' @rdname color
#' @export
hsva <- function(h = 1, s = 1, v = 1, a = 1) {
  ns <- c(length(h), length(s), length(v), length(a))
  reps <- max(ns) / ns
  errs <- c(f0(cmp_ppn_vec(h)          , NULL, "\n \u2022 [h] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1]."),
            f0(cmp_ppn_vec(s)          , NULL, "\n \u2022 [s] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1]."),
            f0(cmp_ppn_vec(v)          , NULL, "\n \u2022 [v] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1]."),
            f0(cmp_ppn_vec(a)          , NULL, "\n \u2022 [a] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1]."),
            f0(all(reps == round(reps)), NULL, "\n \u2022 [h], [s], [v], and [a] are not recyclable (?recyclable)."))
  if (!is.null(errs)) {stop(errs)}
  hsv(h, s, v, a)
}

#' @rdname color
#' @export
blend <- function(x, y, wx = 1, wy = 1, a = NA) {
  browser()
  ns <- c(length(x), length(y), length(wx), length(wy), length(a))
  reps <- max(ns) / ns
  errs <- c(f0(cmp_clr_vec(x)               , NULL, "\n \u2022 [x] must be a complete color vec (?cmp_clr_vec)."),
            f0(cmp_clr_vec(y)               , NULL, "\n \u2022 [x] must be a complete color vec (?cmp_clr_vec)."),
            f0(cmp_pos_vec(wx)              , NULL, "\n \u2022 [wx] must be a complete positive numeric vec (?cmp_pos_vec)."),
            f0(cmp_pos_vec(wy)              , NULL, "\n \u2022 [wu] must be a complete positive numeric vec (?cmp_pos_vec)."),
            f0(all(reps == round(reps))     , NULL, "\n \u2022 [a] must be [NA] or a complete proportion numeric vec(?cmp_ppn_vec)."),
            f0(f0(nas(a), T, cmp_ppn_vec(a)), NULL, "\n \u2022 [x], [y], [wx], [wy], and [a] are not recyclable (?recyclable)."))
  if (!is.null(errs)) {stop(errs)}
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
  r <- max(0, min(1, wx * xr + wy * yr))                                         # weight the red, green, blue, and alpha values of x and y
  g <- max(0, min(1, wx * xg + wy * yg))
  b <- max(0, min(1, wx * xb + wy * yb))
  a <- max(0, min(1, wx * xa + wy * ya))
  a[!is.na(a)] <- a[!is.na(a)]                                                   # use any non-NA values in a to replace weighted alpha values
  rgba(r, g, b, a)                                                               # return the blended color in hexadecimal RGBA format
}

#' @rdname color
#' @export
red <- function(p = 1, s = 0, a = 1) {rgba(p, s, s, a)}

#' @rdname color
#' @export
grn <- function(p = 1, s = 0, a = 1) {rgba(s, p, s, a)}

#' @rdname color
#' @export
blu <- function(p = 1, s = 0, a = 1) {rgba(s, s, p, a)}

#' @rdname color
#' @export
cyn <- function(p = 1, s = 0, a = 1) {rgba(s, p, p, a)}

#' @rdname color
#' @export
mag <- function(p = 1, s = 0, a = 1) {rgba(p, s, p, a)}

#' @rdname color
#' @export
vlt <- function(p = 1, s = 0, a = 1) {rgba(p, s, p, a)}

#' @rdname color
#' @export
ylw <- function(p = 1, s = 0, a = 1) {rgba(p, p, s, a)}

#' @rdname color
#' @export
gry <- function(p = 0.5, a = 1) {rgba(p, p, p, a)}

#' @rdname color
#' @export
blk <- function(a = 1) {rgba(0, 0, 0, a)}

#' @rdname color
#' @export
wht <- function(a = 1) {rgba(1, 1, 1, a)}

#' @rdname color
#' @export
ppl <- function(p = 1, s = 0, a = 1) {blend(red(p, s), blu(p, s), 1, 2, a)}

#' @rdname color
#' @export
orn <- function(p = 1, s = 0, a = 1) {blend(red(p, s), ylw(p, s), 33, 51, a)}

#' @rdname color
#' @export
blu_grn <- function(p = 1, s = 0, a = 1) {blend(blu(p, s), grn(p, s), a)}

#' @rdname color
#' @export
blu_ppl <- function(p = 1, s = 0, a = 1) {blend(blu(p, s), ppl(p, s), a)}

#' @rdname color
#' @export
blu_vlt <- function(p = 1, s = 0, a = 1) {blend(blu(p, s), vlt(p, s), a)}

#' @rdname color
#' @export
ylw_grn <- function(p = 1, s = 0, a = 1) {blend(ylw(p, s), grn(p, s), a)}

#' @rdname color
#' @export
ylw_orn <- function(p = 1, s = 0, a = 1) {blend(ylw(p, s), orn(p, s), a)}

#' @rdname color
#' @export
red_orn <- function(p = 1, s = 0, a = 1) {blend(red(p, s), orn(p, s), a)}

#' @rdname color
#' @export
red_ppl <- function(p = 1, s = 0, a = 1) {blend(red(p, s), ppl(p, s), a)}

#' @rdname color
#' @export
red_vlt <- function(p = 1, s = 0, a = 1) {blend(red(p, s), vlt(p, s), a)}

#' @rdname color
#' @export
pal_cb <- function(ng, nc = NULL, a = 1) {
  errs <- c(f0(cmp_psw_scl(ng)             , NULL, "\n \u2022 [ng] must be a complete positive whole-number scalar (?cmp_psw_scl)."),
            f0(f0(inll(nc), T, cmp_psw_scl), NULL, "\n \u2022 [nc] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl)."),
            f0(cmp_ppn_scl(a)              , NULL, "\n \u2022 [a] must be a complete proportion scalar (?cmp_ppn_scl). That is, in the interval [0, 1]."))
  if (!is.null(errs)) {stop(errs)}
  out <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC", # bright colors
           "#273D54", "#794716", "#712C2D", "#3B5C59", "#2D5128", "#776524", "#583D51", "#804F54", "#4E3B30", "#5D5856") # darkened 50%
  if (length(nc) == 0) {if (nc < 20) {nc <- nc} else {nc <- 20}}                 # IF [nc] was not supplied THEN IF [nc] is less than 20, use as is ELSE  cap at 20
  if (nc > length(out)) {out <- rep.int(out, ceiling(nc / length(out)))}         # if [nc] is larger than 20, repeat colors
  out <- out[1:nc]                                                               # take the first [nc] colors
  scales::alpha(out, a)                                                          # apply the alpha value
}

#' @rdname color
#' @export
pal_swatch <- function(x) {
  if (!cmp_clr_vec(x)) {stop("\n \u2022 [x] must be a complete color vec (?cmp_clr_vec) [i.e., containing only valid character-mode color values].")}
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
