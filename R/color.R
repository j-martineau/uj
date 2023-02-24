#' @name color
#' @encoding UTF-8
#' @family color
#' @family plots
#' @title Simple color creation
#' @description Simple color creation with a wide variety of options (intensity, blending, lightening, darkening, opacity). This family of functions uses three-letter codes for common colors as follows.
#' @details
#' \tabular{lll}{  *Color*   \tab *Color*   \tab *RGB channel values of*        \cr
#'                 *code*.   \tab *name*    \tab *default color version*        \cr
#'                 `wht`     \tab white     \tab `c(r = 1  , g = 1  , b = 1  )` \cr
#'                 `blk`     \tab black     \tab `c(r = 0  , g = 0  , b = 0  )` \cr
#'                 `gry`     \tab grey      \tab `c(r = 0.5, g = 0.5, b = 0.5)` \cr
#'                 `red`     \tab red       \tab `c(r = 1  , g = 0  , b = 0  )` \cr
#'                 `grn`     \tab green     \tab `c(r = 0  , g = 1  , b = 0  )` \cr
#'                 `blu`     \tab blue      \tab `c(r = 0  , g = 0  , b = 1  )` \cr
#'                 `ylw`     \tab yellow    \tab `c(r = 1  , g = 1  , b = 0  )` \cr
#'                 `cyn`     \tab cyan      \tab `c(r = 0  , g = 1  , b = 1  )` \cr
#'                 `mag`     \tab magenta   \tab `c(r = 1  , g = 0  , b = 1  )` \cr
#'                 `vlt`     \tab violet    \tab `c(r = 1  , g = 0  , b = 1  )` \cr
#'                 `ppl`     \tab purple    \tab a `1:2` blend of red:blue.     \cr
#'                 `orn`     \tab orange    \tab a `33:51` blend of red:yellow.    }
#' \cr\cr **Functions in this family**
#' \cr\cr The symbols `{xxx}` and `{yyy}` are placeholders for any given three-letter color codes as described in the table above.
#' \tabular{ll}{  `pal_swatch`   \tab Draws swatches for a collection/palette of colors.                                                                                                                                               \cr   \tab   \cr
#'                `{xxx}{yyy}`   \tab Creates versions of 50/50 blends of the common colors `'{xxx}'` and `{yyy}` by `p` = primary intensity, `s` = secondary intensity, and `a` = alpha, each in the interval `[0, 1]`.               \cr   \tab   \cr
#'                `pal_cb`       \tab Creates colorblind-friendly palettes of `2` to `20` Colors `1-10` vs. `11-20` are bright vs. `50%` darkened versions. Bright/darkened differences may be insufficient for some audiences.        \cr   \tab   \cr
#'                `color`        \tab Takes any valid color representation and lightens, darkens, adds opacity levels, and/or gets the complementary color.                                                                            \cr   \tab   \cr
#'                `blend`        \tab Blends colors `x` and `y` using non-negative numeric weights `wx` and `wy`, and proportional numeric `a` = alpha values. Each corresponding pair of values of these arguments is normalized
#'                                    to sum to 1 to give the proportions of the resulting color that should come from `x` and `y`, respectively.                                                                                      \cr   \tab   \cr
#'                `rgba`         \tab Creates colors from `r` = red, `g` = green, `b` = blue, and `a` = alpha weights, each in the interval `[0, 1]`.                                                                                  \cr   \tab   \cr
#'                `hsva`         \tab Creates colors from `h` = hue, `s` = saturation, `v` = value, and `a` = alpha weights, each in the interval `[0, 1]`.                                                                            \cr   \tab   \cr
#'                `{xxx}`        \tab Creates versions of common color `'{xxx}'` by `p` = primary intensity, `s` = secondary intensity, and `a` = alpha, each in the interval `[0, 1]`.                                                               }
#' @section Recycling: The only arguments not recycled are `na, nc`, and `ng`.
#' @section The Arguments `p` and `s`: `p` is used to indicate the proportion of the RGB values of the most intense version of a hue should be present in a resulting color. `s` is used to indicate how much of complement of each RGB value should be present in a resulting color. The table below gives a helpful heuristic for setting values of `p` and `s`:
#' \tabular{ll}{  **Argument values**   \tab **Resulting color**    \cr
#'                `p = 1, s = 0`        \tab intense                \cr
#'                `p > s > 0`           \tab lighter                \cr
#'                `s < p < 1`           \tab darker                 \cr
#'                `p = s = 1`           \tab white                  \cr
#'                `p = s = 0`           \tab black                  \cr
#'                `p = s = 0.5`         \tab grey                   \cr
#'                `s = 1, p = 0`        \tab intense, complementary \cr
#'                `s > p > 0`           \tab lighter, complementary \cr
#'                `p < s < 1`           \tab darker, complementary    }
#' A concrete numeric example is that calling
#' ```
#'    ylw(p = 0.7, s = 0.3)
#' ```
#' starts with primary and secondary RGB values of
#' ```
#'    prm = c(r = 1, g = 1, b = 0)
#'    sec = c(r = 0, g = 0, b = 1)
#' ```
#' To get final RGB values, the primary RGB values are multiplied by `p = 0.7`, secondary RGB values are multiple by `s = 0.3`, and the results are summed pairwise giving
#' ```
#'    final = c(r = 0.7, g = 0.7, b = 0.3)
#' ```
#' @param x An \link[=atm_vec]{atomic vec} containing valid color representations. Recycled with `y, lighten, darken, comp, a, p, s, wx, wy` where these are valid arguments.
#' @param y An atomic vec containing valid R color representations.
#' @param na `TRUE` or `FALSE` indicating whether `NA` counts as a valid color alpha level.
#' @param lighten A \link[=cmp_ppn_vec]{complete proportion numeric vec} of proportions by which to lighten the colors given in `x` toward white.
#' @param darken A complete proportion numeric vec of proportions by which to darken the colors given in `x` toward black.
#' @param comp A \link[=cmp_lgl_vec]{complete logical vec} indicating whether to return complementary colors.
#' @param r,g,b Complete proportion numeric vecs giving intensities of red, blue, and green from the RGB color space. Recycled with each other and `a`.
#' @param h,s,v Complete proportion numeric vecs giving hue, saturation, and value/brightness from the HSV color space. Recycled√with each other and `a`.
#' @param a A complete proportion numeric vec of 1 or more alpha levels (`0` = transparent, `0.5` = translucent, `1` = opaque). `NA` values in `a` are only applicable with `color(.)` and `blend(.)`) to indicate keeping existing alpha values.
#' @param p,s Complete poportion numeric vecs of 1 or more primary and secondary color intensities (respectively) in the range `[0, 1]`.
#' @param wx,wy \link[=cmp_nnw_vec]{Complete non-negative numeric vecs} giving weights to apply to `x` and `y` in color blending.
#' @param nc,ng \link[=cmp_psw_scl]{Complete positive whole-number scalars} giving the number of colors and groups, respectively.
#' @return A character vector of `1` or more hexadecimal RGB + alpha color representations in the form `'#RRGGBBAA'`.
#' @export
#' @examples
#' color("red")
#' color("#00FF80")
#' color("#FF0000AA")
#' color(c("red", "#00FF80". "#FF0000AA"))
#' rgba(1/3, 2/3, 3/3, 1/2)
#' hsav(1/3, 2/3, 3/3, 1/2)
#' rgba(1/3, 2/3, 3/3, 1/2)
#' blend("red", "blue", wx = 2, wy = 1, a = 0.5)
#' blk(a = 1 / 2)
#' wht(a = 1 / 2)
#' gry(p = 2 /3, a = 0.5)
#' red(p = 2 / 3, s = 1 / 3, a = 1 / 2)
#' grn(p = 2 / 3, s = 1 / 3, a = 1 / 2)
#' blu(p = 2 / 3, s = 1 / 3, a = 1 / 2)
#' cyn(p = 2 / 3, s = 1 / 3, a = 1 / 2)
#' mag(p = 2 / 3, s = 1 / 3, a = 1 / 2)
#' vlt(p = 2 / 3, s = 1 / 3, a = 1 / 2)
#' ylw(p = 2 / 3, s = 1 / 3, a = 1 / 2)
#' ppl(p = 2 / 3, s = 1 / 3, a = 1 / 2))
#' orn(p = 2 / 3, s = 1 / 3, a = 1 / 2))
#' blugrn(p = 2 / 3, s = 1 / 3, a = 1 / 2)
#' bluPPL(p = 2 / 3, s = 1 / 3, a = 1 / 2)
#' bluvlt(p = 2 / 3, s = 1 / 3, a = 1 / 2)
#' ylwgrn(p = 2 / 3, s = 1 / 3, a = 1 / 2)
#' ylworn(p = 2 / 3, s = 1 / 3, a = 1 / 2)
#' redorn(p = 2 / 3, s = 1 / 3, a = 1 / 2)
#' redPPL(p = 2 / 3, s = 1 / 3, a = 1 / 2)
#' redvlt(p = 2 / 3, s = 1 / 3, a = 1 / 2)
#' palCBb(12)
#' palCB(3)
#' \dontrun{
#'   palSWATCH(palCB(12))
#'   palSWATCH(palCB(3))
#' }
color <- function(x, lighten = 0, darken = 0, a = 1, comp = F) {
  ns <- base::c(uj::N(x), uj::N(lighten), uj::N(darken), uj::N(a))
  reps <- base::max(ns) / ns
  uj::errs_if_nots(uj::cmp_clr_vec(x)                  , "[x] must be a complete color vec (?cmp_clr_vec) [i.e., containing only valid character-mode color values].",
                   uj::cmp_ppn_vec(lighten)            , "[lighten] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1]."              ,
                   uj::cmp_ppn_vec(darken)             , "[darken] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1]."               ,
                   uj::cmp_ppn_vec(a)                  , "[a] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1]."                    ,
                   uj::cmp_lgl_scl(comp)               , "[comp] must be TRUE or FALSE."                                                                             ,
                   base::all(reps == base::round(reps)), "[x], [lighten], [darken], and [a] are not recyclable (?recyclable)."                                       , PKG = "uj")
  if (r[1] > 1) {x       <- base::rep.int(x      , r[1])}
  if (r[2] > 1) {lighten <- base::rep.int(lighten, r[2])}
  if (r[3] > 1) {darken  <- base::rep.int(darken , r[3])}
  if (r[4] > 1) {a       <- base::rep.int(a      , r[4])}
  if (r[5] > 1) {comp    <- base::rep.int(comp   , r[5])}
  x <- grDevices::col2rgb(x, T)
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
  if (uj::DEF(a)) {a <- a}                                                       # if an alpha value was supplied, use it
  else if (uj::LEN(x) == 7) {a <- 1}                                             # ...but if there is no existing alpha value, use a = 1
  else {a <- x[3] / 255}                                                         # ...otherwise, use the existing alpha value
  grDevices::rgb(r, g, b, a)
}

#' @rdname color
#' @export
rgba <- function(r = 1, g = 1, b = 1, a = 1) {
  ns <- base::c(uj::N(r), uj::N(g), uj::N(b), uj::N(a))
  reps <- base::max(ns) / ns
  ok.recycle <- base::all(uj::rounded(reps))
  uj::errs_if_nots(uj::cmp_ppn_vec(r), "[r] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].",
                   uj::cmp_ppn_vec(g), "[g] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].",
                   uj::cmp_ppn_vec(b), "[b] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].",
                   uj::cmp_ppn_vec(a), "[a] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].",
                   ok.recycle        , "[r], [g], [b], and [a] are not recyclable (?recyclable)."                              , PKG = "uj")
  grDevices::rgb(r, g, b, a)
}

#' @rdname color
#' @export
hsva <- function(h = 1, s = 1, v = 1, a = 1) {
  ns <- base::c(uj::N(h), uj::N(s), uj::N(v), uj::N(a))
  reps <- base::max(ns) / ns
  ok.recycle <- base::all(uj::rounded(reps))
  uj::errs_if_nots(uj::cmp_ppn_vec(h), "[h] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].",
                   uj::cmp_ppn_vec(s), "[s] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].",
                   uj::cmp_ppn_vec(v), "[v] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].",
                   uj::cmp_ppn_vec(a), "[a] must be a complete proportion vec (?cmp_ppn_vec). That is, in the interval [0, 1].",
                   ok.recycle        , "[h], [s], [v], and [a] are not recyclable (?recyclable)."                              , PKG = "uj")
  grDevices::hsv(h, s, v, a)
}

#' @rdname color
#' @export
blend <- function(x, y, wx = 1, wy = 1, a = NA) {
  ns <- base::c(uj::N(x), uj::N(y), uj::N(wx), uj::N(wy), uj::N(a))
  reps <- base::max(ns) / ns
  ok.recycle <- base::all(uj::rounded(reps))
  uj::errs_if_nots(uj::cmp_clr_vec(x)             , "[x] must be a complete color vec (?cmp_clr_vec)."                    ,
                   uj::cmp_clr_vec(y)             , "[x] must be a complete color vec (?cmp_clr_vec)."                    ,
                   uj::cmp_pos_vec(wx)            , "[wx] must be a complete positive numeric vec (?cmp_pos_vec)."        ,
                   uj::cmp_pos_vec(wy)            , "[wu] must be a complete positive numeric vec (?cmp_pos_vec)."        ,
                   uj::nll_or(reps, "cmp_ppn_vec"), "[a] must be [NA] or a complete proportion numeric vec(?cmp_ppn_vec).",
                   ok.recycle                     , "[x], [y], [wx], [wy], and [a] are not recyclable (?recyclable)."     , PKG = "uj")
  x  <- uj::asCLR(x)                                                             # convert x and y to hexadecimal RGBA
  y  <- uj::asCLR(y)
  wt <- wx + wy                                                                  # rescale x and y weights to sum to 1
  wx <- wx / wt
  wy <- wy / wt
  xr <- uj::todec(base::substr(x, 2, 3)) / 255                                   # convert red values of x and y from hexadecimal to decimal proportion
  yr <- uj::todec(base::substr(y, 2, 3)) / 255
  xg <- uj::todec(base::substr(x, 4, 5)) / 255                                   # convert green values of x and y from hexadecimal to decimal proportion
  yg <- uj::todec(base::substr(y, 4, 5)) / 255
  xb <- uj::todec(base::substr(x, 6, 7)) / 255                                   # convert blue values of x and y from hexadecimal to decimal proportion
  yb <- uj::todec(base::substr(y, 6, 7)) / 255
  xa <- uj::todec(base::substr(x, 8, 9)) / 255                                   # convert alpha values of x and y from hexadecimal to decimal proportion
  ya <- uj::todec(base::substr(y, 8, 9)) / 255
  r <- base::max(0, base::min(1, wx * xr + wy * yr))                             # weight the red, green, blue, and alpha values of x and y
  g <- base::max(0, base::min(1, wx * xg + wy * yg))
  b <- base::max(0, base::min(1, wx * xb + wy * yb))
  a <- base::max(0, base::min(1, wx * xa + wy * ya))
  a[uj::ok(a)] <- a                                                              # use any non-NA values in a to replace weighted alpha values
  uj::rgba(r, g, b, a)                                                           # return the blended color in hexadecimal RGBA format
}

#' @rdname color
#' @export
red <- function(p = 1, s = 0, a = 1) {uj::rgba(p, s, s, a)}

#' @rdname color
#' @export
grn <- function(p = 1, s = 0, a = 1) {uj::rgba(s, p, s, a)}

#' @rdname color
#' @export
blu <- function(p = 1, s = 0, a = 1) {uj::rgba(s, s, p, a)}

#' @rdname color
#' @export
cyn <- function(p = 1, s = 0, a = 1) {uj::rgba(s, p, p, a)}

#' @rdname color
#' @export
mag <- function(p = 1, s = 0, a = 1) {uj::rgba(p, s, p, a)}

#' @rdname color
#' @export
vlt <- function(p = 1, s = 0, a = 1) {uj::rgba(p, s, p, a)}

#' @rdname color
#' @export
ylw <- function(p = 1, s = 0, a = 1) {uj::rgba(p, p, s, a)}

#' @rdname color
#' @export
gry <- function(p = 0.5, a = 1) {uj::rgba(p, p, p, a)}

#' @rdname color
#' @export
blk <- function(a = 1) {uj::rgba(0, 0, 0, a)}

#' @rdname color
#' @export
wht <- function(a = 1) {uj::rgba(1, 1, 1, a)}

#' @rdname color
#' @export
ppl <- function(p = 1, s = 0, a = 1) {uj::blend(uj::red(p, s), uj::blu(p, s), 1, 2, a)}

#' @rdname color
#' @export
orn <- function(p = 1, s = 0, a = 1) {uj::blend(uj::red(p, s), uj::ylw(p, s), 33, 51, a)}

#' @rdname color
#' @export
blugrn <- function(p = 1, s = 0, a = 1) {uj::blend(uj::blu(p, s), uj::grn(p, s), a)}

#' @rdname color
#' @export
bluPPL <- function(p = 1, s = 0, a = 1) {uj::blend(uj::blu(p, s), uj::ppl(p, s), a)}

#' @rdname color
#' @export
bluvlt <- function(p = 1, s = 0, a = 1) {uj::blend(uj::blu(p, s), uj::vlt(p, s), a)}

#' @rdname color
#' @export
ylwgrn <- function(p = 1, s = 0, a = 1) {uj::blend(uj::ylw(p, s), uj::grn(p, s), a)}

#' @rdname color
#' @export
ylworn <- function(p = 1, s = 0, a = 1) {uj::blend(uj::ylw(p, s), uj::orn(p, s), a)}

#' @rdname color
#' @export
redorn <- function(p = 1, s = 0, a = 1) {uj::blend(uj::red(p, s), uj::orn(p, s), a)}

#' @rdname color
#' @export
redPPL <- function(p = 1, s = 0, a = 1) {uj::blend(uj::red(p, s), uj::ppl(p, s), a)}

#' @rdname color
#' @export
redvlt <- function(p = 1, s = 0, a = 1) {uj::blend(uj::red(p, s), uj::vlt(p, s), a)}

#' @rdname color
#' @export
pal_cb <- function(ng, nc = NULL, a = 1) {
  uj::errs_if_nots(uj::cmp_psw_scl(ng)          , "[ng] must be a complete positive whole-number scalar (?cmp_psw_scl)."                     ,
                   uj::nll_or(nc, "cmp_psw_scl"), "[nc] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl)."             ,
                   uj::cmp_ppn_scl(a)           , "[a] must be a complete proportion scalar (?cmp_ppn_scl). That is, in the interval [0, 1].", PKG = "uj")
  y <- base::c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC", # bright colors
               "#273D54", "#794716", "#712C2D", "#3B5C59", "#2D5128", "#776524", "#583D51", "#804F54", "#4E3B30", "#5D5856") # darkened 50%
  if (uj::N0(nc)) {if (nc < 20) {nc <- nc} else {nc <- 20}}                      # IF [nc] was not supplied THEN IF [nc] is less than 20, use as is ELSE  cap at 20
  if (nc > uj::N(y)) {y <- base::rep.int(y, uj::HI(nc / uj::N(y)))}              # if [nc] is larger than 20, repeat colors
  y <- y[1:nc]                                                                   # take the first [nc] colors
  scales::alpha(y, a)                                                            # apply the alpha value
}

#' @rdname color
#' @export
pal_swatch <- function(x) {
  uj::err_if_not(uj::cmp_clr_vec(x), "[x] must be a complete color vec (?cmp_clr_vec) [i.e., containing only valid character-mode color values].", PKG = "uj")
  uj::asCLR(x)                                                                   # convert [x] to hexadecimal RGBA format
  opts <- graphics::par()                                                        # bank current graphical parameters
  graphics::par(mar  = base::c(0, 0, 0, 0),                                      # set margins to zero (entire window is the plot region)
                ann  = F,                                                        # : do not annotate with titles
                xaxs = "i", yaxs = "i",                                          # : tight axes (no pad for xlim and ylim)
                xaxt = "n", yaxt = "n",                                          # : do not draw axes
                bty  = "n",                                                      # : no box around the plot region
                col  = "black")                                                  # : default plotting color
  base::on.exit(graphics::par(opts))                                             # restore the previous graphical parameters when finished
  left <- 0:(uj::N(x) - 1)                                                       # left side of swatch rectangles
  right <- left + 1                                                              # right side of swatch rectangles
  base::plot(x = base::c(0, base::max(right)), y = base::c(0, 1), col = "white") # draw white dots in the bottom left and top right corners
  for (i in 1:base::max(right)) {graphics::rect(left[i], 0, right[i], 1, col = x[i], border = "white")}
}
