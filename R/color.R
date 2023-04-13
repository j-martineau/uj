#' @encoding UTF-8
#' @family color
#' @family plots
#' @title Simple color creation
#' @description Simple color creation with a wide variety of options (intensity, blending, lightening, darkening, opacity). This family of functions uses three-letter codes for common colors as follows.
#' @details
#' \tabular{lll}{  *Color*   \tab *Color*   \tab *RGB channel values of*        \cr
#'                 *code*.   \tab *name*    \tab *default color version*        \cr
#'                 `wht`     \tab white     \tab `c(R = 1  , G = 1  , B = 1  )` \cr
#'                 `blk`     \tab black     \tab `c(R = 0  , G = 0  , B = 0  )` \cr
#'                 `gry`     \tab grey      \tab `c(R = 0.5, G = 0.5, B = 0.5)` \cr
#'                 `red`     \tab red       \tab `c(R = 1  , G = 0  , B = 0  )` \cr
#'                 `grn`     \tab green     \tab `c(R = 0  , G = 1  , B = 0  )` \cr
#'                 `blu`     \tab blue      \tab `c(R = 0  , G = 0  , B = 1  )` \cr
#'                 `ylw`     \tab yellow    \tab `c(R = 1  , G = 1  , B = 0  )` \cr
#'                 `cyn`     \tab cyan      \tab `c(R = 0  , G = 1  , B = 1  )` \cr
#'                 `mag`     \tab magenta   \tab `c(R = 1  , G = 0  , B = 1  )` \cr
#'                 `vlt`     \tab violet    \tab `c(R = 1  , G = 0  , B = 1  )` \cr
#'                 `ppl`     \tab purple    \tab a `1:2` blend of red:blue.     \cr
#'                 `orn`     \tab orange    \tab a `33:51` blend of red:yellow.    }
#' \cr\cr **Functions in this family**
#' \cr\cr The symbols `{xxx}` and `{yyy}` are placeholders for any given three-letter color codes as described in the table above.
#' \tabular{ll}{  `pal_swatch`    \tab Draws swatches for a collection/palette of colors.                                                                                                                                               \cr   \tab   \cr
#'                `{xxx}_{yyy}`   \tab Creates versions of 50/50 blends of the common colors `'{xxx}'` and `{yyy}` by `P` = primary intensity, `S` = secondary intensity, and `A` = alpha, each in the interval `[0, 1]`.               \cr   \tab   \cr
#'                `pal_cb`        \tab Creates colorblind-friendly palettes of `2` to `20` Colors `1-10` vs. `11-20` are bright vs. `50%` darkened versions. Bright/darkened differences may be insufficient for some audiences.        \cr   \tab   \cr
#'                `color`         \tab Takes any valid color representation and lightens, darkens, adds opacity levels, and/or gets the complementary color.                                                                            \cr   \tab   \cr
#'                `blend`         \tab Blends colors `X` and `Y` using non-negative numeric weights `WX` and `WY`, and proportional numeric `A` = alpha values. Each corresponding pair of values of these arguments is normalized
#'                                     to sum to 1 to give the proportions of the resulting color that should come from `X` and `Y`, respectively.                                                                                      \cr   \tab   \cr
#'                `rgba`          \tab Creates colors from `R` = red, `G` = green, `B` = blue, and `A` = alpha weights, each in the interval `[0, 1]`.                                                                                  \cr   \tab   \cr
#'                `hsva`          \tab Creates colors from `H` = hue, `S` = saturation, `V` = value, and `A` = alpha weights, each in the interval `[0, 1]`.                                                                            \cr   \tab   \cr
#'                `{xxx}`         \tab Creates versions of common color `'{xxx}'` by `P` = primary intensity, `S` = secondary intensity, and `A` = alpha, each in the interval `[0, 1]`.                                                               }
#' @section Recycling: The only arguments not recycled are `Na, NC`, and `NG`.
#' @section The Arguments `P` and `S`: `P` is used to indicate the proportion of the RGB values of the most intense version of a hue should be present in a resulting color. `S` is used to indicate how much of complement of each RGB value should be present in a resulting color. The table below gives a helpful heuristic for setting values of `P` and `S`:
#' \tabular{ll}{  **Argument values**   \tab **Resulting color**    \cr
#'                `P = 1, S = 0`        \tab intense                \cr
#'                `P > S > 0`           \tab lighter                \cr
#'                `S < P < 1`           \tab darker                 \cr
#'                `P = S = 1`           \tab white                  \cr
#'                `P = S = 0`           \tab black                  \cr
#'                `P = S = 0.5`         \tab grey                   \cr
#'                `S = 1, P = 0`        \tab intense, complementary \cr
#'                `S > P > 0`           \tab lighter, complementary \cr
#'                `P < S < 1`           \tab darker, complementary    }
#' A concrete numeric example is that calling
#' ```
#'    ylw(P = 0.7, S = 0.3)
#' ```
#' starts with primary and secondary RGB values of
#' ```
#'    prm = c(R = 1, G = 1, B = 0)
#'    sec = c(R = 0, G = 0, B = 1)
#' ```
#' To get final RGB values, the primary RGB values are multiplied by `P = 0.7`, secondary RGB values are multiple by `S = 0.3`, and the results are summed pairwise giving
#' ```
#'    final = c(R = 0.7, G = 0.7, B = 0.3)
#' ```
#' @param X An \link[=atm_vec]{atomic vec} containing valid color representations. Recycled with `Y, Lighten, Darken, Comp, A, P, S, WX, WY` where these are valid arguments.
#' @param Y An atomic vec containing valid R color representations.
#' @param Na `TRUE` or `FALSE` indicating whether `NA` counts as a valid color alpha level.
#' @param Lighten A \link[=cmp_ppn_vec]{complete proportion numeric vec} of proportions by which to lighten the colors given in `X` toward white.
#' @param Darken A complete proportion numeric vec of proportions by which to darken the colors given in `X` toward black.
#' @param Comp A \link[=cmp_lgl_vec]{complete logical vec} indicating whether to return complementary colors.
#' @param R,G,B Complete proportion numeric vecs giving intensities of red, blue, and green from the RGB color space. Recycled with each other and `A`.
#' @param H,S,V Complete proportion numeric vecs giving hue, saturation, and value/brightness from the HSV color space. Recycled√with each other and `A`.
#' @param A A complete proportion numeric vec of 1 or more alpha levels (`0` = transparent, `0.5` = translucent, `1` = opaque). `NA` values in `a` are only applicable with `color(.)` and `blend(.)`) to indicate keeping existing alpha values.
#' @param P,S Complete poportion numeric vecs of 1 or more primary and secondary color intensities (respectively) in the range `[0, 1]`.
#' @param WX,WY \link[=cmp_nnw_vec]{Complete non-negative numeric vecs} giving weights to apply to `X` and `Y` in color blending.
#' @param NC,NG \link[=cmp_psw_scl]{Complete positive whole-number scalars} giving the number of colors and groups, respectively.
#' @return A character vector of `1` or more hexadecimal RGB + alpha color representations in the form `'#RRGGBBAA'`.
#' @export
#' @examples
#' color("red")
#' color("#00FF80")
#' color("#FF0000AA")
#' color(c("red", "#00FF80", "#FF0000AA"))
#' rgba(1/3, 2/3, 3/3, 1/2)
#' hsav(1/3, 2/3, 3/3, 1/2)
#' rgba(1/3, 2/3, 3/3, 1/2)
#' blend("red", "blue", WX = 2, WY = 1, A = 0.5)
#' blk(a = 1 / 2)
#' wht(a = 1 / 2)
#' gry(P = 2 /3, A = 0.5)
#' red(P = 2 / 3, S = 1 / 3, A = 1 / 2)
#' grn(P = 2 / 3, S = 1 / 3, A = 1 / 2)
#' blu(P = 2 / 3, S = 1 / 3, A = 1 / 2)
#' cyn(P = 2 / 3, S = 1 / 3, A = 1 / 2)
#' mag(P = 2 / 3, S = 1 / 3, A = 1 / 2)
#' vlt(P = 2 / 3, S = 1 / 3, A = 1 / 2)
#' ylw(P = 2 / 3, S = 1 / 3, A = 1 / 2)
#' ppl(P = 2 / 3, S = 1 / 3, A = 1 / 2))
#' orn(P = 2 / 3, S = 1 / 3, A = 1 / 2))
#' blu_grn(P = 2 / 3, S = 1 / 3, A = 1 / 2)
#' blu_ppl(P = 2 / 3, S = 1 / 3, A = 1 / 2)
#' blu_vlt(P = 2 / 3, S = 1 / 3, A = 1 / 2)
#' ylw_grn(P = 2 / 3, S = 1 / 3, A = 1 / 2)
#' ylw_orn(P = 2 / 3, S = 1 / 3, A = 1 / 2)
#' red_orn(P = 2 / 3, S = 1 / 3, A = 1 / 2)
#' red_ppl(P = 2 / 3, S = 1 / 3, A = 1 / 2)
#' red_vlt(P = 2 / 3, S = 1 / 3, A = 1 / 2)
#' pal_cb(12)
#' pal_cb(3)
#' \dontrun{
#'   palSWATCH(palCB(12))
#'   palSWATCH(palCB(3))
#' }
color <- function(X, Lighten = 0, Darken = 0, A = 1, Comp = F) {
  uj:::.color_errs(X = X, Lighten = Lighten, Darken = Darken, A = A, Comp = Comp)
  Ns <- base::c(base::length(X), base::length(Lighten), base::length(Darken), base::length(A))
  Reps <- base::max(Ns) / Ns
  Errors <- NULL
  if (base::any(Reps != base::round(Reps))) {uj::stopperr("[X], [Lighten], [Darken], and [A] are not recyclable (?recyclable).", PKG = "uj")}
  if (Reps[1] > 1) {X       <- base::rep.int(X      , Reps[1])}
  if (Reps[2] > 1) {Lighten <- base::rep.int(Lighten, Reps[2])}
  if (Reps[3] > 1) {Darken  <- base::rep.int(Darken , Reps[3])}
  if (Reps[4] > 1) {A       <- base::rep.int(A      , Reps[4])}
  if (Reps[5] > 1) {Comp    <- base::rep.int(Comp   , Reps[5])}
  X <- grDevices::col2rgb(X, T)
  R <- X[1] / 255                                                                # convert red, green, blue color values from hexadecimal to decimal proportions
  G <- X[2] / 255
  B <- X[3] / 255
  R <- R + (1 - R) * Lighten                                                     # Lighten colors toward white by the proportion in whiten
  G <- G + (1 - G) * Lighten
  B <- B + (1 - B) * Lighten
  R <- R * (1 - Darken)                                                          # Darken colors toward black by the proportion in Darken
  G <- G * (1 - Darken)
  B <- B * (1 - Darken)
  R[Comp] <- 1 - R[Comp]                                                         # invert colors for colors marked for complementarity
  G[Comp] <- 1 - G[Comp]
  B[Comp] <- 1 - B[Comp]
  if (!base::is.null(A)) {A <- A}                                                       # if an alpha value was supplied, use it
  else if (base::nchar(X) == 7) {A <- 1}                                             # ...but if there is no existing alpha value, use A = 1
  else {A <- X[3] / 255}                                                         # ...otherwise, use the existing alpha value
  grDevices::rgb(R, G, B, A)
}

#' @rdname color
#' @export
rgba <- function(R = 1, G = 1, B = 1, A = 1) {
  uj:::.color_errs(R = R, G = G, B = B, A = A)
  Ns <- base::c(base::length(R), base::length(G), base::length(B), base::length(A))
  Reps <- base::max(Ns) / Ns
  if (base::any(Reps != base::round(Reps))) {uj::stopperr("[R], [G], [B], and [a] are not recyclable (?recyclable).", PKG = "uj")}
  grDevices::rgb(R, G, B, A)
}

#' @rdname color
#' @export
hsva <- function(H = 1, S = 1, V = 1, A = 1) {
  uj:::.color_errs(H = H, S = S, V = V, A = A)
  Ns <- base::c(base::length(H), base::length(S), base::length(V), base::length(A))
  Reps <- base::max(Ns) / Ns
  if (base::any(Reps != base::round(Reps))) {uj::stopperr("[H], [S], [V], and [a] are not recyclable (?recyclable).", PKG = "uj")}
  grDevices::hsv(H, S, V, A)
}

#' @rdname color
#' @export
blend <- function(X, Y, WX = 1, WY = 1, A = NA) {
  uj:::.color_errs(X = X, Y = Y, WX = WX, WY = WY, A = A, OkNaA = T)
  Ns <- base::c(base::length(X), base::length(Y), base::length(WX), base::length(WY), base::length(A))
  Reps <- base::max(Ns) / Ns
  if (base::any(Reps != base::round(Reps))) {uj::stopperr("[X], [Y], [WX], [WY], and [a] are not recyclable (?recyclable).", PKG = "uj")}
  X  <- uj::as_clr(X)                                                            # convert X and Y to hexadecimal RGBA
  Y  <- uj::as_clr(Y)
  WT <- WX + WY                                                                  # rescale X and Y weights to sum to 1
  WX <- WX / WT
  WY <- WY / WT
  XR <- uj::todec(base::substr(X, 2, 3)) / 255                                   # convert red values of X and Y from hexadecimal to decimal proportion
  YR <- uj::todec(base::substr(Y, 2, 3)) / 255
  XG <- uj::todec(base::substr(X, 4, 5)) / 255                                   # convert green values of X and Y from hexadecimal to decimal proportion
  YG <- uj::todec(base::substr(Y, 4, 5)) / 255
  XB <- uj::todec(base::substr(X, 6, 7)) / 255                                   # convert blue values of X and Y from hexadecimal to decimal proportion
  YB <- uj::todec(base::substr(Y, 6, 7)) / 255
  XA <- uj::todec(base::substr(X, 8, 9)) / 255                                   # convert alpha values of X and Y from hexadecimal to decimal proportion
  YA <- uj::todec(base::substr(Y, 8, 9)) / 255
  R <- base::max(0, base::min(1, WX * XR + WY * YR))                             # weight the red, green, blue, and alpha values of X and Y
  G <- base::max(0, base::min(1, WX * XG + WY * YG))
  B <- base::max(0, base::min(1, WX * XB + WY * YB))
  A <- base::max(0, base::min(1, WX * XA + WY * YA))
  A[uj::ok(A)] <- A                                                              # use any non-NA values in a to replace weighted alpha values
  uj::rgba(R, G, B, A)                                                           # return the blended color in hexadecimal RGBA format
}

#' @rdname color
#' @export
red <- function(P = 1, S = 0, A = 1) {uj::rgba(P, S, S, A)}

#' @rdname color
#' @export
grn <- function(P = 1, S = 0, A = 1) {uj::rgba(S, P, S, A)}

#' @rdname color
#' @export
blu <- function(P = 1, S = 0, A = 1) {uj::rgba(S, S, P, A)}

#' @rdname color
#' @export
cyn <- function(P = 1, S = 0, A = 1) {uj::rgba(S, P, P, A)}

#' @rdname color
#' @export
mag <- function(P = 1, S = 0, A = 1) {uj::rgba(P, S, P, A)}

#' @rdname color
#' @export
vlt <- function(P = 1, S = 0, A = 1) {uj::rgba(P, S, P, A)}

#' @rdname color
#' @export
ylw <- function(P = 1, S = 0, A = 1) {uj::rgba(P, P, S, A)}

#' @rdname color
#' @export
gry <- function(P = 0.5, A = 1) {uj::rgba(P, P, P, A)}

#' @rdname color
#' @export
blk <- function(A = 1) {uj::rgba(0, 0, 0, A)}

#' @rdname color
#' @export
wht <- function(A = 1) {uj::rgba(1, 1, 1, A)}

#' @rdname color
#' @export
ppl <- function(P = 1, S = 0, A = 1) {uj::blend(uj::red(P, S), uj::blu(P, S), 1, 2, A)}

#' @rdname color
#' @export
orn <- function(P = 1, S = 0, A = 1) {uj::blend(uj::red(P, S), uj::ylw(P, S), 33, 51, A)}

#' @rdname color
#' @export
blu_grn <- function(P = 1, S = 0, A = 1) {uj::blend(uj::blu(P, S), uj::grn(P, S), A)}

#' @rdname color
#' @export
blu_ppl <- function(P = 1, S = 0, A = 1) {uj::blend(uj::blu(P, S), uj::ppl(P, S), A)}

#' @rdname color
#' @export
blu_vlt <- function(P = 1, S = 0, A = 1) {uj::blend(uj::blu(P, S), uj::vlt(P, S), A)}

#' @rdname color
#' @export
ylw_grn <- function(P = 1, S = 0, A = 1) {uj::blend(uj::ylw(P, S), uj::grn(P, S), A)}

#' @rdname color
#' @export
ylw_orn <- function(P = 1, S = 0, A = 1) {uj::blend(uj::ylw(P, S), uj::orn(P, S), A)}

#' @rdname color
#' @export
red_orn <- function(P = 1, S = 0, A = 1) {uj::blend(uj::red(P, S), uj::orn(P, S), A)}

#' @rdname color
#' @export
red_ppl <- function(P = 1, S = 0, A = 1) {uj::blend(uj::red(P, S), uj::ppl(P, S), A)}

#' @rdname color
#' @export
red_vlt <- function(P = 1, S = 0, A = 1) {uj::blend(uj::red(P, S), uj::vlt(P, S), A)}

#' @rdname color
#' @export
pal_cb <- function(NG, NC = NULL, A = 1) {
  Errors <- NULL
  if (!uj:::.cmp_psw_scl(NG)) {Errors <- base::c(Errors, "[NG] must be a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!uj::f0(base::is.null(NC), T, uj:::.cmp_psw_scl(NC))) {Errors <- base::c(Errors, "[NC] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!uj:::.cmp_ppn_scl(a)) {Errors <- base::c(Errors, "[a] must be a complete proportion scalar (?cmp_ppn_scl). That is, in the interval [0, 1].")}
  if (!base::is.null(Errors)) {uj::stopperr(err, PKG = "uj")}
  Y <- base::c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC", # bright colors
               "#273D54", "#794716", "#712C2D", "#3B5C59", "#2D5128", "#776524", "#583D51", "#804F54", "#4E3B30", "#5D5856") # darkened 50%
  if (base::length(NC) == 0) {if (NC < 20) {NC <- NC} else {NC <- 20}}                   # IF [NC] was not supplied THEN IF [NC] is less than 20, use as is ELSE  cap at 20
  if (NC > base::length(Y)) {Y <- base::rep.int(Y, base::ceiling(NC / base::length(Y)))} # if [NC] is larger than 20, repeat colors
  Y <- Y[1:NC]                                                                           # take the first [NC] colors
  scales::alpha(Y, A)                                                                    # apply the alpha value
}

#' @rdname color
#' @export
pal_swatch <- function(X) {
  if (!uj:::.cmp_clr_vec(X)) {uj::stopperr("[X] must be a complete color vec (?cmp_clr_vec) [i.e., containing only valid character-mode color values].", PKG = "uj")}
  uj::as_clr(X)                                                                  # convert [X] to hexadecimal RGBA format
  Options <- graphics::par()                                                        # bank current graphical parameters
  graphics::par(mar  = base::c(0, 0, 0, 0),                                      # set margins to zero (entire window is the plot region)
                ann  = F,                                                        # : do not annotate with titles
                xaxs = "i", yaxs = "i",                                          # : tight axes (no pad for xlim and ylim)
                xaxt = "n", yaxt = "n",                                          # : do not draw axes
                bty  = "n",                                                      # : no box around the plot region
                col  = "black")                                                  # : default plotting color
  base::on.exit(graphics::par(Options))                                             # restore the previous graphical parameters when finished
  Left <- 0:(base::length(X) - 1)                                                       # left side of swatch rectangles
  Right <- Left + 1                                                              # right side of swatch rectangles
  base::plot(X = base::c(0, base::max(Right)), Y = base::c(0, 1), col = "white") # draw white dots in the bottom left and top right corners
  for (i in 1:base::max(Right)) {graphics::rect(Left[i], 0, Right[i], 1, col = X[i], border = "white")}
}
