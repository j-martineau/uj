# alert utils ####


#' @encoding UTF-8
#' @family alert_utils
#' @title Validate Formatting Argument Values
#' @param st Optional style spec.
#' @param bg Optional background color spec.
#' @param fg Optional foreground color spec.
#' @param d Character scalar text delimiter.
#' @param nullst Logical scalar indicating whether a `NULL` style spec is valid.
#' @param nullbg Logical scalar indicating whether a `NULL` background color spec is valid.
#' @param nullfg Logical scalar indicating whether a `NULL` foreground color spec is valid.
#' @examples
#' \dontrun{
#'   uj::.fmt_errs(),
#'   uj::.fmt_errs(d = 100),
#'   uj::.fmt_errs(st = "", bg = "", fg = ""),
#'   uj::.fmt_errs(st = "q", bg = "q", fg = "q"),
#'   uj::.fmt_errs(nullst = F, nullbg = F, nullfg = F))
#' }
#' @export
.fmt_errs <- function(st = NULL, bg = NULL, fg = NULL, d = " ", nullst = TRUE, nullbg = TRUE, nullfg = TRUE) {
  st   <- uj::failsafe(st)
  bg   <- uj::failsafe(bg)
  fg   <- uj::failsafe(fg)
  d    <- uj::failsafe(d)
  okST <- uj::f0(base::is.null(st), nullst, uj::.unq_chr_vec(st, valid = base::c(uj::.sts(), base::toupper(uj::.sts()))))
  okBG <- uj::f0(base::is.null(bg), nullbg, uj::.cmp_chr_scl(bg, valid = base::c(uj::.bgs(), base::toupper(uj::.bgs()))))
  okFG <- uj::f0(base::is.null(fg), nullfg, uj::.cmp_chr_scl(fg, valid = base::c(uj::.fgs(), base::toupper(uj::.fgs()))))
  okD  <- uj::.cmp_chr_scl(d)
  errs <- NULL
  if (!okBG) {errs <- base::c(errs, "[bg] must be a character scalar from bg_vals().")}
  if (!okFG) {errs <- base::c(errs, "[fg] must be a character scalar from fg_vals().")}
  if (!okST) {errs <- base::c(errs, "[st] must be a unique character vec from st_vals().")}
  if (!okD ) {errs <- base::c(errs, "[d] must be a non-NA character scalar.")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
}

#' @encoding UTF-8
#' @family alert_utils
#' @title Validate Formatting Argument Values
#' @param x Character vector format spec (element 1 is style, element 2 is foreground color, element 3 is background color). May be a pipe-delimited character scalar.
#' @examples
#' egOkFmt <- function() {
#'   base::list(blank  = uj::.ok_fmt(""),
#'              null   = uj::.ok_fmt(NULL),
#'              bad.st = uj::.ok_fmt("q|r|b"),
#'              ok.all = uj::.ok_fmt("y|r|b"),
#'              padded = uj::.ok_fmt("  yellow | red | bold  "))
#' }
#' egOkFmt()
#' @export
.ok_fmt <- function(x) {
  if (base::is.null(x)) {return(F)}
  else if (base::is.character(x) & base::length(x) == 1) {if (!base::is.na(x)) {if (x == "") {return(T)}}}
  x <- uj::ss("|", x)
  if (base::length(x) != 3 | !base::is.character(x)) {F}
  else {x[1] %in% uj::.bgs() & x[2] %in% uj::.fgs() & base::all(x[3] %in% uj::.sts())}
}

#' @encoding UTF-8
#' @family alert_utils
#' @title Choose from a List of Options with a Given Message and a Variety of Options
#' @param opts   An atomic vector of options to choose from.
#' @param mssg   Character scalar message to prompt the selection.
#' @param all    Logical scalar indicating whether it is valid to select all options.
#' @param none   Logical scalar indicating whether it is valid to select none of the options.
#' @param min    Non-negative integer scalar minimum number of options to select.
#' @param max    Positive integer scalar maximum numver of options to select.
#' @param ft     Character scalar title formatting spec.
#' @param fs     Character scalar subtitle formatting spec.
#' @param fun    Character scalar name of calling function.
#' @param stack  Character vector call stack.
#' @param clear  Logical scalar indicating whether to clear the console before alerting the user to the selection.
#' @param cancel Logical scalar indicating whether cancellation of selection is valid.
#' @return An atomic vector.
#' @examples
#' \dontrun{
#'   uj::.choose_from(letters, "What letter?", T, N, 0, 26, "b|s|u", "w|b|p", "console", "console", F)
#'   uj::.choose_from(0:9    , "What number?", F, F, 1, 1 , "r|y|b", "y|r|i", "console", "console", T, cancel = F)
#' }
#'
#' @export
.choose_from <- function(opts, mssg, all, none, min, max, ft, fs, fun, stack, clear, cancel = T) {
  if (clear) {uj::xcon()}
  if (ft != "") {
    tbg <- base::strsplit(ft, "|", fixed = T)[[1]][1]
    tfg <- base::strsplit(ft, "|", fixed = T)[[1]][2]
    tst <- base::strsplit(ft, "|", fixed = T)[[1]][3]
  } else {tbg <- tfg <- tst <- NULL}
  if (fs != "") {
    sbg <- base::strsplit(fs, "|", fixed = T)[[1]][1]
    sfg <- base::strsplit(fs, "|", fixed = T)[[1]][2]
    sst <- base::strsplit(fs, "|", fixed = T)[[1]][3]
  } else {sbg <- sfg <- NULL}
  uj::alert(mssg, title = "response required", ft = ft, fs = fs, clear = clear)
  if (cancel) {
    if (TRUE) {base::cat(uj::txt("\nCODE   OPTION         ", bg = sbg, fg = sfg, st = sst))}
    if (TRUE) {base::cat("\n   X = { CANCEL }")}
    if (none) {base::cat("\n   N = { NONE }")}
    if (all ) {base::cat("\n   A = { ALL }")}
  } else {
    if (TRUE) {base::cat(uj::txt("\nCODE   OPTION         ", bg = sbg, fg = sfg, st = sst))}
    if (none) {base::cat("\n   N = { NONE }")}
    if (all ) {base::cat("\n   A = { ALL }")}
  }
  for (i in 1:base::length(opts)) {
    code <- base::as.character(i)
    pref <- uj::p0(base::rep.int(" ", 4 - base::nchar(code)), collapse = "")
    inf  <- " = "
    opt  <- base::gsub(" ", " ", opts[i], fixed = T)
    base::cat("\n", pref, code, inf, opt, sep = "")
  }
  base::cat("\n\n")
  if      (min == 1 & max == 1) {base::cat(uj::txt("Enter the code for"                       , min,            "of the above options:", d = " ", bg = tbg, fg = tfg, st = tst))}
  else if (min ==     max     ) {base::cat(uj::txt("Enter a comma separated list of codes for", min,            "of the above options:", d = " ", bg = tbg, fg = tfg, st = tst))}
  else                          {base::cat(uj::txt("Enter a comma separated list of codes for", min, "to", max, "of the above options:", d = " ", bg = tbg, fg = tfg, st = tst))}
  ans <- base::toupper(base::trimws(base::strsplit(base::readline(), ",", fixed = TRUE)[[1]], which = "both"))
  if (base::length(ans) == 1) {
    if (cancel & ans == "X") {uj::stopperr("Canceled by user.", fun = fun, stack = stack)}
    if (none   & ans == "N") {return(NULL)}
    if (all    & ans == "A") {return(opts)}
    ans <- base::as.numeric(ans)
    if (!uj::.cmp_psw_scl(ans)) {uj::stopperr("Invalid selection code"                                         , fun = fun, stack = stack)}
    if (1 < min               ) {uj::stopperr("Too few options selected."                                      , fun = fun, stack = stack)}
    if (ans > uj::N(opts)     ) {uj::stopperr("Selection code is greater than the number of available options.", fun = fun, stack = stack)}
  } else {
    ans <- base::as.numeric(ans)
    if (!uj::cmp_psw_vec(ans)       ) {uj::stopperr("Unrecognized selection code(s)."                                  , fun = fun, stack = stack)}
    if (uj::N(ans) < min            ) {uj::stopperr("Too few options selected."                                        , fun = fun, stack = stack)}
    if (uj::N(ans) > max            ) {uj::stopperr("Too many options selected."                                       , fun = fun, stack = stack)}
    if (base::any(ans > uj::N(opts))) {uj::stopperr("A selection code is greater than the number of available options.", fun = fun, stack = stack)}
  }
  opts[ans]
}

# color_aliases ####

#' @encoding UTF-8
#' @title Color Aliases
#' @description Each color is assigned a set of aliases, all of which will work in this package.
#' @return A character vector.
#' @examples
#' egColorAliases <- function() {
#'   base::list(blk = uj::.blk(), blu = uj::.blu(), cyn = uj::.cyn(), def = uj::.def(), grn = uj::.grn(),
#'              mag = uj::.mag(), red = uj::.red(), sil = uj::.sil(), slv = uj::.slv(), gry = uj::.gry(),
#'              wht = uj::.wht(), yel = uj::.yel(), ylw = uj::.ylw(), bgs = uj::.bgs(), fgs = uj::.fgs())
#' }
#' egColorAliases()
#' @export
color_aliases <- function() {utils::help("color_aliases", package = "uj")}

#' @describeIn color_aliases Aliases for "black".
#' @export
.blk <- function() {base::c("k", "blk", "black")}

#' @describeIn color_aliases Aliases for "blue".
#' @export
.blu <- function() {base::c("b", "blu", "blue")}

#' @describeIn color_aliases Aliases for "cyan".
#' @export
.cyn <- function() {base::c("c", "cyn", "cyan")}

#' @describeIn color_aliases Aliases for "green".
#' @export
.grn <- function() {base::c("g", "grn", "green")}

#' @describeIn color_aliases Aliases for "magenta".
#' @export
.mag <- function() {base::c("m", "mag", "magenta")}

#' @describeIn color_aliases Aliases for "red".
#' @export
.red <- function() {base::c("r", "red")}

#' @describeIn color_aliases Aliases for "silver".
#' @export
.sil <- function() {base::c("s", "sil", "slv", "silver", "gray", "grey", "gry")}

#' @describeIn color_aliases Aliases for "yellow".
#' @export
.slv <- .sil

#' @describeIn color_aliases Aliases for "yellow".
#' @export
.gry <- .sil

#' @describeIn color_aliases Aliases for "white".
#' @export
.wht <- function() {base::c("w", "wht", "white")}

#' @describeIn color_aliases Aliases for "yellow".
#' @export
.yel <- function() {base::c("y", "yel", "ylw", "yellow")}

#' @describeIn color_aliases Aliases for "yellow".
#' @export
.ylw <- .yel

#' @describeIn color_aliases All aliases for background colors.
#' @export
.bgs <- function() {base::sort(base::c(uj::.blk(), uj::.blu(), uj::.cyn(), uj::.grn(), uj::.mag(), uj::.red(), uj::.wht(), uj::.yel(), uj::.def()))}

#' @describeIn color_aliases All aliases for foreground colors.
#' @export
.fgs <- function() {base::sort(base::c(uj::.bgs(), uj::.sil()))}

# style aliases ####

#' @encoding UTF-8
#' @title Style Aliases
#' @description Each style is assigned a set of aliases, all of which will work in this package.
#' @return A character vector.
#' @examples
#' egStyleAliases <- function() {
#'   base::list(bld = uj::.bld(), def = uj::.def(), itl = uj::.itl(), pln = uj::.pln(),
#'              res = uj::.res(), und = uj::.und(), sts = uj::.sts())
#' }
#' egStyleAliases()
#' @export
style_aliases <- function() {utils::help("style_aliases", package = "uj")}

#' @describeIn style_aliases All aliases for bold style.
#' @export
.bld <- function() {base::c("b", "bo", "bld", "bold", "bolded", "s", "st", "str", "strong", "strengthened")}

#' @describeIn style_aliases All aliases for italic style.
#' @export
.itl <- function() {base::c("i", "it", "itl", "italic", "italics", "italicized", "e", "em", "emp", "emph", "emphasis", "emphasized")}

#' @describeIn style_aliases All aliases for plain style
#' @export
.pln <- function() {base::c("p", "pl", "pln", "plain")}

#' @describeIn style_aliases All aliases for default style
#' @export
.def <- function() {base::c("d", "df", "def", "default")}

#' @describeIn style_aliases All aliases for resetting styles and colors back to default values.
#' @export
.res <- function() {base::c("r", "res", "reset")}

#' @describeIn style_aliases All aliases for underline style.
#' @export
.und <- function() {base::c("u", "un", "und", "under", "underline", "underlined")}

#' @describeIn style_aliases All aliases for styles.
#' @export
.sts <- function() {base::sort(base::c(uj::.bld(), uj::.itl(), uj::.pln(), uj::.def(), uj::.und(), uj::.res()))}

# match_alias ####

#' @encoding UTF-8
#' @family alert_utils
#' @title Match Color or Style Aliases
#' @description Returns the Official Color or Style Designation from Any Alias
#' @param x A character scalar alias.
#' @return A 3-letter character scalar.
#' @examples
#' egMatchAlias <- function() {
#'   base::list(
#'     st = base::list(bld = base::list(b = uj::.match_st("b"),
#'                                      bo = uj::.match_st("bo"),
#'                                      bld = uj::.match_st("bld"),
#'                                      bold = uj::.match_st("bold"),
#'                                      bolded = uj::.match_st("bolded"),
#'                                      s = uj::.match_st("s"),
#'                                      st = uj::.match_st("st"),
#'                                      str = uj::.match_st("str"),
#'                                      strong = uj::.match_st("strong"),
#'                                      strengthened = uj::.match_st("strengthened")),
#'                     itl = base::list(i = uj::.match_st("i"),
#'                                      it = uj::.match_st("it"),
#'                                      itl = uj::.match_st("itl"),
#'                                      italic = uj::.match_st("italic"),
#'                                      italicized = uj::.match_st("italicized"),
#'                                      e = uj::.match_st("e"),
#'                                      em = uj::.match_st("em"),
#'                                      emp = uj::.match_st("emp"),
#'                                      emph = uj::.match_st("emph"),
#'                                      emphasis = uj::.match_st("emphasis"),
#'                                      emphasized = uj::.match_st("emphasized")),
#'                     und = base::list(u = uj::.match_st("u"),
#'                                      un = uj::.match_st("un"),
#'                                      und = uj::.match_st("und"),
#'                                      under = uj::.match_st("under"),
#'                                      underline = uj::.match_st("underline"),
#'                                      underlined = uj::.match_st("underlined")),
#'                     pln = base::list(p = uj::.match_st("p"),
#'                                      pl = uj::.match_st("pl"),
#'                                      pln = uj::.match_st("pln"),
#'                                      plain = uj::.match_st("plain"),
#'                                      r = uj::.match_st("r"),
#'                                      res = uj::.match_st("res"),
#'                                      reset = uj::.match_st("reset"),
#'                                      d = uj::.match_st("d"),
#'                                      def = uj::.match_st("def"),
#'                                      default = uj::.match_st("default"))),
#'     fg = base::list(blk = base::list(k = uj::.match_fg("k"),
#'                                      blk = uj::.match_fg("blk"),
#'                                      black = uj::.match_fg("black")),
#'                     blu = base::list(b = uj::.match_fg("b"),
#'                                      blu = uj::.match_fg("blu"),
#'                                      blue = uj::.match_fg("blue")),
#'                     cyn = base::list(c = uj::.match_fg("c"),
#'                                      cyn = uj::.match_fg("cyn"),
#'                                      cyan = uj::.match_fg("cyan")),
#'                     def = base::list(d = uj::.match_fg("d"),
#'                                      def = uj::.match_fg("def"),
#'                                      default = uj::.match_fg("default")),
#'                     grn = base::list(g = uj::.match_fg("g"),
#'                                      grn = uj::.match_fg("grn"),
#'                                      green = uj::.match_fg("green")),
#'                     mag = base::list(m = uj::.match_fg("m"),
#'                                      mag = uj::.match_fg("mag"),
#'                                      magenta = uj::.match_fg("magenta")),
#'                     red = base::list(r = uj::.match_fg("r"),
#'                                      red = uj::.match_fg("red")),
#'                     wht = base::list(w = uj::.match_fg("w"),
#'                                      wht = uj::.match_fg("wht"),
#'                                      white = uj::.match_fg("white")),
#'                     yel = base::list(y = uj::.match_fg("y"),
#'                                      yel = uj::.match_fg("yel"),
#'                                      ylw = uj::.match_fg("ylw"),
#'                                      yellow = uj::.match_fg("yellow"))),
#'     bg = base::list(blk = base::list(k = uj::.match_bg("k"),
#'                                      blk = uj::.match_bg("blk"),
#'                                      black = uj::.match_bg("black")),
#'                     blu = base::list(b = uj::.match_bg("b"),
#'                                      blu = uj::.match_bg("blu"),
#'                                      blue = uj::.match_bg("blue")),
#'                     cyn = base::list(c = uj::.match_bg("c"),
#'                                      cyn = uj::.match_bg("cyn"),
#'                                      cyan = uj::.match_bg("cyan")),
#'                     def = base::list(d = uj::.match_bg("d"),
#'                                      def = uj::.match_bg("def"),
#'                                      default = uj::.match_bg("default")),
#'                     grn = base::list(g = uj::.match_bg("g"),
#'                                      grn = uj::.match_bg("grn"),
#'                                      green = uj::.match_bg("green")),
#'                     mag = base::list(m = uj::.match_bg("m"),
#'                                      mag = uj::.match_bg("mag"),
#'                                      magenta = uj::.match_bg("magenta")),
#'                     red = base::list(r = uj::.match_bg("r"),
#'                                      red = uj::.match_bg("red")),
#'                     sil = base::list(s = uj::.match_bg("s"),
#'                                      gry = uj::.match_bg("gry"),
#'                                      sil = uj::.match_bg("sil"),
#'                                      slv = uj::.match_bg("slv"),
#'                                      gray = uj::.match_bg("gray"),
#'                                      grey = uj::.match_bg("grey"),
#'                                      silver = uj::.match_bg("silver")),
#'                     wht = base::list(w = uj::.match_bg("w"),
#'                                      wht = uj::.match_bg("wht"),
#'                                      white = uj::.match_bg("white")),
#'                     yel = base::list(y = uj::.match_bg("y"),
#'                                      yel = uj::.match_bg("yel"),
#'                                      ylw = uj::.match_bg("ylw"),
#'                                      yellow = uj::.match_bg("yellow")))
#'   )
#' }
#' egMatchAlias()
#' @export
match_alias <- function() {utils::help("match_alias", package = "uj")}

#' @describeIn match_alias Matches any valid alias for background color.
#' @export
.match_bg <- function(x) {
  if      (x %in% uj::.blu()) {"blu"}
  else if (x %in% uj::.cyn()) {"cyn"}
  else if (x %in% uj::.grn()) {"grn"}
  else if (x %in% uj::.blk()) {"blk"}
  else if (x %in% uj::.mag()) {"mag"}
  else if (x %in% uj::.red()) {"red"}
  else if (x %in% uj::.wht()) {"wht"}
  else if (x %in% uj::.yel()) {"yel"}
  else                        {"def"}
}

#' @describeIn match_alias Matches any valid alias for foreground color.
#' @export
.match_fg <- function(x) {
  if      (x %in% uj::.blu()) {"blu"}
  else if (x %in% uj::.cyn()) {"cyn"}
  else if (x %in% uj::.grn()) {"grn"}
  else if (x %in% uj::.blk()) {"blk"}
  else if (x %in% uj::.mag()) {"mag"}
  else if (x %in% uj::.red()) {"red"}
  else if (x %in% uj::.sil()) {"sil"}
  else if (x %in% uj::.wht()) {"wht"}
  else if (x %in% uj::.yel()) {"yel"}
  else                        {"def"}
}

#' @describeIn match_alias Matches any valid alias for style.
#' @export
.match_st <- function(x) {
  if      (x %in% uj::.bld()) {"bld"}
  else if (x %in% uj::.itl()) {"itl"}
  else if (x %in% uj::.pln()) {"pln"}
  else if (x %in% uj::.und()) {"und"}
  else                        {"def"}
}

# alert ####

#' @encoding UTF-8
#' @title Console-Based User Dialog (may also use package `svDialogs` functions)
#' @description All functions \link[=collapse_dots]{collapses} `...` args into a prompt. Each posts an alert to the console, posts the prompt (if any), followed by a specific action.
#' @details
#' \tabular{lrll}{   \tab `acknowledge` \tab   \tab Waits for user to acknowledge.                    \cr
#'                   \tab               \tab   \tab                                                   \cr
#'                   \tab `OK, CANCEL`  \tab   \tab Offers OK and CANCEL options.\eqn{^{(3)}}         \cr
#'                   \tab               \tab   \tab                                                   \cr
#'                   \tab `choose_doc`  \tab   \tab Asks user to choose a document.\eqn{^{(1)}}       \cr
#'                   \tab `choose_dir`  \tab   \tab Asks user to choose a directory.\eqn{^{(1)}}      \cr
#'                   \tab               \tab   \tab                                                   \cr
#'                   \tab `choose1`     \tab   \tab Asks user to choose `1` option.\eqn{^{(1)}}       \cr
#'                   \tab `chooseN`     \tab   \tab Asks user to choose `1+` options\eqn{^{(1,2)}}    \cr
#'                   \tab               \tab   \tab                                                   \cr
#'                   \tab `YES, NO`     \tab   \tab Offers YES, NO, and CANCEL options.\eqn{^{(1,3)}} \cr
#'                   \tab               \tab   \tab                                                   \cr
#'                   \tab `ask_new`     \tab   \tab Asks for a list of new values.\eqn{^{(1)}}        \cr
#'                   \tab               \tab   \tab                                                   \cr
#'                   \tab `alert`       \tab   \tab No subsequent action.                             \cr
#'                   \tab               \tab   \tab                                                   \cr
#'                   \tab `ask`         \tab   \tab Asks for typed input.\eqn{^{(1)}}                 \cr
#'                   \tab               \tab   \tab                                                     }
#'  \tabular{l}{  \eqn{^{(1)}} Stops execution if user chooses `CANCEL`.                     \cr
#'                \eqn{^{(2)}} Stops execution if `stop | (type = "error" & is.null(stop))`. \cr
#'                \eqn{^{(3)}} Returns `TRUE` if user choice matches the function name.        }
#' The following give templates for what the user sees, where any value derived from arguments will be absent if it is
#' or resolves to a blank string.
#' \cr\cr
#' \strong{\code{alert}}
#' \tabular{lrll}{   \tab **CONDITION**                       \tab   \tab **VALUE**                      \cr
#'                   \tab \code{\link[=g]{g(d, title) != ""}} \tab   \tab formatted title from `title`.  \cr
#'                   \tab \code{\link[=g]{g(d, sub) != ""  }} \tab   \tab formatted subtitle from `sub`. \cr
#'                   \tab \code{\link[=g]{g(d, ...) != ""  }} \tab   \tab formatted message from `...`.  \cr
#'                   \tab                                     \tab   \tab `< blank line >`               \cr
#'                   \tab \code{\link[=g]{g(d, ps) != ""   }} \tab   \tab formatted postscript from `ps`.  }
#' all other templates incorporate a call to `alert(.)` with components as shown below.
#' \cr\cr\strong{\code{acknowledge}}
#' \tabular{lrll}{   \tab **COMPONENT**   \tab   \tab **VALUE**                                \cr
#'                   \tab Title           \tab   \tab `'ACKNOWLEDGMENT REQUIRED'`              \cr
#'                   \tab Subtitle        \tab   \tab `'< g(d, sub) >'`                        \cr
#'                   \tab Message         \tab   \tab `'< g(d, ...) >'`                        \cr
#'                   \tab                 \tab   \tab `< blank line >`                         \cr
#'                   \tab Postscript      \tab   \tab `'press [return] or [enter] to continue:'` }
#' \strong{\code{choose1}}
#' \tabular{lrll}{   \tab **COMPONENT**   \tab   \tab **VALUE**                         \cr
#'                   \tab title           \tab   \tab `'RESPONSE REQUIRED'`             \cr
#'                   \tab Subtitle        \tab   \tab `'< g(d, sub) >'`                 \cr
#'                   \tab Message         \tab   \tab `'< g(d, ...) >'`                 \cr
#'                   \tab                 \tab   \tab `< blank line >`                  \cr
#'                   \tab Postscript      \tab   \tab `'choose an option - Select one'` \cr
#'                   \tab                 \tab   \tab `< blank line >`                  \cr
#'                   \tab Cancel.Option   \tab   \tab `'X: { CANCEL }`'                 \cr
#'                   \tab Option.1        \tab   \tab `'1: < opts[1] >'`                \cr
#'                   \tab Option.2        \tab   \tab `'2: < opts[2] >'`                \cr
#'                   \tab ...             \tab   \tab ...                               \cr
#'                   \tab Option.N        \tab   \tab `'< N >: < opts[n] >'`            \cr
#'                   \tab Prompt          \tab   \tab `'Selection:'`                      }
#' \strong{\code{chooseN}}
#' \tabular{lrll}{   \tab **COMPONENT**   \tab   \tab **VALUE**                                                                      \cr
#'                   \tab Title           \tab   \tab `'RESPONSE REQUIRED'`                                                          \cr
#'                   \tab Subtitle        \tab   \tab `'< g(d, sub) >'`                                                              \cr
#'                   \tab Message         \tab   \tab `'< g(d, ...) >'`                                                              \cr
#'                   \tab                 \tab   \tab `< blank line >`                                                               \cr
#'                   \tab Postscript      \tab   \tab `'choose < n | between n1 and n2 > opts - Select one or more'`                 \cr
#'                   \tab                 \tab   \tab `< blank line >`                                                               \cr
#'                   \tab Cancel.Option   \tab   \tab `'X: { CANCEL }'`                                                              \cr
#'                   \tab Option.1        \tab   \tab `'1: < opts[1] >'`                                                             \cr
#'                   \tab Option.2        \tab   \tab `'2: < opts[2] >'`                                                             \cr
#'                   \tab ...             \tab   \tab ...                                                                            \cr
#'                   \tab Option.N        \tab   \tab `'< N >: < opts[n] >'`                                                         \cr
#'                   \tab ALL.option      \tab   \tab `'A: { ALL }'`                                                                 \cr
#'                   \tab NONE.option     \tab   \tab `'N: { NONE }'`                                                                \cr
#'                   \tab Prompt          \tab   \tab `'Enter one or more numbers separated by spaces and then ENTER, or 0 to cancel'` }
#' \strong{\code{NO}} and \strong{\code{YES}}
#' \tabular{lrll}{   \tab **COMPONENT**   \tab   \tab **VALUE**                         \cr
#'                   \tab Title           \tab   \tab `'RESPONSE REQUIRED'`             \cr
#'                   \tab Subtitle        \tab   \tab '`< g(d, sub) >'`                 \cr
#'                   \tab Message         \tab   \tab `'< g(d, ...) >'`                 \cr
#'                   \tab                 \tab   \tab `< blank line >`                  \cr
#'                   \tab Postscript      \tab   \tab `'choose an option - Select one'` \cr
#'                   \tab                 \tab   \tab `< blank line >`                  \cr
#'                   \tab Cancel.option   \tab   \tab `'1: { CANCEL }'`                 \cr
#'                   \tab Yes.option      \tab   \tab `'2: { YES }'`                    \cr
#'                   \tab No.option       \tab   \tab `'3: { NO }'`                     \cr
#'                   \tab Prompt          \tab   \tab `'Selection:'`                      }
#' \strong{\code{OK}} and \strong{\code{CANCEL}}
#' \tabular{lrll}{   \tab **COMPONENT**   \tab   \tab **VALUE**                         \cr
#'                   \tab Title           \tab   \tab `'RESPONSE REQUIRED'`             \cr
#'                   \tab Subtitle        \tab   \tab `'< g(d, sub) >'`                 \cr
#'                   \tab Message         \tab   \tab `'< g(d, ...) >'`                 \cr
#'                   \tab                 \tab   \tab `< blank line >`                  \cr
#'                   \tab Postscript      \tab   \tab `'choose an option - Select one'` \cr
#'                   \tab                 \tab   \tab `< blank line >`                  \cr
#'                   \tab Cancel.option   \tab   \tab `'1: { CANCEL }'`                 \cr
#'                   \tab OK.option       \tab   \tab `'2: { OK }'`                     \cr
#'                   \tab Prompt          \tab   \tab `'Selection: '`                     }
#' \strong{\code{ask}}
#' \tabular{lrll}{   \tab **COMPONENT**   \tab   \tab **VALUE**               \cr
#'                   \tab Title           \tab   \tab `'RESPONSE REQUIRED'`   \cr
#'                   \tab Subtitle        \tab   \tab `'< g(d, sub) >'`       \cr
#'                   \tab Message         \tab   \tab `'< g(d, ...) >'`       \cr
#'                   \tab                 \tab   \tab `< blank line >`        \cr
#'                   \tab Postscript      \tab   \tab `'enter your response: '` }
#' \strong{\code{ask_new}}
#' \tabular{lrll}{   \tab **COMPONENT**   \tab   \tab **VALUE**                                                       \cr
#'                   \tab Title           \tab   \tab `'RESPONSE REQUIRED'`                                           \cr
#'                   \tab Subtitle        \tab   \tab `'< g(d, sub) >'`                                               \cr
#'                   \tab Message         \tab   \tab `'Enter a pipe-separated list of < N(opts) > replacement values`\cr
#'                   \tab                 \tab   \tab `for the following pipe-separated original values: '`           \cr
#'                   \tab                 \tab   \tab `< blank line >`                                                \cr
#'                   \tab Old.Values      \tab   \tab `'< paste0(old, collapse = " | ") >'`                           \cr
#'                   \tab                 \tab   \tab `< blank line >`                                                \cr
#'                   \tab Postscript      \tab   \tab `'enter your response: '`                                       }
#' \strong{\code{choose_dir}}
#' \tabular{lrll}{   \tab **COMPONENT**   \tab   \tab **VALUE**                                        \cr
#'                   \tab Title           \tab   \tab `'ACKNOWLEDGMENT REQUIRED'`                      \cr
#'                   \tab Subtitle        \tab   \tab `'< g(d, sub) >'`                                \cr
#'                   \tab Message         \tab   \tab `'In the next dialog box, select a < dir.type >.'` }
#' \strong{\code{choose_doc}}
#' \tabular{lrll}{   \tab **COMPONENT**   \tab   \tab **VALUE**                                        \cr
#'                   \tab Title           \tab   \tab `'ACKNOWLEDGMENT REQUIRED'`                      \cr
#'                   \tab Subtitle        \tab   \tab `'< g(d, sub) >'`                                \cr
#'                   \tab Message         \tab   \tab `'In the next dialog box, select a < doc.type >.'` }
#' @section Specifying formats: When formatting arguments (`ft`, `fs`, `fm`, and `fp`) take the special value `""`, the corresponding alert elements (`title`, `sub`, `...`, and `ps`, respectively) are posted to the console without special formatting.
#' \cr\cr Otherwise, formatting arguments must be \link[uj::cmp_str_vec]{complete string vecs} that when \link[=av]{atomized} and \link[=ssplit]{split along pipes} results in a three-element character vector, the first element of which is used to specify \link[=bg]{text background color} and must be a value from \code{\link{bg_vals}()}, the second element of which is used to specify \link[=fg]{text foreground color} and must be a value from \code{\link{fg_vals}()}, and the last of which specifies \link[=st]{text style} and must be a value from \code{\link{st_vals}()}.
#' @param ... An arbitrary, optional number of arguments which are \link[=av]{atomized} into a character scalar message to be posted to the console.
#' @param old A \link[=chr_vec]{character vec} of unique values to be replaced.
#' @param type A character scalar describing the type of replacement values to be entered.
#' @param opts An atomic vector listing opts to choose from.
#' @param n An optional \link[=cmp_psw_scl]{complete positive numeric whole-number scalar} (?cmp_psw_scl) indicating the number of opts that must be selected. Must be contained in `1:length(opts)`.
#' @param all Scalar `TRUE` or `FALSE` indicating whether to add an `{ ALL }` value to `opts`.
#' @param title A title for the alert (`type = 'error'` results in stopping execution after posting the alert). Should be short to avoid formatting problems.
#' @param sub A character scalar alert subtitle to post to the console on the line following the title. Should be short to avoid formatting problems.
#' @param ps A character scalar suffix to post to the console on the line following the alert message contained in `...`.
#' @param none Scalar `TRUE` or `FALSE` indicating whether to add a `{ NONE }` value to `opts` (implying that it is valid to select none).
#' @param ft,fs,fm,fp Formatting values consistent with the description in the *specifying formats* section giving formatting instructions for, respectively, the title (in `title`), subtitle (in `sub`), message (in `...`), and postscript (in `ps`).
#' @param min An optional \link[=CMP]{complete} \link[=PSW]{positive numeric whole-number} \link[=SCL]{scalar} indicating the minimum number of opts that may be selected. Must be `NULL` when `n` is non-`NULL`.
#' @param max An optional complete positive numeric whole-number scalar indicating the maximum number of opts that may be selected. Must be `NULL` when `n` is non-`NULL`.
#' @param def A character scalar containing a default message if \link[=av]{atomizing} and collapsing `...` to a character scalar results in a blank string (`""`).
#' @param u Scalar `TRUE` or `FALSE` indicating whether replacement values must be unique.
#' @param d A character scalar delimiter for collapsing `...` args into a character scalar.
#' @param clear A non-`NA` logical scalar indicating whether to clear the console before each interaction with the user.
#' @return **The** `NULL` **object** \cr\cr `acknowledge, alert`
#' \cr\cr  **A character scalar**    \cr\cr `choose_dir` (a directory path) \cr `choose_doc` (a document path) \cr `ask`
#' \cr\cr  **A character vector**    \cr\cr `ask_new`
#' \cr\cr  **An atomic vector**      \cr\cr `chooseN`
#' \cr\cr  **An atomic scalar**      \cr\cr `choose1`
#' \cr\cr  **A logical scalar**      \cr\cr `CANCEL, YES, NO, OK`
#' @examples
#' \dontrun{
#'   egAlert <- function() {
#'     partA <- "two-part"
#'     partB <- "message"
#'     fmtT  <- base::c("yellow", "red", "plain")
#'     fmtS  <- base::c("blk|wht", "und")
#'     fmtM  <- "b|y|i"
#'     title <- "Title"
#'     sub   <- "Subtitle"
#'     opts  <- base::paste("option", letters[1:10])
#'     mssg1 <- "Do you want to continue?"
#'     mssg2 <- "Why do you want to continue?"
#'     uj::alert(partA, partB, d = " ")
#'     uj::alert(partA, partB, title = title, d = " ")
#'     uj::alert(partA, partB, sub = sub, d = " ")
#'     uj::alert(partA, partB, title = title, sub = sub, d = " ")
#'     uj::alert(partA, partB, title = title, sub = sub, d = " ", fm = fmtM)
#'     uj::alert(partA, partB, title = title, sub = sub, d = " ", fs = fmtS)
#'     uj::alert(partA, partB, title = title, sub = sub, d = " ", ft = fmtT)
#'     uj::alert(partA, partB, title = title, sub = sub, d = " ", ft = fmtT, fs = fmtS, fm = fmtM)
#'     uj::alert(title = title, sub = sub, ft = fmtT, fs = fmtS)
#'     uj::alert(title = title, sub = sub)
#'     uj::alert(sub = sub, fs = fmtS)
#'     uj::acknowledge(partA, partB)
#'     ch1 <- uj::choose1(opts)
#'     ch2 <- uj::chooseN(opts)
#'     ch3 <- uj::chooseN(opts, all = T, none = F, min = 6, max = 10)
#'     no  <- uj::NO(mssg1)
#'     ok  <- uj::OK(mssg2)
#'     yes <- uj::YES(mssg1)
#'     can <- uj::CANCEL(mssg1)
#'     ASK <- uj::ask(mssg2)
#'     NEW <- uj::ask_new(opts)
#'     dir <- uj::choose_dir(dir.type = "directory for R scripts")
#'     doc <- uj::choose_doc(doc.type = "document to read")
#'     base::list(ch1 = ch1, ch2 = ch2, ch3 = ch3, no  = no , ok  = ok, yes = yes,
#'                can = can, ASK = ASK, NEW = NEW, dir = dir, doc = doc)
#'   }
#'   egAlert()
#' }
#' @export
alert <- function(..., title = "alert", sub = "", ps = "", def = "", ft = "r|w|b", fs = "k|y|p", fm = "", fp = "k|y|i", d = " ", clear = FALSE) {
  fmt <- function(x, f) {
    if (f == "") {return(x)}
    f <- uj::av(base::strsplit(f, "|", fixed = TRUE))
    uj::txt(uj::p0(" ", x, " "), bg = f[1], fg = f[2], st = f[3])
  }
  msg   <- uj::p0(base::as.character(uj::av(...)), collapse = d)
  title <- uj::p0(base::toupper(base::as.character(uj::av(title))), collapse = d)
  sub   <- uj::p0(base::as.character(uj::av(sub)), collapse = d)
  ps    <- uj::p0(base::as.character(uj::av(ps)), collapse = d)
  def   <- uj::p0(base::as.character(uj::av(def)), collapse = d)
  ft    <- uj::f0(!uj::.cmp_chr(ft), "r|w|b", uj::p0(ft, collapse = "|"))
  fs    <- uj::f0(!uj::.cmp_chr(fs), "k|y|i", uj::p0(fs, collapse = "|"))
  fp    <- uj::f0(!uj::.cmp_chr(fp), "k|y|i", uj::p0(fp, collapse = "|"))
  fm    <- uj::f0(!uj::.cmp_chr(fm), "k|w|p", uj::p0(fm, collapse = "|"))
  if (msg == "") {msg <- def}
  errs <- NULL
  if (!uj::.ok_fmt(ft)    ) {errs <- base::c(errs, "[ft] must be consistent with the description the [specifying formats] section of the [alert] topic of package [uj] (?alert).")}
  if (!uj::.ok_fmt(fs)    ) {errs <- base::c(errs, "[fs] must be consistent with the description the [specifying formats] section of the [alert] topic of package [uj] (?alert).")}
  if (!uj::.ok_fmt(fm)    ) {errs <- base::c(errs, "[fm] must be consistent with the description the [specifying formats] section of the [alert] topic of package [uj] (?alert).")}
  if (!uj::.ok_fmt(fp)    ) {errs <- base::c(errs, "[fp] must be consistent with the description the [specifying formats] section of the [alert] topic of package [uj] (?alert).")}
  if (!uj::.cmp_chr_scl(d)) {errs <- base::c(errs, "[d] must be a complete character scalar (?cmp_chr_scl).")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  err <- title == "ERROR"
  if (clear) {
    base::gc(verbose = FALSE)
    base::cat("\014")
  }
  if (title != "") {base::cat("\n", fmt(title, ft), "\n", sep = "")}
  if (sub   != "") {base::cat("\n", fmt(sub  , fs), "\n", sep = "")}
  if (msg   != "") {base::cat("\n", fmt(msg  , fm), "\n", sep = "")}
  if (ps    != "") {base::cat("\n", fmt(ps   , fp), "\n", sep = "")} else {cat("\n")}
  if (err        ) {uj::stopperr("")}
}

#' @rdname alert
#' @export
acknowledge <- function(..., sub = "", ps = "", ft = "r|w|b", fs = "k|y|p", fp = "k|y|i", d = " ", clear = FALSE) {
  uj::alert(..., title = "acknowledgment required", sub = sub, ps = "press [return] or [enter] to continue", ft = ft, fs = fs, fp = fp, d = d, clear = clear)
  base::readline()
  a <- NULL
}

#' @rdname alert
#' @export
choose1 <- function(opts, ..., ft = "r|w|b", fs = "k|y|p", d = " ", clear = FALSE) {
  if (!uj::.unq_atm_vec(opts)) {uj::stopperr("[opts] must be a unique atomic vec (?unq_atm_vec).")}
  msg <- uj::p0(uj::av(...), collapse = d)
  uj::.choose_from(opts, msg, F, F, 1, 1, ft, fs, "choose1", uj::callers(), clear)
}

#' @rdname alert
#' @export
chooseN <- function(opts, ..., n = NULL, min = NULL, max = NULL, all = base::is.null(c(n, max)), none = FALSE, ft = "r|w|b", fs = "k|y|p", d = " ", clear = FALSE) {
  errs <- NULL
  if (!uj::.unq_atm_mvc(opts)                              ) {errs <- base::c(errs, "[opts] must be a unique atomic multivec (?unq_atm_mvc).")}
  if (!uj::.cmp_lgl_scl(all)                               ) {errs <- base::c(errs, "[all] must be TRUE or FALSE.")}
  if (!uj::.cmp_lgl_scl(none)                              ) {errs <- base::c(errs, "[none] must be TRUE or FALSE.")}
  if (!uj::f0(base::is.null(n  ), T, uj::.cmp_psw_scl(n  ))) {errs <- base::c(errs, "[n] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!uj::f0(base::is.null(min), T, uj::.cmp_psw_scl(min))) {errs <- base::c(errs, "[min] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!uj::f0(base::is.null(max), T, uj::.cmp_psw_scl(max))) {errs <- base::c(errs, "[max] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  defMin   <- !base::is.null(min)
  defMax   <- !base::is.null(max)
  defN     <- !base::is.null(n)
  msg      <- uj::p0(uj::av(...), collapse = d)
  nOpts    <- base::length(opts)
  okLo     <- uj::f0(!defN  , T, n   >= 1    )
  okHi     <- uj::f0(!defN  , T, n   <= nOpts)
  okMin    <- uj::f0(!defMin, T, min <= nOpts)
  okMax    <- uj::f0(!defMax, T, max <= nOpts)
  okCombo  <- (!defMin & !defMax) | !defN
  okMinMax <- uj::f0(!defMin | !defMax, T, min <= max)
  if (!okLo    ) {errs <- base::c(errs, "[n] is out of bounds [min(n) < 1].")}
  if (!okHi    ) {errs <- base::c(errs, "[n] is out of bounds [max(n) > length(opts)].")}
  if (!okMin   ) {errs <- base::c(errs, "[min] is out of bounds [min > length(opts)].")}
  if (!okMax   ) {errs <- base::c(errs, "[max] is out of bounds [max > length(opts)].")}
  if (!okCombo ) {errs <- base::c(errs, "When [n] is supplied, [min] and [max] must be NULL.")}
  if (!okMinMax) {errs <- base::c(errs, "[min] and [max] are inconsistent [min > max].")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  min <- uj::f0(defN, n, uj::f0(defMin, min, uj::f0(none, 0, 1)))
  max <- uj::f0(defN, n, uj::f0(defMax, max, nOpts))
  uj::.choose_from(opts, msg, all, none, min, max, ft, fs, "chooseN", uj::callers(), clear)
}

#' @rdname alert
#' @export
NO <- function(..., ft = "r|w|b", fs = "k|y|p", d = " ", clear = FALSE) {
  msg <- uj::p0(uj::av(...), collapse = d)
  uj::.choose_from(base::c("yes", "no"), msg, F, F, 1, 1, ft, fs, "NO", uj::callers(), clear) == "no"
}

#' @rdname alert
#' @export
YES <- function(..., ft = "r|w|b", fs = "k|y|p", d = " ", clear = FALSE) {
  msg <- uj::p0(uj::av(...), collapse = d)
  uj::.choose_from(base::c("yes", "no"), msg, F, F, 1, 1, ft, fs, "YES", uj::callers(), clear) == "yes"
}

#' @rdname alert
#' @export
OK <- function(..., ft = "r|w|b", fs = "k|y|p", d = " ", clear = FALSE) {
  msg <- uj::p0(uj::av(...), collapse = d)
  uj::.choose_from(base::c("ok", "cancel"), msg, F, F, 1, 1, ft, fs, "OK", uj::callers(), clear, can = F) == "ok"
}

#' @rdname alert
#' @export
CANCEL <- function(..., ft = "r|w|b", fs = "k|y|p", d = " ", clear = FALSE) {
  msg <- uj::p0(uj::av(...), collapse = d)
  uj::.choose_from(base::c("ok", "cancel"), msg, F, F, 1, 1, ft, fs, "CANCEL", uj::callers(), clear, can = F) == "cancel"
}

#' @rdname alert
#' @export
ask <- function(..., Default = "", sub = "", ft = "r|w|b", fs = "k|y|p", fm = "", fp = "k|y|i", d = " ", clear = FALSE) {
  msg <- uj::trm(..., d = "")
  msg <- uj::p0(msg, "\n\n(enter '{cancel}' to cancel)")
  uj::alert(msg, title = "response required", sub = sub, ps = "enter your response:", ft = ft, fs = fs, fm = fm, fp = fp, d = d, clear = clear)
  ans <- base::readline()
  can <- uj::f0(base::length == 0, T, ans == "{cancel}")
  if (can) {uj::stopperr("Action canceled by user.")}
  ans
}

#' @rdname alert
#' @export
ask_new <- function(old, type = "replacement values", u = TRUE, sub = "", ft = "r|w|b", fs = "k|y|p", fm = "", fp = "k|y|i", d = "|", clear = FALSE) {
  errs <- NULL
  if (!uj::.unq_atm_vec(old )) {errs <- base::c(errs, "[old] must be a unique atomic vec (?unq_atm_vec).")}
  if (!uj::.cmp_str_scl(type)) {errs <- base::c(errs, "[type] must be a complete string scalar (?cmp_str_scl).")}
  if (!uj::.cmp_lgl_scl(u   )) {errs <- base::c(errs, "[u] must be TRUE or FALSE.")}
  if (!uj::.cmp_ch1_scl(d   )) {errs <- base::c(errs, "[d] must be a single character.")}
  if (!base::is.null(errs   )) {uj::stopperr(errs)}
  D <- uj::f0(d == "|", "pipe (|)",
       uj::f0(d == ".", "dot (.)",
       uj::f0(d == ":", "colon (:)",
       uj::f0(d == ";", "semicolon (;)",
       uj::f0(d == "~", "tilde (~)",
       uj::f0(d == "^", "caret (^)",
       uj::f0(d == " ", "space",
       uj::f0(d == "`", "backtick (`)", uj::p0("'", d, "'")))))))))
  lst <- uj::p0(old, collapse = uj::p0(" ", d, " "))
  qst <- uj::p0("Enter a %s separated list of %d %s for the following %s separated original values:\n\n  %s\n\n(enter '{cancel}' to cancel)\n")
  qst <- base::sprintf(qst, D, base::length(old), type, D, lst)
  uj::alert(qst, title = "response required", ps = "ENTER YOUR RESPONSE", sub = "", ft = ft, fs = fs, fm = fm, fp = fp, d = "", clear = clear)
  ans  <- base::readline()
  ans  <- uj::av(base::strsplit(ans, d, fixed = T))
  ans  <- base::trimws(ans, which = "both")
  ans  <- ans[ans != ""]
  nAns <- uj::N(ans)
  can  <- uj::f0(nAns == 0, T, uj::f0(nAns > 1, F, ans == "{cancel}"))
  if (can                  ) {uj::stopperr("Action cancelled by user.")}
  if (nAns != uj::N(old)   ) {uj::stopperr("Numbers of old and new values do not match.")}
  if (u & !uj::unq_vec(ans)) {uj::stopperr("Replacement values are not unique.")}
  ans
}

#' @rdname alert
#' @export
choose_dir <- function(dir.type = "directory", sub = "", ft = "r|w|b", fs = "k|y|p", fm = "", fp = "k|y|i", d = " ", clear = FALSE) {
  if (!uj::.cmp_chr_scl(dir.type)) {uj::stopperr("[dir.type] must be a complete character scalar (?cmp_chr_scl).")}
  uj::acknowledge(uj::p0("In the next dialog box, select a ", dir.type, "."), sub = sub, ft = ft, fs = fs, fm = fm, fp = fp, d = d, clear = clear)
  path <- svDialogs::dlg_dir(title = uj::p0("Select a ", dir.type, ":"))$res
  if (base::length(path) == 0) {uj::stopperr("Action canceled by user.")}
  path
}

#' @rdname alert
#' @export
choose_doc <- function(doc.type = "document", sub = "", ft = "r|w|b", fs = "", fm = "", fp = "k|y|i", d = " ", clear = FALSE) {
  if (!uj::.cmp_chr_scl(doc.type)) {uj::stopperr("[doc.type] must be a complete character scalar (?cmp_chr_scl).")}
  uj::acknowledge(uj::p0("In the next dialog box, select a ", doc.type, "."), sub = sub, ft = ft, fs = fs, fm = fm, fp = fp, d = d, clear = clear)
  path <- svDialogs::dlg_open(title = uj::p0("Select a ", doc.type, ":"))$res
  if (base::length(path) == 0) {uj::stopperr("Action canceled by user.")}
  path
}

# crayons ####

#' @title Wrappers for Package \code{\link[crayon]{crayon}} Functions to Style and Color Console Text
#' @description Functions in this family apply styles and colors to text for display on the console, where the text to display is identified by \link[=g]{collapsing} `...` args into a character scalar.
#' @section Background color functions:
#' \tabular{ll}{  `bg_vals`   \tab Get all valid values of arg `bg`.                                   \cr
#'                `bg_red`    \tab Set to red (\code{\link[crayon:bgRed]{crayon::bgRed}}).              \cr
#'                `bg_blu`    \tab Set to blue (\code{\link[crayon:bgBlue]{crayon::bgBlue}}).           \cr
#'                `bg_cyn`    \tab Set to cyan (\code{\link[crayon:bgCyan]{crayon::bgCyan}}).           \cr
#'                `bg_blk`    \tab Set to black (\code{\link[crayon:bgBlack]{crayon::bgBlack}}).        \cr
#'                `bg_grn`    \tab Set to green (\code{\link[crayon:bgGreen]{crayon::bgGreen}}).        \cr
#'                `bg_wht`    \tab Set to white (\code{\link[crayon:bgWhite]{crayon::bgWhite}}).        \cr
#'                `bg_yel`    \tab Set to yellow (\code{\link[crayon:bgYellow]{crayon::bgYellow}}).     \cr
#'                `bg_mag`    \tab Set to magenta (\code{\link[crayon:bgMagenta]{crayon::bgMagenta}}).  \cr
#'                `bg_def`    \tab Set to system default.                                               \cr
#'                `bg`        \tab Set to value of arg `bg`.                                              }
#' Valid values of `bg` are the following:
#' \tabular{ll}{  magenta   \tab `'m'`, `'mag'`, `'magenta'`        \cr
#'                default   \tab `'d'`, `'def'`, `'default'`        \cr
#'                yellow    \tab `'y'`, `'yel'`,`'ylw'`, `'yellow'` \cr
#'                black     \tab `'k'`, `'blk'`, `'black'`          \cr
#'                white     \tab `'w'`, `'wht'`, `'white'`          \cr
#'                green     \tab `'g'`, `'grn'`, `'green'`          \cr
#'                blue      \tab `'b'`, `'blu'`, `'blue'`           \cr
#'                cyan      \tab `'c'`, `'cyn'`, `'cyan'`           \cr
#'                red       \tab `'r'`, `'red'`                       }
#' NOTE: Silver (grey) is *not* valid for *background*.
#' @section Foreground color functions:
#' \tabular{ll}{  `fg_vals`   \tab Get all valid values of `fg` (foreground text color).              \cr
#'                `fg_red`    \tab Set to red (\code{\link[crayon:red]{crayon::red}}).                 \cr
#'                `fg_blu`    \tab Set to red (\code{\link[crayon:blue]{crayon::blue}}).               \cr
#'                `fg_cyn`    \tab Set to red (\code{\link[crayon:cyan]{crayon::cyan}}).               \cr
#'                `fg_blk`    \tab Set to red (\code{\link[crayon:black]{crayon::black}}).             \cr
#'                `fg_grn`    \tab Set to red (\code{\link[crayon:green]{crayon::green}}).             \cr
#'                `fg_wht`    \tab Set to red (\code{\link[crayon:white]{crayon::white}}).             \cr
#'                `fg_yel`    \tab Set to red (\code{\link[crayon:yellow]{crayon::yellow}}).           \cr
#'                `fg_mag`    \tab Set to red (\code{\link[crayon:magenta]{crayon::magenta}}).         \cr
#'                `fg_gry`    \tab Set to silver (grey) (\code{\link[crayon:silver]{crayon::silver}}). \cr
#'                `fg_sil`    \tab Set to silver (grey).                                               \cr
#'                `fg_slv`    \tab Set to silver (grey).                                               \cr
#'                `fg_def`    \tab Set to system default.                                              \cr
#'                `fg`        \tab Set to value of arg `fg`.                                            }
#' Valid values of `fg` are :
#' \tabular{ll}{  silver (grey)   \tab `'s'`, `'sil'`, `'slv'`, `'silver'`, `'gry'`, `'gray'`, `'grey'` \cr
#'                magenta         \tab `'m'`, `'mag'`, `'magenta'`                                      \cr
#'                default         \tab `'d'`, `'def'`, `'default'`                                     \cr
#'                yellow          \tab `'y'`, `'yel'`, `'ylw'`, `'yellow'`                              \cr
#'                black           \tab `'k'`, `'blk'`, `'black'`                                        \cr
#'                white           \tab `'w'`, `'wht'`, `'white'`                                        \cr
#'                green           \tab `'g'`, `'grn'`, `'green'`                                        \cr
#'                blue            \tab `'b'`, `'blu'`, `'blue'`                                         \cr
#'                cyan            \tab `'c'`, `'cyn'`, `'cyan'`                                         \cr
#'                red             \tab `'r'`, `'red'`                                                     }
#' NOTE: Silver (grey) is *only* valid for *foreground*.
#' @section Style functions:
#' \tabular{ll}{  `st_vals`   \tab Get all valid values of arg `st`.                              \cr
#'                `st_bld`    \tab Set to bold (\code{\link[crayon:bgRed]{crayon::bold}}).         \cr
#'                `st_pln`    \tab Set to plain (\code{\link[crayon:bgBlue]{crayon::reset}}).      \cr
#'                `st_itl`    \tab Set to italic (\code{\link[crayon:bgBlue]{crayon::italic}}).    \cr
#'                `st_und`    \tab Set to underline (\code{\link[crayon:bgBlue]{crayon::italic}}). \cr
#'                `st_def`    \tab Set to system default.                                          \cr
#'                `st`        \tab Set to value of arg `st`.                                        }
#' Valid values of `st` are:
#' \tabular{ll}{  underline   \tab `'u', 'un', 'und', 'under', 'underline',`  \cr
#'                            \tab `'underlined'`                             \cr
#'                default     \tab `'d', 'def', 'default'`                    \cr
#'                italic      \tab `'i', 'it', 'itl', 'ital', 'italic',`      \cr
#'                            \tab `'italics', 'italicized', 'e', 'em',`      \cr
#'                            \tab `'emp', 'emph', 'emphasis', 'emphasized'`  \cr
#'                plain       \tab `'p', 'pl', 'pln', 'plain',  'r', 're',`   \cr
#'                            \tab `'res', 'reset'`                           \cr
#'                bold        \tab `'b', 'bo', 'bld', 'bold', 'bolded', 's',` \cr
#'                            \tab `'st', 'str', 'strong'`                      }
#' @section Generic function:
#' \tabular{ll}{  `txt`   \tab Styles text using args `st`, `bg`, `fg`.}
#' @param ... An arbitrary number of objects to be \link[=g]{collapsed} to a character scalar to be styled.
#' @param st A quoted or unquoted, case-insensitive, character scalar text style from `st_vals()`. May also be `NULL` for function `st_vals`.
#' @param bg A quoted or unquoted, case-insensitive, character scalar text background color from `bg_vals()`. May also be `NULL` for function `bg_vals`.
#' @param fg A quoted or unquoted, case-insensitive, character scalar text foreground color from `fg_vals()`. May also be `NULL` for function `fg_vals`.
#' @param d A non-`NA` character scalar delimiter for \link[=g]{collapsing} `...` args into a character scalar.
#' @return **A character vector** \cr\cr `st_vals, fg_vals, bg_vals`
#' \cr\cr  **A character scalar** \cr\cr All others
#' @examples
#'
#' bg_vals()
#' bg_vals("k")
#' bg_vals("mag")
#' bg_vals("yellow")
#'
#' fg_vals()
#' fg_vals("s")
#' fg_vals("blu")
#' fg_vals("white")
#'
#' st_vals()
#' st_vals("b")
#' st_vals("itl")
#' st_vals("plain")
#' st_vals("underline")
#'
#' \dontrun{
#'   ## because formatting doesn't show up in help viewer examples output
#'
#'   egGrayons <- function() {
#'     egArg1 <- "Some "
#'     egArg2 <- c("text", " to display", "\nP.S. ")
#'     cat("\n", st_bld(egArg1, egArg2, "note"))
#'     cat("\n", st_def(egArg1, egArg2, "note"))
#'     cat("\n", st_itl(egArg1, egArg2, "note"))
#'     cat("\n", st_pln(egArg1, egArg2, "note"))
#'     cat("\n", st_und(egArg1, egArg2, "note"))
#'     cat("\n", st("u"        , egArg1, egArg2, "note"))
#'     cat("\n", st("itl"      , egArg1, egArg2, "note"))
#'     cat("\n", st("under"    , egArg1, egArg2, "note"))
#'     cat("\n", st_itl(st_und(egArg1, egArg2, "note")))
#'     cat("\n", st("underline", st("itl", egArg1, egArg2, "note")))
#'     cat("\n", fg_blk(egArg1, egArg2))
#'     cat("\n", fg_blu(egArg1, egArg2))
#'     cat("\n", fg_cyn(egArg1, egArg2))
#'     cat("\n", fg_def(egArg1, egArg2))
#'     cat("\n", fg_grn(egArg1, egArg2))
#'     cat("\n", fg_gry(egArg1, egArg2))
#'     cat("\n", fg_sil(egArg1, egArg2))
#'     cat("\n", fg_slv(egArg1, egArg2))
#'     cat("\n", fg_mag(egArg1, egArg2))
#'     cat("\n", fg_red(egArg1, egArg2))
#'     cat("\n", fg_wht(egArg1, egArg2))
#'     cat("\n", fg_yel(egArg1, egArg2))
#'     cat("\n", fg_def(egArg1, egArg2))
#'     cat("\n", fg("s", egArg1, egArg2, "note"))
#'     cat("\n", fg("sil", egArg1, egArg2, "note"))
#'     cat("\n", fg("grey", egArg1, egArg2, "note"))
#'     cat("\n", bg_blk(egArg1, egArg2, "note"))
#'     cat("\n", bg_blu(egArg1, egArg2, "note"))
#'     cat("\n", bg_cyn(egArg1, egArg2, "note"))
#'     cat("\n", bg_def(egArg1, egArg2, "note"))
#'     cat("\n", bg_grn(egArg1, egArg2, "note"))
#'     cat("\n", bg_mag(egArg1, egArg2, "note"))
#'     cat("\n", bg_red(egArg1, egArg2, "note"))
#'     cat("\n", bg_wht(egArg1, egArg2, "note"))
#'     cat("\n", bg_yel(egArg1, egArg2, "note"))
#'     cat("\n", bg_def(egArg1, egArg2, "note"))
#'     cat("\n", bg("y"     , egArg1, egArg2, "note"))
#'     cat("\n", bg("blu"   , egArg1, egArg2, "note"))
#'     cat("\n", bg("yellow", egArg1, egArg2, "note"))
#'     cat("\n", bg("y", fg("r", st("u", st("i", egArg1, egArg2, "note")))))
#'     cat("\n", txt(egArg1, egArg2, "note", bg = "blue", fg = "w", st = c("i", "under")))
#'   }
#'   egCrayons()
#' }
#' @export
crayons <- function() {utils::help("crayons", package = "dlg")}

#' @rdname crayons
#' @export
st_vals <- function(st = NULL) {
  uj::.fmt_errs(st = st)
  if (!base::is.null(st)) {
    st <- uj::.match_st(st)
    if      (st == "bld") {uj::.bld()}
    else if (st == "itl") {uj::.itl()}
    else if (st == "def") {uj::.def()}
    else if (st == "und") {uj::.und()}
    else if (st == "pln") {uj::.pln()}
    else                  {uj::.res()}
  } else                  {uj::.sts()}
}

#' @rdname crayons
#' @export
bg_vals <- function(bg = NULL) {
  uj::.fmt_errs(bg = bg)
  if (!base::is.null(bg)) {
    bg <- uj::.match_bg(bg)
    if      (bg == "red") {uj::.red()}
    else if (bg == "blu") {uj::.blu()}
    else if (bg == "cyn") {uj::.cyn()}
    else if (bg == "blk") {uj::.blk()}
    else if (bg == "grn") {uj::.grn()}
    else if (bg == "wht") {uj::.wht()}
    else if (bg == "mag") {uj::.mag()}
    else if (bg == "yel") {uj::.yel()}
    else                  {uj::.def()}
  } else                  {uj::.bgs()}
}

#' @rdname crayons
#' @export
fg_vals <- function(fg = NULL) {
  uj::.fmt_errs(fg = fg)
  if (!base::is.null(fg)) {
    fg <- uj::.match_fg(fg)
    if      (fg == "red") {uj::.red()}
    else if (fg == "blu") {uj::.blu()}
    else if (fg == "cyn") {uj::.cyn()}
    else if (fg == "blk") {uj::.blk()}
    else if (fg == "grn") {uj::.grn()}
    else if (fg == "wht") {uj::.wht()}
    else if (fg == "sil") {uj::.sil()}
    else if (fg == "mag") {uj::.mag()}
    else if (fg == "yel") {uj::.yel()}
    else                  {uj::.def()}
  } else                  {uj::.fgs()}
}

# style ####

#' @rdname crayons
#' @export
st_bld <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::bold(uj::g(d, ...))}

#' @rdname crayons
#' @export
st_itl <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::italic(uj::g(d, ...))}

#' @rdname crayons
#' @export
st_und <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::underline(uj::g(d, ...))}

#' @rdname crayons
#' @export
st_res <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::reset(uj::g(d, ...))}

#' @rdname crayons
#' @export
st_def <- st_res

#' @rdname crayons
#' @export
st_pln <- st_res

#' @rdname crayons
#' @export
st <- function(st, ..., d = " ") {
  uj::.fmt_errs(st = st, d = d, nullst = F)
  st <- base::tolower(st)
  x  <- uj::g(d, ...)
  if (base::any(st %in% uj::.bld())) {x <- uj::st_bld(x)}
  if (base::any(st %in% uj::.itl())) {x <- uj::st_itl(x)}
  if (base::any(st %in% uj::.und())) {x <- uj::st_und(x)}
  if      (base::any(st %in% uj::.pln())) {x <- uj::st_res(x)}
  else if (base::any(st %in% uj::.def())) {x <- uj::st_res(x)}
  else if (base::any(st %in% uj::.res())) {x <- uj::st_res(x)}
  x
}

# fg color ####

#' @rdname crayons
#' @export
fg_blk <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::black(uj::g(d, ...))}

#' @rdname crayons
#' @export
fg_blu <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::blue(uj::g(d, ...))}

#' @rdname crayons
#' @export
fg_cyn <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::cyan(uj::g(d, ...))}

#' @rdname crayons
#' @export
fg_def <- function(..., d = " ") {uj::.fmt_errs(d = d); uj::g(d, ...)}

#' @rdname crayons
#' @export
fg_grn <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::green(uj::g(d, ...))}

#' @rdname crayons
#' @export
fg_gry <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::silver(uj::g(d, ...))}

#' @rdname crayons
#' @export
fg_mag <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::magenta(uj::g(d, ...))}

#' @rdname crayons
#' @export
fg_red <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::red(uj::g(d, ...))}

#' @rdname crayons
#' @export
fg_sil <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::silver(uj::g(d, ...))}

#' @rdname crayons
#' @export
fg_slv <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::silver(uj::g(d, ...))}

#' @rdname crayons
#' @export
fg_wht <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::white(uj::g(d, ...))}

#' @rdname crayons
#' @export
fg_yel <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::yellow(uj::g(d, ...))}

#' @rdname crayons
#' @export
fg <- function(fg, ..., d = " ") {
  uj::.fmt_errs(fg = fg, d = d, nullfg = F)
  fg <- base::tolower(fg)
  x  <- uj::g(d, ...)
  if      (fg %in% uj::.red()) {x <- uj::fg_red(x)}
  else if (fg %in% uj::.blu()) {x <- uj::fg_blu(x)}
  else if (fg %in% uj::.cyn()) {x <- uj::fg_cyn(x)}
  else if (fg %in% uj::.blk()) {x <- uj::fg_blk(x)}
  else if (fg %in% uj::.grn()) {x <- uj::fg_grn(x)}
  else if (fg %in% uj::.wht()) {x <- uj::fg_wht(x)}
  else if (fg %in% uj::.sil()) {x <- uj::fg_sil(x)}
  else if (fg %in% uj::.yel()) {x <- uj::fg_yel(x)}
  else if (fg %in% uj::.mag()) {x <- uj::fg_mag(x)}
  else                         {x <- uj::fg_def(x)}
  x
}

# bg color ####

#' @rdname crayons
#' @export
bg_blk <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::bgBlack(uj::g(d, ...))}

#' @rdname crayons
#' @export
bg_blu <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::bgBlue(uj::g(d, ...))}

#' @rdname crayons
#' @export
bg_cyn <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::bgCyan(uj::g(d, ...))}

#' @rdname crayons
#' @export
bg_def <- function(..., d = " ") {uj::.fmt_errs(d = d); uj::g(d, ...)}

#' @rdname crayons
#' @export
bg_grn <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::bgGreen(uj::g(d, ...))}

#' @rdname crayons
#' @export
bg_mag <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::bgMagenta(uj::g(d, ...))}

#' @rdname crayons
#' @export
bg_red <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::bgRed(uj::g(d, ...))}

#' @rdname crayons
#' @export
bg_wht <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::bgWhite(uj::g(d, ...))}

#' @rdname crayons
#' @export
bg_yel <- function(..., d = " ") {uj::.fmt_errs(d = d); crayon::bgYellow(uj::g(d, ...))}

#' @rdname crayons
#' @export
bg <- function(bg, ..., d = " ") {
  uj::.fmt_errs(bg = bg, d = d, nullbg = F)
  bg <- base::tolower(bg)
  x  <- uj::g(d, ...)
  if      (bg %in% uj::.red()) {x <- uj::bg_red(x)}
  else if (bg %in% uj::.blu()) {x <- uj::bg_blu(x)}
  else if (bg %in% uj::.cyn()) {x <- uj::bg_cyn(x)}
  else if (bg %in% uj::.blk()) {x <- uj::bg_blk(x)}
  else if (bg %in% uj::.grn()) {x <- uj::bg_grn(x)}
  else if (bg %in% uj::.wht()) {x <- uj::bg_wht(x)}
  else if (bg %in% uj::.yel()) {x <- uj::bg_yel(x)}
  else if (bg %in% uj::.mag()) {x <- uj::bg_mag(x)}
  else                         {x <- uj::bg_def(x)}
  x
}

# generic ####

#' @rdname crayons
#' @export
txt <- function(..., bg = NULL, fg = NULL, st = NULL, d = " ") {
  cat("\n bg = ", bg , ", fg = ", fg, " st = ", st)
  uj::.fmt_errs(bg = bg, fg = fg, st = st, d = d, nullbg = T, nullfg = T, nullst = T)
  x <- uj::g(d, ...)
  if (!base::is.null(st)) {x <- uj::st(base::tolower(st), x)}
  if (!base::is.null(bg)) {x <- uj::bg(base::tolower(bg), x)}
  if (!base::is.null(fg)) {x <- uj::fg(base::tolower(fg), x)}
  x
}
