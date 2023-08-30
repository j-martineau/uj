#' @name crayons
#' @title Wrappers for package \code{\link[crayon]{crayon}} functions to styled and color console text
#' @description Functions in this family apply styles and colors to text for display on the console, where the text to display is identified by \link[=glue_dots]{collapsing} `...` args into a character scalar.
#' @section Background color functions:
#' \tabular{ll}{  `bg_vals`   \tab Get all valid values of arg `.BG`.                                    \cr
#'                `bg_red`    \tab Set to red (\code{\link[crayon:bgRed]{crayon::bgRed}}).              \cr
#'                `bg_blu`    \tab Set to blue (\code{\link[crayon:bgBlue]{crayon::bgBlue}}).           \cr
#'                `bg_cyn`    \tab Set to cyan (\code{\link[crayon:bgCyan]{crayon::bgCyan}}).           \cr
#'                `bg_blk`    \tab Set to black (\code{\link[crayon:bgBlack]{crayon::bgBlack}}).        \cr
#'                `bg_grn`    \tab Set to green (\code{\link[crayon:bgGreen]{crayon::bgGreen}}).        \cr
#'                `bg_wht`    \tab Set to white (\code{\link[crayon:bgWhite]{crayon::bgWhite}}).        \cr
#'                `bg_ylw`    \tab Set to yellow (\code{\link[crayon:bgYellow]{crayon::bgYellow}}).     \cr
#'                `bg_mag`    \tab Set to magenta (\code{\link[crayon:bgMagenta]{crayon::bgMagenta}}).  \cr
#'                `bg_def`    \tab Set to system default.                                               \cr
#'                `bg`        \tab Set to value of arg `.BG`.                                              }
#' \cr\cr Valid values of `.BG` are the following:
#' \tabular{ll}{  magenta   \tab `'m'`, `'mag'`, `'magenta'` \cr
#'                default   \tab `'d'`, `'def'`, `'default'` \cr
#'                yellow    \tab `'y'`, `'ylw'`, `'yellow'`  \cr
#'                black     \tab `'k'`, `'blk'`, `'black'`   \cr
#'                white     \tab `'w'`, `'wht'`, `'white'`   \cr
#'                green     \tab `'g'`, `'grn'`, `'green'`   \cr
#'                blue      \tab `'b'`, `'blu'`, `'blue'`    \cr
#'                cyan      \tab `'c'`, `'cyn'`, `'cyan'`    \cr
#'                red       \tab `'r'`, `'red'`                }
#' NOTE: Silver (grey) is *not* valid for *background*.
#' @section Foreground color functions:
#' \tabular{ll}{  `fg_vals`   \tab Get all valid values of `.FG` (foreground text color).              \cr
#'                `fg_red`    \tab Set to red (\code{\link[crayon:red]{crayon::red}}).                 \cr
#'                `fg_blu`    \tab Set to red (\code{\link[crayon:blue]{crayon::blue}}).               \cr
#'                `fg_cyn`    \tab Set to red (\code{\link[crayon:cyan]{crayon::cyan}}).               \cr
#'                `fg_blk`    \tab Set to red (\code{\link[crayon:black]{crayon::black}}).             \cr
#'                `fg_grn`    \tab Set to red (\code{\link[crayon:green]{crayon::green}}).             \cr
#'                `fg_wht`    \tab Set to red (\code{\link[crayon:white]{crayon::white}}).             \cr
#'                `fg_ylw`    \tab Set to red (\code{\link[crayon:yellow]{crayon::yellow}}).           \cr
#'                `fg_mag`    \tab Set to red (\code{\link[crayon:magenta]{crayon::magenta}}).         \cr
#'                `fg_gry`    \tab Set to silver (grey) (\code{\link[crayon:silver]{crayon::silver}}). \cr
#'                `fg_sil`    \tab Set to silver (grey).                                               \cr
#'                `fg_slv`    \tab Set to silver (grey).                                               \cr
#'                `fg_def`    \tab Set to system default.                                              \cr
#'                `fg`        \tab Set to value of arg `.FG`.                                            }
#' \cr\cr Valid values of `.FG` are :
#' \tabular{ll}{  silver (grey)   \tab `'s'`, `'sil'`, `'slv'`, `'silver'`, `'gry'`, `'gray'`, `'grey'` \cr
#'                magenta         \tab `'m'`, `'mag'`, `'magenta'`                                      \cr
#'                default         \tab `'.D'`, `'def'`, `'default'`                                     \cr
#'                yellow          \tab `'y'`, `'ylw'`, `'yellow'`                                       \cr
#'                black           \tab `'k'`, `'blk'`, `'black'`                                        \cr
#'                white           \tab `'w'`, `'wht'`, `'white'`                                        \cr
#'                green           \tab `'g'`, `'grn'`, `'green'`                                        \cr
#'                blue            \tab `'b'`, `'blu'`, `'blue'`                                         \cr
#'                cyan            \tab `'c'`, `'cyn'`, `'cyan'`                                         \cr
#'                red             \tab `'r'`, `'red'`                                                     }
#' NOTE: Silver (grey) is *only* valid for *foreground*.
#' @section Style functions:
#' \tabular{ll}{  `st_vals`   \tab Get all valid values of arg `.ST`.                              \cr
#'                `st_bld`    \tab Set to bold (\code{\link[crayon:bgRed]{crayon::bold}}).         \cr
#'                `st_pln`    \tab Set to plain (\code{\link[crayon:bgBlue]{crayon::reset}}).      \cr
#'                `st_itl`    \tab Set to italic (\code{\link[crayon:bgBlue]{crayon::italic}}).    \cr
#'                `st_und`    \tab Set to underline (\code{\link[crayon:bgBlue]{crayon::italic}}). \cr
#'                `st_def`    \tab Set to system default.                                          \cr
#'                `st`        \tab Set to value of arg `.ST`.                                        }
#' \cr\cr Valid values of `st` are:
#' \tabular{ll}{  underline   \tab `'u'`, `'un'`, `'und'`, `'under'`, `'underline'`, `'underlined'`                                                                         \cr   \tab   \cr
#'                default     \tab `'.D'`, `'def'`, `'default'`                                                                                                             \cr   \tab   \cr
#'                italic      \tab `'i'`, `'it'`, `'itl'`, `'ital'`, `'italic'`, `'italics'`, `'italicized'`,`'e'`, `'em'`, `'emp'`, `'emph'`, `'emphasis'`, `'emphasized'` \cr   \tab   \cr
#'                plain       \tab `'p'`,` 'pl'`, `'pln'`, `'plain'`, `'r'`, `'re'`, `'res`', `'reset'`                                                                     \cr   \tab   \cr
#'                bold        \tab `'b'`, `'bo'`, `'bld'`, `'bold'`, `'bolded'`, `'s'`, `'st'`, `'str'`, `'strong'`                                                                        }
#' @section Generic function:
#' \tabular{ll}{  `txt`   \tab Styles text using args `.ST`, `.BG`, `.FG`.}
#' @param ... An arbitrary number of objects to be \link[=glue_dots]{collapsed} to a character scalar to be styled.
#' @param .ST A quoted or unquoted, case-insensitive, character scalar text style from `st_vals()`. May also be `NULL` for function `st_vals`.
#' @param .BG A quoted or unquoted, case-insensitive, character scalar text background color from `bg_vals()`. May also be `NULL` for function `bg_vals`.
#' @param .FG A quoted or unquoted, case-insensitive, character scalar text foreground color from `fg_vals()`. May also be `NULL` for function `fg_vals`.
#' @param .D A non-`NA` character scalar delimiter for \link[=glue_dots]{collapsing} `...` args into a character scalar.
#' @return **A character vector** \cr\cr `st_vals, fg_vals, bg_vals`
#' \cr\cr  **A character scalar** \cr\cr All others
#' @examples
#' egArg1 <- "Some "
#' egArg2 <- c("text", " to display", "\nP.S. ")
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
#' cat(st_bld(egArg1, egArg2, "note"))
#' cat(st_def(egArg1, egArg2, "note"))
#' cat(st_itl(egArg1, egArg2, "note"))
#' cat(st_pln(egArg1, egArg2, "note"))
#' cat(st_und(egArg1, egArg2, "note"))
#' cat(st_itl(st_und(egArg1, egArg2, "note")))
#'
#' cat(st("u", egArg1, egArg2, "note"))
#' cat(st("itl", egArg1, egArg2, "note"))
#' cat(st("under", egArg1, egArg2, "note"))
#' cat(st("underline", st("itl", egArg1, egArg2, "note")))
#'
#' cat(fg_blk(egArg1, egArg2))
#' cat(fg_blu(egArg1, egArg2))
#' cat(fg_cyn(egArg1, egArg2))
#' cat(fg_def(egArg1, egArg2))
#' cat(fg_grn(egArg1, egArg2))
#' cat(fg_gry(egArg1, egArg2))
#' cat(fg_sil(egArg1, egArg2))
#' cat(fg_slv(egArg1, egArg2))
#' cat(fg_mag(egArg1, egArg2))
#' cat(fg_red(egArg1, egArg2))
#' cat(fg_wht(egArg1, egArg2))
#' cat(fg_ylw(egArg1, egArg2))
#' cat(fg_def(egArg1, egArg2))
#'
#' cat(fg("s", egArg1, egArg2, "note"))
#' cat(fg("sil", egArg1, egArg2, "note"))
#' cat(fg("grey", egArg1, egArg2, "note"))
#'
#' cat(bg_blk(egArg1, egArg2, "note"))
#' cat(bg_blu(egArg1, egArg2, "note"))
#' cat(bg_cyn(egArg1, egArg2, "note"))
#' cat(bg_def(egArg1, egArg2, "note"))
#' cat(bg_grn(egArg1, egArg2, "note"))
#' cat(bg_mag(egArg1, egArg2, "note"))
#' cat(bg_red(egArg1, egArg2, "note"))
#' cat(bg_wht(egArg1, egArg2, "note"))
#' cat(bg_ylw(egArg1, egArg2, "note"))
#' cat(bg_def(egArg1, egArg2, "note"))
#'
#' cat(bg("y", egArg1, egArg2, "note"))
#' cat(bg("blu", egArg1, egArg2, "note"))
#' cat(bg("yellow", egArg1, egArg2, "note"))
#'
#' cat(bg("y", fg("r", st("u", st("i", egArg1, egArg2, "note")))))
#' cat(txt(egArg1, egArg2, "note", .BG = "blue", .FG = "w", .ST = c("i", "under")))
#'
#' @export
st_vals <- function(.ST = NULL) {
  uj:::.crayons_errs(.ST = .ST)
  if (!base::is.null(.ST)) {
    .ST <- uj:::.match_st_val(.ST)
    if      (.ST == "bld") {uj::v(CrayonBold)}
    else if (.ST == "itl") {uj::v(CrayonItalic)}
    else if (.ST == "def") {uj::v(CrayonDefault)}
    else if (.ST == "und") {uj::v(CrayonUnderline)}
    else                   {uj::v(CrayonPlain)}
  } else                   {uj::v(CrayonStyles)}
}

#' @rdname crayons
#' @export
bg_vals <- function(.BG = NULL) {
  uj:::.crayons_errs(.BG = .BG)
  if (!base::is.null(.BG)) {
    .BG <- uj:::.match_st_val(.BG)
    if      (.BG == "red") {uj::v(CrayonRed)}
    else if (.BG == "blu") {uj::v(CrayonBlue)}
    else if (.BG == "cyn") {uj::v(CrayonCyan)}
    else if (.BG == "blk") {uj::v(CrayonBlack)}
    else if (.BG == "grn") {uj::v(CrayonGreen)}
    else if (.BG == "wht") {uj::v(CrayonWhite)}
    else if (.BG == "mag") {uj::v(CrayonMagenta)}
    else                   {uj::v(CrayonDefault)}
  } else                   {uj::v(CrayonBackgroundColors)}
}

#' @rdname crayons
#' @export
fg_vals <- function(.FG = NULL) {
  uj:::.crayons_errs(.FG = .FG)
  if (!base::is.null(.FG)) {
    .FG <- uj:::.match_st_val(.FG)
    if      (.FG == "red") {uj::v(CrayonRed)}
    else if (.FG == "blu") {uj::v(CrayonBlue)}
    else if (.FG == "cyn") {uj::v(CrayonCyan)}
    else if (.FG == "blk") {uj::v(CrayonBlack)}
    else if (.FG == "grn") {uj::v(CrayonGreen)}
    else if (.FG == "wht") {uj::v(CrayonWhite)}
    else if (.FG == "sil") {uj::v(CrayonSilver)}
    else if (.FG == "mag") {uj::v(CrayonMagenta)}
    else                   {uj::v(CrayonDefault)}
  } else                   {uj::v(CrayonForegroundColors)}
}

# style ####

#' @rdname crayons
#' @export
st_bld <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::bold(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
st_def <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  uj:::.glue_args(.D, ...)
}

#' @rdname crayons
#' @export
st_itl <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::italic(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
st_pln <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  uj:::.glue_args(.D, ...)
}

#' @rdname crayons
#' @export
st_und <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::underline(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
st <- function(.ST, ..., .D = " ") {
  uj:::.crayons_errs(.ST = .ST, .D = .D, OK.NULL.SZT = F)
  .ST <- base::tolower(.ST)
  x <- uj:::.glue_args(.D, ...)
  if      (.ST %in% uj::v(CrayonBold)     ) {x <- uj::st_bld(x)}
  else if (.ST %in% uj::v(CrayonPlain)    ) {x <- uj::st_pln(x)}
  else if (.ST %in% uj::v(CrayonItalic)   ) {x <- uj::st_itl(x)}
  else if (.ST %in% uj::v(CrayonUnderline)) {x <- uj::st_und(x)}
  else                                      {x <- uj::st_def(x)}
  x
}

# fg color ####

#' @rdname crayons
#' @export
fg_blk <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::black(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
fg_blu <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::blue(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
fg_cyn <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::cyan(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
fg_def <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  uj:::.glue_args(.D, ...)
}

#' @rdname crayons
#' @export
fg_grn <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::green(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
fg_gry <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::silver(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
fg_mag <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::magenta(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
fg_red <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::red(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
fg_sil <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::silver(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
fg_slv <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::silver(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
fg_wht <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::white(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
fg_ylw <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::yellow(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
fg <- function(.FG, ..., .D = " ") {
  uj:::.crayons_errs(.FG = .FG, .D = .D, OK.NULL.FG = F)
  .FG <- base::tolower(.FG)
  x <- uj:::.glue_args(.D, ...)
  if      (.FG %in% uj::v(CrayonRed)    ) {x <- uj::fg_red(x)}
  else if (.FG %in% uj::v(CrayonBlue)   ) {x <- uj::fg_blu(x)}
  else if (.FG %in% uj::v(CrayonCyan)   ) {x <- uj::fg_cyn(x)}
  else if (.FG %in% uj::v(CrayonBlack)  ) {x <- uj::fg_blk(x)}
  else if (.FG %in% uj::v(CrayonGreen)  ) {x <- uj::fg_grn(x)}
  else if (.FG %in% uj::v(CrayonWhite)  ) {x <- uj::fg_wht(x)}
  else if (.FG %in% uj::v(CrayonSilver) ) {x <- uj::fg_sil(x)}
  else if (.FG %in% uj::v(CrayonYellow) ) {x <- uj::fg_ylw(x)}
  else if (.FG %in% uj::v(CrayonMagenta)) {x <- uj::fg_mag(x)}
  else                                    {x <- uj::fg_def(x)}
  x
}

# bg color ####

#' @rdname crayons
#' @export
bg_blk <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::bgBlack(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
bg_blu <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::bgBlue(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
bg_cyn <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::bgCyan(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
bg_def <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  uj:::.glue_args(.D, ...)
}

#' @rdname crayons
#' @export
bg_grn <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::bgGreen(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
bg_mag <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::bgMagenta(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
bg_red <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::bgRed(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
bg_wht <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::bgWhite(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
bg_ylw <- function(..., .D = " ") {
  uj:::.crayons_errs(.D = .D)
  crayon::bgYellow(uj:::.glue_args(.D, ...))
}

#' @rdname crayons
#' @export
bg <- function(.BG, ..., .D = " ") {
  uj:::.crayons_errs(.BG = .BG, .D = .D, OK.NULL.BG = F)
  .BG <- base::tolower(.BG)
  x <- uj:::.glue_args(.D, ...)
  if      (.BG %in% uj::v(CrayonRed)    ) {x <- uj::bg_red(x)}
  else if (.BG %in% uj::v(CrayonBlue)   ) {x <- uj::bg_blu(x)}
  else if (.BG %in% uj::v(CrayonCyan)   ) {x <- uj::bg_cyn(x)}
  else if (.BG %in% uj::v(CrayonBlack)  ) {x <- uj::bg_blk(x)}
  else if (.BG %in% uj::v(CrayonGreen)  ) {x <- uj::bg_grn(x)}
  else if (.BG %in% uj::v(CrayonWhite)  ) {x <- uj::bg_wht(x)}
  else if (.BG %in% uj::v(CrayonYellow) ) {x <- uj::bg_ylw(x)}
  else if (.BG %in% uj::v(CrayonMagenta)) {x <- uj::bg_mag(x)}
  else                                   {x <- uj::bg_def(x)}
  x
}

# generic ####

#' @rdname crayons
#' @export
txt <- function(..., .BG = NULL, .FG = NULL, .ST = NULL, .D = " ") {
  uj:::.crayons_errs(.BG = .BG, .FG = .FG, .ST = .ST, .D = .D, OK.NULL.BG = T, OK.NULL.FG = T, OK.NULL.ST = T)
  x <- uj:::.glue_args(.D, ...)
  if (!base::is.null(.ST)) {x <- uj::st(base::tolower(.ST), x)}
  if (!base::is.null(.BG)) {x <- uj::bg(base::tolower(.BG), x)}
  if (!base::is.null(.FG)) {x <- uj::fg(base::tolower(.FG), x)}
  x
}
