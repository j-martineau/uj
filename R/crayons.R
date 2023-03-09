#' @name crayons
#' @title Wrappers for package \code{\link[crayon]{crayon}} functions to styled and color console text
#' @description Functions in this family apply styles and colors to text for display on the console, where the text to display is identified by \link[=glue_dots]{collapsing} `...` args into a character scalar.
#' @section Background color functions:
#' \tabular{ll}{  `bg_vals`   \tab Get all valid values of arg `bg`.                                    \cr
#'                `bg_red`    \tab Set to red (\code{\link[crayon:bgRed]{crayon::bgRed}}).              \cr
#'                `bg_blu`    \tab Set to blue (\code{\link[crayon:bgBlue]{crayon::bgBlue}}).           \cr
#'                `bg_cyn`    \tab Set to cyan (\code{\link[crayon:bgCyan]{crayon::bgCyan}}).           \cr
#'                `bg_blk`    \tab Set to black (\code{\link[crayon:bgBlack]{crayon::bgBlack}}).        \cr
#'                `bg_grn`    \tab Set to green (\code{\link[crayon:bgGreen]{crayon::bgGreen}}).        \cr
#'                `bg_wht`    \tab Set to white (\code{\link[crayon:bgWhite]{crayon::bgWhite}}).        \cr
#'                `bg_ylw`    \tab Set to yellow (\code{\link[crayon:bgYellow]{crayon::bgYellow}}).     \cr
#'                `bg_mag`    \tab Set to magenta (\code{\link[crayon:bgMagenta]{crayon::bgMagenta}}).  \cr
#'                `bg_def`    \tab Set to system default.                                               \cr
#'                `bg`        \tab Set to value of arg `bg`.                                              }
#' \cr\cr Valid values of `bg` are the following:
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
#' \tabular{ll}{  `fg_vals`   \tab Get all valid values of `st` (foreground text color).               \cr
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
#'                `fg`        \tab Set to value of arg `fg`.                                             }
#' \cr\cr Valid values of `fg` are :
#' \tabular{ll}{  silver (grey)   \tab `'s'`, `'sil'`, `'slv'`, `'silver'`, `'gry'`, `'gray'`, `'grey'` \cr
#'                magenta         \tab `'m'`, `'mag'`, `'magenta'`                                      \cr
#'                default         \tab `'d'`, `'def'`, `'default'`                                      \cr
#'                yellow          \tab `'y'`, `'ylw'`, `'yellow'`                                       \cr
#'                black           \tab `'k'`, `'blk'`, `'black'`                                        \cr
#'                white           \tab `'w'`, `'wht'`, `'white'`                                        \cr
#'                green           \tab `'g'`, `'grn'`, `'green'`                                        \cr
#'                blue            \tab `'b'`, `'blu'`, `'blue'`                                         \cr
#'                cyan            \tab `'c'`, `'cyn'`, `'cyan'`                                         \cr
#'                red             \tab `'r'`, `'red'`                                                     }
#' NOTE: Silver (grey) is *only* valid for *foreground*.
#' @section Style functions:
#' \tabular{ll}{  `st_vals`   \tab Get all valid values of arg `st`.                               \cr
#'                `st_bld`    \tab Set to bold (\code{\link[crayon:bgRed]{crayon::bold}}).         \cr
#'                `st_pln`    \tab Set to plain (\code{\link[crayon:bgBlue]{crayon::reset}}).      \cr
#'                `st_itl`    \tab Set to italic (\code{\link[crayon:bgBlue]{crayon::italic}}).    \cr
#'                `st_und`    \tab Set to underline (\code{\link[crayon:bgBlue]{crayon::italic}}). \cr
#'                `st_def`    \tab Set to system default.                                          \cr
#'                `st`        \tab Set to value of arg `st`.                                         }
#' \cr\cr Valid values of `st` are:
#' \tabular{ll}{  underline   \tab `'u'`, `'un'`, `'und'`, `'under'`, `'underline'`, `'underlined'`                                                                         \cr   \tab   \cr
#'                default     \tab `'d'`, `'def'`, `'default'`                                                                                                              \cr   \tab   \cr
#'                italic      \tab `'i'`, `'it'`, `'itl'`, `'ital'`, `'italic'`, `'italics'`, `'italicized'`,`'e'`, `'em'`, `'emp'`, `'emph'`, `'emphasis'`, `'emphasized'` \cr   \tab   \cr
#'                plain       \tab `'p'`,` 'pl'`, `'pln'`, `'plain'`, `'r'`, `'re'`, `'res`', `'reset'`                                                                     \cr   \tab   \cr
#'                bold        \tab `'b'`, `'bo'`, `'bld'`, `'bold'`, `'bolded'`, `'s'`, `'st'`, `'str'`, `'strong'`                                                                        }
#' @section Generic function:
#' \tabular{ll}{  `txt`   \tab Styles text using args `st`, `bg`, `fg`.}
#' @param ... An arbitrary number of objects to be \link[=glue_dots]{collapsed} to a character scalar to be styled.
#' @param st A quoted or unquoted, case-insensitive, character scalar text style from `st_vals()`. May also be `NULL` for function `st_vals`.
#' @param bg A quoted or unquoted, case-insensitive, character scalar text background color from `bg_vals()`. May also be `NULL` for function `bg_vals`.
#' @param fg A quoted or unquoted, case-insensitive, character scalar text foreground color from `fg_vals()`. May also be `NULL` for function `fg_vals`.
#' @param d A non-`NA` character scalar delimiter for \link[=glue_dots]{collapsing} `...` args into a character scalar.
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
#' cat(txt(egArg1, egArg2, "note", bg = "blue", fg = "w", st = c("i", "under")))
#'
#' @export
st_vals <- function(st = NULL) {
  uj:::.crayon_errs(st = st)
  st <- base::tolower(st)
  uj::f0(st %in% uj:::.st_bld, uj:::.st_bld,
         uj::f0(st %in% uj:::.st_def, uj:::.st_def,
                uj::f0(st %in% uj:::.st_itl, uj:::.st_itl,
                       uj::f0(st %in% uj:::.st_pln, uj:::.st_pln,
                              uj::f0(st %in% uj:::.st_und, uj:::.st_und, uj:::.st_all)))))
}

#' @rdname crayons
#' @export
bg_vals <- function(bg = NULL) {
  uj:::.crayon_errs(bg = bg)
  bg <- base::tolower(bg)
  uj::f0(bg %in% uj:::.xg_blu, uj:::.xg_blu,
         uj::f0(bg %in% uj:::.xg_cyn, uj:::.xg_cyn,
                uj::f0(bg %in% uj:::.xg_def, uj:::.xg_def,
                       uj::f0(bg %in% uj:::.xg_grn, uj:::.xg_grn,
                              uj::f0(bg %in% uj:::.xg_blk, uj:::.xg_blk,
                                     uj::f0(bg %in% uj:::.xg_mag, uj:::.xg_mag,
                                            uj::f0(bg %in% uj:::.xg_red, uj:::.xg_red,
                                                   uj::f0(bg %in% uj:::.xg_wht, uj:::.xg_wht,
                                                          uj::f0(bg %in% uj:::.xg_ylw, uj:::.xg_ylw, uj:::.bg_all)))))))))
}

#' @rdname crayons
#' @export
fg_vals <- function(fg = NULL) {
  uj:::.crayon_errs(fg = fg)
  fg <- base::tolower(fg)
  uj::f0(fg %in% uj:::.xg_blu, uj:::.xg_blu,
         uj::f0(fg %in% uj:::.xg_cyn, uj:::.xg_cyn,
                uj::f0(fg %in% uj:::.xg_def, uj:::.xg_def,
                       uj::f0(fg %in% uj:::.xg_grn, uj:::.xg_grn,
                              uj::f0(fg %in% uj:::.xg_blk, uj:::.xg_blk,
                                     uj::f0(fg %in% uj:::.xg_mag, uj:::.xg_mag,
                                            uj::f0(fg %in% uj:::.xg_red, uj:::.xg_red,
                                                   uj::f0(fg %in% uj:::.xg_sil, uj:::.xg_sil,
                                                          uj::f0(fg %in% uj:::.xg_wht, uj:::.xg_wht,
                                                                 uj::f0(fg %in% uj:::.xg_ylw, uj:::.xg_ylw, uj:::.fg_all))))))))))
}

# style ####

#' @rdname crayons
#' @export
st_bld <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::bold(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
st_def <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  uj:::.glue_args(d, ...)
}

#' @rdname crayons
#' @export
st_itl <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::italic(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
st_pln <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  uj:::.glue_args(d, ...)
}

#' @rdname crayons
#' @export
st_und <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::underline(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
st <- function(st, ..., d = " ") {
  uj:::.crayon_errs(st = st, d = d, st.null.ok = F)
  st <- base::tolower(st)
  vals <- base::c("bld", "def", "itl", "pln", "und")
  x <- uj:::.glue_args(d, ...)
  for (val in base::tolower(vals)) {if (base::any(st %in% uj::st_vals(val))) {return(uj::run("st_", val, "(x)"))}}
  x
}

# fg color ####

#' @rdname crayons
#' @export
fg_blk <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::black(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
fg_blu <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::blue(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
fg_cyn <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::cyan(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
fg_def <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  uj:::.glue_args(d, ...)
}

#' @rdname crayons
#' @export
fg_grn <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::green(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
fg_gry <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::silver(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
fg_mag <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::magenta(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
fg_red <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::red(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
fg_sil <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::silver(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
fg_slv <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::silver(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
fg_wht <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::white(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
fg_ylw <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::yellow(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
fg <- function(fg, ..., d = " ") {
  uj:::.crayon_errs(fg = fg, d = d, fg.null.ok = F)
  fg <- base::tolower(fg)
  vals <- base::c("blk", "blu", "cyn", "def", "grn", "sil", "mag", "red", "wht", "ylw")
  x <- uj:::.glue_args(d, ...)
  for (val in base::tolower(vals)) {if (base::any(st %in% uj::st_vals(val))) {text <- uj::run("st_", val, "(x)")}}
  x
}

# bg color ####

#' @rdname crayons
#' @export
bg_blk <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::bgBlack(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
bg_blu <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::bgBlue(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
bg_cyn <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::bgCyan(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
bg_def <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  uj:::.glue_args(d, ...)
}

#' @rdname crayons
#' @export
bg_grn <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::bgGreen(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
bg_mag <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::bgMagenta(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
bg_red <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::bgRed(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
bg_wht <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::bgWhite(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
bg_ylw <- function(..., d = " ") {
  uj:::.crayon_errs(d = d)
  crayon::yellow(uj:::.glue_args(d, ...))
}

#' @rdname crayons
#' @export
bg <- function(bg, ..., d = " ") {
  uj:::.crayon_errs(bg = bg, d = d, bg.null.ok = F)
  bg <- base::tolower(bg)
  vals <- base::c("blk", "blu", "cyn", "def", "grn", "mag", "red", "wht", "ylw")
  x <- uj:::.glue_args(d, ...)
  for (val in base::tolower(vals)) {if (bg %in% uj::bg_vals(val)) {return(uj::run("bg_", val, "(x)"))}}
  x
}

# generic ####

#' @rdname crayons
#' @export
txt <- function(..., bg = NULL, fg = NULL, st = NULL, d = " ") {
  uj:::.crayon_errs(bg = bg, fg = fg, st = st, d = d)
  bg <- base::tolower(bg)
  fg <- base::tolower(fg)
  st <- base::tolower(fg)
  text <- uj:::.glue_args(d, ...)
  if (!base::is.null(st)) {x <- uj::st(st, text)}
  if (!base::is.null(bg)) {x <- uj::bg(bg, text)}
  if (!base::is.null(fg)) {x <- uj::fg(fg, text)}
  text
}
