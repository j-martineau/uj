# indicator values ####

#' @name crayons
#' @title Wrappers for package \code{\link[crayon]{crayon}} functions to styled and color console text
#' @description Functions in this family apply styles and colors to text for display on the console, where the text to display is identified by \link[=glue_dots]{collapsing} `...` args into a character scalar.
#' @section Background color functions:
#' \tabular{ll}{  `bg_vals`        \tab Get all valid values of arg `bg`.                                    \cr   \tab  }
#' \tabular{ll}{  `bg_red`         \tab Set to red (\code{\link[crayon:bgRed]{crayon::bgRed}}).              \cr
#'                `bg_blu`         \tab Set to blue (\code{\link[crayon:bgBlue]{crayon::bgBlue}}).           \cr
#'                `bg_cyn`         \tab Set to cyan (\code{\link[crayon:bgCyan]{crayon::bgCyan}}).           \cr
#'                `bg_blk`         \tab Set to black (\code{\link[crayon:bgBlack]{crayon::bgBlack}}).        \cr
#'                `bg_grn`         \tab Set to green (\code{\link[crayon:bgGreen]{crayon::bgGreen}}).        \cr
#'                `bg_wht`         \tab Set to white (\code{\link[crayon:bgWhite]{crayon::bgWhite}}).        \cr
#'                `bg_ylw`         \tab Set to yellow (\code{\link[crayon:bgYellow]{crayon::bgYellow}}).     \cr
#'                `bg_mag`         \tab Set to magenta (\code{\link[crayon:bgMagenta]{crayon::bgMagenta}}).  \cr
#'                `bg_def`         \tab Set to system default.                                               \cr   \tab  }
#' \tabular{ll}{  `bg`             \tab Set to value of arg `bg`.                                                        }
#' \cr\cr Valid values of `bg` are the following:
#' \tabular{ll}{  magenta         \tab `'m'`, `'mag'`, `'magenta'` \cr   \tab  }
#' \tabular{ll}{  default         \tab `'d'`, `'def'`, `'default'` \cr   \tab  }
#' \tabular{ll}{  yellow          \tab `'y'`, `'ylw'`, `'yellow'`  \cr   \tab  }
#' \tabular{ll}{  black           \tab `'k'`, `'blk'`, `'black'`   \cr
#'                white           \tab `'w'`, `'wht'`, `'white'`   \cr
#'                green           \tab `'g'`, `'grn'`, `'green'`   \cr   \tab  }
#' \tabular{ll}{  blue            \tab `'b'`, `'blu'`, `'blue'`    \cr
#'                cyan            \tab `'c'`, `'cyn'`, `'cyan'`    \cr   \tab  }
#' \tabular{ll}{  red             \tab `'r'`, `'red'`                          }
#' NOTE: Silver (grey) is *not* valid for *background*.
#' @section Foreground color functions:
#' \tabular{ll}{  `fg_vals`        \tab Get all valid values of `st` (foreground text color).               \cr   \tab  }
#' \tabular{ll}{  `fg_red`         \tab Set to red (\code{\link[crayon:red]{crayon::red}}).                 \cr
#'                `fg_blu`         \tab Set to red (\code{\link[crayon:blue]{crayon::blue}}).               \cr
#'                `fg_cyn`         \tab Set to red (\code{\link[crayon:cyan]{crayon::cyan}}).               \cr
#'                `fg_blk`         \tab Set to red (\code{\link[crayon:black]{crayon::black}}).             \cr
#'                `fg_grn`         \tab Set to red (\code{\link[crayon:green]{crayon::green}}).             \cr
#'                `fg_wht`         \tab Set to red (\code{\link[crayon:white]{crayon::white}}).             \cr
#'                `fg_ylw`         \tab Set to red (\code{\link[crayon:yellow]{crayon::yellow}}).           \cr
#'                `fg_mag`         \tab Set to red (\code{\link[crayon:magenta]{crayon::magenta}}).         \cr
#'                `fg_gry`         \tab Set to silver (grey) (\code{\link[crayon:silver]{crayon::silver}}). \cr
#'                `fg_sil`         \tab Set to silver (grey).                                               \cr
#'                `fg_slv`         \tab Set to silver (grey).                                               \cr
#'                `fg_def`         \tab Set to system default.                                              \cr   \tab  }
#' \tabular{ll}{  `fg`        v    \tab Set to value of arg `fg`.                                                       }
#' \cr\cr Valid values of `fg` are :
#' \tabular{ll}{  silver (grey)   \tab `'s'`, `'sil'`, `'slv'`, `'silver'`, `'gry'`, `'gray'`, `'grey'`    \cr   \tab  }
#' \tabular{ll}{  magenta         \tab `'m'`, `'mag'`, `'magenta'`                                         \cr
#'                default         \tab `'d'`, `'def'`, `'default'`                                         \cr   \tab  }
#' \tabular{ll}{  yellow          \tab `'y'`, `'ylw'`, `'yellow'`                                          \cr   \tab  }
#' \tabular{ll}{  black           \tab `'k'`, `'blk'`, `'black'`                                           \cr
#'                white           \tab `'w'`, `'wht'`, `'white'`                                           \cr
#'                green           \tab `'g'`, `'grn'`, `'green'`                                           \cr   \tab  }
#' \tabular{ll}{  blue            \tab `'b'`, `'blu'`, `'blue'`                                            \cr
#'                cyan            \tab `'c'`, `'cyn'`, `'cyan'`                                            \cr   \tab  }
#' \tabular{ll}{  red             \tab `'r'`, `'red'`                                                                  }
#' NOTE: Silver (grey) is *only* valid for *foreground*.
#' @section Style functions:
#' \tabular{ll}{  `st_vals`       \tab Get all valid values of arg `st`.                                   \cr   \tab  }
#' \tabular{ll}{  `st_bld`        \tab Set to bold (\code{\link[crayon:bgRed]{crayon::bold}}).             \cr
#'                `st_pln`        \tab Set to plain (\code{\link[crayon:bgBlue]{crayon::reset}}).          \cr
#'                `st_itl`        \tab Set to italic (\code{\link[crayon:bgBlue]{crayon::italic}}).        \cr
#'                `st_und`        \tab Set to underline (\code{\link[crayon:bgBlue]{crayon::italic}}).     \cr
#'                `st_def`        \tab Set to system default.                                              \cr   \tab  }
#' \tabular{ll}{  `st`            \tab Set to value of arg `st`.                                                       }
#' \cr\cr Valid values of `st` are:
#' \tabular{ll}{  underline       \tab `'u'`, `'un'`, `'und'`, `'under'`, `'underline'`, `'underlined'`                                                                         \cr   \tab  }
#' \tabular{ll}{  default         \tab `'d'`, `'def'`, `'default'`                                                                                                              \cr   \tab  }
#' \tabular{ll}{  italic          \tab `'i'`, `'it'`, `'itl'`, `'ital'`, `'italic'`, `'italics'`, `'italicized'`,`'e'`, `'em'`, `'emp'`, `'emph'`, `'emphasis'`, `'emphasized'` \cr   \tab  }
#' \tabular{ll}{  plain           \tab `'p'`,` 'pl'`, `'pln'`, `'plain'`, `'r'`, `'re'`, `'res`', `'reset'`                                                                     \cr   \tab  }
#' \tabular{ll}{  bold            \tab `'b'`, `'bo'`, `'bld'`, `'bold'`, `'bolded'`, `'s'`, `'st'`, `'str'`, `'strong'`                                                                     }
#' @section Generic function:
#' \tabular{ll}{  `txt`           \tab Styles text using args `st`, `bg`, `fg`.}
#' @param ... An arbitrary number of objects to be \link[=glue_dots]{collapsed} to a character scalar to be styled.
#' @param st A quoted or unquoted, case-insensitive, character scalar text style from `st_vals()`. May also be `NULL` for function `st_vals`.
#' @param bg A quoted or unquoted, case-insensitive, character scalar text background color from `bg_vals()`. May also be `NULL` for function `bg_vals`.
#' @param fg A quoted or unquoted, case-insensitive, character scalar text foreground color from `fg_vals()`. May also be `NULL` for function `fg_vals`.
#' @param d A non-`NA` character scalar delimiter for \link[=glue_dots]{collapsing} `...` args into a character scalar.
#' @return **A character vector** \cr `st_vals, fg_vals, bg_vals`
#' \cr\cr  **A character scalar** \cr All others
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
  st <- uj::failsafe(base::tolower(st))
  B <- base::c("b", "bo", "bld", "bold", "bolded", "s", "st", "str", "strong")
  D <- base::c("d", "def", "default")
  I <- base::c("i", "it", "itl", "ital", "italic", "italics", "italicized", "e", "em", "emp", "emph", "emphasis", "emphasized")
  P <- base::c("p", "pl", "pln", "plain", "r", "re", "res", "reset")
  U <- base::c("u", "un", "und", "under", "underline", "underlined")
  A <- base::sort(base::c(B, D, I, P, U))
  uj::f0(uj::isIN1(st, B), B,
         uj::f0(uj::isIN1(st, D), D,
                uj::f0(uj::isIN1(st, I), I,
                       uj::f0(uj::isIN1(st, P), P,
                              uj::f0(uj::isIN1(st, U),
                                     U, uj::f0(uj::NLL(st), A,
                                               uj::stopper("[st] must be NULL or a character scalar from st_vals().", PKG = "uj")))))))
}

#' @rdname crayons
#' @export
bg_vals <- function(bg = NULL) {
  bg <- uj::failsafe(base::tolower(bg))
  B <- base::c("b", "blu", "blue")
  C <- base::c("c", "cyn", "cyan")
  D <- base::c("d", "def", "default")
  G <- base::c("g", "grn", "green")
  K <- base::c("k", "blk", "black")
  M <- base::c("m", "mag", "magenta")
  R <- base::c("r", "red")
  W <- base::c("w", "wht", "white")
  Y <- base::c("y", "ylw", "yellow")
  A <- base::sort(base::c(K, B, C, D, G, M, R, W, Y))
  uj::f0(uj::isIN1(bg, B), B,
         uj::f0(uj::isIN1(bg, C), C,
                uj::f0(uj::isIN1(bg, D), D,
                       uj::f0(uj::isIN1(bg, G), G,
                              uj::f0(uj::isIN1(bg, K), K,
                                     uj::f0(uj::isIN1(bg, M), M,
                                            uj::f0(uj::isIN1(bg, R), R,
                                                   uj::f0(uj::isIN1(bg, W), W,
                                                          uj::f0(uj::isIN1(bg, Y), Y,
                                                                 uj::f0(uj::NLL(bg), A,
                                                                        uj::stopper("[bg] must be NULL or a character scalar from bg_vals().", PKG = "uj")))))))))))
}

#' @rdname crayons
#' @export
fg_vals <- function(fg = NULL) {
  fg <- uj::failsafe(base::tolower(fg))
  B <- base::c("b", "blu", "blue")
  C <- base::c("c", "cyn", "cyan")
  D <- base::c("d", "def", "default")
  G <- base::c("g", "grn", "green")
  K <- base::c("k", "blk", "black")
  M <- base::c("m", "mag", "magenta")
  R <- base::c("r", "red")
  S <- base::c("s", "sil", "gry", "slv", "grey", "silver")
  W <- base::c("w", "wht", "white")
  Y <- base::c("y", "ylw", "yellow")
  A <- base::sort(base::c(K, B, C, D, G, R, S, M, W, Y))
  uj::f0(uj::isIN1(fg, B), B,
         uj::f0(uj::isIN1(fg, C), C,
                uj::f0(uj::isIN1(fg, D), D,
                       uj::f0(uj::isIN1(fg, G), G,
                              uj::f0(uj::isIN1(fg, K), K,
                                     uj::f0(uj::isIN1(fg, M), M,
                                            uj::f0(uj::isIN1(fg, R), R,
                                                   uj::f0(uj::isIN1(fg, S), S,
                                                          uj::f0(uj::isIN1(fg, W), W,
                                                                 uj::f0(uj::isIN1(fg, Y),
                                                                        Y, uj::f0(uj::NLL(fg), A,
                                                                                  uj::stopper("[fg] must be NULL or a character scalar from fg_vals().", PKG = "uj"))))))))))))
}

# style ####

#' @rdname crayons
#' @export
st_bld <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::bold(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
st_def <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  uj::g(d, uj::av(...))
}

#' @rdname crayons
#' @export
st_itl <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::italic(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
st_pln <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  uj::g(d, uj::av(...))
}

#' @rdname crayons
#' @export
st_und <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::underline(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
st <- function(st, ..., d = " ") {
  st <- base::tolower(uj::f0(!uj::unqSTRvec(st), "error", st))
  uj::errs_if_nots(uj::cmp_chr_scl(d)           , "[d] must be a complete character scalar (?cmp_chr_scl)."     ,
                   uj::isIN1(st, uj::st_vals()), "[st] must be a unique string vec (?unqSTRvec) from st_vals().", PKG = "uj")
  vals <- base::c("bld", "def", "itl", "pln", "und")
  text <- uj::g(d, uj::av(...))
  for (val in base::tolower(vals)) {if (uj::anyIN(st, uj::st_vals(val))) {text <- uj::run("st_", val, "(text)")}}
  text
}

# fg color ####

#' @rdname crayons
#' @export
fg__blk <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::black(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
fg_blu <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::blue(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
fg_cyn <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::cyan(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
fg_def <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  uj::g(d, uj::av(...))
}

#' @rdname crayons
#' @export
fg_grn <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::green(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
fg_gry <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::silver(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
fg_mag <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::magenta(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
fg_red <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::red(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
fg_sil <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::silver(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
fg_slv <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::silver(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
fg_wht <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::white(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
fg_ylw <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::yellow(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
fg <- function(fg, ..., d = " ") {
  fg <- base::tolower(uj::f0(!uj::cmp_str_scl(fg), "error", fg))
  uj::errs_if_nots(uj::cmp_chr_scl(d)          , "[d] must be a complete character scalar (?cmp_chr_scl)."      ,
                   uj::isIN1(fg, uj::fg_vals()), "[fg] must be a unique string vec (?unqSTRvec) from fg_vals().", PKG = "uj")
  vals <- base::c("blk", "blu", "cyn", "def", "grn", "mag", "red", "gry", "wht", "ylw")
  for (val in base::tolower(vals)) {if (uj::isIN1(fg, uj::fg_vals(val))) {return(uj::run("fg_", val, "(..., d = d)"))}}
}

# bg color ####

#' @rdname crayons
#' @export
bg_blk <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::bgBlack(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
bg_blu <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::bgBlue(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
bg_cyn <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::bgCyan(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
bg_def <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  uj::g(d, uj::av(...))
}

#' @rdname crayons
#' @export
bg_grn <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::bgGreen(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
bg_mag <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::bgMagenta(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
bg_red <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::bgRed(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
bg_wht <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::bgWhite(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
bg_ylw <- function(..., d = " ") {
  uj::err_if_not(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl).", PKG = "uj")
  crayon::yellow(uj::g(d, uj::av(...)))
}

#' @rdname crayons
#' @export
bg <- function(bg, ..., d = " ") {
  bg <- base::tolower(uj::f0(!uj::cmp_str_scl(bg), "error", bg))
  uj::errs_if_nots(uj::cmp_chr_scl(d)           , "[d] must be a complete character scalar (?cmp_chr_scl)."          ,
                 uj::isIN1(bg, uj::bg_vals()), "[bg] must be a unique string scalar (?unqSTRscl) from bg_vals().", PKG = "uj")
  vals <- base::c("blk", "blu", "cyn", "def", "grn", "mag", "red", "wht", "ylw")
  for (val in base::tolower(vals)) {if (uj::isIN1(bg, uj::bg_vals(val))) {return(uj::run("bg_", val, "(...)"))}}
}

# generic ####

#' @rdname crayons
#' @export
txt <- function(..., bg = NULL, fg = NULL, st = NULL, d = " ") {
  if (uj::NLL(bg)) {bg <- "def"}
  if (uj::NLL(fg)) {fg <- "def"}
  if (uj::NLL(st)) {st <- "def"}
  bg <- base::tolower(uj::f0(!uj::cmp_str_scl(bg), "error", bg))
  fg <- base::tolower(uj::f0(!uj::cmp_str_scl(bg), "error", bg))
  st <- base::tolower(uj::f0(!uj::unq_str_vec(st), "error", st))
  ok.b <- uj::isIN1(bg, uj::bg_vals())
  ok.f <- uj::isIN1(fg, uj::fg_vals())
  ok.s <- uj::f0(uj::unq_str_vec(st), uj::allIN(st, uj::st_vals()), F)
  uj::errs_if_nots(uj::cmp_chr_scl(d), "[d] must be a complete character scalar (?cmp_chr_scl)."                            ,
                   ok.b              , "[bg] must be NULL or a character scalar from uj::bg_vals()."                        ,
                   ok.f              , "[fg] must be NULL or a character scalar from uj::fg_vals()."                        ,
                   ok.s              , "[st] must be NULL or a unique string vec (?unqSTRvec) of values from uj::st_vals().", PKG = "uj")
  x <- uj::glue_dots(...)
  if (uj::DEF(st)) {x <- uj::st(st, x)}
  if (uj::DEF(bg)) {x <- uj::bg(bg, x)}
  if (uj::DEF(fg)) {x <- uj::fg(fg, x)}
  x
}
