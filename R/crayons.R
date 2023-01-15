# indicator values ####

#' @name crayons
#' @title Wrappers for package \code{\link[crayon]{crayon}} functions to styled and color console text
#' @description Functions in this family call \code{\link{collapse_dots}(...)} before applying styles or colors.
#' \tabular{rl}{
#'      `txt`   \tab Processes text using `st`, `bg`, `fg`.
#'   \cr `st`   \tab Styles text using arg `st`.
#'   \cr `fg`   \tab Colors text foreground using arg `fg`.
#'   \cr `bg`   \tab Colors text background using arg `bg`.
#'   \cr        \tab  
#' } \tabular{rl}{
#'       `st_bld`   \tab Calls \code{\link[crayon:bold]{crayon::bold}}.
#'   \cr `st_itl`   \tab Calls \code{\link[crayon:italic]{crayon::italic}}.
#'   \cr `st_und`   \tab Calls \code{\link[crayon:underline]{crayon::underline}}.
#'   \cr            \tab  
#'   \cr `st_pln`   \tab Calls \code{\link[crayon:reset]{crayon::reset}}.
#'   \cr `st_res`   \tab (`crayon`'s alias for plain text)
#'   \cr            \tab  
#'   \cr `fg_blk`   \tab Calls \code{\link[crayon:black]{crayon::black}}
#'   \cr `fg_blk`   \tab Calls \code{\link[crayon:black]{crayon::black}}.
#'   \cr `fg_blu`   \tab Calls \code{\link[crayon:blue]{crayon::blue}}.
#'   \cr `fg_cyn`   \tab Calls \code{\link[crayon:cyan]{crayon::cyan}}.
#'   \cr `fg_grn`   \tab Calls \code{\link[crayon:green]{crayon::green}}.
#'   \cr `fg_mag`   \tab Calls \code{\link[crayon:magenta]{crayon::magenta}}.
#'   \cr `fg_red`   \tab Calls \code{\link[crayon:red]{crayon::red}}
#'   \cr `fg_wht`   \tab Calls \code{\link[crayon:white]{crayon::white}}.
#'   \cr `fg_ylw`   \tab Calls \code{\link[crayon:yellow]{crayon::yellow}}.
#'   \cr            \tab  
#'   \cr `fg_gry`   \tab Calls \code{\link[crayon:silver]{crayon::silver}}.
#'   \cr `fg_sil`   \tab (`crayon`'s alias for grey)
#'   \cr            \tab  
#'   \cr `bg_blk`   \tab Calls \code{\link[crayon:bgBlack]{crayon::bgBlack}}.
#'   \cr `bg_blu`   \tab Calls \code{\link[crayon:bgBlue]{crayon::bgBlue}}.
#'   \cr `bg_cyn`   \tab Calls \code{\link[crayon:bgCyan]{crayon::bgCyan}}.
#'   \cr `bg_grn`   \tab Calls \code{\link[crayon:bgGreen]{crayon::bgGreen}}.
#'   \cr `bg_mag`   \tab Calls \code{\link[crayon:bgMagenta]{crayon::bgMagenta}}.
#'   \cr `bg_red`   \tab Calls \code{\link[crayon:bgRed]{crayon::bgRed}}.
#'   \cr `bg_wht`   \tab Calls \code{\link[crayon:bgWhite]{crayon::bgWhite}}.
#'   \cr `bg_ylw`   \tab Calls \code{\link[crayon:bgYellow]{crayon::bgYellow}}.
#'   \cr            \tab  
#' } \tabular{rl}{
#'       `st_vals`   \tab Gets all valid values of arg `st`.
#'   \cr `bg_vals`   \tab Gets all valid values of arg `bg`.
#'   \cr `fg_vals`   \tab Gets all valid values of arg `fg`.
#' }
#' See *details* for style-specific and color-specific valid indicator values.
#' @details The following table lists all valid values of `bg` for allowed colors:
#' \tabular{rl}{
#'      magenta   \tab `'m'`, `'mag'`, `'magenta'`
#'   \cr yellow   \tab `'y'`, `'ylw'`, `'yellow'`
#'   \cr  black   \tab `'k'`, `'blk'`, `'black'`
#'   \cr  white   \tab `'w'`, `'wht'`, `'white'`
#'   \cr  green   \tab `'g'`, `'grn'`, `'green'`
#'   \cr   blue   \tab `'b'`, `'blu'`, `'blue'`
#'   \cr   cyan   \tab `'c'`, `'cyn'`, `'cyan'`
#'   \cr    red   \tab `'r'`, `'red'`
#' }
#' \cr Valid values of `fg` are the valid values of `bg` plus the following indicators for grey: `'gry'`, `'gray'`, `'grey'`, `'s'`, `'sil'`, `'silver'`.
#' \cr\cr The following table lists all valid values of `st` for allowed styles:
#' \tabular{rl}{
#'       underline   \tab `'u'`, `'un'`, `'und'`, `'under'`, `'underline'`, `'underlined'`
#'   \cr    italic   \tab `'i'`, `'it'`, `'itl'`, `'ital'`, `'italic'`, `'italics'`, `'italicized'`, `'e'`, `'em'`, `'emp'`, `'emph'`, `'emphasis'`, `'emphasized'`
#'   \cr     plain   \tab `'p'`,` 'pl'`, `'pln'`, `'plain'`, `'r'`, `'re'`, `'res`', `'reset'`
#'   \cr      bold   \tab `'b'`, `'bo'`, `'bld'`, `'bold'`, `'bolded'`, `'s'`, `'st'`, `'str'`, `'strong'`
#' }
#' \cr Calling `st_vals(st)`, `bg_vals(bg)`, and `fg_vals(fg)` gets a character vector of all indicator values synonymous with the values in `st`, `bg` and `fg`, respectively.
#' @param ... An arbitrary number of objects to be \link[=collapse_dots]{collapsed} to a character scalar to be styled.
#' @param st A quoted or unquoted, case-insensitive, character scalar text style from `st_vals()`. May also be `NULL` for function `st_vals`.
#' @param bg A quoted or unquoted, case-insensitive, character scalar text background color from `bg_vals()`. May also be `NULL` for function `bg_vals`.
#' @param fg A quoted or unquoted, case-insensitive, character scalar text foreground color from `fg_vals()`. May also be `NULL` for function `fg_vals`.
#' @return *A character vector* \cr   `st_vals, fg_vals, bg_vals`
#'  \cr\cr *A character scalar* \cr   All others
#' @export
st_vals <- function(st = NULL) {
  B <- base::c("b", "bo", "bld", "bold", "bolded", "s", "st", "str", "strong")
  I <- base::c("i", "it", "itl", "ital", "italic", "italics", "italicized", "e", "em", "emp", "emph", "emphasis", "emphasized")
  P <- base::c("p", "pl", "pln", "plain", "r", "re", "res", "reset")
  U <- base::c("u", "un", "und", "under", "underline", "underlined")
  A <- base::sort(base::c(B, I, P, U))
  uj::f0(uj::isIN(st, B), B,
         uj::f0(uj::isIN(st, I), I,
                uj::f0(uj::isIN(st, P), P,
                       uj::f0(uj::isIN(st, U), U,
                              uj::f0(base::is.null(st), A, stop(uj::.errs("[st] must be NULL or a character scalar from st_val().")))))))
}

#' @rdname crayons
#' @export
bg_vals <- function(bg = NULL) {
  B <- base::c("b", "blu", "blue")
  C <- base::c("c", "cyn", "cyan")
  G <- base::c("g", "grn", "green")
  K <- base::c("k", "blk", "black")
  M <- base::c("m", "mag", "magenta")
  R <- base::c("r", "red")
  W <- base::c("w", "wht", "white")
  Y <- base::c("y", "ylw", "yellow")
  A <- base::sort(base::c(K, B, C, G, M, R, W, Y))
  uj::f0(uj::isIN(bg, B), B,
         uj::f0(uj::isIN(bg, C), C,
                uj::f0(uj::isIN(bg, G), G,
                       uj::f0(uj::isIN(bg, K), K,
                              uj::f0(uj::isIN(bg, M), M,
                                     uj::f0(uj::isIN(bg, R), R,
                                            uj::f0(uj::isIN(bg, W), W,
                                                   uj::f0(uj::isIN(bg, Y), Y,
                                                          uj::f0(base::is.null(bg), A, stop(uj::format_errs(pkg = "uj", "[bg] must be NULL or a character scalar from bg_vals().")))))))))))
}

#' @rdname crayons
#' @export
fg_vals <- function(fg = NULL) {
  B <- base::c("b", "blu", "blue")
  C <- base::c("c", "cyn", "cyan")
  G <- base::c("g", "grn", "green")
  K <- base::c("k", "blk", "black")
  M <- base::c("m", "mag", "magenta")
  R <- base::c("r", "red")
  S <- base::c("s", "gry", "gray", "grey", "sil", "slv", "silver")
  W <- base::c("w", "wht", "white")
  Y <- base::c("y", "ylw", "yellow")
  A <- base::sort(base::c(K, B, C, G, R, S, M, W, Y))
  uj::f0(uj::isIN(fg, B), B,
         uj::f0(uj::isIN(fg, C), C,
                uj::f0(uj::isIN(fg, G), G,
                       uj::f0(uj::isIN(fg, K), K,
                              uj::f0(uj::isIN(fg, M), M,
                                     uj::f0(uj::isIN(fg, R), R,
                                            uj::f0(uj::isIN(fg, S), S,
                                                   uj::f0(uj::isIN(fg, W), W,
                                                          uj::f0(uj::isIN(fg, Y), Y,
                                                                 uj::f0(base::is.null(fg), A, stop(uj::format_errs(pkg = "uj", "[fg] must be NULL or a character scalar from fg_vals()."))))))))))))
}

# style ####

#' @rdname crayons
#' @export
st_bld <- function(...) {crayon::bold(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
st_itl <- function(...) {crayon::italic(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
st_pln <- function(...) {crayon::reset(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
st_res <- function(...) {crayon::reset(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
st_und <- function(...) {crayon::underline(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
st <- function(st, ...) {
  st <- base::tolower(uj::f0(!uj::cmp_str_scl(st), "error", st))
  if (uj::f0(st %in% uj::st_vals(), st, "error") == "error") {base::stop(uj::format_errs(pkg = "uj", "[st] must be a character scalar from st_vals()."))}
  vals <- c("bld", "itl", "pln", "und")
  for (val in vals) {if (uj::isIN(st, st_vals(val))) {return(uj::run("st_", val, "(...)"))}}
}

# fg color ####

#' @rdname crayons
#' @export
fg_blk <- function(...) {crayon::black(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
fg_blu <- function(...) {crayon::blue(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
fg_cyn <- function(...) {crayon::cyan(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
fg_grn <- function(...) {crayon::green(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
fg_gry <- function(...) {crayon::silver(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
fg_sil <- function(...) {crayon::silver(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
fg_mag <- function(...) {crayon::magenta(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
fg_red <- function(...) {crayon::red(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
fg_wht <- function(...) {crayon::white(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
fg_ylw <- function(...) {crayon::yellow(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
fg <- function(fg, ...) {
  fg <- base::tolower(uj::f0(!uj::cmp_str_scl(fg), "error", fg))
  if (uj::f0(fg %in% uj::fg_vals(), fg, "error") == "error") {base::stop(uj::format_errs(pkg = "uj", "[fg] must be a character scalar from fg_vals()."))}
  vals <- c("blk", "blu", "cyn", "grn", "mag", "red", "gry", "wht", "ylw")
  for (val in vals) {if (uj::isIN(fg, fg_vals(val))) {return(uj::run("fg_", val, "(...)"))}}
}

# bg color ####

#' @rdname crayons
#' @export
bg_blk <- function(...) {crayon::bgBlack(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
bg_blu <- function(...) {crayon::bgBlue(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
bg_cyn <- function(...) {crayon::bgCyan(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
bg_grn <- function(...) {crayon::bgGreen(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
bg_mag <- function(...) {crayon::bgMagenta(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
bg_red <- function(...) {crayon::bgRed(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
bg_wht <- function(...) {crayon::bgWhite(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
bg_ylw <- function(...) {crayon::bgYellow(uj::collapse_dots(...))}

#' @rdname crayons
#' @export
bg <- function(bg, ...) {
  bg <- base::tolower(uj::f0(!uj::cmp_str_scl(bg), "error", bg))
  if (uj::f0(bg %in% uj::bg_vals(), bg, "error") == "error") {base::stop(uj::format_errs(pkg = "uj", "[bg] must be a character scalar from bg_vals()."))}
  vals <- c("blk", "blu", "cyn", "grn", "mag", "red", "wht", "ylw")
  for (val in vals) {if (uj::isIN(bg, bg_vals(val))) {return(uj::run("bg_", val, "(...)"))}}
}

# generic ####

#' @rdname crayons
#' @export
txt <- function(..., bg = NULL, fg = NULL, st = NULL) {
  x <- uj::collapse_dots(...)
  if (!base::is.null(st)) {x <- uj::st(st, x)}
  if (!base::is.null(bg)) {x <- uj::bg(bg, x)}
  if (!base::is.null(fg)) {x <- uj::fg(fg, x)}
  x
}
