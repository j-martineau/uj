#' @name dialog
#' @encoding UTF-8
#' @family dialogs
#' @family user
#' @title Dialog with users using package `svDialogs`
#' @description All functions \link[=collapse_dots]{collapses} `...` args into a prompt. Each posts an alert to the console, posts the prompt (if any), followed by the actions detailed in the table below.
#' \tabular{rl}{
#'      `acknowledge`   \tab Waits for user to acknowledge.\eqn{^1}
#'   \cr `choose_doc`   \tab Asks user to choose a document.\eqn{^2}
#'   \cr `choose_dir`   \tab Asks user to choose a directory.\eqn{^2}
#'   \cr    `choose1`   \tab Asks user to choose `1` option.\eqn{^2}
#'   \cr    `chooseN`   \tab Asks user to choose `1+` options.\eqn{^{2,3}}
#'   \cr      `alert`   \tab None.\eqn{^1}
#'   \cr        `ask`   \tab Asks for typed input.\eqn{^2}
#'   \cr     `asknew`   \tab Asks for a list of new values.\eqn{^2}
#'   \cr    `YES, NO`   \tab Offers `YES/NO/CANCEL`.\eqn{^{2,4}}
#'   \cr `OK, CANCEL`   \tab Offers `OK/CANCEL`.\eqn{^4}
#' }
#' ` `\eqn{^{1.}} Default style, bg color, and fg color depend on `type`.
#' \cr ` `\eqn{^{2.}} Stops execution if user chooses `CANCEL`.
#' \cr ` `\eqn{^{3.}} Stops execution if `stop | (type = 'error' & is.null(stop))`.
#' \cr ` `\eqn{^{4.}} Returns `TRUE` if user choice matches the function name.
#' @param ... An arbitrary, optional number of arguments which are \link[=av]{atomized} into a character scalar message to be posted to the console.
#' @param type A character scalar type of alert to post to the console. Predefined alert types are `c('alert', 'error', 'update', 'warning')`. Custom types are also accepted, but they should be short to avoid formatting problems.
#' @param title `NULL` or a character scalar alert title to post to the console. Should be relatively short to avoid formatting problems.
#' @param st `NULL` or a character scalar alert style specification from \code{link{st_vals}()}.
#' @param fg `NULL` or a character scalar alert foreground text color specification from \code{\link{fg_vals}()}
#' @param bg `NULL` or an character scalar alert background text color specification from \code{\link{bg_vals}()}
#' @param ST `NULL` or a character scalar message style specification from \code{link{st_vals}()}.
#' @param FG `NULL` or a character scalar message foreground text color specification from \code{\link{fg_vals}()}
#' @param BG `NULL` or an character scalar message background text color specification from \code{\link{bg_vals}()}
#' @param stop An optional scalar `TRUE` or `FALSE` indicating whether to stop execution. Defaults to `TRUE` for `type = 'error'`, otherwise defaults to `FALSE`.
#' @param options An atomic vector list of options to choose from.
#' @param all Scalar `TRUE` or `FALSE` indicating whether to add an `{ ALL }` value to `options`.
#' @param none Scalar `TRUE` or `FALSE` indicating whether to add a `{ NONE }` value to `options` (implying that it is valid to select none).
#' @param n An optional \link[=cmp_psw_scl]{complete positive numeric whole-number scalar} (?cmp_psw_scl) indicating the number of options that must be selected. Must be contained in `1:length(options)`.
#' @param min.n An optional \link[=icmp]{complete} \link[=ipsw]{positive numeric whole-number} \link[=iscl]{scalar} (?icmp, ?ipsw, ?iscl) indicating the minimum number of options that may be selected. Must be `NULL` when `n` is non-`NULL`.
#' @param max.n An optional complete positive numeric whole-number scalar indicating the maximum number of options that may be selected. Must be `NULL` when `n` is non-`NULL`.
#' @param old A \link[=chr_vec]{character vec} of unique values to be replaced.
#' @param unq Scalar `TRUE` or `FALSE` indicating whether replacement values must be unique.
#' @param blank A character scalar containing a default message if \link[=av]{atomizing} and collapsing `...` to a character scalar results in a blank string (`""`).
#' @return *A character scalar* \cr   `choose_dir` (a directory path) \cr   `choose_doc` (a document path) \cr   `ask`
#'  \cr\cr *A character vector* \cr   `asknew`
#'  \cr\cr *An atomic vector* \cr   `chooseN`
#'  \cr\cr *An atomic scalar* \cr   `choose1`
#'  \cr\cr *A logical scalar* \cr   `CANCEL, YES, NO, OK`
#'  \cr\cr `NULL` \cr   `acknowledge, alert`
#' @export
alert <- function(..., type = "alert", title = NULL, st = NULL, fg = NULL, bg = NULL, ST = NULL, FG = NULL, BG = NULL, stop = NULL, blank = "") {
  tl <- function(x) {uj::f0(uj::cmp_str_scl(x), base::tolower(base::trimws(x, which = "both")), x)}
  type <- base::toupper(tl(type))
  title <- tl(title)
  st <- tl(st); ST <- tl(ST)
  fg <- tl(fg); FG <- tl(FG)
  bg <- tl(bg); BG <- tl(BG)
  sts <- uj::st_vals()
  bgs <- uj::bg_vals()
  fgs <- uj::fg_vals()
  css <- "cmp_str_scl"
  cls <- "cmp_lgl_scl"
  tf <- c(T, F)
  errs  <- base::c(uj::f0(uj::cmp_str_scl(type    ), NULL, "[type] must be a complete charastring (?cmp_chr_str)."),
                   uj::f0(uj::nll_or(title, css   ), NULL, "[title] must be NULL or a complete string (?cmp_chr_str)."),
                   uj::f0(uj::nll_or(st, css, sts ), NULL, "[st] must be NULL or a character scalar from uj::st_vals()."),
                   uj::f0(uj::nll_or(bg, css, bgs ), NULL, "[bg] must be NULL or a character scalar from uj::bg_vals()."),
                   uj::f0(uj::nll_or(fg, css, fgs ), NULL, "[fg] must be NULL or a character scalar from uj::fg_vals()."),
                   uj::f0(uj::nll_or(ST, css, sts ), NULL, "[ST] must be NULL or a character scalar from uj::st_vals()."),
                   uj::f0(uj::nll_or(BG, css, bgs ), NULL, "[BG] must be NULL or a character scalar from uj::bg_vals()."),
                   uj::f0(uj::nll_or(FG, css, fgs ), NULL, "[FG] must be NULL or a character scalar from uj::fg_vals()."),
                   uj::f0(uj::nll_or(stop, cls, tf), NULL, "[stop] must be NULL, scalar TRUE, or scalar FALSE."))
  if (!is.null(errs)) {base::stop(uj::format_errs(pkg = "uj", errs))}
  title <- base::trimws(base::paste0(uj::av(title), collapse = ""), which = "both")
  title <- uj::f0(title == "", "", base::paste0(": ", title))
  title <- uj::p0(" ", type, title, " ")
  spc <- base::gsub(" ", " ", uj::spaces(base::nchar(title)), fixed = T)
  st <- uj::f0(uj::isIN(st, sts), st, uj::f0(type == "UPDATE", "pln", uj::f0(type == "ALERT", "itl", uj::f0(type == "WARNING", "und", uj::f0(type == "ERROR", "bld", "pln")))))
  bg <- uj::f0(uj::isIN(bg, bgs), bg, uj::f0(type == "UPDATE", "blk", uj::f0(type == "ALERT", "red", uj::f0(type == "WARNING", "red", uj::f0(type == "ERROR", "ylw", "gry")))))
  fg <- uj::f0(uj::isIN(fg, fgs), fg, uj::f0(type == "UPDATE", "gry", uj::f0(type == "ALERT", "wht", uj::f0(type == "WARNING", "ylw", uj::f0(type == "ERROR", "red", "blk")))))
  stop <- uj::f0(base::is.null(stop), type == "error", stop)
  buffer <- uj::bg(bg, spc)
  title <- uj::txt(title, bg = bg, fg = fg, st = st)
  mssg <- uj::txt(uj::collapse_dots(..., blank. = blank), bg = BG, fg = FG, st = ST)
  base::cat(base::paste0("\n", buffer, "\n", title, "\n", buffer, "\n\n", uj::f0(mssg == "", "", base::paste0(mssg, "\n\n"))))
  if (stop) {stop("", call. = F)}
}

#' @rdname dialog
#' @export
acknowledge <- function(..., st = NULL, bg = NULL, fg = NULL, ST = NULL, BG = NULL, FG = NULL) {
  uj::alert(..., title = "Acknowledgement required", st = st, bg = bg, fg = fg, ST = ST, BG = BG, FG = FG)
  base::cat(uj::bg("w", uj::fg("r", "Hit [return] or [enter] to continue: ")))
  base::readline()
}

#' @rdname dialog
#' @export
choose1 <- function(options, ..., st = NULL, bg = NULL, fg = NULL, ST = NULL, BG = NULL, FG = NULL) {
  if (!uj::unq_atm_vec(options)) {base::stop(uj::format_errs(pkg = "uj", "[options] must be a unique atomic vec (?unq_atm_vec)."))}
  uj::alert(..., title = "Response required", blank = "Select an option", st = st, bg = bg, fg = fg, ST = ST, BG = BG, FG = FG)
  char.options <- base::c("{ CANCEL }", base::as.character(options))
  answer <- svDialogs::dlg_list(char.options)$res
  if (base::length(answer) == 0 | uj::isEQ(answer, "{ CANCEL }")) {uj::alert(type = "warning", title = "Action canceled", stop = T)}
  options[base::as.character(options) == answer]
}

#' @rdname dialog
#' @export
chooseN <- function(options, ..., all = TRUE, none = FALSE, n = NULL, min.n = NULL, max.n = NULL, st = NULL, bg = NULL, fg = NULL, ST = NULL, BG = NULL, FG = NULL) {
  errs <- base::c(uj::f0(uj::unq_atm_mvc(options        ), NULL, "[options] must be a unique atomic multivec (?unq_atm_mvec)."                    ),
                  uj::f0(uj::cmp_lgl_scl(all            ), NULL, "[all] must be TRUE or FALSE."                                                   ),
                  uj::f0(uj::cmp_lgl_scl(none           ), NULL, "[none] must be TRUE or FALSE."                                                  ),
                  uj::f0(uj::nll_or(n    , "cmp_psw_scl"), NULL, "[n] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl)."    ),
                  uj::f0(uj::nll_or(min.n, "cmp_psw_scl"), NULL, "[min.n] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl)."),
                  uj::f0(uj::nll_or(max.n, "cmp_psw_scl"), NULL, "[max.n] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl)."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  mssg <- uj::collapse_dots(...)
  n.opt <- base::length(options)
  def.n <- !base::is.null(n)
  def.min <- !base::is.null(min.n)
  def.max <- !base::is.null(max.n)
  ok.lo <- uj::f0(!def.n, T, n >= 1)
  ok.hi <- uj::f0(!def.n, T, n <= n.opt)
  ok.min <- uj::f0(!def.min, T, min.n <= n.opt)
  ok.max <- uj::f0(!def.max, T, max.n <= n.opt)
  ok.combo <- (!def.min & !def.max) | !def.n
  ok.min.max <- uj::f0(!def.min | !def.max, T, min.n <= max.n)
  errs <- base::c(uj::f0(ok.lo, NULL, "[n] is out of bounds ==> min(n) < 1."),
                  uj::f0(ok.hi, NULL, "[n] is out of bounds ==> max(n) > length(options)."),
                  uj::f0(ok.min, NULL, "[min.n] is out of bounds ==> min.n > length(options)."),
                  uj::f0(ok.max, NULL, "[max.n] is out of bounds ==> max.n > length(options)."),
                  uj::f0(ok.combo, NULL, "When [n] is supplied, [min.n] and [max.n] must be NULL."),
                  uj::f0(ok.min.max, NULL, "[min.n] and [max.n] are inconsistent ==> min.n > max.n."))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  min.n <- uj::f0(def.n, n, uj::f0(def.min, min.n, uj::f0(none, 0, 1)))
  max.n <- uj::f0(def.n, n, uj::f0(def.max, max.n, n.opt))
  n <- min.n:max.n
  infix <- uj::f0(min.n == max.n, uj::p0("exactly ", max.n), base::paste0("from ", min.n, " to ", max.n))
  all <- base::length(options) %in% n
  none <- 0 %in% n
  char.options <- base::c("{ CANCEL }", base::as.character(options), uj::f0(all, "{ ALL }", NULL), uj::f0(none, "{ NONE }", NULL))
  mssg <- base::paste0(mssg, uj::f0(mssg == "", "", "\n\n"), "CHOOSE ", infix, " OPTIONS")
  uj::alert(mssg, title = "Response required", st = st, bg = bg, fg = fg, ST = ST, BG = BG, FG = FG)
  answer <- base::unique(svDialogs::dlg_list(char.options, title = "", multiple = T)$res)
  if (uj::isEQ(answer, "{ CANCEL }")) {uj::alert(type = "warning", title = "Action canceled", stop = T)}
  answer <- answer[answer != "{ CANCEL }"]
  if (none & base::length(answer) == 0 | uj::isEQ(answer, "{ NONE }")) {return(NULL)}
  if (uj::isEQ("{ ALL }", answer)) {return(options[!(options == "{ CANCEL }")])}
  else if (uj::anyIN(answer, c("{ ALL }", "{ NONE }"))) {uj::alert(type = "error", title = "Invalid selection")}
  else if (!(base::length(answer) %in% n)) {uj::alert(type = "error", title = "Invalid number of options selected")}
  else {options[base::as.character(options) %in% base::as.character(answer)]}
}

#' @rdname dialog
#' @export
NO <- function(..., st = NULL, bg = NULL, fg = NULL, ST = NULL, BG = NULL, FG = NULL) {
  x <- uj::choose1(base::c("{ YES }", "{ NO }"), ..., st = st, bg = bg, fg = fg, ST = ST, BG = BG, FG = FG)
  uj::f0(uj::isEQ(x, "{ YES }"), F, uj::f0(uj::isEQ(x, "{ NO }"), T, uj::alert(type = "warning", title = "Action canceled", stop = T)))
}

#' @rdname dialog
#' @export
YES <- function(..., st = NULL, bg = NULL, fg = NULL, ST = NULL, BG = NULL, FG = NULL) {
  x <- uj::choose1(base::c("{ YES }", "{ NO }"), ..., st = st, bg = bg, fg = fg, ST = ST, BG = BG, FG = FG)
  uj::f0(uj::isEQ(x, "{ YES }"), T, uj::f0(uj::isEQ(x, "{ NO }"), F, uj::alert(type = "warning", title = "Action canceled", stop = T)))
}

#' @rdname dialog
#' @export
OK <- function(..., st = NULL, bg = NULL, fg = NULL, ST = NULL, BG = NULL, FG = NULL) {uj::isEQ(uj::choose1("{ OK }", ..., st = st, bg = bg, fg = fg, ST = ST, BG = BG, FG = FG), "{ OK }")}

#' @rdname dialog
#' @export
CANCEL <- function(..., st = NULL, bg = NULL, fg = NULL, ST = NULL, BG = NULL, FG = NULL) {uj::isEQ(uj::choose1("{ OK }", ..., st = st, bg = bg, fg = fg, ST = ST, BG = BG, FG = FG), "{ CANCEL }")}

#' @rdname dialog
#' @export
ask <- function(..., st = NULL, bg = NULL, fg = NULL, ST = NULL, BG = NULL, FG = NULL) {
  mssg <- base::trimws(base::paste0(uj::av(...), collapse = ""), which = "both")
  uj::alert(mssg, title = "Response required", st = st, bg = bg, fg = fg, ST = ST, BG = BG, FG = FG)
  base::cat("\n\n")
  base::cat(crayon::bold(crayon::bgWhite(crayon::red("Enter your response: "))))
  answer <- base::readline()
  if (base::length(answer) == 0) {uj::alert(type = "warning", title = "Action canceled", stop = T)}
  answer
}

#' @rdname dialog
#' @export
asknew <- function(old, unq = TRUE, st = NULL, bg = NULL, fg = NULL, ST = NULL, BG = NULL, FG = NULL) {
  errs <- base::c(uj::f0(uj::unq_atm_mvc(old), NULL, "[old] must be a unique atomic multivec (?unq_atm_mvec)."),
                  uj::f0(uj::isTF(unq)       , NULL, "[unq] must be TRUE or FALSE."                           ))
  if (!base::is.null(errs)) {stop(uj::format_errs(pkg = "uj", errs))}
  list <- base::paste0(old, collapse = " | ")
  question <- uj::spf("Enter a pipe-separated list of %d replacement values for the following pipe-separated original values:\n\n%s\n\n", base::length(old), list)
  uj::alert(question, title = "Response required", st = st, bg = bg, fg = fg, ST = ST, BG = BG, FG = FG)
  base::cat(uj::bg_red(uj::fg_ylw(uj::st_bld("Enter your response: "))))
  answer <- base::readline()
  answer <- uj::av(base::strsplit(answer, "|", fixed = T))
  answer <- base::trimws(answer, which = "both")
  answer <- answer[answer != ""]
  n.ans <- base::length(answer)
  if (n.ans == base::length(old)) {uj::f0(!unq | uj::unq_vec(answer), answer, uj::alert(type = "error", title = "Replacement values are not unique"))}
  else if (n.ans == 0) {uj::alert(type = "warning", title = "Action canceled", stop = T)}
  else {uj::alert(type = "error", title = "Numbers of old and new values do not match", stop = T)}
}

#' @rdname dialog
#' @export
choose_dir <- function(dir.type = "directory", st = NULL, bg = NULL, fg = NULL, ST = NULL, BG = NULL, FG = NULL) {
  if (!uj::cmp_chr_scl(dir.type)) {stop(uj::format_errs(pkg = "uj", "[dir.type] must be a complete character scalar (?cmp_chr_scl)"))}
  uj::acknowledge("In the next dialog box, select a ", dir.type, ".", st = st, bg = bg, fg = fg, ST = ST, BG = BG, FG = FG)
  path <- svDialogs::dlg_dir(title = uj::p0("Select a ", dir.type, ":"))$res
  if (base::length(path) == 0) {uj::alert(type = "warning", "Action canceled", stop = T)}
  path
}

#' @rdname dialog
#' @export
choose_doc <- function(doc.type = "document", st = NULL, bg = NULL, fg = NULL, ST = NULL, BG = NULL, FG = NULL) {
  if (!uj::cmp_chr_scl(doc.type)) {stop(uj::format_errs(pkg = "uj", "[doc.type] must be a complete character scalar (?cmp_chr_scl)"))}
  uj::acknowledge("In the next dialog box, select a ", doc.type, ".", st = st, bg = bg, fg = fg, ST = ST, BG = BG, FG = FG)
  path <- svDialogs::dlg_open(title = uj::p0("Select a ", doc.type, ":"))$res
  if (base::length(path) == 0) {uj::alert(type = "warning", title = "Action canceled", stop = T)}
  path
}
