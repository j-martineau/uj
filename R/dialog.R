#' @name dialog
#' @encoding UTF-8
#' @family dialogs
#' @family user
#' @title Dialog with users using package `svDialogs`
#' @description All functions \link[=collapse_dots]{collapses} `...` args into a prompt. Each posts an alert to the console, posts the prompt (if any), followed by a specific action.
#' @details
#' \tabular{ll}{  `acknowledge`   \tab Waits for user to acknowledge.\eqn{^{(1)}}        \cr   \tab   \cr
#'                `OK, CANCEL`    \tab Offers `OK/CANCEL`.\eqn{^{(1,4)}}                 \cr   \tab   \cr
#'                `choose_doc`    \tab Asks user to choose a document.\eqn{^{(2)}}       \cr
#'                `choose_dir`    \tab Asks user to choose a directory.\eqn{^{(2)}}      \cr   \tab   \cr
#'                `choose1`       \tab Asks user to choose `1` option.\eqn{^{(1,2)}}     \cr
#'                `chooseN`       \tab Asks user to choose `1+` options.\eqn{^{(1,2,3)}} \cr   \tab   \cr
#'                `YES, NO`       \tab Offers `YES/NO/CANCEL`.\eqn{^{(1,2,4)}}           \cr   \tab   \cr
#'                `ask_new`       \tab Asks for a list of new values.\eqn{^{(1,2)}}      \cr   \tab   \cr
#'                `alert`         \tab None.\eqn{^{(1)}}                                 \cr   \tab   \cr
#'                `ask`           \tab Asks for typed input.\eqn{^{(1,2)}}               \cr   \tab     }
#'  \tabular{l}{  \eqn{^{(1)}} Default style, background color, and foreground color of console text depend on `type`. \cr
#'                \eqn{^{(2)}} Stops execution if user chooses `CANCEL`.                                               \cr
#'                \eqn{^{(3)}} Stops execution if `stop | (type = 'error' & is.null(stop))`.                           \cr
#'                \eqn{^{(4)}} Returns `TRUE` if user choice matches the function name.                                  }
#' @section Specifying formats: When not `NULL` format arguments (`bg.format`, `fg.format`, and `st.format`) must be \link[=cmp_str_vec]{complete string vecs} that when \link[=av]{atomized} and \link[=ssP]{split along pipes} results in a three-element character vector, the first element of which is used to specify \link[=bg]{text background color} and must be a value from \code{\link{bg_vals}()}, the second element of which is used to specify \link[=fg]{text foreground color} and must be a value from \code{\link{fg_vals}()}, and the last of which specifies \link[=st]{text style} and must be a value from \code{\link{st_vals}()}.
#' @param ... An arbitrary, optional number of arguments which are \link[=av]{atomized} into a character scalar message to be posted to the console.
#' @param d A character scalar delimiter for collapsing `...` args into a character scalar.
#' @param n An optional \link[=cmp_psw_scl]{complete positive numeric whole-number scalar} (?cmp_psw_scl) indicating the number of options that must be selected. Must be contained in `1:length(options)`.
#' @param all Scalar `TRUE` or `FALSE` indicating whether to add an `{ ALL }` value to `options`.
#' @param lab A character scalar alert title to post to the console. Should be relatively short to avoid formatting problems.
#' @param old A \link[=chr_vec]{character vec} of unique values to be replaced.
#' @param unq Scalar `TRUE` or `FALSE` indicating whether replacement values must be unique.
#' @param none Scalar `TRUE` or `FALSE` indicating whether to add a `{ NONE }` value to `options` (implying that it is valid to select none).
#' @param type A character scalar type of alert to post to the console. `type = 'error'` results in stopping execution after raising the alert. Predefined alert types are `c('alert', 'error', 'update', 'warning')`. Custom types are also accepted, but they should be short to avoid formatting problems.
#' @param flab A formatting value consistent with the description in the *specifying formats* section giving formatting instructions for the alert title.
#' @param fmsg A formatting value consistent with the description in the *specifying formats* section giving formatting instructions for the alert message.
#' @param ftype A formatting value consistent with the description in the *specifying formats* section giving formatting instructions for the alert type.
#' @param min.n An optional \link[=CMP]{complete} \link[=PSW]{positive numeric whole-number} \link[=SCL]{scalar} indicating the minimum number of options that may be selected. Must be `NULL` when `n` is non-`NULL`.
#' @param max.n An optional complete positive numeric whole-number scalar indicating the maximum number of options that may be selected. Must be `NULL` when `n` is non-`NULL`.
#' @param blank A character scalar containing a default message if \link[=av]{atomizing} and collapsing `...` to a character scalar results in a blank string (`""`).
#' @param options An atomic vector list of options to choose from.
#' @return **The** `NULL` **object** \cr\cr `acknowledge, alert`
#' \cr\cr  **A character scalar**    \cr\cr `choose_dir` (a directory path) \cr `choose_doc` (a document path) \cr `ask`
#' \cr\cr  **A character vector**    \cr\cr `ask_new`
#' \cr\cr  **An atomic vector**      \cr\cr `chooseN`
#' \cr\cr  **An atomic scalar**      \cr\cr `choose1`
#' \cr\cr  **A logical scalar**      \cr\cr `CANCEL, YES, NO, OK`
#' @examples
#' egA <- "two-part"
#' egB <- "message"
#' egFT <- c("yellow", "red", "plain")
#' egFL <- c("blk|wht", "und")
#' egFM <- "b|y|i"
#' egType = "type"
#' egLab = "title"
#' egOpts <- paste("option", letters[1:10])
#' egMsg1 <- "Do you want to continue?"
#' egMsg2 <- "Why do you want to continue?"
#'
#' alert(egA, egB, d = " ")
#' alert(egA, egB, type = egType, d = " ")
#' alert(egA, egB, lab = l, d = " ")
#' alert(egA, egB, type = egType, lab = egLab, d = " ")
#' alert(egA, egB, type = egType, lab = egLab, d = " ", fmsg = egFM)
#' alert(egA, egB, type = egType, lab = egLab, d = " ", flab = egFL)
#' alert(egA, egB, type = egType, lab = egLab, d = " ", ftype = egFT)
#' alert(egA, egB, type = egType, lab = egLab, d = " ", ftype = egFT, flab = egFL, fmsg = egFM)
#' alert(type = egType, lab = egLab, ftype = egFT, flab = egFL)
#' alert(type = egType, ftype = egFT)
#' alert(lab = egLab, flab = egFL)
#' \dontrun{
#'   acknowledge(egA, egB)
#'   choose1(egOpts)
#'   chooseN(egOpts)
#'   chooseN(egOpts, all = T, none = F, min = 6, max = 10)
#'   NO(egMsg1)
#'   OK(egMsg2)
#'   YES(egMsg1)
#'   CANCEL(egMsg1)
#'   ask(Msg2)
#'   ask_new(egOpts)
#'   choose_dir(dir.type = "directory for R scripts")
#'   choose_doc(doc.type = "document to read")
#' }
#' @export
alert <- function(..., type = "alert", lab = "", blank = "", ftype = "r|y|p", flab = "k|y|i", fmsg = "d|d|d", d = " ") {
  lab <- uj:::.chr(lab)
  msg <- uj:::.chr(...)
  type <- base::tolower(uj:::.chr(type))
  flab <- uj::f0(base::is.null(flab), base::c("blk", "ylw", "itl"), uj::f0(uj:::.cmp_chr_vec(flab), uj:::.ssplit(flab), NULL))
  fmsg <- uj::f0(base::is.null(fmsg), base::c("def", "def", "def"), uj::f0(uj:::.cmp_chr_vec(fmsg), uj:::.ssplit(fmsg), NULL))
  ftype <- uj::f0(base::is.null(ftype), base::c("red", "wht", "bld"), uj::f0(uj:::.cmp_chr_vec(ftype), uj:::.ssplit(ftype), NULL))
  blank <- uj:::.chr(blank)
  errs <- NULL
  if (!uj:::.cmp_chr_scl(type)) {errs <- base::c(errs, "[type] must be a complete character scalar (?cmp_chr_scl).")}
  if (!uj:::.cmp_chr_scl(lab)) {errs <- base::c(errs, "[lab] must be a complete character scalar (?cmp_chr_scl).")}
  if (!uj:::.cmp_chr_scl(blank)) {errs <- base::c(errs, "[blank] must be NULL or a complete character scalar (?cmp_chr_scl).")}
  if (!uj:::.ok_fmt(ftype)) {errs <- base::c(errs, "[ftype] must be consistent with the description the [specifying formats] section of the [alert] topic of package [uj] (?uj::alert).")}
  if (!uj:::.ok_fmt(flab)) {errs <- base::c(errs, "[flab] must be consistent with the description the [specifying formats] section of the [alert] topic of package [uj] (?uj::alert).")}
  if (!uj:::.ok_fmt(fmsg)) {errs <- base::c(errs, "[fmsg] must be consistent with the description the [specifying formats] section of the [alert] topic of package [uj] (?uj::alert).")}
  if (!uj:::.cmp_chr_scl(d)) {errs <- base::c(errs, "[d] must be a complete character scalar (?cmp_chr_scl).")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  if (type != "") {type <- uj::txt(base::toupper(type), bg = ftype[1], fg = ftype[2], st = ftype[3])}
  if (lab != "") {lab <- uj::txt(lab, bg = flab[1], fg = flab[2], st = flab[3])}
  if (msg != "") {msg <- uj::txt(msg, bg = fmsg[1], fg = fmsg[2], st = fmsg[3])}
  base::cat(base::paste0("\n", type, uj::f0(type == "", "", "\n"), lab, uj::f0(lab == "", "", "\n"), msg, "\n"))
  if (type == "error") {uj::stopperr("", FUN = uj::caller())}
}

#' @rdname dialog
#' @export
acknowledge <- function(..., ftype = "y|r|p", flab = "k|y|i", fmsg = "d|d|d", d = " ") {
  uj::alert(..., type = "alert", lab = "Acknowledgement required", ftype = ftype, flab = flab, fmsg = fmsg, d = d)
  base::cat(uj::txt("r", "Press [return] or [enter] to continue: ", bg = "yellow", fg = "red", st = "plain"))
  base::readline()
  NULL
}

#' @rdname dialog
#' @export
choose1 <- function(options, ..., ftype = "y|r|p", flab = "k|y|i", fmsg = "d|d|d", d = " ") {
  if (!uj:::.unq_atm_vec(options)) {uj::stopperr("[options] must be a unique atomic vec (?unq_atm_vec).", PKG = "uj")}
  uj::alert(..., title = "Response required", blank = "", ftype = ftype, flab = flab, fmsg = fmsg, d = d)
  char.options <- base::c("{ CANCEL }", uj::as_chr(options))
  answer <- svDialogs::dlg_list(char.options)$res
  if (uj::n0(answer) | uj::is_eq0(answer, "{ CANCEL }")) {uj::stopperr("Action cancelled by user choice.", FUN = uj::caller())}
  options[uj::as_chr(options) == answer]
}

#' @rdname dialog
#' @export
chooseN <- function(options, ..., all = TRUE, none = FALSE, n = NULL, min.n = NULL, max.n = NULL, ftype = "y|r|p", flab = "k|y|i", fmsg = "d|d|d", d = " ") {
  errs <- NULL
  if (!uj:::.unq_atm_mvc(options)) {errs <- base::c(errs, "[options] must be a unique atomic multivec (?unq_atm_mvc).")}
  if (!uj::cmp_lgl_scl(all)) {errs <- base::c(errs, "[all] must be TRUE or FALSE.")}
  if (!uj::cmp_lgl_scl(none)) {errs <- base::c(errs, "[none] must be TRUE or FALSE.")}
  if (!uj::f0(base::is.null(n), T, uj:::.cmp_psw_scl(n))) {errs <- base::c(errs, "[n] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!uj::f0(base::is.null(min.n), T, uj:::.cmp_psw_scl(min.n))) {errs <- base::c(errs, "[min.n] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!uj::f0(base::is.null(max.n), T, uj:::.cmp_psw_scl(max.n))) {errs <- base::c(errs, "[max.n] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  def.min <- !base::is.null(min.n)
  def.max <- !base::is.null(max.n)
  def.n <- !base::is.null(n)
  mssg <- base::paste0(uj::av(...), collapse = d)
  n.opt <- base::length(options)
  ok.lo <- uj::f0(!def.n, T, n >= 1)
  ok.hi <- uj::f0(!def.n, T, n <= n.opt)
  ok.min <- uj::f0(!def.min, T, min.n <= n.opt)
  ok.max <- uj::f0(!def.max, T, max.n <= n.opt)
  ok.combo <- (!def.min & !def.max) | !def.n
  ok.min.max <- uj::f0(!def.min | !def.max, T, min.n <= max.n)
  if (!ok.lo) {errs <- base::c(errs, "[n] is out of bounds ==> min(n) < 1.")}
  if (!ok.hi) {errs <- base::c(errs, "[n] is out of bounds ==> max(n) > length(options).")}
  if (!ok.min) {errs <- base::c(errs, "[min.n] is out of bounds ==> min.n > length(options).")}
  if (!ok.max) {errs <- base::c(errs, "[max.n] is out of bounds ==> max.n > length(options).")}
  if (!ok.combo) {errs <- base::c(errs, "When [n] is supplied, [min.n] and [max.n] must be NULL.")}
  if (!ok.min.max) {errs <- base::c(errs, "[min.n] and [max.n] are inconsistent ==> min.n > max.n.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  min.n <- uj::f0(def.n, n, uj::f0(def.min, min.n, uj::f0(none, 0, 1)))
  max.n <- uj::f0(def.n, n, uj::f0(def.max, max.n, n.opt))
  n <- min.n:max.n
  infix <- uj::f0(min.n == max.n, uj::p0("exactly ", max.n), uj::p0("from ", min.n, " to ", max.n))
  all <- all | base::length(options) %in% n
  none <- none | 0 %in% n
  char.options <- base::c("{ CANCEL }", base::as.character(options), uj::f0(all, "{ ALL }", NULL), uj::f0(none, "{ NONE }", NULL))
  mssg <- base::paste0(mssg, uj::f0(mssg == "", "", "\n\n"), "CHOOSE ", infix, " OPTIONS")
  uj::alert(mssg, title = "Response required", ftype = ftype, flab = flab, fmsg = fmsg, d = d)
  answer <- uj::uv(svDialogs::dlg_list(char.options, title = "", multiple = T)$res)
  if (base::length(answer) == 0 & !none) {uj::stopperr("Action cancelled by user choice.", FUN = uj::caller())}
  if (base::length(answer) == 1) {if (answer == "{ CANCEL }") {uj::stopperr("Action cancelled by user choice.", FUN = uj::caller())}}
  answer <- answer[answer != "{ CANCEL }"]
  if (none & base::length(answer) == 0) {NULL}
  else if (none & base::all(answer == "{ NONE }")) {NULL}
  else if (base::all(answer == "{ ALL }")) {options[!(options == "{ CANCEL }")]}
  else if (base::any(answer %in% base::c("{ ALL }", "{ NONE }"))) {uj::stopperr("Invalid selection", FUN = uj::caller())}
  else if (!(base::length(answer) %in% n)) {uj::stopperr("Invalid number of options selected", FUN = uj::caller())}
  else {options[base::as.character(options) %in% base::as.character(answer)]}
}

#' @rdname dialog
#' @export
NO <- function(..., ftype = "y|r|p", flab = "k|y|i", fmsg = "d|d|d", d = " ") {
  x <- uj::choose1(base::c("{ YES }", "{ NO }"), ..., ftype = ftype, flab = flab, fmsg = fmsg, d = d)
  uj::f0(x == "{ YES }", F, uj::f0(x == "{ NO }", T, uj::stopperr("Action canceled by user choice.", FUN = uj::caller())))
}

#' @rdname dialog
#' @export
YES <- function(..., ftype = "y|r|p", flab = "k|y|i", fmsg = "d|d|d", d = " ") {
  x <- uj::choose1(base::c("{ YES }", "{ NO }"), ..., ftype = ftype, flab = flab, fmsg = fmsg, d = d)
  uj::f0(x == "{ YES }", T, uj::f0(x == "{ NO }", F, uj::stopperr("Action canceled by user choice.", FUN = uj::caller())))
}

#' @rdname dialog
#' @export
OK <- function(..., ftype = "y|r|p", flab = "k|y|i", fmsg = "d|d|d", d = " ") {uj::choose1("{ OK }", ..., ftype = ftype, flab = flab, fmsg = fmsg, d = d) == "{ OK }"}

#' @rdname dialog
#' @export
CANCEL <- function(..., ftype = "y|r|p", flab = "k|y|i", fmsg = "d|d|d", d = " ") {uj::choose1("{ OK }", ..., ftype = ftype, flab = flab, fmsg = fmsg, d = d) == "{ CANCEL }"}

#' @rdname dialog
#' @export
ask <- function(..., ftype = "y|r|p", flab = "k|y|i", fmsg = "d|d|d", d = " ") {
  mssg <- base::trimws(uj::g0(uj::av(...)), which = "both")
  uj::alert(mssg, title = "Response required", ftype = ftype, flab = flab, fmsg = fmsg, d = d)
  base::cat("\n\n")
  base::cat(crayon::bold(crayon::bgWhite(crayon::red("Enter your response: "))))
  answer <- base::readline()
  uj::f0(base::length(answer) == 0, uj::stopperr("Action canceled by user choice.", FUN = uj::caller()), answer)
}

#' @rdname dialog
#' @export
ask_new <- function(old, unq = TRUE, ftype = "y|r|p", flab = "k|y|i", fmsg = "d|d|d", d = " ") {
  errs <- NULL
  if (!uj:::.unq_atm_vec(old)) {errs <- base::c(errs, "[old] must be a unique atomic vec (?unq_atm_vec).")}
  if (!uj:::.cmp_lgl_scl(unq)) {errs <- base::c(errs, "[unq] must be TRUE or FALSE.")}
  if (!base::is.null(errs)) {uj::stopperr(errs, PKG = "uj")}
  list <- paste0(old, collapse = " | ")
  question <- base::sprintf("Enter a pipe-separated list of %d replacement values for the following pipe-separated original values:\n\n%s\n\n", base::length(old), list)
  uj::alert(question, title = "Response required", ftype = ftype, flab = flab, fmsg = fmsg, d = d)
  base::cat(uj::bg_red(uj::fg_ylw(uj::st_bld("Enter your response: "))))
  answer <- base::readline()
  answer <- uj::av(base::strsplit(answer, "|", fixed = T))
  answer <- base::trimws(answer, which = "both")
  answer <- answer[answer != ""]
  n.ans <- uj::N(answer)
  if (n.ans == 0) {uj::stopperr("Action cancelled by user", FUN = uj::caller())}
  if (n.ans != uj::N(old)) {uj::stopperr("Numbers of old and new values do not match", FUN = uj::caller())}
  if (unq & !uj::unq_vec(answer)) {uj::stopperr("Replacement values are not unique", FUN = uj::caller())}
  answer
}

#' @rdname dialog
#' @export
choose_dir <- function(dir.type = "directory", ftype = "y|r|p", flab = "k|y|i", fmsg = "d|d|d", d = " ") {
  if (!uj::cmp_chr_scl(dir.type)) {uj::stopperr("[dir.type] must be a complete character scalar (?cmp_chr_scl)", PKG = "uj")}
  uj::acknowledge("In the next dialog box, select a ", dir.type, ".", ftype = ftype, flab = flab, fmsg = fmsg, d = d)
  path <- svDialogs::dlg_dir(title = base::paste0("Select a ", dir.type, ":"))$res
  if (base::length(path) == 0) {uj::stopperr("Action canceled by user choice", FUN = uj::caller())}
  path
}

#' @rdname dialog
#' @export
choose_doc <- function(doc.type = "document", ftype = "y|r|p", flab = "k|y|i", fmsg = "d|d|d", d = " ") {
  if (!uj:::.cmp_chr_scl(doc.type)) {uj::stopperr("[doc.type] must be a complete character scalar (?cmp_chr_scl)", PKG = "uj")}
  uj::acknowledge("In the next dialog box, select a ", doc.type, ".", ftype = ftype, flab = flab, fmsg = fmsg, d = d)
  path <- svDialogs::dlg_open(title = base::paste0("Select a ", doc.type, ":"))$res
  if (base::length(path) == 0) {uj::stopperr("Action canceled by user choice", FUN = uj::caller())}
  path
}
