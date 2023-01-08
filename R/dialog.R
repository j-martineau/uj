#' @name dialog
#' @encoding UTF-8
#' @family dialogs
#' @family user
#' @title Dialog boxes using package `svDialogs`
#' @description All functions in this family take `...` arguments, which are \link[=av]{atomized} and collapsed into a prompt to be displayed in a dialog box.
#' \tabular{rl}{ `alert`   \tab Posts an alert to the console. Default style, background color, and foreground color depend on the value of the argument `type`, which can be overridden using the arguments `style`, `fg`, and `bg`.
#'   \cr                   \tab  
#'   \cr   `acknowledge`   \tab Posts an alert, then posts a message requesting acknowledgement, After user acknowledges, launches file selection dialog box.
#'   \cr                   \tab  
#'   \cr    `choose_doc`   \tab Posts an acknowledgement alert. After user acknowledges, launches file selection dialog box.
#'   \cr                   \tab  
#'   \cr    `choose_dir`   \tab Posts an acknowledgement alert. After user acknowledges, launches directory selection dialog box.
#'   \cr                   \tab  
#'   \cr       `choose1`   \tab Posts an alert followed by a message asking user to select `1` option from a list. Stops execution if user select the always-included, always-first option `'{ CANCEL }'`.
#'   \cr                   \tab  
#'   \cr       `choosen`   \tab Posts an alert followed by a message asking user to select from among a list of options. Stops execution if user selects the always-included, always-first option `'{ CANCEL }'`.
#'   \cr                   \tab  
#'   \cr        `asknew`   \tab Posts an alert followed by a message asking user to enter a pipe-separated list of values to replace existing values.
#'   \cr                   \tab  
#'   \cr           `ask`   \tab Posts an alert followed by a message asking user for typed input.
#'   \cr                   \tab  
#'   \cr           `msg`   \tab Posts an alert followed by a message to hit enter to continue.
#'   \cr                   \tab  
#'   \cr           `YES`   \tab Posts an alert followed by a message offering choices  of `'{ CANCEL }'`, `'{ YES }'` and `'{ NO }'`. Stops execution if user selects `'{ CANCEL }'`, returns `TRUE` if user selects `'{ YES }'`, otherwise, returns `FALSE`.
#'   \cr                   \tab  
#'   \cr            `NO`   \tab Posts an alert followed by a message offering choices  of `'{ CANCEL }'`, `'{ YES }'` and `'{ NO }'`. Stops execution if user selects `'{ CANCEL }'`, returns `TRUE` if user selects `'{ NO }'`, otherwise, returns `FALSE`.
#'   \cr                   \tab  
#'   \cr            `OK`   \tab Posts an alert followed by a message offering choices of `'{ CANCEL }'` and `'{ OK }'`. Returns `TRUE` if user selects `{ OK }`, otherwise return `FALSE`.
#'   \cr                   \tab  
#'   \cr        `CANCEL`   \tab Posts an alert followed by a message offering choices of `'{ CANCEL }'` and `'{ OK }'`. Returns `TRUE` if user selects `{ CANCEL }`, otherwise return `FALSE`.
#' }
#' @param ... An arbitrary, optional number of arguments which are \link[=av]{atomized} into a character scalar message to be posted to the console.
#' @param type A character scalar type of alert to post to the console. Predefined alert types are `c('alert', 'error', 'update', 'warning')`. Custom types are also accepted.
#' @param title An optional character scalar alert title to post to the console.
#' @param style An optional character scalar style specification. Valid values are `c('bold', 'italic', 'plain', 'underline')`. Defaults to `'plain'`.
#' @param fg An optional character scalar foreground text color. Valid values are `c('black', 'red', 'green', 'yellow', 'blue', 'magenta', 'cyan', 'white', 'grey')`. Defaults to system default.
#' @param bg An optional character scalar background text color. Valid values are `c('black', 'red', 'green', 'yellow', 'blue', 'magenta', 'cyan', 'white')`. Defaults to system default.
#' @param stop An optional scalar `TRUE` or `FALSE` indicating whether to stop execution. Defaults to `TRUE` for `type = 'error'`, otherwise defaults to `FALSE`.
#' @param options An atomic vector list of options to choose from.
#' @param all Scalar `TRUE` or `FALSE` indicating whether to add an `{ ALL }` value to `options`.
#' @param none Scalar `TRUE` or `FALSE` indicating whether to add a `{ NONE }` value to `options` (implying that it is valid to select none).
#' @param n An optional \link[=cmp_psw_scl]{complete positive numeric whole-number scalar} (?cmp_psw_scl) indicating the number of options that must be selected. Must be contained in `1:length(options)`.
#' @param min.n An optional \link[=icmp]{complete} \link[=ipsw]{positive numeric whole-number} \link[=iscl]{scalar} (?icmp, ?ipsw, ?iscl) indicating the minimum number of options that may be selected. Must be `NULL` when `n` is non-`NULL`.
#' @param max.n An optional complete positive numeric whole-number scalar indicating the maximum number of options that may be selected. Must be `NULL` when `n` is non-`NULL`.
#' @param old A \link[=chr_vec]{character vec} of unique values to be replaced.
#' @param unq Scalar `TRUE` or `FALSE` indicating whether replacement values must be unique.
#' @return *A character scalar*
#'  \cr   `choose_dir` (a directory path)
#'  \cr   `choose_doc` (a document path)
#'  \cr   `ask`
#'  \cr
#'  \cr *A character vector*
#'  \cr   `asknew`
#'  \cr
#'  \cr *An atomic vector*
#'  \cr   `chooseN`
#'  \cr
#'  \cr *An atomic scalar*
#'  \cr   `choose1`
#'  \cr
#'  \cr *A logical scalar*
#'  \cr   `CANCEL`
#'  \cr   `YES`
#'  \cr   `NO`
#'  \cr   `OK`
#'  \cr
#'  \cr `NULL`
#'  \cr   `acknowledge`
#'  \cr   `alert`
#' @export
alert <- function(..., type = "alert", title = NULL, style = NULL, fg = NULL, bg = NULL, stop = NULL) {
  type  <- uj::f0(uj::cmp_chr_scl(type), base::toupper(type), "UPDATE")
  type  <- base::trimws(type, which = "both")
  type  <- uj::f0(uj::isEQ(type, ""), "UPDATE", type)
  title <- base::trimws(base::paste0(uj::av(title), collapse = ""), which = "both")
  title <- uj::f0(title == "", "", base::paste0(": ", title))
  title <- uj::p0(" ", type, title, " ")
  sts   <- base::c("bold", "italic", "plain", "underline")
  fgs   <- base::c("black", "red", "green", "yellow", "blue", "magenta", "cyan", "white", "grey")
  bgs   <- base::c("black", "red", "green", "yellow", "blue", "magenta", "cyan", "white")
  style <- uj::f0(uj::isIN(style, sts), style, uj::f0(type == "UPDATE", "plain", uj::f0(type == "ALERT", "italic", uj::f0(type == "WARNING", "underline", uj::f0(type == "ERROR", "bold", "plain")))))
  fg    <- uj::f0(uj::isIN(fg, fgs), fg, uj::f0(type == "UPDATE", "black", uj::f0(type == "ALERT", "red", uj::f0(type == "WARNING", "red", uj::f0(type == "ERROR", "yellow", "grey")))))
  bg    <- uj::f0(uj::isIN(bg, bgs), bg, uj::f0(type == "UPDATE", "grey", uj::f0(type == "ALERT", "white", uj::f0(type == "WARNING", "yellow", uj::f0(type == "ERROR", "red", "black")))))
  stop  <- uj::f0(base::isTRUE(stop) | base::isFALSE(stop), stop, type == "ERROR")
  style <- uj::f0(style == "plain", "reset", style)
  fg    <- uj::f0(fg == "grey", "silver", fg)
  bg    <- uj::p0("bg", base::toupper(base::substr(bg, 1, 1)), base::substr(bg, 2, base::nchar(bg)))
  mssg  <- base::paste0(uj::av(...), collapse = "")
  mssg  <- uj::f0(mssg == "", "\n", base::paste0("\n", mssg, "\n\n"))
  code1 <- uj::p0("crayon::", style, "(crayon::", bg, "(crayon::", fg, "(mssg )))")
  code2 <- uj::p0("crayon::", style, "(crayon::", bg, "(crayon::", fg, "(title)))")
  mssg  <- uj::run(code1)
  title <- uj::run(code2)
  base::cat(title)
  base::cat(mssg)
  base::cat("\n")
  if (stop) {stop("", call. = F)}
}

#' @rdname dialog
#' @export
acknowledge <- function(..., style = NULL, bg = NULL, fg = NULL) {
  uj::alert(..., title = "Acknowledgement required", style = style, bg = bg, fg = fg)
  base::cat(crayon::bgWhite(crayon::red("Hit [return] or [enter] to continue: ")))
  base::readline()
  base::cat("\n")
}

#' @rdname dialog
#' @export
choose1 <- function(options, ...) {
  question <- base::paste0(uj::av(...), collapse = "")
  question <- uj::p0(uj::f0(question == "", "Select an option:", question))
  errs <- uj::f0(uj::unq_atm_vec(options), NULL, "[options] must be a unique atomic vec (?unq_atm_vec).")
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  uj::alert(question, title = "Response required")
  char.options <- base::c("{ CANCEL }", base::as.character(options))
  answer <- svDialogs::dlg_list(char.options)$res
  uj::xwarn()
  if (base::length(answer) == 0 | uj::isEQ(answer, "{ CANCEL }")) {uj::alert(type = "warning", title = "Action canceled", stop = T)}
  options[base::as.character(options) == answer]
}

#' @rdname dialog
#' @export
chooseN <- function(options, ..., all = TRUE, none = FALSE, n = NULL, min.n = NULL, max.n = NULL) {
  question <- base::paste0(uj::av(...), collapse = "")
  question <- uj::f0(question == "", "Choose from among the options listed below.", question)
  errs <- base::c(
    uj::f0(uj::unq_atm_mvc(options        ), NULL, "[options] must be a unique atomic multivec (?unq_atm_mvec)."                    ),
    uj::f0(question != ""                  , NULL, "[...] must contain atomic values upon atomization (?av) to form a question."    ),
    uj::f0(uj::cmp_lgl_scl(all            ), NULL, "[all] must be TRUE or FALSE."                                                   ),
    uj::f0(uj::cmp_lgl_scl(none           ), NULL, "[none] must be TRUE or FALSE."                                                  ),
    uj::f0(uj::nll_or(n    , "cmp_psw_scl"), NULL, "[n] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl)."    ),
    uj::f0(uj::nll_or(min.n, "cmp_psw_scl"), NULL, "[min.n] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl)."),
    uj::f0(uj::nll_or(max.n, "cmp_psw_scl"), NULL, "[max.n] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")
  )
  if (!base::is.null(errs)) {stop(.errs(errs))}
  options <- options
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
  errs <- base::c(
    uj::f0(ok.lo, NULL, "[n] is out of bounds // min(n) < 1."),
    uj::f0(ok.hi, NULL, "[n] is out of bounds // max(n) > length(options)."),
    uj::f0(ok.min, NULL, "[min.n] is out of bounds // min.n > length(options)."),
    uj::f0(ok.max, NULL, "[max.n] is out of bounds // max.n > length(options)."),
    uj::f0(ok.combo, NULL, "When [n] is supplied, [min.n] and [max.n] must be NULL."),
    uj::f0(ok.min.max, NULL, "[min.n] and [max.n] are inconsistent // min.n > max.n." )
  )
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  min.n <- uj::f0(def.n, n, uj::f0(def.min, min.n, uj::f0(none, 0, 1)))
  max.n <- uj::f0(def.n, n, uj::f0(def.max, max.n, n.opt))
  n <- min.n:max.n
  infix <- uj::f0(min.n == max.n, uj::p0("exactly ", max.n), uj::p0("from ", min.n, " to ", max.n))
  all <- base::length(options) %in% n
  none <- 0 %in% n
  char.options <- base::c("{ CANCEL }", base::as.character(options), uj::f0(all, "{ ALL }", NULL), uj::f0(none, "{ NONE }", NULL))
  question <- base::paste0(question, "\n(choose ", infix, " options)\n\n")
  uj::alert(title = "Response required")
  answer <- base::unique(svDialogs::dlg_list(char.options, title = question, multiple = T)$res)
  uj::xwarn()
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
NO <- function(...) {
  answer <- uj::choose1(base::c("{ YES }", "{ NO }"), ...)
  if      (uj::isEQ(answer, "{ YES }")) {F}
  else if (uj::isEQ(answer, "{ NO }" )) {T}
  else    {uj::alert(type = "warning", title = "Action canceled", stop = T)}
}

#' @rdname dialog
#' @export
YES <- function(...) {
  answer <- uj::choose1(base::c("{ YES }", "{ NO }"), ...)
  if      (uj::isEQ(answer, "{ YES }")) {T}
  else if (uj::isEQ(answer, "{ NO }" )) {F}
  else    {uj::alert(type = "warning", title = "Action canceled", stop = T)}
}

#' @rdname dialog
#' @export
OK <- function(...) {uj::isEQ(uj::choose1("{ OK }", ...), "{ OK }")}

#' @rdname dialog
#' @export
CANCEL <- function(...) {uj::isEQ(uj::choose1("{ OK }", ...), "{ CANCEL }")}

#' @rdname dialog
#' @export
ask <- function(...) {
  uj::alert(title = "Response required")
  base::cat(base::paste0(base::c(uj::av(...), "\n"), collapse = ""))
  base::cat(crayon::bold(crayon::bgWhite(crayon::red("\nEnter your response: "))))
  answer <- base::readline()
  uj::xwarn()
  if (base::length(answer) == 0) {uj::alert(type = "warning", title = "Action canceled", stop = T)}
  answer
}

#' @rdname dialog
#' @export
asknew <- function(old, unq = TRUE) {
  errs <- base::c(uj::f0(uj::unq_atm_mvc(old), NULL, "[old] must be a unique atomic multivec (?unq_atm_mvec)."),
                  uj::f0(uj::isTF(unq)       , NULL, "[unq] must be TRUE or FALSE."                           ))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  list <- base::paste0(old, collapse = " | ")
  uj::alert(title = "Response required")
  question <- uj::spf("Enter a pipe-separated list of %d replacement values for the following pipe-separated original values:\n\n%s\n\n", length(old), list)
  base::cat(question)
  base::cat(crayon::bold(crayon::bgWhite(crayon::red("Enter your response: "))))
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
choose_dir <- function(dir.type = "directory") {
  if (!uj::cmp_chr_scl(dir.type)) {stop(uj:::.errs("[dir.type] must be a complete character scalar (?cmp_chr_scl)"))}
  uj::acknowledge("In the next dialog box, select a ", dir.type, ".")
  path <- svDialogs::dlg_dir(title = uj::p0("Select a ", dir.type, ":"))$res
  if (base::length(path) == 0) {uj::alert(type = "warning", "Action canceled", stop = T)}
  uj::xwarn()
  path
}

#' @rdname dialog
#' @export
choose_doc <- function(doc.type = "document") {
  if (!uj::cmp_chr_scl(doc.type)) {stop(uj:::.errs("[doc.type] must be a complete character scalar (?cmp_chr_scl)"))}
  uj::acknowledge("In the next dialog box, select a ", doc.type, ".")
  path <- svDialogs::dlg_open(title = uj::p0("Select a ", doc.type, ":"))$res
  if (base::length(path) == 0) {uj::alert(type = "warning", title = "Action canceled", stop = T)}
  uj::xwarn()
  path
}
