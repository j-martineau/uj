#' @name dialog
#' @encoding UTF-8
#' @family dialogs
#' @family user
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
#'  \tabular{l}{  \eqn{^{(1)}} Stops execution if user chooses `CANCEL`.                    \cr
#'                \eqn{^{(2)}} Stops execution if `stop | (type = "error" & isnull(stop))`. \cr
#'                \eqn{^{(3)}} Returns `TRUE` if user choice matches the function name.       }
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
#' @param n An optional \link[ppp:cmp_psw_scl]{complete positive numeric whole-number scalar} (?cmp_psw_scl) indicating the number of opts that must be selected. Must be contained in `1:length(opts)`.
#' @param all Scalar `TRUE` or `FALSE` indicating whether to add an `{ ALL }` value to `opts`.
#' @param title A title for the alert (`type = 'error'` results in stopping execution after posting the alert). Should be short to avoid formatting problems.
#' @param sub A character scalar alert subtitle to post to the console on the line following the title. Should be short to avoid formatting problems.
#' @param ps A character scalar suffix to post to the console on the line following the alert message contained in `...`.
#' @param none Scalar `TRUE` or `FALSE` indicating whether to add a `{ NONE }` value to `opts` (implying that it is valid to select none).
#' @param ft,fs,fm,fp Formatting values consistent with the description in the *specifying formats* section giving formatting instructions for, respectively, the title (in `title`), subtitle (in `sub`), message (in `...`), and postscript (in `ps`).
#' @param min An optional \link[ppp:CMP]{complete} \link[ppp:PSW]{positive numeric whole-number} \link[ppp:SCL]{scalar} indicating the minimum number of opts that may be selected. Must be `NULL` when `n` is non-`NULL`.
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
#'
#'   ## because formatting doesn't show up in help viewer examples output
#'
#'   egA    <- "two-part"
#'   egB    <- "message"
#'
#'   egFT   <- c("yellow", "red", "plain")
#'   egFS   <- c("blk|wht", "und")
#'   egFM   <- "b|y|i"
#'
#'   egT    <- "Title"
#'   egS    <- "Subtitle"
#'
#'   egOpts <- paste("option", letters[1:10])
#'
#'   egMsg1 <- "Do you want to continue?"
#'   egMsg2 <- "Why do you want to continue?"
#'
#'   alert(egA, egB, d = " ")
#'   alert(egA, egB, title = egTitle, d = " ")
#'   alert(egA, egB, sub = l, d = " ")
#'   alert(egA, egB, title = egTitle, sub = egSub, d = " ")
#'   alert(egA, egB, title = egTitle, sub = egSub, d = " ", fm = egFM)
#'   alert(egA, egB, title = egTitle, sub = egSub, d = " ", fs = egFS)
#'   alert(egA, egB, title = egTitle, sub = egSub, d = " ", ft = egFT)
#'   alert(egA, egB, title = egTitle, sub = egSub, d = " ", ft = egFT, fs = egFS, fm = egFM)
#'   alert(title = egT, sub = egSub, ft = egFT, fs = egFS)
#'   alert(title = egT, sub = egS)
#'   alert(sub = egS, fs = egFS)
#'
#'   acknowledge(egA, egB)
#'
#'   choose1(egOpts)
#'   chooseN(egOpts)
#'   chooseN(egOpts, all = T, none = F, min = 6, max = 10)
#'
#'   NO(egMsg1)
#'   OK(egMsg2)
#'   YES(egMsg1)
#'   CANCEL(egMsg1)
#'
#'   ask(Msg2)
#'
#'   ask_new(egOpts)
#'
#'   choose_dir(dir.type = "directory for R scripts")
#'
#'   choose_doc(doc.type = "document to read")
#'
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
  if (!uj::ok_fmt(ft)     ) {errs <- base::c(errs, "[ft] must be consistent with the description the [specifying formats] section of the [alert] topic of package [dlg] (?alert).")}
  if (!uj::ok_fmt(fs)     ) {errs <- base::c(errs, "[fs] must be consistent with the description the [specifying formats] section of the [alert] topic of package [dlg] (?alert).")}
  if (!uj::ok_fmt(fm)     ) {errs <- base::c(errs, "[fm] must be consistent with the description the [specifying formats] section of the [alert] topic of package [dlg] (?alert).")}
  if (!uj::ok_fmt(fp)     ) {errs <- base::c(errs, "[fp] must be consistent with the description the [specifying formats] section of the [alert] topic of package [dlg] (?alert).")}
  if (!uj::.cmp_chr_scl(d)) {errs <- base::c(errs, "[d] must be a complete character scalar (?cmp_chr_scl).")}
  if (!base::is.null(errs)) {uj::stopperr(errs, pkg = "dlg")}
  err <- title == "ERROR"
  if (clear) {
    base::gc(verbose = FALSE)
    base::cat("\014")
  }
  if (title != "") {base::cat("\n", fmt(title, ft), "\n", sep = "")}
  if (sub   != "") {base::cat("\n", fmt(sub  , fs), "\n", sep = "")}
  if (msg   != "") {base::cat("\n", fmt(msg  , fm), "\n", sep = "")}
  if (ps    != "") {base::cat("\n", fmt(ps   , fp), "\n", sep = "")} else {cat("\n")}
  if (err        ) {uj::stopperr("", pkg = "dlg")}
}

#' @rdname dialog
#' @export
acknowledge <- function(..., sub = "", ps = "", ft = "r|w|b", fs = "k|y|p", fp = "k|y|i", d = " ", clear = FALSE) {
  uj::alert(..., title = "acknowledgment required", sub = sub, ps = "press [return] or [enter] to continue", ft = ft, fs = fs, fp = fp, d = d, clear = clear)
  base::readline()
  a <- NULL
}

#' @rdname dialog
#' @export
choose1 <- function(opts, ..., ft = "r|w|b", fs = "k|y|p", d = " ", clear = FALSE) {
  if (!uj::unq_atm_vec(opts)) {uj::stopperr("[opts] must be a unique atomic vec (?unq_atm_vec).", pkg = "dlg")}
  msg <- uj::p0(uj::av(...), collapse = d)
  uj::choose_from(opts, msg, F, F, 1, 1, ft, fs, "choose1", uj::callers(), clear)
}

#' @rdname dialog
#' @export
chooseN <- function(opts, ..., n = NULL, min = NULL, max = NULL, all = base::is.null(c(n, max)), none = FALSE, ft = "r|w|b", fs = "k|y|p", d = " ", clear = FALSE) {
  errs <- NULL
  if (!uj::unq_atm_mvc(opts)                                ) {errs <- base::c(errs, "[opts] must be a unique atomic multivec (?unq_atm_mvc).")}
  if (!uj::.cmp_lgl_scl(all)                                ) {errs <- base::c(errs, "[all] must be TRUE or FALSE.")}
  if (!uj::.cmp_lgl_scl(none)                               ) {errs <- base::c(errs, "[none] must be TRUE or FALSE.")}
  if (!uj::f0(base::is.null(n  ), T, uj::.cmp_psw_scl(n  ))) {errs <- base::c(errs, "[n] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!uj::f0(base::is.null(min), T, uj::.cmp_psw_scl(min))) {errs <- base::c(errs, "[min] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!uj::f0(base::is.null(max), T, uj::.cmp_psw_scl(max))) {errs <- base::c(errs, "[max] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!base::is.null(errs)) {uj::stopperr(errs, pkg = "dlg")}
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
  if (!base::is.null(errs)) {uj::stopperr(errs, pkg = "dlg")}
  min <- uj::f0(defN, n, uj::f0(defMin, min, uj::f0(none, 0, 1)))
  max <- uj::f0(defN, n, uj::f0(defMax, max, nOpts))
  uj::choose_from(opts, msg, all, none, min, max, ft, fs, "chooseN", uj::callers(), clear)
}

#' @rdname dialog
#' @export
NO <- function(..., ft = "r|w|b", fs = "k|y|p", d = " ", clear = FALSE) {
  msg <- uj::p0(uj::av(...), collapse = d)
  uj::choose_from(base::c("yes", "no"), msg, F, F, 1, 1, ft, fs, "NO", uj::callers(), clear) == "no"
}

#' @rdname dialog
#' @export
YES <- function(..., ft = "r|w|b", fs = "k|y|p", d = " ", clear = FALSE) {
  msg <- uj::p0(uj::av(...), collapse = d)
  uj::choose_from(base::c("yes", "no"), msg, F, F, 1, 1, ft, fs, "YES", uj::callers(), clear) == "yes"
}

#' @rdname dialog
#' @export
OK <- function(..., ft = "r|w|b", fs = "k|y|p", d = " ", clear = FALSE) {
  msg <- uj::p0(uj::av(...), collapse = d)
  uj::choose_from(base::c("ok", "cancel"), msg, F, F, 1, 1, ft, fs, "OK", uj::callers(), clear, can = F) == "ok"
}

#' @rdname dialog
#' @export
CANCEL <- function(..., ft = "r|w|b", fs = "k|y|p", d = " ", clear = FALSE) {
  msg <- uj::p0(uj::av(...), collapse = d)
  uj::choose_from(base::c("ok", "cancel"), msg, F, F, 1, 1, ft, fs, "CANCEL", uj::callers(), clear, can = F) == "cancel"
}

#' @rdname dialog
#' @export
ask <- function(..., Default = "", sub = "", ft = "r|w|b", fs = "k|y|p", fm = "", fp = "k|y|i", d = " ", clear = FALSE) {
  msg <- uj::trm(..., d = "")
  msg <- uj::p0(msg, "\n\n(enter '{cancel}' to cancel)")
  uj::alert(msg, title = "response required", sub = sub, ps = "enter your response:", ft = ft, fs = fs, fm = fm, fp = fp, d = d, clear = clear)
  ans <- base::readline()
  can <- uj::f0(base::length == 0, T, ans == "{cancel}")
  if (can) {uj::stopperr("Action canceled by user.", pkg = "dlg")}
  ans
}

#' @rdname dialog
#' @export
ask_new <- function(old, type = "replacement values", u = TRUE, sub = "", ft = "r|w|b", fs = "k|y|p", fm = "", fp = "k|y|i", d = "|", clear = FALSE) {
  errs <- NULL
  if (!uj::.unq_atm_vec(old )) {errs <- base::c(errs, "[old] must be a unique atomic vec (?unq_atm_vec).")}
  if (!uj::.cmp_str_scl(type)) {errs <- base::c(errs, "[type] must be a complete string scalar (?cmp_str_scl).")}
  if (!uj::.cmp_lgl_scl(u   )) {errs <- base::c(errs, "[u] must be TRUE or FALSE.")}
  if (!uj::.cmp_ch1_scl(d   )) {errs <- base::c(errs, "[d] must be a single character.")}
  if (!base::is.null(errs    )) {uj::stopperr(errs, pkg = "dlg")}
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
  if (can                   ) {uj::stopperr("Action cancelled by user.", pkg = "dlg")}
  if (nAns != uj::N(old)   ) {uj::stopperr("Numbers of old and new values do not match.", pkg = "dlg")}
  if (u & !uj::unq_vec(ans)) {uj::stopperr("Replacement values are not unique.", pkg = "dlg")}
  ans
}

#' @rdname dialog
#' @export
choose_dir <- function(dir.type = "directory", sub = "", ft = "r|w|b", fs = "k|y|p", fm = "", fp = "k|y|i", d = " ", clear = FALSE) {
  if (!uj::.cmp_chr_scl(dir.type)) {uj::stopperr("[dir.type] must be a complete character scalar (?cmp_chr_scl)", pkg = "dlg")}
  uj::acknowledge(uj::p0("In the next dialog box, select a ", dir.type, "."), sub = sub, ft = ft, fs = fs, fm = fm, fp = fp, d = d, clear = clear)
  path <- svDialogs::dlg_dir(title = uj::p0("Select a ", dir.type, ":"))$res
  if (base::length(path) == 0) {uj::stopperr("Action canceled by user.", pkg = "dlg")}
  path
}

#' @rdname dialog
#' @export
choose_doc <- function(doc.type = "document", sub = "", ft = "r|w|b", fs = "", fm = "", fp = "k|y|i", d = " ", clear = FALSE) {
  if (!uj::.cmp_chr_scl(doc.type)) {uj::stopperr("[doc.type] must be a complete character scalar (?cmp_chr_scl)", pkg = "dlg", clear = FALSE)}
  uj::acknowledge(uj::p0("In the next dialog box, select a ", doc.type, "."), sub = sub, ft = ft, fs = fs, fm = fm, fp = fp, d = d, clear = clear)
  path <- svDialogs::dlg_open(title = uj::p0("Select a ", doc.type, ":"))$res
  if (base::length(path) == 0) {uj::stopperr("Action canceled by user.", pkg = "dlg")}
  path
}
