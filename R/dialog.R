#' @name dialog
#' @encoding UTF-8
#' @family dialogs
#' @family user
#' @title Dialog with users via the console or packages `svDialogs`.
#' @description All functions \link[=collapse_dots]{collapses} `...` args into a prompt. Each posts an alert to the console, posts the prompt (if any), followed by a specific action.
#' @details
#' \tabular{ll}{  `acknowledge`   \tab Waits for user to acknowledge.                        \cr
#'                                \tab                                                       \cr
#'                `OK, CANCEL`    \tab Offers OK and CANCEL options.\eqn{^{(3)}}             \cr
#'                                \tab                                                       \cr
#'                `choose_doc`    \tab Asks user to choose a document.\eqn{^{(1)}}           \cr
#'                `choose_dir`    \tab Asks user to choose a directory.\eqn{^{(1)}}          \cr
#'                                \tab                                                       \cr
#'                `choose1`       \tab Asks user to choose `1` option.\eqn{^{(1)}}           \cr
#'                `chooseN`       \tab Asks user to choose `1+` options.\eqn{^{(1,2)}}       \cr
#'                                \tab                                                       \cr
#'                `YES, NO`       \tab Offers YES, NO, and CANCEL options.\eqn{^{(1,3)}}     \cr
#'                                \tab                                                       \cr
#'                `ask_new`       \tab Asks for a list of new values.\eqn{^{(1)}}            \cr
#'                                \tab                                                       \cr
#'                `alert`         \tab .NONE.                                                 \cr
#'                                \tab                                                       \cr
#'                `ask`           \tab Asks for typed input.\eqn{^{(1)}}                     \cr
#'                                \tab                                                         }
#'  \tabular{l}{  \eqn{^{(1)}} Stops execution if user chooses `CANCEL`.                     \cr
#'                \eqn{^{(2)}} Stops execution if `stop | (type = "error" & is.null(stop))`. \cr
#'                \eqn{^{(3)}} Returns `TRUE` if user choice matches the function name.        }
#' The following give templates for what the user sees, where any value derived from arguments will be absent if it is or resolves to a blank string.
#' \cr\cr
#' \strong{\code{alert}}
#' \tabular{ll}{
#'     **CONDITION**           \tab **VALUE**                       \cr
#'     `g(.D, .TITLE) != ""`   \tab formatted title from `.TITLE`.  \cr
#'     `g(.D, .SUB) != ""`     \tab formatted subtitle from `.SUB`. \cr
#'     `g(.D, ...) != ""`      \tab formatted message from `...`.   \cr
#'                             \tab `< blank line >`                \cr
#'     `g(.D, .PS) != ""`      \tab formatted postscript from `.PS`.  }
#' .ALL other templates incorporate a call to `alert(.)` with components as shown below.
#' \cr\cr\strong{\code{acknowledge}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                                 \cr
#'     .TITLE          \tab `'ACKNOWLEDGMENT REQUIRED'`               \cr
#'     Subtitle        \tab `'< g(.D, .SUB) >'`                       \cr
#'     Message         \tab `'< g(.D, ...) >'`                        \cr
#'                     \tab `< blank line >`                          \cr
#'     Postscript      \tab `'press [return] or [enter] to continue:'`  }
#' \strong{\code{choose1}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                         \cr
#'     .TITLE          \tab `'RESPONSE REQUIRED'`             \cr
#'     Subtitle        \tab `'< g(.D, .SUB) >'`               \cr
#'     Message         \tab `'< g(.D, ...) >'`                \cr
#'                     \tab `< blank line >`                  \cr
#'     Postscript      \tab `'choose an option - Select one'` \cr
#'                     \tab `< blank line >`                  \cr
#'     Cancel.Option   \tab `'1: { CANCEL }`'                 \cr
#'     Option.1        \tab `'2: < options[1] >'`             \cr
#'     Option.2        \tab `'3: < options[2] >'`             \cr
#'     ...             \tab ...                               \cr
#'     Pption.n        \tab `'< n + 1 >: < options[n] >'`     \cr
#'     Prompt          \tab `'Selection:'`                      }
#' \strong{\code{chooseN}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                                                                      \cr
#'     .TITLE           \tab `'RESPONSE REQUIRED'`                                                         \cr
#'     Subtitle        \tab `'< g(.D, .SUB) >'`                                                            \cr
#'     Message         \tab `'< g(.D, ...) >'`                                                             \cr
#'                     \tab `< blank line >`                                                               \cr
#'     Postscript      \tab `'choose < n | between n1 and n2 > options - Select one or more'`              \cr
#'                     \tab `< blank line >`                                                               \cr
#'     Cancel.Option   \tab `'1: { CANCEL }'`                                                              \cr
#'     Option.1        \tab `'2: < options[1] >'`                                                          \cr
#'     Option.2        \tab `'3: < options[2] >'`                                                          \cr
#'     ...             \tab ...                                                                            \cr
#'     Option.n        \tab `'< n + 1 >: < options[n] >'`                                                  \cr
#'     .ALL.option     \tab `'< n + 2 >: { ALL }'`                                                         \cr
#'     .NONE.option    \tab `'< n + 3 >: { NONE }'`                                                        \cr
#'     Prompt          \tab `'Enter one or more numbers separated by spaces and then ENTER, or 0 to cancel'` }
#' \strong{\code{NO}} and \strong{\code{YES}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                         \cr
#'     Title           \tab `'RESPONSE REQUIRED'`             \cr
#'     Subtitle        \tab '`< g(.D, .SUB) >'`               \cr
#'     Message         \tab `'< g(.D, ...) >'`                \cr
#'                     \tab `< blank line >`                  \cr
#'     Postscript      \tab `'choose an option - Select one'` \cr
#'                     \tab `< blank line >`                  \cr
#'     Cancel.Option   \tab `'1: { CANCEL }'`                 \cr
#'     Yes.Option      \tab `'2: { YES }'`                    \cr
#'     No.Option       \tab `'3: { NO }'`                     \cr
#'     Prompt          \tab `'Selection:'`                      }
#' \strong{\code{OK}} and \strong{\code{CANCEL}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                         \cr
#'     Title           \tab `'RESPONSE REQUIRED'`             \cr
#'     Subtitle        \tab `'< g(.D, .SUB) >'`               \cr
#'     Message         \tab `'< g(.D, ...) >'`                \cr
#'                     \tab `< blank line >`                  \cr
#'     Postscript      \tab `'choose an option - Select one'` \cr
#'                     \tab `< blank line >`                  \cr
#'     Cancel.Option   \tab `'1: { CANCEL }'`                 \cr
#'     OK.Option       \tab `'2: { OK }'`                     \cr
#'     Prompt          \tab `'Selection: '`                     }
#' \strong{\code{ask}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                 \cr
#'     Title           \tab `'RESPONSE REQUIRED'`     \cr
#'     Subtitle        \tab `'< g(.D, .SUB) >'`       \cr
#'     Message         \tab `'< g(.D, ...) >'`        \cr
#'                     \tab `< blank line >`          \cr
#'     Postscript      \tab `'enter your response: '`   }
#' \strong{\code{ask_new}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                                                                                                                \cr
#'     .TITLE           \tab `'RESPONSE REQUIRED'`                                                                                                   \cr
#'     Subtitle        \tab `'< g(.D, .SUB) >'`                                                                                                      \cr
#'     Message         \tab `'Enter a pipe-separated list of < .N(options) > replacement values for the following pipe-separated original values: '` \cr
#'                     \tab `< blank line >`                                                                                                         \cr
#'     old.Values      \tab `'< paste0(old, collapse = " | ") >'`                                                                                    \cr
#'                     \tab `< blank line >`                                                                                                         \cr
#'     Postscript      \tab `'enter your response: '`                                                                                                  }
#' \strong{\code{choose_dir}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                                          \cr
#'     Title           \tab `'ACKNOWLEDGMENT REQUIRED'`                        \cr
#'     Subtitle        \tab `'< g(.D, .SUB) >'`                                \cr
#'     Message         \tab `'In the next dialog box, select a < dir.type >.'`   }
#' \strong{\code{choose_doc}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                                          \cr
#'     Title           \tab `'ACKNOWLEDGMENT REQUIRED'`                        \cr
#'     Subtitle        \tab `'< g(.D, .SUB) >'`                                \cr
#'     Message         \tab `'In the next dialog box, select a < doc.type >.'`   }
#' @section Specifying formats: When formatting arguments (`.FT`, `.FS`, `.FM`, and `.FP`) take the special value `""`, the corresponding alert elements (`.TITLE`, `.SUB`, `...`, and `.PS`, respectively) are posted to the console without special formatting.
#' \cr\cr Otherwise, formatting arguments must be \link[=cmp_str_vec]{complete string vecs} that when \link[=av]{atomized} and \link[=ssP]{split along pipes} results in a three-element character vector, the first element of which is used to specify \link[=bg]{text background color} and must be a value from \code{\link{bg_vals}()}, the second element of which is used to specify \link[=fg]{text foreground color} and must be a value from \code{\link{fg_vals}()}, and the last of which specifies \link[=st]{text style} and must be a value from \code{\link{st_vals}()}.
#' @param ... An arbitrary, optional number of arguments which are \link[=av]{atomized} into a character scalar message to be posted to the console.
#' @param old A \link[=chr_vec]{character vec} of unique values to be replaced.
#' @param options An atomic vector listing options to choose from.
#' @param .D A character scalar delimiter for collapsing `...` args into a character scalar.
#' @param .N An optional \link[=cmp_psw_scl]{complete positive numeric whole-number scalar} (?cmp_psw_scl) indicating the number of options that must be selected. Must be contained in `1:length(options)`.
#' @param .ALL Scalar `TRUE` or `FALSE` indicating whether to add an `{ ALL }` value to `options`.
#' @param .TITLE A .TITLE for the alert (`type = 'error'` results in stopping execution after posting the alert). Should be short to avoid formatting problems.
#' @param .SUB A character scalar alert subtitle to post to the console on the line following the title Should be short to avoid formatting problems.
#' @param .PS A character scalar suffix to post to the console on the line following the alert message contained in `...`.
#' @param .U Scalar `TRUE` or `FALSE` indicating whether replacement values must be unique.
#' @param .NONE Scalar `TRUE` or `FALSE` indicating whether to add a `{ NONE }` value to `options` (implying that it is valid to select none).
#' @param .FT,.FS,.FM,.FP Formatting values consistent with the description in the *specifying formats* section giving formatting instructions for, respectively, the title (in `.TITLE`), subtitle (in `.SUB`), message (in `...`), and postscript (in `.PS`).
#' @param .MIN An optional \link[=CMP]{complete} \link[=PSW]{positive numeric whole-number} \link[=SCL]{scalar} indicating the minimum number of options that may be selected. Must be `NULL` when `.N` is non-`NULL`.
#' @param .MAX An optional complete positive numeric whole-number scalar indicating the maximum number of options that may be selected. Must be `NULL` when `.N` is non-`NULL`.
#' @param .DEF A character scalar containing a default message if \link[=av]{atomizing} and collapsing `...` to a character scalar results in a blank string (`""`).
#' @param .CLEAR A non-`NA` logical scalar indicating whether to clear the console before each interaction with the user.
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
#' egFS <- c("blk|wht", "und")
#' egFM <- "b|y|i"
#' egT = ".TITLE"
#' egS = "Subtitle"
#' egOpts <- paste("option", letters[1:10])
#' egM1 <- "Do you want to continue?"
#' egM2 <- "Why do you want to continue?"
#' alert(egA, egB, .D = " ")
#' alert(egA, egB, .TITLE = egTitle, .D = " ")
#' alert(egA, egB, .SUB = l, .D = " ")
#' alert(egA, egB, .TITLE = egTitle, .SUB = egSub, .D = " ")
#' alert(egA, egB, .TITLE = egTitle, .SUB = egSub, .D = " ", .FM = egFM)
#' alert(egA, egB, .TITLE = egTitle, .SUB = egSub, .D = " ", .FS = egFS)
#' alert(egA, egB, .TITLE = egTitle, .SUB = egSub, .D = " ", .FT = egFT)
#' alert(egA, egB, .TITLE = egTitle, .SUB = egSub, .D = " ", .FT = egFT, .FS = egFS, .FM = egFM)
#' alert(.TITLE = egT, .SUB = egSub, .FT = egFT, .FS = egFS)
#' alert(.TITLE = egT, .SUB = egS)
#' alert(.SUB = egS, .FS = egFS)
#' \dontrun{
#'   acknowledge(egA, egB)
#'   choose1(egOpts)
#'   chooseN(egOpts)
#'   chooseN(egOpts, .ALL = T, .NONE = F, Min = 6, Max = 10)
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
alert <- function(..., .TITLE = "alert", .SUB = "", .PS = "", .DEF = "", .FT = "r|w|b", .FS = "k|y|p", .FM = "", .FP = "k|y|i", .D = " ", .CLEAR = FALSE) {
  fmt <- function(x, f) {
    if (f == "") {return(x)}
    f <- uj::av(base::strsplit(f, "|", fixed = TRUE))
    uj::txt(base::paste0(" ", x, " "), BG = f[1], FG = f[2], ST = f[3])
  }
  .TITLE <- base::paste0(base::toupper(base::as.character(uj::av(.TITLE))), collapse = .D)
  .SUB <- base::paste0(base::as.character(uj::av(.SUB)), collapse = .D)
  Message <- base::paste0(base::as.character(uj::av(...)), collapse = .D)
  .PS <- base::paste0(base::as.character(uj::av(.PS)), collapse = .D)
  .DEF <- base::paste0(base::as.character(uj::av(.DEF)), collapse = .D)
  .FT <- uj::f0(!uj:::.cmp_chr(.FT), "r|w|b", base::paste0(.FT, collapse = "|"))
  .FS <- uj::f0(!uj:::.cmp_chr(.FS), "k|y|i", base::paste0(.FS, collapse = "|"))
  .FP <- uj::f0(!uj:::.cmp_chr(.FP), "k|y|i", base::paste0(.FP, collapse = "|"))
  .FM <- uj::f0(!uj:::.cmp_chr(.FM), "k|w|p", base::paste0(.FM, collapse = "|"))
  if (Message == "") {Message <- .DEF}
  Errors <- NULL
  if (!uj:::.ok_fmt(.FT)) {Errors <- base::c(Errors, "[.FT] must be consistent with the description the [specifying formats] section of the [alert] topic of package [uj] (?uj::alert).")}
  if (!uj:::.ok_fmt(.FS)) {Errors <- base::c(Errors, "[.FS] must be consistent with the description the [specifying formats] section of the [alert] topic of package [uj] (?uj::alert).")}
  if (!uj:::.ok_fmt(.FM)) {Errors <- base::c(Errors, "[.FM] must be consistent with the description the [specifying formats] section of the [alert] topic of package [uj] (?uj::alert).")}
  if (!uj:::.ok_fmt(.FP)) {Errors <- base::c(Errors, "[.FP] must be consistent with the description the [specifying formats] section of the [alert] topic of package [uj] (?uj::alert).")}
  if (!uj:::.cmp_chr_scl(.D)) {Errors <- base::c(Errors, "[.D] must be a complete character scalar (?cmp_chr_scl).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  Error <- .TITLE == "ERROR"
  if (.CLEAR) {
    base::gc(verbose = FALSE)
    base::cat("\014")
  }
  if (.TITLE != "") {base::cat("\n", fmt(.TITLE, .FT), sep = "")}
  if (.SUB != "") {base::cat("\n", fmt(.SUB, .FS), sep = "")}
  if (Message != "") {base::cat("\n", fmt(Message, .FM), sep = "")}
  if (.PS != "") {base::cat("\n", fmt(.PS, .FP), sep = "")} else {cat("\n")}
  if (Error) {uj::stopperr("", .PKG = "uj")}
}

#' @rdname dialog
#' @export
acknowledge <- function(..., .SUB = "", .PS = "", .FT = "r|w|b", .FS = "k|y|p", .FP = "k|y|i", .D = " ", .CLEAR = FALSE) {
  uj::alert(..., .TITLE = "acknowledgment required", .SUB = .SUB, .PS = "press [return] or [enter] to continue", .FT = .FT, .FS = .FS, .FP = .FP, .D = .D, .CLEAR = .CLEAR)
  base::readline()
  NULL
}

#' @rdname dialog
#' @export
choose1 <- function(options, ..., .FT = "r|w|b", .FS = "k|y|p", .D = " ", .CLEAR = FALSE) {
  if (!uj:::.unq_atm_vec(options)) {uj::stopperr("[options] must be a unique atomic vec (?unq_atm_vec).", .PKG = "uj")}
  Message <- base::paste0(uj::av(...), collapse = .D)
  uj:::.cat_choose_list(options, Message, F, F, 1, 1, .FT, .FS, "choose1", uj::caller(), .CLEAR)
}

#' @rdname dialog
#' @export
chooseN <- function(options, ..., .ALL = TRUE, .NONE = FALSE, .N = NULL, .MIN = NULL, .MAX = NULL, .FT = "r|w|b", .FS = "k|y|p", .D = " ", .CLEAR = FALSE) {
  Errors <- NULL
  if (!uj:::.unq_atm_mvc(options)) {Errors <- base::c(Errors, "[options] must be a unique atomic multivec (?unq_atm_mvc).")}
  if (!uj::cmp_lgl_scl(.ALL)) {Errors <- base::c(Errors, "[.ALL] must be TRUE or FALSE.")}
  if (!uj::cmp_lgl_scl(.NONE)) {Errors <- base::c(Errors, "[.NONE] must be TRUE or FALSE.")}
  if (!uj::f0(base::is.null(.N), T, uj:::.cmp_psw_scl(.N))) {Errors <- base::c(Errors, "[.N] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!uj::f0(base::is.null(.MIN), T, uj:::.cmp_psw_scl(.MIN))) {Errors <- base::c(Errors, "[.MIN] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!uj::f0(base::is.null(.MAX), T, uj:::.cmp_psw_scl(.MAX))) {Errors <- base::c(Errors, "[.MAX] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  DefMin <- !base::is.null(.MIN)
  DefMax <- !base::is.null(.MAX)
  DefN <- !base::is.null(.N)
  Message <- base::paste0(uj::av(...), collapse = .D)
  nOptions <- base::length(options)
  OkLo <- uj::f0(!DefN, T, .N >= 1)
  OkHi <- uj::f0(!DefN, T, .N <= nOptions)
  OkMin <- uj::f0(!DefMin, T, .MIN <= nOptions)
  OkMax <- uj::f0(!DefMax, T, .MAX <= nOptions)
  OkCombo <- (!DefMin & !DefMax) | !DefN
  OkMinMax <- uj::f0(!DefMin | !DefMax, T, .MIN <= .MAX)
  if (!OkLo) {Errors <- base::c(Errors, "[.N] is out of bounds [min(.N) < 1].")}
  if (!OkHi) {Errors <- base::c(Errors, "[.N] is out of bounds [max(.N) > length(options)].")}
  if (!OkMin) {Errors <- base::c(Errors, "[.MIN] is out of bounds [.MIN > length(options)].")}
  if (!OkMax) {Errors <- base::c(Errors, "[.MAX] is out of bounds [.MAX > length(options)].")}
  if (!OkCombo) {Errors <- base::c(Errors, "When [.N] is supplied, [.MIN] and [.MAX] must be NULL.")}
  if (!OkMinMax) {Errors <- base::c(Errors, "[.MIN] and [.MAX] are inconsistent [.MIN > .MAX].")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  .MIN <- uj::f0(DefN, .N, uj::f0(DefMin, .MIN, uj::f0(.NONE, 0, 1)))
  .MAX <- uj::f0(DefN, .N, uj::f0(DefMax, .MAX, nOptions))
  uj:::.cat_choose_list(options, Message, .ALL, .NONE, .MIN, .MAX, .FT, .FS, "chooseN", uj::callers(), .CLEAR)
}

#' @rdname dialog
#' @export
NO <- function(..., .FT = "r|w|b", .FS = "k|y|p", .D = " ", .CLEAR = FALSE) {
  Message <- base::paste0(uj::av(...), collapse = .D)
  uj:::.cat_choose_list(base::c("yes", "no"), Message, F, F, 1, 1, .FT, .FS, "NO", uj::callers(), .CLEAR) == "no"
}

#' @rdname dialog
#' @export
YES <- function(..., .FT = "r|w|b", .FS = "k|y|p", .D = " ", .CLEAR = FALSE) {
  Message <- base::paste0(uj::av(...), collapse = .D)
  uj:::.cat_choose_list(base::c("yes", "no"), Message, F, F, 1, 1, .FT, .FS, "YES", uj::callers(), .CLEAR) == "yes"
}

#' @rdname dialog
#' @export
OK <- function(..., .FT = "r|w|b", .FS = "k|y|p", .D = " ", .CLEAR = FALSE) {
  Message <- base::paste0(uj::av(...), collapse = .D)
  uj:::.cat_choose_list(base::c("ok", "cancel"), Message, F, F, 1, 1, .FT, .FS, "OK", uj::callers(), .CLEAR, .cancel = F) == "ok"
}

#' @rdname dialog
#' @export
CANCEL <- function(..., .FT = "r|w|b", .FS = "k|y|p", .D = " ", .CLEAR = FALSE) {
  Message <- base::paste0(uj::av(...), collapse = .D)
  uj:::.cat_choose_list(base::c("ok", "cancel"), Message, F, F, 1, 1, .FT, .FS, "CANCEL", uj::callers(), .CLEAR, .cancel = F) == "cancel"
}

#' @rdname dialog
#' @export
ask <- function(..., Default = "", .SUB = "", .FT = "r|w|b", .FS = "k|y|p", .FM = "", .FP = "k|y|i", .D = " ", .CLEAR = FALSE) {
  Message <- base::trimws(uj::g0(uj::av(...)), which = "both")
  Message <- base::paste0(Message, "\n(enter '{cancel}' to cancel)")
  uj::alert(Message, .TITLE = "response required", .SUB = .SUB, .PS = "enter your response:", .FT = .FT, .FS = .FS, .FM = .FM, .FP = .FP, .D = .D, .CLEAR = .CLEAR)
  Answer <- base::readline()
  Cancel <- uj::f0(base::length == 0, T, Answer == "{cancel}")
  uj::f0(Cancel, uj::stopperr("Action canceled by user.", .PKG = "uj"), Answer)
}

#' @rdname dialog
#' @export
ask_new <- function(old, .U = TRUE, .SUB = "", .FT = "r|w|b", .FS = "k|y|p", .FM = "", .FP = "k|y|i", .D = " ", .SEP = "|", .CLEAR = FALSE) {
  Errors <- NULL
  if (!uj:::.unq_atm_vec(old)) {Errors <- base::c(Errors, "[old] must be a unique atomic vec (?unq_atm_vec).")}
  if (!uj:::.cmp_lgl_scl(.U)) {Errors <- base::c(Errors, "[.U] must be TRUE or FALSE.")}
  if (!uj:::.cmp_ch1_scl(.SEP)) {Errors <- base::c(Errors, "[.SEP] must be a single character.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  SepName <- uj::f0(.SEP == "|", "pipe (|)", uj::f0(.SEP == ".", "dot (.)", uj::f0(.SEP == ":", "colon (:)", uj::f0(.SEP == "~", "tilde (~)", uj::f0(.SEP == "^", "caret (^)", uj::f0(.SEP == " ", "space", base::paste0("'", .SEP, "'")))))))
  List <- paste0(old, collapse = base::paste0(" ", .SEP, " "))
  Question <- base::paste0("Enter a '%s' separated list of %d replacement values for the following '%s'-separated original values:\n\n%s\n\n(enter '{cancel}' to cancel)")
  Question <- base::sprintf(Question, SepName, base::length(old), SepName, List)
  uj::alert(Question, .TITLE = "response required", .SUB = "", .PS = "enter your response: ", .FT = .FT, .FS = .FS, .FM = .FM, .FP = .FP, .D = .D, .CLEAR = .CLEAR)
  Answer <- base::readline()
  Answer <- uj::av(base::strsplit(Answer, .SEP, fixed = T))
  Answer <- base::trimws(Answer, which = "both")
  Answer <- Answer[Answer != ""]
  nAnswers <- uj::.N(Answer)
  Cancel <- uj::f0(nAnswers == 0, T, uj::f0(nAnswers > 1, F, Answer == "{cancel}"))
  if (Cancel) {uj::stopperr("Action cancelled by user.", .PKG = "uj")}
  if (nAnswers != uj::.N(old)) {uj::stopperr("Numbers of old and new values do not match.", .PKG = "uj")}
  if (.U & !uj::unq_vec(Answer)) {uj::stopperr("Replacement values are not unique.", .PKG = "uj")}
  Answer
}

#' @rdname dialog
#' @export
choose_dir <- function(dir.type = "directory", .SUB = "", .FT = "r|w|b", .FS = "k|y|p", .FM = "", .FP = "k|y|i", .D = " ", .CLEAR = FALSE) {
  if (!uj::cmp_chr_scl(dir.type)) {uj::stopperr("[dir.type] must be a complete character scalar (?cmp_chr_scl)", .PKG = "uj")}
  uj::acknowledge(base::paste0("In the next dialog box, select a ", dir.type, "."), .SUB = .SUB, .FT = .FT, .FS = .FS, .FM = .FM, .FP = .FP, .D = .D, .CLEAR = .CLEAR)
  Path <- svDialogs::dlg_dir(.TITLE = base::paste0("Select a ", dir.type, ":"))$res
  if (base::length(Path) == 0) {uj::stopperr("Action canceled by user.", .PKG = "uj")}
  Path
}

#' @rdname dialog
#' @export
choose_doc <- function(doc.type = "document", .SUB = "", .FT = "r|w|b", .FS = "", .FM = "", .FP = "k|y|i", .D = " ", .CLEAR = FALSE) {
  if (!uj:::cmp_chr_scl(doc.type)) {uj::stopperr("[doc.type] must be a complete character scalar (?cmp_chr_scl)", .PKG = "uj", .CLEAR = FALSE)}
  uj::acknowledge(base::paste0("In the next dialog box, select a ", doc.type, "."), .SUB = .SUB, .FT = .FT, .FS = .FS, .FM = .FM, .FP = .FP, .D = .D, .CLEAR = .CLEAR)
  Path <- svDialogs::dlg_open(.TITLE = base::paste0("Select a ", doc.type, ":"))$res
  if (base::length(Path) == 0) {uj::stopperr("Action canceled by user.", .PKG = "uj")}
  Path
}
