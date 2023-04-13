#' @name dialog
#' @encoding UTF-8
#' @family dialogs
#' @family user
#' @title Dialog with users using package `svDialogs`
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
#'                `alert`         \tab None.                                                 \cr
#'                                \tab                                                       \cr
#'                `ask`           \tab Asks for typed input.\eqn{^{(1)}}                     \cr
#'                                \tab                                                         }
#'  \tabular{l}{  \eqn{^{(1)}} Stops execution if user chooses `CANCEL`.                     \cr
#'                \eqn{^{(2)}} Stops execution if `stop | (type = "error" & is.null(stop))`. \cr
#'                \eqn{^{(3)}} Returns `TRUE` if user choice matches the function name.        }
#' The following give templates for what the user sees, where any value derived from arguments will be absent if it is or resolves to a Blank string.
#' \cr\cr
#' \strong{\code{alert}}
#' \tabular{ll}{
#'     **CONDITION**         \tab **VALUE**                      \cr
#'     `g(D, Title) != ""`   \tab formatted Title from `Title`.  \cr
#'     `g(D, Sub) != ""`     \tab formatted subtitle from `Sub`. \cr
#'     `g(D, ...) != ""`     \tab formatted message from `...`.  \cr
#'                           \tab `< Blank line >`               \cr
#'     `g(D, PS) != ""`      \tab formatted postscript from `PS`.  }
#' All other templates incorporate a call to `alert(.)` with components as shown below.
#' \cr\cr\strong{\code{acknowledge}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                                \cr
#'     Title           \tab `'ACKNOWLEDGMENT REQUIRED'`              \cr
#'     Subtitle        \tab `'< g(D, Sub) >'`                        \cr
#'     Message         \tab `'< g(D, ...) >'`                        \cr
#'                     \tab `< Blank line >`                         \cr
#'     Postscript      \tab `'press [return] or [enter] to continue:'` }
#' \strong{\code{choose1}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                         \cr
#'     Title           \tab `'RESPONSE REQUIRED'`             \cr
#'     Subtitle        \tab `'< g(D, Sub) >'`                 \cr
#'     Message         \tab `'< g(D, ...) >'`                 \cr
#'                     \tab `< Blank line >`                  \cr
#'     Postscript      \tab `'choose an option - Select one'` \cr
#'                     \tab `< Blank line >`                  \cr
#'     Cancel.Option   \tab `'1: { CANCEL }`'                 \cr
#'     Option.1        \tab `'2: < Options[1] >'`             \cr
#'     Option.2        \tab `'3: < Options[2] >'`             \cr
#'     ...             \tab ...                               \cr
#'     Pption.n        \tab `'< n + 1 >: < Options[n] >'`     \cr
#'     Prompt          \tab `'Selection:'`                      }
#' \strong{\code{chooseN}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                                                                      \cr
#'     Title           \tab `'RESPONSE REQUIRED'`                                                          \cr
#'     Subtitle        \tab `'< g(D, Sub) >'`                                                              \cr
#'     Message         \tab `'< g(D, ...) >'`                                                              \cr
#'                     \tab `< Blank line >`                                                               \cr
#'     Postscript      \tab `'choose < n | between n1 and n2 > options - Select one or more'`              \cr
#'                     \tab `< Blank line >`                                                               \cr
#'     Cancel.Option   \tab `'1: { CANCEL }'`                                                              \cr
#'     Option.1        \tab `'2: < Options[1] >'`                                                          \cr
#'     Option.2        \tab `'3: < Options[2] >'`                                                          \cr
#'     ...             \tab ...                                                                            \cr
#'     Option.n        \tab `'< n + 1 >: < Options[n] >'`                                                  \cr
#'     All.option      \tab `'< n + 2 >: { ALL }'`                                                         \cr
#'     None.option     \tab `'< n + 3 >: { NONE }'`                                                        \cr
#'     Prompt          \tab `'Enter one or more numbers separated by spaces and then ENTER, or 0 to cancel'` }
#' \strong{\code{NO}} and \strong{\code{YES}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                         \cr
#'     Title           \tab `'RESPONSE REQUIRED'`             \cr
#'     Subtitle        \tab '`< g(D, Sub) >'`                 \cr
#'     Message         \tab `'< g(D, ...) >'`                 \cr
#'                     \tab `< Blank line >`                  \cr
#'     Postscript      \tab `'choose an option - Select one'` \cr
#'                     \tab `< Blank line >`                  \cr
#'     Cancel.Option   \tab `'1: { CANCEL }'`                 \cr
#'     Yes.Option      \tab `'2: { YES }'`                    \cr
#'     No.Option       \tab `'3: { NO }'`                     \cr
#'     Prompt          \tab `'Selection:'`                      }
#' \strong{\code{OK}} and \strong{\code{CANCEL}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                         \cr
#'     Title           \tab `'RESPONSE REQUIRED'`             \cr
#'     Subtitle        \tab `'< g(D, Sub) >'`                 \cr
#'     Sessage         \tab `'< g(D, ...) >'`                 \cr
#'                     \tab `< Blank line >`                  \cr
#'     Postscript      \tab `'choose an option - Select one'` \cr
#'                     \tab `< Blank line >`                  \cr
#'     Cancel.Option   \tab `'1: { CANCEL }'`                 \cr
#'     OK.Option       \tab `'2: { OK }'`                     \cr
#'     Prompt          \tab `'Selection: '`                     }
#' \strong{\code{ask}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**               \cr
#'     Title           \tab `'RESPONSE REQUIRED'`   \cr
#'     Subtitle        \tab `'< g(D, Sub) >'`       \cr
#'     Message         \tab `'< g(D, ...) >'`       \cr
#'                     \tab `< Blank line >`        \cr
#'     Postscript      \tab `'enter your response: '` }
#' \strong{\code{ask_new}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                                                                                                               \cr
#'     Title           \tab `'RESPONSE REQUIRED'`                                                                                                   \cr
#'     Subtitle        \tab `'< g(D, Sub) >'`                                                                                                       \cr
#'     Message         \tab `'Enter a pipe-separated list of < N(Options) > replacement values for the following pipe-separated original values: '` \cr
#'                     \tab `< Blank line >`                                                                                                        \cr
#'     Old.Values      \tab `'< paste0(old, collapse = " | ") >'`                                                                                   \cr
#'                     \tab `< Blank line >`                                                                                                        \cr
#'     Postscript      \tab `'enter your response: '`                                                                                                 }
#' \strong{\code{choose_dir}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                                        \cr
#'     Title           \tab `'ACKNOWLEDGMENT REQUIRED'`                      \cr
#'     Subtitle        \tab `'< g(D, Sub) >'`                                \cr
#'     Message         \tab `'In the next dialog box, select a < dir.type >.'` }
#' \strong{\code{choose_doc}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                                        \cr
#'     Title           \tab `'ACKNOWLEDGMENT REQUIRED'`                      \cr
#'     Subtitle        \tab `'< g(D, Sub) >'`                                \cr
#'     Message         \tab `'In the next dialog box, select a < doc.type >.'` }
#' @section Specifying formats: When formatting arguments (`FT`, `FS`, `FM`, and `FP`) take the special value `""`, the corresponding alert elements (`Title`, `Sub`, `...`, and `PS`, respectively) are posted to the console without special formatting.
#' \cr\cr Otherwise, formatting arguments must be \link[=cmp_str_vec]{complete string vecs} that when \link[=av]{atomized} and \link[=ssP]{split along pipes} results in a three-element character vector, the first element of which is used to specify \link[=bg]{text background color} and must be a value from \code{\link{bg_vals}()}, the second element of which is used to specify \link[=fg]{text foreground color} and must be a value from \code{\link{fg_vals}()}, and the last of which specifies \link[=st]{text style} and must be a value from \code{\link{st_vals}()}.
#' @param ... An arbitrary, optional number of arguments which are \link[=av]{atomized} into a character scalar message to be posted to the console.
#' @param D A character scalar delimiter for collapsing `...` args into a character scalar.
#' @param N An optional \link[=cmp_psw_scl]{complete positive numeric whole-number scalar} (?cmp_psw_scl) indicating the number of options that must be selected. Must be contained in `1:length(Options)`.
#' @param All Scalar `TRUE` or `FALSE` indicating whether to add an `{ ALL }` value to `Options`.
#' @param Title A Title for the alert (`type = 'error'` results in stopping execution after posting the alert). Should be short to avoid formatting problems.
#' @param Sub A character scalar alert subtitle to post to the console on the line following the Title. Should be short to avoid formatting problems.
#' @param PS A character scalar suffix to post to the console on the line following the alert message contained in `...`.
#' @param Old A \link[=chr_vec]{character vec} of unique values to be replaced.
#' @param Unq Scalar `TRUE` or `FALSE` indicating whether replacement values must be unique.
#' @param None Scalar `TRUE` or `FALSE` indicating whether to add a `{ NONE }` value to `Options` (implying that it is valid to select none).
#' @param FT,FS,FM,FP Formatting values consistent with the description in the *specifying formats* section giving formatting instructions for, respectively, the Title (in `titel`), subtitle (in `Sub`), message (in `...`), and postscript (in `PS`).
#' @param MinM An optional \link[=CMP]{complete} \link[=PSW]{positive numeric whole-number} \link[=SCL]{scalar} indicating the minimum number of options that may be selected. Must be `NULL` when `n` is non-`NULL`.
#' @param MaxN An optional complete positive numeric whole-number scalar indicating the maximum number of options that may be selected. Must be `NULL` when `n` is non-`NULL`.
#' @param Blank A character scalar containing a Default message if \link[=av]{atomizing} and collapsing `...` to a character scalar results in a Blank string (`""`).
#' @param Options An atomic vector listing options to choose from.
#' @param .clear A non-`NA` logical scalar indicating whether to clear the console before each interaction with the user.
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
#' egT = "Title"
#' egS = "Subtitle"
#' egOpts <- paste("option", letters[1:10])
#' egM1 <- "Do you want to continue?"
#' egM2 <- "Why do you want to continue?"
#' alert(egA, egB, D = " ")
#' alert(egA, egB, Title = egTitle, D = " ")
#' alert(egA, egB, Sub = l, D = " ")
#' alert(egA, egB, Title = egTitle, Sub = egSub, D = " ")
#' alert(egA, egB, Title = egTitle, Sub = egSub, D = " ", FM = egFM)
#' alert(egA, egB, Title = egTitle, Sub = egSub, D = " ", FS = egFS)
#' alert(egA, egB, Title = egTitle, Sub = egSub, D = " ", FT = egFT)
#' alert(egA, egB, Title = egTitle, Sub = egSub, D = " ", FT = egFT, FS = egFS, FM = egFM)
#' alert(Title = egT, Sub = egSub, FT = egFT, FS = egFS)
#' alert(Title = egT, Sub = egS)
#' alert(Sub = egS, fS = egFS)
#' \dontrun{
#'   acknowledge(egA, egB)
#'   choose1(egOpts)
#'   chooseN(egOpts)
#'   chooseN(egOpts, All = T, None = F, Min = 6, Max = 10)
#'   NO(egMsg1)
#'   OK(egMsg2)
#'   YES(egMsg1)
#'   CANCEL(egMsg1)
#'   ask(Msg2)
#'   ask_new(egOpts)
#'   choose_dir(DirType = "directory for R scripts")
#'   choose_doc(DocType = "document to read")
#' }
#' @export
alert <- function(..., Title = "alert", Sub = "", PS = "", Blank = "", FT = "r|w|b", FS = "k|y|p", FM = "", FP = "k|y|i", D = " ", .clear = FALSE) {
  fmt <- function(x, f) {
    if (f == "") {return(x)}
    f <- uj::av(base::strsplit(f, "|", fixed = TRUE))
    uj::txt(base::paste0(" ", x, " "), BG = f[1], FG = f[2], ST = f[3])
  }
  Title <- base::paste0(base::toupper(base::as.character(uj::av(Title))), collapse = D)
  Sub <- base::paste0(base::as.character(uj::av(Sub)), collapse = D)
  Message <- base::paste0(base::as.character(uj::av(...)), collapse = D)
  PS <- base::paste0(base::as.character(uj::av(PS)), collapse = D)
  Blank <- base::paste0(base::as.character(uj::av(Blank)), collapse = D)
  FT <- uj::f0(!uj:::.cmp_chr(FT), "r|w|b", base::paste0(FT, collapse = "|"))
  FS <- uj::f0(!uj:::.cmp_chr(FS), "k|y|i", base::paste0(FS, collapse = "|"))
  FP <- uj::f0(!uj:::.cmp_chr(FP), "k|y|i", base::paste0(FP, collapse = "|"))
  FM <- uj::f0(!uj:::.cmp_chr(FM), "k|w|p", base::paste0(FM, collapse = "|"))
  if (Message == "") {Message <- Blank}
  Errors <- NULL
  if (!uj:::.ok_fmt(FT)) {Errors <- base::c(Errors, "[FT] must be consistent with the description the [specifying formats] section of the [alert] topic of package [uj] (?uj::alert).")}
  if (!uj:::.ok_fmt(FS)) {Errors <- base::c(Errors, "[FS] must be consistent with the description the [specifying formats] section of the [alert] topic of package [uj] (?uj::alert).")}
  if (!uj:::.ok_fmt(FM)) {Errors <- base::c(Errors, "[FM] must be consistent with the description the [specifying formats] section of the [alert] topic of package [uj] (?uj::alert).")}
  if (!uj:::.ok_fmt(FP)) {Errors <- base::c(Errors, "[FP] must be consistent with the description the [specifying formats] section of the [alert] topic of package [uj] (?uj::alert).")}
  if (!uj:::.cmp_chr_scl(D)) {Errors <- base::c(Errors, "[D] must be a complete character scalar (?cmp_chr_scl).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  Error <- Title == "ERROR"
  if (.clear) {
    base::gc(verbose = FALSE)
    base::cat("\014")
  }
  if (Title != "") {base::cat("\n", fmt(Title, FT), sep = "")}
  if (Sub != "") {base::cat("\n", fmt(Sub, FS), sep = "")}
  if (Message != "") {base::cat("\n", fmt(Message, FM), sep = "")}
  if (PS != "") {base::cat("\n", fmt(PS, FP), sep = "")} else {cat("\n")}
  if (Error) {uj::stopperr("", PKG = "uj")}
}

#' @rdname dialog
#' @export
acknowledge <- function(..., Sub = "", FT = "r|w|b", FS = "k|y|p", FM = "", FP = "k|y|i", D = " ", .clear = FALSE) {
  uj::alert(..., Title = "acknowledgment required", Sub = Sub, PS = "press [return] or [enter] to continue", FT = FT, FS = FS, FM = FM, FP = FP, D = D, .clear = .clear)
  base::readline()
  NULL
}

#' @rdname dialog
#' @export
choose1 <- function(Options, ..., Sub = "", FT = "r|w|b", FS = "k|y|p", FM = "", FP = "k|y|i", D = " ", .clear = FALSE) {
  if (!uj:::.unq_atm_vec(Options)) {uj::stopperr("[Options] must be a unique atomic vec (?unq_atm_vec).", PKG = "uj")}
  uj::alert(..., Title = "response required", Sub = Sub, PS = "choose an option", FT = FT, FS = FS, FM = FM, FP = FP, D = D, .clear = .clear)
  CharOptions <- base::c("{ CANCEL }", base::as.character(Options))
  Answer <- svDialogs::dlg_list(CharOptions)$res
  if (base::length(Answer) == 0 | uj::f0(base::length(Answer) == 1, Answer == "{ CANCEL }", F)) {uj::stopperr("Action cancelled by user.", PKG = "uj")}
  Options[base::as.character(Options) == Answer]
}

#' @rdname dialog
#' @export
chooseN <- function(Options, ..., Sub = "", All = TRUE, None = FALSE, N = NULL, MinN = NULL, MaxN = NULL, FT = "r|w|b", FS = "k|y|p", FM = "", FP = "k|y|i", D = " ", .clear = FALSE) {
  Errors <- NULL
  if (!uj:::.unq_atm_mvc(Options)) {Errors <- base::c(Errors, "[Options] must be a unique atomic multivec (?unq_atm_mvc).")}
  if (!uj::cmp_lgl_scl(All)) {Errors <- base::c(Errors, "[All] must be TRUE or FALSE.")}
  if (!uj::cmp_lgl_scl(None)) {Errors <- base::c(Errors, "[None] must be TRUE or FALSE.")}
  if (!uj::f0(base::is.null(N), T, uj:::.cmp_psw_scl(N))) {Errors <- base::c(Errors, "[N] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!uj::f0(base::is.null(MinN), T, uj:::.cmp_psw_scl(MinN))) {Errors <- base::c(Errors, "[MinN] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!uj::f0(base::is.null(MaxN), T, uj:::.cmp_psw_scl(MaxN))) {Errors <- base::c(Errors, "[MaxN] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  DefMin <- !base::is.null(MinN)
  DefMax <- !base::is.null(MaxN)
  DefN <- !base::is.null(N)
  Message <- base::paste0(uj::av(...), collapse = D)
  nOptions <- base::length(Options)
  OkLo <- uj::f0(!DefN, T, N >= 1)
  OkHi <- uj::f0(!DefN, T, N <= nOptions)
  OkMin <- uj::f0(!DefMin, T, MinN <= nOptions)
  OkMax <- uj::f0(!DefMax, T, MaxN <= nOptions)
  OkCombo <- (!DefMin & !DefMax) | !DefN
  OkMinMax <- uj::f0(!DefMin | !DefMax, T, MinN <= MaxN)
  if (!OkLo) {Errors <- base::c(Errors, "[N] is out of bounds [min(N) < 1].")}
  if (!OkHi) {Errors <- base::c(Errors, "[N] is out of bounds [max(N) > length(Options)].")}
  if (!OkMin) {Errors <- base::c(Errors, "[MinN] is out of bounds [MinN > length(Options)].")}
  if (!OkMax) {Errors <- base::c(Errors, "[MaxN] is out of bounds [MaxN > length(Options)].")}
  if (!OkCombo) {Errors <- base::c(Errors, "When [N] is supplied, [MinN] and [MaxN] must be NULL.")}
  if (!OkMinMax) {Errors <- base::c(Errors, "[MinN] and [MaxN] are inconsistent [MinN > MaxN].")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  MinN <- uj::f0(DefN, N, uj::f0(DefMin, MinN, uj::f0(None, 0, 1)))
  MaxN <- uj::f0(DefN, N, uj::f0(DefMax, MaxN, nOptions))
  N <- MinN:MaxN
  Infix <- uj::f0(MinN == MaxN, MaxN, uj::p0("between ", MinN, " and ", MaxN))
  All <- All | base::length(Options) %in% N
  None <- None | 0 %in% N
  CharOptions <- base::c("{ CANCEL }", base::as.character(Options), uj::f0(All, "{ ALL }", NULL), uj::f0(None, "{ NONE }", NULL))
  PS <- base::paste0("choose ", Infix, " options")
  uj::alert(Message, Title = "response required", Sub = Sub, PS = PS, FT = FT, FS = FS, FM = FM, D = D, .clear = .clear)
  Answer <- uj::uv(svDialogs::dlg_list(CharOptions, Title = "", multiple = T)$res)
  if (base::length(Answer) == 0 & !None) {uj::stopperr("Action cancelled by user.", PKG = "uj")}
  if (base::length(Answer) == 1) {if (Answer == "{ CANCEL }") {uj::stopperr("Action cancelled by user.", PKG = "uj")}}
  Answer <- Answer[Answer != "{ CANCEL }"]
  if (None & base::length(Answer) == 0) {NULL}
  else if (None & base::all(Answer == "{ NONE }")) {NULL}
  else if (base::all(Answer == "{ ALL }")) {Options[!(Options == "{ CANCEL }")]}
  else if (base::any(Answer %in% base::c("{ ALL }", "{ NONE }"))) {uj::stopperr("Invalid selection.", PKG = "uj")}
  else if (!(base::length(Answer) %in% N)) {uj::stopperr("Invalid number of options selected.", PKG = "uj")}
  else {Options[base::as.character(Options) %in% base::as.character(Answer)]}
}

#' @rdname dialog
#' @export
NO <- function(..., Sub = "", FT = "r|w|b", FS = "k|y|p", FM = "", FP = "k|y|i", D = " ", .clear = FALSE) {
  X <- uj::choose1(base::c("{ YES }", "{ NO }"), ..., Sub = Sub, FT = FT, FS = FS, FM = FM, FP = FP, D = D, .clear = .clear)
  uj::f0(X == "{ YES }", F, uj::f0(X == "{ NO }", T, uj::stopperr("Action canceled by user.", PKG = "uj")))
}

#' @rdname dialog
#' @export
YES <- function(..., Sub = "", FT = "r|w|b", FS = "k|y|p", FM = "", FP = "k|y|i", D = " ", .clear = FALSE) {
  X <- uj::choose1(base::c("{ YES }", "{ NO }"), ..., FT = FT, FS = FS, FM = FM, FP = FP, D = D, .clear = .clear)
  uj::f0(X == "{ YES }", T, uj::f0(X == "{ NO }", F, uj::stopperr("Action canceled by user.", PKG = "uj")))
}

#' @rdname dialog
#' @export
OK <- function(..., Sub = "", FT = "r|w|b", FS = "k|y|p", FM = "", FP = "k|y|i", D = " ", .clear = FALSE) {uj::choose1("{ OK }", ..., FT = FT, FS = FS, FM = FM, FP = FP, D = D, .clear = .clear) == "{ OK }"}

#' @rdname dialog
#' @export
CANCEL <- function(..., Sub = "", FT = "r|w|b", FS = "k|y|p", FM = "", FP = "k|y|i", D = " ", .clear = FALSE) {uj::choose1("{ OK }", ..., FT = FT, FS = FS, FM = FM, FP = FP, D = D, .clear = .clear) == "{ CANCEL }"}

#' @rdname dialog
#' @export
ask <- function(..., Default = "", Sub = "", FT = "r|w|b", FS = "k|y|p", FM = "", FP = "k|y|i", D = " ", .clear = FALSE) {
  Message <- base::trimws(uj::g0(uj::av(...)), which = "both")
  Message <- base::paste0(Message, "\n(enter '{cancel}' to cancel)")
  uj::alert(Message, Title = "response required", Sub = Sub, PS = "enter your response:", FT = FT, FS = FS, FM = FM, FP = FP, D = D, .clear = .clear)
  Answer <- base::readline()
  Cancel <- uj::f0(base::length == 0, T, Answer == "{cancel}")
  uj::f0(Cancel, uj::stopperr("Action canceled by user.", PKG = "uj"), Answer)
}

#' @rdname dialog
#' @export
ask_new <- function(Old, Unq = TRUE, Sub = "", FT = "r|w|b", FS = "k|y|p", FM = "", FP = "k|y|i", D = " ", Sep = "|", .clear = FALSE) {
  Errors <- NULL
  if (!uj:::.unq_atm_vec(Old)) {Errors <- base::c(Errors, "[Old] must be a unique atomic vec (?unq_atm_vec).")}
  if (!uj:::.cmp_lgl_scl(Unq)) {Errors <- base::c(Errors, "[Unq] must be TRUE or FALSE.")}
  if (!uj:::.cmp_ch1_scl(Sep)) {Errors <- base::c(Errors, "[Sep] must be a single character.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  SepName <- uj::f0(Sep == "|", "pipe (|)", uj::f0(Sep == ".", "dot (.)", uj::f0(Sep == ":", "colon (:)", uj::f0(Sep == "~", "tilde (~)", uj::f0(Sep == "^", "caret (^)", uj::f0(Sep == " ", "space", base::paste0("'", Sep, "'")))))))
  List <- paste0(Old, collapse = base::paste0(" ", Sep, " "))
  Question <- base::paste0("Enter a '%s' separated list of %d replacement values for the following '%s'-separated original values:\n\n%s\n\n(enter '{cancel}' to cancel)")
  Question <- base::sprintf(Question, SepName, base::length(Old), SepName, List)
  uj::alert(Question, Title = "response required", Sub = "", PS = "enter your response: ", FT = FT, FS = FS, FM = FM, FP = FP, D = D, .clear = .clear)
  Answer <- base::readline()
  Answer <- uj::av(base::strsplit(Answer, Sep, fixed = T))
  Answer <- base::trimws(Answer, which = "both")
  Answer <- Answer[Answer != ""]
  nAnswers <- uj::N(Answer)
  Cancel <- uj::f0(nAnswers == 0, T, uj::f0(nAnswers > 1, F, Answer == "{cancel}"))
  if (Cancel) {uj::stopperr("Action cancelled by user.", PKG = "uj")}
  if (nAnswers != uj::N(Old)) {uj::stopperr("Numbers of old and new values do not match.", PKG = "uj")}
  if (Unq & !uj::unq_vec(Answer)) {uj::stopperr("Replacement values are not unique.", PKG = "uj")}
  Answer
}

#' @rdname dialog
#' @export
choose_dir <- function(DirType = "directory", Sub = "", FT = "r|w|b", FS = "k|y|p", FM = "", FP = "k|y|i", D = " ", .clear = FALSE) {
  if (!uj::cmp_chr_scl(DirType)) {uj::stopperr("[DirType] must be a complete character scalar (?cmp_chr_scl)", PKG = "uj")}
  uj::acknowledge(base::paste0("In the next dialog box, select a ", DirType, "."), Sub = Sub, FT = FT, FS = FS, FM = FM, FP = FP, D = D, .clear = .clear)
  Path <- svDialogs::dlg_dir(Title = base::paste0("Select a ", DirType, ":"))$res
  if (base::length(Path) == 0) {uj::stopperr("Action canceled by user.", PKG = "uj")}
  Path
}

#' @rdname dialog
#' @export
choose_doc <- function(DocType = "document", Sub = "", FT = "r|w|b", FS = "", FM = "", FP = "k|y|i", D = " ", .clear = FALSE) {
  if (!uj:::cmp_chr_scl(DocType)) {uj::stopperr("[DocType] must be a complete character scalar (?cmp_chr_scl)", PKG = "uj", .clear = FALSE)}
  uj::acknowledge(base::paste0("In the next dialog box, select a ", DocType, "."), Sub = Sub, FT = FT, FS = FS, FM = FM, FP = FP, D = D, .clear = .clear)
  Path <- svDialogs::dlg_open(Title = base::paste0("Select a ", DocType, ":"))$res
  if (base::length(Path) == 0) {uj::stopperr("Action canceled by user.", PKG = "uj")}
  Path
}
