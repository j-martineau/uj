#' @encoding UTF-8
#' @family conversions
#' @family strings
#' @title Convert string case
#' @description Convert among lower, sentence, title, and upper cases.
#' @details Functions in this family convert string case as follows:
#' \tabular{ll}{  `to_title, tc`   \tab Convert to \link[stringr:str_to_sentence]{title case}.    \cr   \tab   \cr
#'                `to_sent, sc`    \tab Convert to \link[stringr:str_to_sentence]{sentence case}. \cr   \tab     }
#' \tabular{ll}{  `to_case`        \tab Convert to case `case`.                                   \cr   \tab     }
#' \tabular{ll}{  `lc`             \tab Convert to \link[base:tolower]{lower case}.               \cr   \tab   \cr
#'                `uc`             \tab Convert to \link[base:toupper]{upper case}.                              }
#' @param ... An arbitrary number of non-empty character arguments.
#' @param case A case insensitive \link[=cmp_ch1_scl]{complete onechar scalar}. Either `'l'`, `'s'`, `'t'`, or `'u'` to indicate lower, sentence, title, or upper case, respectively.
#' @return A character vector.
#' @examples
#' egSentence <- "This is a sentence."
#' egTitle <- "This is a Title"
#' egWords <- c("word1", "word2")
#' egList <- list(egWords, "word3")
#'
#' egWords
#' egList
#'
#' tc(egSentence, egTitle)
#' sc(egSentence, egTitle)
#' lc(egSentence, egTitle)
#' uc(egSentence, egTitle)
#'
#' to_sent(egSentence, egTitle, egWords, egList)
#' to_title(egSentence, egTitle, egWords, egList)
#'
#' to_case("s", egTitle, egList)
#' to_case("u", egList)
#' @export
to_case <- function(case, ...) {
  x <- uj::av(...)
  uj::errs_if_nots(uj::isIN1(case, "l", "s", "t", "u"), "[case] must be either 'l', 's', 't', or 'u'.",
                   uj::cmp_chr_vec(x), "[...] must be contain character values."                      , PKG = "uj")
  uj::f0(case == "l", base::tolower(x), uj::f0(case == "u", base::toupper(x), uj::f0(case == "t", stringr::str_to_title(x), stringr::str_to_sentence(x))))
}

#' @rdname to_case
#' @export
to_sent <- function(...) {uj::to_case("s", ...)}

#' @rdname to_case
#' @export
to_title <- function(...) {uj::to_case("t", ...)}

#' @rdname to_case
#' @export
lc <- function(...) {uj::to_case("l", ...)}

#' @rdname to_case
#' @export
sc <- function(...) {uj::to_case("s", ...)}

#' @rdname to_case
#' @export
tc <- function(...) {uj::to_case("t", ...)}

#' @rdname to_case
#' @export
uc <- function(...) {uj::to_case("u", ...)}
