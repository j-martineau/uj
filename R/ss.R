#' @encoding UTF-8
#' @family strings
#' @family chars
#' @title Split strings and select/check for elements
#' @description *Order of operations*
#' \cr\cr All functions in this family follow the same order of operations when performing string splitting:
#' \itemize{\item \link[=av]{Atomize} `...`, collapsing it to a simple atomic vector.
#'          \item Coerce the result to a character vector.
#'          \item Split each element of the vector along the delimiter(s) in `d`, producing a potentially longer character vector.
#'          \item If `.N` is not `NULL`, extract the `n`-th elements(s) from the result.
#'          \item If `.TRM = TRUE`, trim white space (i.e., spaces, tabs, newlines) from both ends of each element of the result.
#'          \item If `.SQZ = TRUE`, remove leading and trailing white space and replace any multi character interior white-space sequences inside the result with a single space.
#'          \item If `.U = TRUE`, reduce the result to unique values.}
#' @details **The function** `ss`
#' \cr\cr Performs the operations as described in the *order of operations* section.
#' \cr\cr **The functions** `ch/chars`
#' \cr\cr Split strings in constituent characters (by using a blank-string delimiter).
#' \cr\cr **The function** `sstb`
#' \cr\cr Requires that splitting each element of `x` along the delimiter `d` and post-processing the results based on optional args `.TRM`, `.SQZ`, `.DROP`, `n`, and `.U` will result in vectors of the same length. Those resulting same-length vectors are then placed into a data.frame. For example, the following console excerpt demonstrates a call to `ss_tb` and its result.
#' ```
#' > sstb('|', 'a|b|c|d', 'e|f|g|h', 'i|j|k|l', 'm|n|o|p', name = 'original', part = 'letter')
#'
#'   original letter.1 letter.2 letter.3 letter.4
#' 1  a|b|c|d        a        b        c        d
#' 2  e|f|g|h        e        f        g        h
#' 3  i|j|k|l        i        j        k        l
#' 4  m|n|o|p        m        n        o        p
#' ```
#' \cr **Functions extending** `ss` ** for common delimiters**
#' \cr\cr Extension functions are supplied for common delimiters, signified by codes appended to `ss` function names:
#' \tabular{lll}{  **Code**   \tab **name**            \tab **Common delimiter**                                                              \cr
#'                 `'0'`      \tab blank\eqn{^{(1)}}   \tab `''`                                                                              \cr
#'                 `'1'`      \tab space               \tab `' '`                                                                             \cr
#'                 `'_p'`     \tab pipe                \tab `'|'`                                                                             \cr
#'                 `'_d'  `   \tab dot                 \tab `'.'`                                                                             \cr
#'                 `'_b'`     \tab broken pipe         \tab `'¦'`                                                                               }
#'  \tabular{ll}{  `     `    \tab \eqn{^{(1)}} Splits strings into constituent characters, making `ss0(.)` functionally equivalent to `ch(.)`. }
#' \cr **Functions filtering for unique values**
#' \cr\cr Prepending `.U` to a function name reduces the result of calling the original function to unique values.
#' @param ... An arbitrary number of objects to be \link[=av]{atomized} before splitting.
#' @param x A \link[=chr_vec]{character vec} of string(s) to be split.
#' @param name A \link[=cmp_chr_scl]{complete character scalar} name of the variable to hold the original strings.
#' @param part A complete character scalar prefix for labeling components of vectors resulting from split strings.
#' @param .D A complete character vec of a delimiter or delimiters to use in splitting strings.
#' @param .TRM `TRUE` or `FALSE` indicating whether to trim white space from each side of each element of the result.
#' @param .SQZ `TRUE` or `FALSE` indicating whether to squeeze the result by removing extra internal whitespace.
#' @param .DROP `TRUE` or `FALSE`. For functions associated character-wise splitting (i.e., ending in `0` or `ch`), indicates whether to drop resulting values that are not letters, digits, or spaces. For all others, indicates whether to drop resulting blank string values.
#' @param .N An optional \link[=cmp_psw_scl]{complete positive whole-number vec} specifying one or more elements to be extracted from the result.
#' @param .U complete non-`NA` scalar indicating whether to reduce the result to unique values.
#' @return **A character vector**            \cr\cr `s_pdb uss_pdb` \cr `ss_db, uss_db` \cr `ss_pb, uss_pb` \cr `ss_pd, uss_pd` \cr `ss_b, uss_b` \cr `ss_d, uss_d` \cr `ss_p, uss_p` \cr `ss1, uss1` \cr `ss, uss`
#' \cr\cr  **A \link[=CH1]{onechar} vector** \cr\cr `ss0 uss0`      \cr `ch, chars, uch, uchars`
#' \cr\cr  **A data.frame **                 \cr\cr `ss_tb, uss_tb`
#' @examples
#' ss("", "super-cooled")
#' ss("|", "super||cooled", "super|heated")
#' ss_0("super-cooled", "super-heated")
#' ss_1("super cooled", "super heated")
#' ss_p("super|cooled", "super|heated", .U = TRUE)
#' ss_d("super.cooled", "super.heated")
#' ss_b("super¦cooled", "super¦heated")
#' ss_pd("super|cooled", "super.heated")
#' ss_pb("super|cooled", "super¦heated")
#' ss_db("super.cooled", "super¦heated")
#' ss_pdb("super|cooled¦|super|heated", .U = TRUE)
#' ss_pdb(" super|cooled  ¦super..  heated", .N = 3)
#' ss_pdb(" super|cooled  ¦super..  heated", .TRM = F, .SQZ = F, .DROP = F, .N = 3)
#' uss("", "super-cooled")
#' uss("|", "super|cooled", "super|heated")
#' uss_0("super-cooled", "super-heated")
#' uss_1("super cooled", "super heated")
#' uss_p("super|cooled", "super|heated")
#' uss_p("super|cooled", "super|heated")
#' uss_d("super.cooled", "super.heated")
#' uss_b("super¦cooled", "super¦heated")
#' uss_pd("super|cooled", "super.heated")
#' uss_pb("super|cooled", "super¦heated")
#' uss_db("super.cooled", "super¦heated")
#' uss_pdb("super|cooled¦super|heated")
#' ss_tb("|", 'a|b|c|d', 'e|f|g|h', 'i|j|k|l', 'm|n|o|p')
#' ss_tb("|", 'a|b|c|d', 'e|f|g|h', 'i|j|k|l', 'm|n|o|p', name = 'original', part = 'letter')
#' @export
ss <- function(.D, ..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .U = FALSE, .N = NULL) {
  Errors <- NULL
  if (!base::all(base::sapply(base::list(...), uj:::.CHR))) {Errors <- base::c(Errors, "[...] must contain at least one argument, all of which must be character generics (?chrGEN).")}
  if (!uj:::.cmp_lgl_scl(.SQZ)) {Errors <- base::c(Errors, "[.SQZ] must be TRUE or FALSE.")}
  if (!uj:::.cmp_lgl_scl(.TRM)) {Errors <- base::c(Errors, "[.TRM] must be TRUE or FALSE.")}
  if (!uj:::.cmp_chr_vec(.D)) {Errors <- base::c(Errors, "[.D] must be a complete character vec (?cmp_chr_vec).")}
  if (!uj:::.cmp_lgl_scl(.U)) {Errors <- base::c(Errors, "[.U] must be TRUE or FALSE.")}
  if (!uj::f0(base::is.null(.N), T, uj:::.cmp_psw_vec(.N))) {Errors <- base::c(Errors, "[.N] must be NULL or a complete positive whole-number vec (?cmp_psw_vec).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  x <- uj::av(...)
  for (d in .D) {x <- uj::av(base::strsplit(base::as.character(x), d, fixed = T))}
  if (.TRM) {x <- uj::trm(x)}
  if (.SQZ) {x <- uj::sqz(x)}
  if (.DROP) {x <- x[x != ""]}
  if (!base::is.null(.N)) {x <- x[.N]}
  if (.U) {x <- base::unique(x)}
  x
}

#' @rdname ss
#' @export
ch <- function(..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL, .U = FALSE) {
  x <- base::as.character(uj::av(...))
  if (.TRM) {x <- stringr::str_trim(x, side = "both")}
  if (.SQZ) {x <- stringr::str_squish(x)}
  x <- uj::ss("", x, .TRM = TRUE, .SQZ = TRUE, .DROP = FALSE, .N = .N, .U = .U)
  x <- x[x != ""]
  if (.DROP) {x <- x[x %in% base::c(letters, LETTERS, 0:9, " ")]}
  uj::ss("", ..., .TRM = .TRM, .SQZ = .SQZ, .N = .N, .U = .U)
}

#' @rdname ss
#' @export
chars <- ch

#' @rdname ss
#' @export
ss0 <- ch

#' @rdname ss
#' @export
ss_tb <- function(.D, ..., name = "string", part = "part", .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .U = FALSE, .N = NULL) {
  x <- uj::av(...)
  Y <- base::lapply(x, ss, .D = .D, .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .N = .N, .U = .U)
  nDots <- base::length(x)
  if (nDots > 1) {
    if (!uj::recyclable_ns(base::lengths(Y))) {uj::stopperr("[...] args must be recyclable upon splitting and post-processing using optional args.", .PKG = "uj")}
    Ns <- base::lengths(Y)
    if (base::length(base::unique(Ns)) > 1) {
      Reps <- base::max(Ns) / Ns
      if (base::any(Reps > 1)) {for (i in 1:nDots) {Y[i] <- base::rep.int(Y[[i]], Reps[i])}}
    }}
  Y <- base::matrix(Y, nrow = nDots, byrow = T)
  if (base::length(part) == 1) {base::colnames(Y) <- base::paste0(part, ".", 1:base::ncol(Y))} else {base::colnames(Y) <- part}
  x <- base::matrix(x, ncol = 1)
  base::colnames(x) <- name
  x <- base::cbind(x, Y)
  base::rownames(x) <- NULL
  tibble::as_tibble(x)
}

#' @rdname ss
#' @export
ss1 <- function(..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL, .U = FALSE) {uj::ss(" ", ..., .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .N = .N, .U = .U)}

#' @rdname ss
#' @export
ss_p <- function(..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL, .U = FALSE) {uj::ss("|", ..., .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .N = .N, .U = .U)}

#' @rdname ss
#' @export
ss_d <- function(..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL, .U = FALSE) {uj::ss(".", ..., .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .N = .N, .U = .U)}

#' @rdname ss
#' @export
ss_b <- function(..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL, .U = FALSE) {uj::ss("¦", ..., .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .N = .N, .U = .U)}

#' @rdname ss
#' @export
ss_pd <- function(..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL, .U = FALSE) {uj::ss(c("|", "."), ..., .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .N = .N, .U = .U)}

#' @rdname ss
#' @export
ss_pb <- function(..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL, .U = FALSE) {uj::ss(c("|", "¦"), ..., .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .N = .N, .U = .U)}

#' @rdname ss
#' @export
ss_db <- function(..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL, .U = FALSE) {uj::ss(c(".", "¦"), ..., .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .N = .N, .U = .U)}

#' @rdname ss
#' @export
ss_pdb <- function(..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL, .U = FALSE) {uj::ss(c("|", ".", "¦"), ..., .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .N = .N, .U = .U)}

#' @rdname ss
#' @export
uss <- function(d, ..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL) {uj::ss(d, ..., .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .U = TRUE, .N = .N)}

#' @rdname ss
#' @export
uch <- function(..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL) {uj::ch(..., .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .U = TRUE, .N = .N)}

#' @rdname ss
#' @export
uchars <- uch

#' @rdname ss
#' @export
uss0 <- function(..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL) {uj::ss0(..., .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .U = TRUE, .N = .N)}

#' @rdname ss
#' @export
uss_tb <- function(d, ..., name = "string", part = "part", .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL) {uj::ss_tb(d, ..., name = name, part = part, .SQZ = .SQZ, .DROP = .DROP, .U = TRUE, .N = .N)}

#' @rdname ss
#' @export
uss1 <- function(..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL) {uj::ss1(..., .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .U = TRUE, .N = .N)}

#' @rdname ss
#' @export
uss_p <- function(..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL) {uj::ss_p(..., .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .U = TRUE, .N = .N)}

#' @rdname ss
#' @export
uss_d <- function(..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL) {uj::ss_d(..., .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .U = TRUE, .N = .N)}

#' @rdname ss
#' @export
uss_b <- function(..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL) {uj::ss_b(..., .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .U = TRUE, .N = .N)}

#' @rdname ss
#' @export
uss_pd <- function(..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL) {uj::ss_pd(..., .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .U = TRUE, .N = .N)}

#' @rdname ss
#' @export
uss_pb <- function(..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL) {uj::ss_pb(..., .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .U = TRUE, .N = .N)}

#' @rdname ss
#' @export
uss_db <- function(..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL) {uj::ss_db(..., .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .U = TRUE, .N = .N)}

#' @rdname ss
#' @export
uss_pdb <- function(..., .TRM = TRUE, .SQZ = TRUE, .DROP = TRUE, .N = NULL) {uj::ss_pdb(..., .TRM = .TRM, .SQZ = .SQZ, .DROP = .DROP, .U = TRUE, .N = .N)}
