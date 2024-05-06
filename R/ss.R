#' @encoding UTF-8
#' @family strings
#' @family chars
#' @title Split strings and select/check for elements
#' @description *Order of operations*
#' \cr\cr All functions in this family follow the same order of operations when performing string splitting:
#' \itemize{\item \link[=av]{Atomize} `...`, collapsing it to a simple atomic vector.
#'          \item Coerce the result to a character vector.
#'          \item Split each element of the vector along the delimiter(s) in `d`, producing a potentially longer character vector.
#'          \item If `n` is not `NULL`, extract the `n`-th elements(s) from the result.
#'          \item If `trm = TRUE`, trim white space (i.e., spaces, tabs, newlines) from both ends of each element of the result.
#'          \item If `sqz = TRUE`, remove leading and trailing white space and replace any multi character interior white-space sequences inside the result with a single space.
#'          \item If `u = TRUE`, reduce the result to unique values.}
#' @details **The function** `ss`
#' \cr\cr Performs the operations as described in the *order of operations* section.
#' \cr\cr **The functions** `ch/chars`
#' \cr\cr Split strings in constituent characters (by using a blank-string delimiter).
#' \cr\cr **The function** `sstb`
#' \cr\cr Requires that splitting each element of `x` along the delimiter `d` and post-processing the results based on optional args `trm`, `sqz`, `drop`, `n`, and `u` will result in vectors of the same length. Those resulting same-length vectors are then placed into a data.frame. For example, the following console excerpt demonstrates a call to `ss_tb` and its result.
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
#' \cr\cr Prepending `u` to a function name reduces the result of calling the original function to unique values.
#' @param ... An arbitrary number of objects to be \link[=av]{atomized} before splitting.
#' @param x A \link[=chr_vec]{character vec} of string(s) to be split.
#' @param name A \link[=cmp_chr_scl]{complete character scalar} name of the variable to hold the original strings.
#' @param part A complete character scalar prefix for labeling components of vectors resulting from split strings.
#' @param d A complete character vec of a delimiter or delimiters to use in splitting strings.
#' @param trm `TRUE` or `FALSE` indicating whether to trim white space from each side of each element of the result.
#' @param sqz `TRUE` or `FALSE` indicating whether to squeeze the result by removing extra internal whitespace.
#' @param drop `TRUE` or `FALSE`. For functions associated character-wise splitting (i.e., ending in `0` or `ch`), indicates whether to drop resulting values that are not letters, digits, or spaces. For all others, indicates whether to drop resulting blank string values.
#' @param n An optional \link[=cmp_psw_scl]{complete positive whole-number vec} specifying one or more elements to be extracted from the result.
#' @param u complete non-`NA` scalar indicating whether to reduce the result to unique values.
#' @return **A character vector**            \cr\cr `s_pdb uss_pdb` \cr `ss_db, uss_db` \cr `ss_pb, uss_pb` \cr `ss_pd, uss_pd` \cr `ss_b, uss_b` \cr `ss_d, uss_d` \cr `ss_p, uss_p` \cr `ss1, uss1` \cr `ss, uss`
#' \cr\cr  **A \link[=CH1]{onechar} vector** \cr\cr `ss0 uss0`      \cr `ch, chars, uch, uchars`
#' \cr\cr  **A data.frame **                 \cr\cr `ss_tb, uss_tb`
#' @examples
#' ss("", "super-cooled")
#' ss("|", "super||cooled", "super|heated")
#' ss_0("super-cooled", "super-heated")
#' ss_1("super cooled", "super heated")
#' ss_p("super|cooled", "super|heated", u = TRUE)
#' ss_d("super.cooled", "super.heated")
#' ss_b("super¦cooled", "super¦heated")
#' ss_pd("super|cooled", "super.heated")
#' ss_pb("super|cooled", "super¦heated")
#' ss_db("super.cooled", "super¦heated")
#' ss_pdb("super|cooled¦|super|heated", u = TRUE)
#' ss_pdb(" super|cooled  ¦super..  heated", n = 3)
#' ss_pdb(" super|cooled  ¦super..  heated", trm = F, sqz = F, drop = F, n = 3)
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
ss <- function(d, ..., trm = TRUE, sqz = TRUE, drop = TRUE, u = FALSE, n = NULL) {
  errs <- NULL
  if (!base::all(base::sapply(base::list(...), uj::.CHR))) {errs <- base::c(errs, "[...] must contain at least one argument, all of which must be character generics (?chr_gen).")}
  if (!uj::.cmp_lgl_scl(sqz)) {errs <- base::c(errs, "[sqz] must be TRUE or FALSE.")}
  if (!uj::.cmp_lgl_scl(trm)) {errs <- base::c(errs, "[trm] must be TRUE or FALSE.")}
  if (!uj::.cmp_chr_vec(d)) {errs <- base::c(errs, "[d] must be a complete character vec (?cmp_chr_vec).")}
  if (!uj::.cmp_lgl_scl(u)) {errs <- base::c(errs, "[u] must be TRUE or FALSE.")}
  if (!uj::f0(base::isnull(n), T, uj::.cmp_psw_vec(n))) {errs <- base::c(errs, "[n] must be NULL or a complete positive whole-number vec (?cmp_psw_vec).")}
  if (!base::isnull(errs)) {uj::stopperr(errs, pkg = "uj")}
  x <- uj::av(...)
  for (d in d) {x <- uj::av(base::strsplit(base::as.character(x), d, fixed = T))}
  if (trm) {x <- uj::trm(x)}
  if (sqz) {x <- uj::sqz(x)}
  if (drop) {x <- x[x != ""]}
  if (!base::isnull(n)) {x <- x[n]}
  if (u) {x <- base::unique(x)}
  x
}

#' @rdname ss
#' @export
ch <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {
  x <- base::as.character(uj::av(...))
  if (trm) {x <- stringr::str_trim(x, side = "both")}
  if (sqz) {x <- stringr::str_squish(x)}
  x <- uj::ss("", x, trm = TRUE, sqz = TRUE, drop = FALSE, n = n, u = u)
  x <- x[x != ""]
  if (drop) {x <- x[x %in% base::c(letters, LETTERS, 0:9, " ")]}
  uj::ss("", ..., trm = trm, sqz = sqz, n = n, u = u)
}

#' @rdname ss
#' @export
chars <- ch

#' @rdname ss
#' @export
ss0 <- ch

#' @rdname ss
#' @export
ss_tb <- function(d, ..., name = "string", part = "part", trm = TRUE, sqz = TRUE, drop = TRUE, u = FALSE, n = NULL) {
  x <- uj::av(...)
  y <- base::lapply(x, ss, d = d, trm = trm, sqz = sqz, drop = drop, n = n, u = u)
  nDots <- base::length(x)
  if (nDots > 1) {
    if (!uj::recyclable_ns(base::lengths(y))) {uj::stopperr("[...] args must be recyclable upon splitting and post-processing using optional args.", pkg = "uj")}
    ns <- base::lengths(y)
    if (base::length(base::unique(ns)) > 1) {
      reps <- base::max(ns) / ns
      if (base::any(reps > 1)) {for (i in 1:nDots) {y[i] <- base::rep.int(y[[i]], reps[i])}}
    }}
  y <- base::matrix(y, nrow = nDots, byrow = T)
  if (base::length(part) == 1) {base::colnames(y) <- base::paste0(part, ".", 1:base::ncol(y))} else {base::colnames(y) <- part}
  x <- base::matrix(x, ncol = 1)
  base::colnames(x) <- name
  x <- base::cbind(x, y)
  base::rownames(x) <- NULL
  tibble::as_tibble(x)
}

#' @rdname ss
#' @export
ss1 <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss(" ", ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ss_p <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss("|", ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ss_d <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss(".", ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ss_b <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss("¦", ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ss_pd <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss(c("|", "."), ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ss_pb <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss(c("|", "¦"), ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ss_db <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss(c(".", "¦"), ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ss_pdb <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss(c("|", ".", "¦"), ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
uss <- function(d, ..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ss(d, ..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
uch <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ch(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
uchars <- uch

#' @rdname ss
#' @export
uss0 <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ss0(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
uss_tb <- function(d, ..., name = "string", part = "part", trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ss_tb(d, ..., name = name, part = part, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
uss1 <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ss1(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
uss_p <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ss_p(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
uss_d <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ss_d(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
uss_b <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ss_b(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
uss_pd <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ss_pd(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
uss_pb <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ss_pb(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
uss_db <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ss_db(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
uss_pdb <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ss_pdb(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}
