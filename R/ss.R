#' @encoding UTF-8
#' @family strings
#' @family chars
#' @title Split strings and select/check for elements
#' @description All functions in this family follow the same order of operations when performing string splitting:
#' \cr
#' \cr **Order of operations**
#' \tabular{rl}{
#'       `1.`  \tab \link[=av]{Atomize} `...`, collapsing
#'   \cr       \tab it to a simple atomic  vector.
#'   \cr `2.`  \tab Coerce the result to a character vector.
#'   \cr `3.`  \tab Split each element of the vector along
#'   \cr       \tab the delimiter(s) in `d`, producing a
#'   \cr       \tab potentially longer character vector.
#'   \cr `4.`  \tab If `n` is not `NULL`, extract the `n`-th
#'   \cr       \tab elements(s) from the result.
#'   \cr `5.`  \tab If `trm = TRUE`, trim white space
#'   \cr       \tab (i.e., spaces, tabs, newlines) from both
#'   \cr       \tab ends of each element of the result.
#'   \cr `6.`  \tab If `sqz = TRUE`, remove leading and
#'   \cr       \tab trailing white space and replace any
#'   \cr       \tab multi character interior white-space
#'   \cr       \tab sequences inside the result with a
#'   \cr       \tab single space.
#'   \cr `7.`  \tab If `u = TRUE`, reduce the result to
#'   \cr       \tab unique values.
#' }
#' \cr
#' **The function **`ss`
#' \cr
#' \cr Performs the operations as described in the *order of operations* section.
#' \cr
#' \cr
#' \cr **The functions **`ch`** and** `chars`
#' \cr
#' \cr Split strings in constituent characters (by using a blank-string delimiter).
#' \cr
#' \cr
#' \cr **The function **`sstb`
#' \cr
#' \cr Requires that splitting each element of `x` along the delimiter `d` and post-processing the results based on optional args `trm`, `sqz`, `drop`, `n`, and `u` will result in vectors of the same length. Those resulting same-length vectors are then placed into a data.frame. For example, the following console excerpt demonstrates a call to `sstb` and its result.
#' ```
#' > sstb('|', 'a|b|c|d', 'e|f|g|h', 'i|j|k|l', 'm|n|o|p', name = 'original', part = 'letter')
#'
#'   original letter.1 letter.2 letter.3 letter.4
#' 1  a|b|c|d        a        b        c        d
#' 2  e|f|g|h        e        f        g        h
#' 3  i|j|k|l        i        j        k        l
#' 4  m|n|o|p        m        n        o        p
#' ```
#' \cr
#' **Functions extending **`ss`** for common delimiters**
#' \cr
#' \cr Extension functions are supplied for common delimiters, signified by codes appended to `ss` function names:
#' \tabular{cll}{
#'    *Code*   \tab *Name*        \tab   *Common delimiter*
#'   \cr `'0'` \tab blank`*`      \tab   `''`
#'   \cr `'1'` \tab space         \tab   `' '`
#'   \cr `'P'` \tab pipe          \tab   `'|'`
#'   \cr `'D'` \tab dot           \tab   `'.'`
#'   \cr `'B'` \tab broken pipe   \tab   `'¦'`
#' }
#' `*` Splits strings into constituent characters, making `ss0(.)` functionally equivalent to `ch(.)`.
#' \cr
#' \cr
#' \cr **Functions filtering for unique values**
#' \cr
#' \cr Prepending `u` to a function name reduces the result of calling the original function to unique values.
#' @param ... An arbitrary number of objects to be \link[=av]{atomized} before splitting.
#' @param x A \link[=chr_vec]{character vec} of string(s) to be split.
#' @param name A \link[=cmp_chr_scl]{complete character scalar} name of the variable to hold the original strings.
#' @param part A complete character scalar prefix for labeling components of vectors resulting from split strings.
#' @param d A complete character vec of delimiter or delimiters to use in splitting strings.
#' @param trm A non-`NA` logical scalar indicating whether to trim white space from each side of each element of the result.
#' @param sqz A non-`NA` logical scalar indicating whether to squeeze the result by removing extra internal whitespace.
#' @param drop A non-`NA` logical scalar. For functions associated character-wise splitting (i.e., ending in `0` or `ch`), indicates whether to drop resulting values that are not letters, digits, or spaces. For all others, indicates whether to drop resulting blank string values.
#' @param n An optional \link[=cmp_psw_scl]{complete positive whole-number vec} specifying one or more elements to be extracted from the result.
#' @param u complete non-`NA` scalar indicating whether to reduce the result to unique values.
#' @return *A character vector*
#'   \cr   `ssPDB, ussPDB`
#'   \cr   `ssDB,  ussDB`
#'   \cr   `ssPB,  ussPB`
#'   \cr   `ssPD,  ussPD`
#'   \cr   `ssB,   ussB`
#'   \cr   `ssD,   ussD`
#'   \cr   `ssP,   ussP`
#'   \cr   `ss1,   uss1`
#'   \cr   `ss,    uss`
#'   \cr\cr *A \link[=ich1]{1-char} vector*
#'   \cr   `ss0, uss0`
#'   \cr   `ch,  uch`
#'   \cr\cr *A data.frame *
#'   \cr   `sstb, usstb`
#' @examples
#' ss("", "super-cooled")
#' ss("|", "super||cooled", "super|heated")
#' ss0("super-cooled", "super-heated")
#' ss1("super cooled", "super heated")
#' ssP("super|cooled", "super|heated", u = TRUE)
#' ssD("super.cooled", "super.heated")
#' ssB("super¦cooled", "super¦heated")
#' ssPD("super|cooled", "super.heated")
#' ssPB("super|cooled", "super¦heated")
#' ssDB("super.cooled", "super¦heated")
#' ssPDB("super|cooled¦|super|heated", u = TRUE)
#' ssPDB(" super|cooled  ¦super..  heated", n = 3)
#' ssPDB(" super|cooled  ¦super..  heated", trm = F, sqz = F, drop = F, n = 3)
#'
#' uss("", "super-cooled")
#' uss("|", "super|cooled", "super|heated")
#' uss0("super-cooled", "super-heated")
#' uss1("super cooled", "super heated")
#' ussP("super|cooled", "super|heated")
#' ussP("super|cooled", "super|heated")
#' ussD("super.cooled", "super.heated")
#' ussB("super¦cooled", "super¦heated")
#' ussPD("super|cooled", "super.heated")
#' ussPB("super|cooled", "super¦heated")
#' ussDB("super.cooled", "super¦heated")
#' ussPDB("super|cooled¦super|heated")
#'
#' sstb("|", 'a|b|c|d', 'e|f|g|h', 'i|j|k|l', 'm|n|o|p')
#' sstb("|", 'a|b|c|d', 'e|f|g|h', 'i|j|k|l', 'm|n|o|p', name = 'original', part = 'letter')
#' @export
ss <- function(d, ..., trm = TRUE, sqz = TRUE, drop = TRUE, u = FALSE, n = NULL) {
  errs <- base::c(f0(base::all(base::sapply(base::list(...), uj::ichr))  , NULL, "[...] must contain at least one argument, all of which must be character generics (?chr_gen)."),
                  uj::f0(uj::isTF(sqz)                             , NULL, "[sqz] must be TRUE or FALSE."),
                  uj::f0(uj::isTF(trm)                             , NULL, "[trm] must be TRUE or FALSE."),
                  uj::f0(uj::cmp_chr_vec(d)                        , NULL, "[d] must be a complete character vec (?cmp_chr_vec)."),
                  uj::f0(uj::isTF(u)                               , NULL, "[u] must be TRUE or FALSE."),
                  uj::f0(uj::f0(uj::inll(n), T, uj::cmp_psw_vec(n)), NULL, "[n] must be NULL or a complete positive whole-number vec (?cmp_psw_vec)."))
  if (!base::is.null(errs)) {stop(uj:::.errs(errs))}
  x <- uj::av(...)
  for (dd in d) {x <- uj::av(base::strsplit(base::as.character(uj::av(x)), dd, fixed = T))}
  if (trm) {x <- base::trimws(x)}
  if (sqz) {x <- uj::sqz(x)}
  if (drop) {x <- x[x != ""]}
  if (!uj::inll(n)) {x <- x[n]}
  if (u) {x <- base::unique(x)}
  x
}

#' @rdname ss
#' @export
ch <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {
  x <- uj::av(...)
  if (trm) {x <- base::trimws(x)}
  if (sqz) {x <- uj::sqz(x)}
  x <- uj::ss("", x, trm = TRUE, sqz = TRUE, drop = FALSE, n = n, u = u)
  x <- x[x != ""]
  if (drop) {x <- x[x %in% c(letters, LETTERS, 0:9, " ")]}
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
sstb <- function(d, ..., name = "string", part = "part", trm = TRUE, sqz = TRUE, drop = TRUE, u = FALSE, n = NULL) {
  x <- uj::av(...)
  y <- base::lapply(x, ss, d = d, trm = trm, sqz = sqz, drop = drop, n = n, u = u)
  nd <- base::length(x)
  if (nd > 1) {
    if (!uj::recyclable_n(base::lengths(y))) {stop(uj:::.errs("[...] args must be recyclable upon splitting and post-processing using optional args."))}
    ns <- base::lengths(y)
    if (base::length(base::unique(ns)) > 1) {
      reps <- base::max(base::lengths(y)) / base::lengths(y)
      if (base::any(reps > 1)) {for (i in 1:nd) {y[i] <- base::rep.int(y[[i]], reps[i])}}
    }}
  y <- base::matrix(y, nrow = nd, byrow = T)
  if (base::length(part) == 1) {base::colnames(y) <- base::paste0(part, ".", 1:uj::nc(y))}
  else {base::colnames(y) <- part}
  x <- base::matrix(x, ncol = 1)
  base::colnames(x) <- name
  x <- base::cbind(x, y)
  base::rownames(x) <- NULL
  base::as.data.frame(x, stringsAsFactors = F)
}

#' @rdname ss
#' @export
ss1 <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss(" ", ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ssP <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss("|", ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ssD <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss(".", ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ssB <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss("¦", ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ssPD <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss(c("|", "."), ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ssPB <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss(c("|", "¦"), ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ssDB <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss(c(".", "¦"), ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ssPDB <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss(c("|", ".", "¦"), ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
uss <- function(d, ..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ss(d, ..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
uch <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ch(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
uss0 <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ss0(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
usstb <- function(d, ..., name = "string", part = "part", trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::sstb(d, ..., name = name, part = part, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
uss1 <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ss1(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
ussP <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ssP(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
ussD <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ssD(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
ussB <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ssB(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
ussPD <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ssPD(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
ussPB <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ssPB(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
ussDB <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ssDB(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
ussPDB <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ssPDB(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}
