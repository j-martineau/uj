#' @name ss
#' @family chars
#' @family strings
#' @title Split Strings and Select/Check for Elements
#' @description **`ss`** splits strings using the delimiter(s) in `d` following this sequence:
#'   \enumerate{
#'   \item Reduce `...` to an atomic vector of all collective atomic values.
#'   \item Convert the result to mode character.
#'   \item Replace each element of the result with that element's constituent parts as delimited by `d`, producing a potentially longer character vector.
#'   \item If `n` is not `NULL`, extracts the `n`-th elements(s) from the result.
#'   \item If `trm` is `TRUE`, trims white space (i.e., spaces, tabs, newlines) from both ends of each element of the result.
#'   \item If `sqz` is `TRUE`, removes leading and trailing white space and replaces any multi-character interior white-space strings inside the result with a single space.
#'   \item If `u` is `TRUE`, reduces the result to unique values.}
#'   **`ch`** does the same as `ss` but with a blank string delimiter, resulting in a vector of single characters (if `sqz = TRUE`), also removes blank strings from the result.
#'   \cr\cr
#'   **Extension functions** are supplied for common delimiters, signified by codes appended to `ss` function names:\tabular{ll}{
#'     *Code (name)*            \tab *Delimiter invoked*
#'     \cr`'1'` (space)         \tab`' '`
#'     \cr`'P'` (pipe)          \tab`'|'`
#'     \cr`'D'` (dot)           \tab`'.'`
#'     \cr`'B'` (broken pipe)   \tab`'¦'`
#'   }
#'   **`uch`** reduces the result of `ch` to unique values.
#'   \cr\cr
#'   **`sstb`** assumes that splitting each element of `x` along the delimiter `d` results vectors of the same length, which are placed into a data.frame. For example, the following console excerpt demonstrates a call to `sstb` and its result.
#'   ```
#'   > sstb(x = c('a|b|c|d', 'e|f|g|h', 'i|j|k|l', 'm|n|o|p'),
#'   +      d = '|', name = 'original', part = 'letter')
#'
#'     original letter.1 letter.2 letter.3 letter.4
#'   1  a|b|c|d        a        b        c        d
#'   2  e|f|g|h        e        f        g        h
#'   3  i|j|k|l        i        j        k        l
#'   4  m|n|o|p        m        n        o        p
#'   ```
#' @param ... An arbitrary number of objects to be \link[=av]{atomized} before splitting.
#' @param x A \link[=chr_vec]{character vec} of string(s) to be split.
#' @param name A \link[=cmp_chr_scl]{complete character scalar} name of the variable to hold the original strings.
#' @param part A complete character scalar prefix for labeling components of vectors resulting from split strings.
#' @param d A complete character vec of delimiter or delimiters to use in splitting strings.
#' @param trm A non-`NA` logical scalar indicating whether to trim white space from each side of each element of the result.
#' @param sqz A non-`NA` logical scalar indicating whether to squeeze the result by removing either empty strings (for `ss` functions) or characters that are neither letters, digits, nor spaces (for `ch`).
#' @param n An optional \link[=cmp_psw_scl]{complete positive whole-number scalar} specifying an element to be extracted from the result. @param u complete non-`NA` scalar indicating whether to reduce the result to unique values.
#' @return \itemize{
#'   \item **`sstb`**: a data.frame.
#'   \item **all others**: a character vector.
#' }
#' @export
ss <- function(d, ..., trm = T, sqz = T, u = F, n = NULL) {
  errs <- c(f0(all(sapply(list(...), ichr))  , NULL, "\n \u2022 [...] must contain at least one argument, all of which must be character generics (?chr_gen)."),
            f0(cmp_chr_vec(d)                , NULL, "\n \u2022 [d] must be a complete character vec (?cmp_chr_vec)."),
            f0(isTF(trm)                     , NULL, "\n \u2022 [trm] must be TRUE or FALSE."),
            f0(isTF(sqz)                     , NULL, "\n \u2022 [sqz] must be TRUE or FALSE."),
            f0(isTF(u)                       , NULL, "\n \u2022 [u] must be TRUE or FALSE."),
            f0(f0(inll(n), T, cmp_psw_vec(n)), NULL, "\n \u2022 [n] must be NULL or a complete positive whole-number vec (?cmp_psw_vec)."))
  if (!is.null(errs)) {stop(errs)}
  x <- av(...)
  for (dd in d) {x <- av(strsplit(as.character(av(x)), dd, fixed = T))}
  if (trm) {x <- trimws(x)}
  if (sqz) {x <- x[x != ""]}
  if (!inll(n)) {x <- x[n]}
  if (u) {x <- unique(x)}
  x
}

#' @rdname ss
#' @export
ss1 <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss(" ", ..., trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssP <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss("|", ..., trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssD <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss(".", ..., trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssB <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss("¦", ..., trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssPD <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss(c("|", "."), ..., trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssPB <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss(c("|", "¦"), ..., trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssDB <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss(c(".", "¦"), ..., trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ssPDB <- function(..., trm = T, sqz = T, n = NULL, u = F) {ss(c("|", ".", "¦"), ..., trm = trm, sqz = sqz, n = NULL, u = F)}

#' @rdname ss
#' @export
ch <- function(..., trm = T, sqz = T, n = NULL, u = F) {
  errs <- c(f0(all(sapply(list(...), ichr))     , NULL, "\n \u2022 [...] must contain at least one argument, all of which must be character generics (?chr_gen)."),
            f0(isTF(trm)                        , NULL, "\n \u2022 [trm] must be TRUE or FALSE."),
            f0(isTF(sqz)                        , NULL, "\n \u2022 [sqz] must be TRUE or FALSE."),
            f0(isTF(u)                          , NULL, "\n \u2022 [u] must be TRUE or FALSE."),
            f0(f0(is.null(n), T, cmp_psw_vec(n)), NULL, "\n \u2022 [n] must be NULL or a complete positive whole-number vec (?cmp_psw_vec)."))
  if (!is.null(errs)) {stop(errs)}
  x <- ss("", av(...), trm = trm, u = u)
  if (sqz) {x <- x[x %in% c(letters, LETTERS, 0:9, " ")]}
  if (idef(n)) {x <- x[n]}
  x
}

#' @rdname ss
#' @export
uch <- function(..., trm = T, sqz = T, n = NULL) {ch(..., trm = trm, sqz = sqz, n = n, u = T)}

#' @rdname ss
#' @export
sstb <- function(x, d, name = "string", parts = "part") {
  ss0 <- function(xx, dd) {ss(dd, xx)}
  x <- tibble::tibble(x)
  y <- NULL
  for (xx in x) {}
  y <- t(sapply(x, ss0, dd = d))                                                 # : transpose matrix after applying split string to each value of [x.]
  if (n1(parts)) {colnames(y) <- da0(parts, v(dot), 1:nc(y))}                    # : IF the same part label should be used, build the labels
  else {colnames(y) <- parts}                                                    # : ELSE name with the full specified vector
  colnames(x) <- name                                                            # : put [x.] in its own column tibble
  x <- cb(x, y)                                                                  # : column bind the original and split versions
  rownames(x) <- NULL                                                            # : remove row names
  x                                                                              # : return value
}                                                                                # END
