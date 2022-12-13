#' @name ss
#' @family chars
#' @family strings
#' @title Split Strings and Select/Check for Elements
#' @description **`ss`**
#' This function splits strings using the delimiter(s) in `d` following this sequence:
#' \enumerate{
#'   \item Reduce `...` to an atomic vector of all collective atomic values.
#'   \item Convert the result to mode character.
#'   \item Replace each element of the result with that element's constituent parts as delimited by `d`, producing a potentially longer character vector.
#'   \item If `n` is not `NULL`, extracts the `n`-th elements(s) from the result.
#'   \item If `trm` is `TRUE`, trims white space (i.e., spaces, tabs, newlines) from both ends of each element of the result.
#'   \item If `sqz` is `TRUE`, removes leading and trailing white space and replaces any multi-character interior white-space strings inside the result with a single space.
#'   \item If `u` is `TRUE`, reduces the result to unique values.
#' }
#' **Extension functions**
#' \cr Extension functions are supplied for common delimiters, signified by codes appended to `ss` function names:
#' \tabular{rll}{
#'     *Code*   \tab *Name* \tab   *Delimiter*
#'   \cr`'0'`   \tab blank  \tab   `''`
#'   \cr`'1'`   \tab space  \tab   `' '`
#'   \cr`'P'`   \tab pipe   \tab   `'|'`
#'   \cr`'D'`   \tab dot    \tab   `'.'`
#'   \cr`'B'`   \tab broken \tab   `'¦'`
#'   \cr        \tab pipe   \tab   
#' }
#' In addition, `ch` aliases `ss0`.
#' \cr\cr
#' **Unique-value functions**
#' \cr Prepending `u` to a function name reduces the result of calling that function to unique values.
#' \cr\cr
#' **`sstb`**
#' \cr Assumes that splitting each element of `x` along the delimiter `d` results vectors of the same length, which are placed into a data.frame. For example, the following console excerpt demonstrates a call to `sstb` and its result.
#' ```
#' > sstb('|', 'a|b|c|d', 'e|f|g|h', 'i|j|k|l', 'm|n|o|p', name = 'original', part = 'letter')
#'
#'   original letter.1 letter.2 letter.3 letter.4
#' 1  a|b|c|d        a        b        c        d
#' 2  e|f|g|h        e        f        g        h
#' 3  i|j|k|l        i        j        k        l
#' 4  m|n|o|p        m        n        o        p
#' ```
#' @param ... An arbitrary number of objects to be \link[=av]{atomized} before splitting.
#' @param x A \link[=chr_vec]{character vec} of string(s) to be split.
#' @param name A \link[=cmp_chr_scl]{complete character scalar} name of the variable to hold the original strings.
#' @param part A complete character scalar prefix for labeling components of vectors resulting from split strings.
#' @param d A complete character vec of delimiter or delimiters to use in splitting strings.
#' @param trm A non-`NA` logical scalar indicating whether to trim white space from each side of each element of the result.
#' @param sqz A non-`NA` logical scalar indicating whether to squeeze the result by removing extra internal whitespace.
#' @param drop A non-`NA` logical scalar. For functions associated character-wise splitting (i.e., ending in `0` or `ch`), indicates whether to drop resulting values that are not letters, digits, or spaces. For all others, indicates whether to drop resulting blank string values.
#' @param n An optional \link[=cmp_psw_scl]{complete positive whole-number scalar} specifying an element to be extracted from the result.
#' @param u complete non-`NA` scalar indicating whether to reduce the result to unique values.
#' @return All functions return a character vector except for `sstb`, which returns a data.frame.
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
  errs <- c(f0(all(sapply(list(...), ichr))  , NULL, "[...] must contain at least one argument, all of which must be character generics (?chr_gen)."),
            f0(isTF(sqz)                     , NULL, "[sqz] must be TRUE or FALSE."),
            f0(isTF(trm)                     , NULL, "[trm] must be TRUE or FALSE."),
            f0(cmp_chr_vec(d)                , NULL, "[d] must be a complete character vec (?cmp_chr_vec)."),
            f0(isTF(u)                       , NULL, "[u] must be TRUE or FALSE."),
            f0(f0(inll(n), T, cmp_psw_vec(n)), NULL, "[n] must be NULL or a complete positive whole-number vec (?cmp_psw_vec)."))
  if (!is.null(errs)) {stop(.errs(errs))}
  x <- av(...)
  for (dd in d) {x <- av(strsplit(as.character(av(x)), dd, fixed = T))}
  if (trm) {x <- trimws(x)}
  if (sqz) {x <- sqz(x)}
  if (drop) {x <- x[x != ""]}
  if (!inll(n)) {x <- x[n]}
  if (u) {x <- unique(x)}
  x
}

#' @rdname ss
#' @export
ss0 <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {
  x <- av(...)
  if (trm) {x <- trimws(x)}
  if (sqz) {x <- sqz(x)}
  x <- ss("", x, trm = TRUE, sqz = TRUE, drop = FALSE, n = n, u = u)
  x <- x[x != ""]
  if (drop) {x <- x[x %in% c(letters, LETTERS, 0:9, " ")]}
  ss("", ..., trm = trm, sqz = sqz, n = n, u = u)
}

#' @rdname ss
#' @export
ss1 <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {ss(" ", ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ssP <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {ss("|", ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ssD <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {ss(".", ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ssB <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {ss("¦", ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ssPD <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {ss(c("|", "."), ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ssPB <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {ss(c("|", "¦"), ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ssDB <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {ss(c(".", "¦"), ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ssPDB <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {ss(c("|", ".", "¦"), ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @rdname ss
#' @export
ch <- ss0

#' @rdname ss
#' @export
uss <- function(d, ..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {ss(d, ..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
uss0 <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {ss0(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
uss1 <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {ss1(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
ussP <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {ssP(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
ussD <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {ssD(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
ussB <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {ssB(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
ussPD <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {ssPD(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
ussPB <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {ssPB(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
ussDB <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {ssDB(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
ussPDB <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {ssPDB(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
uch <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {ch(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @rdname ss
#' @export
sstb <- function(d, ..., name = "string", part = "part") {
  SS <- function(X, D) {av(strsplit(X, D, fixed = TRUE))}
  x <- av(...)
  n <- length(x)
  y <- sapply(x, SS, D = d)
  y <- matrix(y, nrow = n, byrow = T)
  if (length(part) == 1) {colnames(y) <- paste0(part, ".", 1:nc(y))}             # : IF the same part label should be used, build the labels
  else {colnames(y) <- part}                                                     # : ELSE name with the full specified vector
  x <- matrix(x, ncol = 1)
  colnames(x) <- name                                                            # : put [x.] in its own column tibble
  x <- cbind(x, y)                                                               # : column bind the original and split versions
  rownames(x) <- NULL                                                            # : remove row names
  as.data.frame(x, stringsAsFactors = F)                                         # : return value
}                                                                                # END
