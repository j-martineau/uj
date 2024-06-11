#' @encoding UTF-8
#' @family strings
#' @family chars
#' @title Split strings and select/check for elements
#' @description *Order of operations*
#' \cr\cr All functions in this family follow the same order of operations based on processing specs in `d`, `trm`, `sqz`, `n`, and `u` in performing string splitting:
#' \itemize{\item \link[=av]{Atomize} `...`, collapsing it to a simple atomic vector.
#'          \item Coerce the result to a character vector.
#'          \item Split each element of the vector along the delimiter(s) in `d`, producing a potentially longer character vector.
#'          \item If `n` is not `NULL`, extract the `n`-th elements(s) from the result.
#'          \item If `trm = TRUE`, trim white space (i.e., spaces, tabs, newlines) from both ends of each element of the result.
#'          \item If `sqz = TRUE`, remove leading and trailing white space and replace any multi character interior white-space sequences inside the result with a single space.
#'          \item If `u = TRUE`, reduce the result to unique values.}
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
#' @examples
#' ss("", "super-cooled")
#' ss("|", "super||cooled", "super|heated")
#' ss0("super-cooled", "super-heated")
#' ss1("super cooled", "super heated")
#' ssp("super|cooled", "super|heated", u = TRUE)
#' ssd("super.cooled", "super.heated")
#' ssb("super¦cooled", "super¦heated")
#' sspd("super|cooled", "super.heated")
#' sspb("super|cooled", "super¦heated")
#' ssdb("super.cooled", "super¦heated")
#' sspdb("super|cooled¦|super|heated", u = TRUE)
#' sspdb(" super|cooled  ¦super..  heated", n = 3)
#' sspdb(" super|cooled  ¦super..  heated", trm = F, sqz = F, drop = F, n = 3)
#' uss("", "super-cooled")
#' uss("|", "super|cooled", "super|heated")
#' uss0("super-cooled", "super-heated")
#' uss1("super cooled", "super heated")
#' ussp("super|cooled", "super|heated")
#' ussp("super|cooled", "super|heated")
#' ussd("super.cooled", "super.heated")
#' ussb("super¦cooled", "super¦heated")
#' usspd("super|cooled", "super.heated")
#' usspb("super|cooled", "super¦heated")
#' ussdb("super.cooled", "super¦heated")
#' usspdb("super|cooled¦super|heated")
#' sstb("|", 'a|b|c|d', 'e|f|g|h', 'i|j|k|l', 'm|n|o|p')
#' sstb("|", 'a|b|c|d', 'e|f|g|h', 'i|j|k|l', 'm|n|o|p', name = 'original', part = 'letter')
#' @export
ss_help <- function() {utils::help("ss_help", package = "uj")}

#' @describeIn ss_help Splits strings in `...` along the delimiter `d` subject to processing specs in `trm`, `sqz`, `drop`, `u`, and `n`. Returns a character vector.
#' @export
ss <- function(d, ..., trm = TRUE, sqz = TRUE, drop = TRUE, u = FALSE, n = NULL) {
  errs <- NULL
  if (!base::all(base::sapply(base::list(...), uj::.CHR))) {errs <- base::c(errs, "[...] must contain at least one argument, all of which must be character generics (?chr_gen).")}
  if (!uj::.cmp_lgl_scl(sqz)) {errs <- base::c(errs, "[sqz] must be TRUE or FALSE.")}
  if (!uj::.cmp_lgl_scl(trm)) {errs <- base::c(errs, "[trm] must be TRUE or FALSE.")}
  if (!uj::.cmp_chr_vec(d)) {errs <- base::c(errs, "[d] must be a complete character vec (?cmp_chr_vec).")}
  if (!uj::.cmp_lgl_scl(u)) {errs <- base::c(errs, "[u] must be TRUE or FALSE.")}
  if (!uj::f0(base::is.null(n), T, uj::.cmp_psw_vec(n))) {errs <- base::c(errs, "[n] must be NULL or a complete positive whole-number vec (?cmp_psw_vec).")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  x <- uj::av(...)
  for (d in d) {x <- uj::av(base::strsplit(base::as.character(x), d, fixed = T))}
  if (trm) {x <- uj::trm(x)}
  if (sqz) {x <- uj::sqz(x)}
  if (drop) {x <- x[x != ""]}
  if (!base::is.null(n)) {x <- x[n]}
  if (u) {x <- base::unique(x)}
  x
}

#' @describeIn ss_help Requires that splitting each element of `x` along the delimiter `d` and post-processing the results based on optional args `trm`, `sqz`, `drop`, `n`, and `u` will result in vectors of the same length. Those resulting same-length vectors are then placed into a data.frame. For example, the following console excerpt demonstrates a call to `ss_tb` and its result.
#' ```
#' > sstb('|', 'a|b|c|d', 'e|f|g|h', 'i|j|k|l', 'm|n|o|p', name = 'original', part = 'letter')
#'
#'   original letter.1 letter.2 letter.3 letter.4
#' 1  a|b|c|d        a        b        c        d
#' 2  e|f|g|h        e        f        g        h
#' 3  i|j|k|l        i        j        k        l
#' 4  m|n|o|p        m        n        o        p
#' ```
#' @export
sstb <- function(d, ..., name = "string", part = "part", trm = TRUE, sqz = TRUE, drop = TRUE, u = FALSE, n = NULL) {
  x <- uj::av(...)
  y <- base::lapply(x, ss, d = d, trm = trm, sqz = sqz, drop = drop, n = n, u = u)
  nDots <- base::length(x)
  if (nDots > 1) {
    if (!uj::recyclable_ns(base::lengths(y))) {uj::stopperr("[...] args must be recyclable upon splitting and post-processing using optional args.")}
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

#' @describeIn ss_help Split strings in `...` into constituent characters subject to processing specs in `trm`, `sqz`, `drop`, `n`, and `u`. Returns a character vector composed of single characters.
#' @export
ss0 <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {
  x <- base::as.character(uj::av(...))
  if (trm) {x <- stringr::str_trim(x, side = "both")}
  if (sqz) {x <- stringr::str_squish(x)}
  x <- uj::ss("", x, trm = TRUE, sqz = TRUE, drop = FALSE, n = n, u = u)
  x <- x[x != ""]
  if (drop) {x <- x[x %in% base::c(letters, LETTERS, 0:9, " ")]}
  uj::ss("", ..., trm = trm, sqz = sqz, n = n, u = u)
}

#' @describeIn ss_help An alias for `ss0(.)`
#' @export
chars <- ss0

#' @describeIn ss_help An alias for `ss0(.)`
#' @export
ch <- ss0

#' @describeIn ss_help Splits strings in `...` using a space (`' '`) as a delimiter, subject to processing specs in `trm`, `sqz`, `drop`, `n`, and `u`. Returns a character vector.
#' @export
ss1 <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss(" ", ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @describeIn ss_help Splits strings in `...` using a pipe (`'|'`) as a delimiter, subject to processing specs in `trm`, `sqz`, `drop`, `n`, and `u`. Returns a character vector.
#' @export
ssp <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss("|", ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @describeIn ss_help Splits strings in `...` using a dot/period (`'.'`) as a delimiter, subject to processing specs in `trm`, `sqz`, `drop`, `n`, and `u`. Returns a character vector.
#' @export
ssd <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss(".", ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @describeIn ss_help Splits strings in `...` using a broken pipe (`'¦'`) as a delimiter, subject to processing specs in `trm`, `sqz`, `drop`, `n`, and `u`. Returns a character vector.
#' @export
ssb <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss("¦", ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @describeIn ss_help Splits strings in `...` using both pipes (`'|'`) and dots/periods (`'.'`) as delimiters, subject to processing specs in `trm`, `sqz`, `drop`, `n`, and `u`. Returns a character vector.
#' @export
sspd <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss(c("|", "."), ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @describeIn ss_help Splits strings in `...` using both pipes (`'|'`) and broken pipe (`'¦'`) as delimiters, subject to processing specs in `trm`, `sqz`, `drop`, `n`, and `u`. Returns a character vector.
#' @export
sspb <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss(c("|", "¦"), ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @describeIn ss_help Splits strings in `...` using both dots/periods (`'.'`) and broken pipe (`'¦'`) as delimiters, subject to processing specs in `trm`, `sqz`, `drop`, `n`, and `u`. Returns a character vector.
#' @export
ssdb <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss(c(".", "¦"), ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @describeIn ss_help Splits strings in `...` using pipes (`'|'`), dots/periods (`'.'`), and broken pipe (`'¦'`) as delimiters, subject to processing specs in `trm`, `sqz`, `drop`, `n`, and `u`. Returns a character vector.
#' @export
sspdb <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL, u = FALSE) {uj::ss(c("|", ".", "¦"), ..., trm = trm, sqz = sqz, drop = drop, n = n, u = u)}

#' @describeIn ss_help Splits strings in `...` using the delimiter `d` and returns the unique values with pre-processing subject to specs in `trm`, `sqz`, and `drop` and post-processing subject to `n`. Returns a character vector.
#' @export
uss <- function(d, ..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ss(d, ..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @describeIn ss_help Splits strings in `...` into constituent character and returns the unique values with pre-processing subject to specs in `trm`, `sqz`, and `drop` and post-processing subject to `n`. Returns a character vector.
#' @export
uss0 <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ch(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @describeIn ss_help An alias for `uss0`.
#' @export
uchars <- uss0

#' @describeIn ss_help An alias for `uss0`.
#' @export
uch <- uss0

#' @describeIn ss_help Calls `sstb` and returns only unique rows of the result,  with pre-processing subject to specs in `trm`, `sqz`, and `drop` and post-processing subject to `n`. Returns a data.frame.
#' @export
usstb <- function(d, ..., name = "string", part = "part", trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ss_tb(d, ..., name = name, part = part, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @describeIn ss_help Splits strings in `...` using a space (`' '`) as delimiter and returns the unique values with pre-processing subject to specs in `trm`, `sqz`, and `drop` and post-processing subject to `n`. Returns a character vector.
#' @export
uss1 <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ss1(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @describeIn ss_help Splits strings in `...` using a pipe (`'|'`) as delimiter and returns the unique values with pre-processing subject to specs in `trm`, `sqz`, and `drop` and post-processing subject to `n`. Returns a character vector.
#' @export
ussp <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ssp(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @describeIn ss_help Splits strings in `...` using a dot/period (`'.'`) as delimiter and returns the unique values with pre-processing subject to specs in `trm`, `sqz`, and `drop` and post-processing subject to `n`. Returns a character vector.
#' @export
ussd <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ssd(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @describeIn ss_help Splits strings in `...` using a broken pipe (`'¦'`) as delimiter and returns the unique values with pre-processing subject to specs in `trm`, `sqz`, and `drop` and post-processing subject to `n`. Returns a character vector.
#' @export
ussb <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ssb(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @describeIn ss_help Splits strings in `...` using pipes (`'|'`) and dots/periods (`'.'`) as delimiters and returns the unique values with pre-processing subject to specs in `trm`, `sqz`, and `drop` and post-processing subject to `n`. Returns a character vector.
#' @export
usspd <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::sspd(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @describeIn ss_help Splits strings in `...` using pipes (`'|'`) and broken pipes (`'¦'`) as delimiters and returns the unique values with pre-processing subject to specs in `trm`, `sqz`, and `drop` and post-processing subject to `n`. Returns a character vector.
#' @export
usspb <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::sspb(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @describeIn ss_help Splits strings in `...` using dots/periods (`'.'`) and broken pipes (`'¦'`) as delimiters and returns the unique values with pre-processing subject to specs in `trm`, `sqz`, and `drop` and post-processing subject to `n`. Returns a character vector.
#' @export
ussdb <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::ssdb(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}

#' @describeIn ss_help Splits strings in `...` using pipes (`'|'`), dots/periods (`'.'`), and broken pipes (`'¦'`) as delimiters and returns the unique values with pre-processing subject to specs in `trm`, `sqz`, and `drop` and post-processing subject to `n`. Returns a character vector.
#' @export
usspdb <- function(..., trm = TRUE, sqz = TRUE, drop = TRUE, n = NULL) {uj::sspdb(..., trm = trm, sqz = sqz, drop = drop, u = TRUE, n = n)}
