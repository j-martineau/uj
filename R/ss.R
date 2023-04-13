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
#'          \item If `Trm = TRUE`, trim white space (i.e., spaces, tabs, newlines) from both ends of each element of the result.
#'          \item If `Sqz = TRUE`, remove leading and trailing white space and replace any multi character interior white-space sequences inside the result with a single space.
#'          \item If `u = TRUE`, reduce the result to unique values.}
#' @details **The function** `ss`
#' \cr\cr Performs the operations as described in the *order of operations* section.
#' \cr\cr **The functions** `ch/chars`
#' \cr\cr Split strings in constituent characters (by using a blank-string delimiter).
#' \cr\cr **The function** `sstb`
#' \cr\cr Requires that splitting each element of `X` along the delimiter `d` and post-processing the results based on optional args `Trm`, `Sqz`, `Drop`, `n`, and `u` will result in vectors of the same length. Those resulting same-length vectors are then placed into a data.frame. For example, the following console excerpt demonstrates a call to `ss_tb` and its result.
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
#' \tabular{lll}{  **Code**   \tab **Name**            \tab **Common delimiter**                                                              \cr
#'                 `'0'`      \tab blank\eqn{^{(1)}}   \tab `''`                                                                              \cr
#'                 `'1'`      \tab space               \tab `' '`                                                                             \cr
#'                 `'_p'`     \tab pipe                \tab `'|'`                                                                             \cr
#'                 `'_d'  `   \tab dot                 \tab `'.'`                                                                             \cr
#'                 `'_b'`     \tab broken pipe         \tab `'¦'`                                                                               }
#'  \tabular{ll}{  `     `    \tab \eqn{^{(1)}} Splits strings into constituent characters, making `ss0(.)` functionally equivalent to `ch(.)`. }
#' \cr **Functions filtering for unique values**
#' \cr\cr Prepending `u` to a function name reduces the result of calling the original function to unique values.
#' @param ... An arbitrary number of objects to be \link[=av]{atomized} before splitting.
#' @param X A \link[=chr_vec]{character vec} of string(s) to be split.
#' @param Name A \link[=cmp_chr_scl]{complete character scalar} name of the variable to hold the original strings.
#' @param Part A complete character scalar prefix for labeling components of vectors resulting from split strings.
#' @param D A complete character vec of a delimiter or delimiters to use in splitting strings.
#' @param Trm `TRUE` or `FALSE` indicating whether to trim white space from each side of each element of the result.
#' @param Sqz `TRUE` or `FALSE` indicating whether to squeeze the result by removing extra internal whitespace.
#' @param Drop `TRUE` or `FALSE`. For functions associated character-wise splitting (i.e., ending in `0` or `ch`), indicates whether to Drop resulting values that are not letters, digits, or spaces. For all others, indicates whether to Drop resulting blank string values.
#' @param N An optional \link[=cmp_psw_scl]{complete positive whole-number vec} specifying one or more elements to be extracted from the result.
#' @param U complete non-`NA` scalar indicating whether to reduce the result to unique values.
#' @return **A character vector**            \cr\cr `s_pdb uss_pdb` \cr `ss_db, uss_db` \cr `ss_pb, uss_pb` \cr `ss_pd, uss_pd` \cr `ss_b, uss_b` \cr `ss_d, uss_d` \cr `ss_p, uss_p` \cr `ss1, uss1` \cr `ss, uss`
#' \cr\cr  **A \link[=CH1]{onechar} vector** \cr\cr `ss0 uss0`      \cr `ch, chars, uch, uchars`
#' \cr\cr  **A data.frame **                 \cr\cr `ss_tb, uss_tb`
#' @examples
#' ss("", "super-cooled")
#' ss("|", "super||cooled", "super|heated")
#' ss_0("super-cooled", "super-heated")
#' ss_1("super cooled", "super heated")
#' ss_p("super|cooled", "super|heated", U = TRUE)
#' ss_d("super.cooled", "super.heated")
#' ss_b("super¦cooled", "super¦heated")
#' ss_pd("super|cooled", "super.heated")
#' ss_pb("super|cooled", "super¦heated")
#' ss_db("super.cooled", "super¦heated")
#' ss_pdb("super|cooled¦|super|heated", U = TRUE)
#' ss_pdb(" super|cooled  ¦super..  heated", N = 3)
#' ss_pdb(" super|cooled  ¦super..  heated", Trm = F, Sqz = F, Drop = F, N = 3)
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
ss <- function(D, ..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, U = FALSE, N = NULL) {
  Errors <- NULL
  if (!base::all(base::sapply(base::list(...), uj:::.CHR))) {Errors <- base::c(Errors, "[...] must contain at least one argument, all of which must be character generics (?chrGEN).")}
  if (!uj:::.cmp_lgl_scl(Sqz)) {Errors <- base::c(Errors, "[Sqz] must be TRUE or FALSE.")}
  if (!uj:::.cmp_lgl_scl(Trm)) {Errors <- base::c(Errors, "[Trm] must be TRUE or FALSE.")}
  if (!uj:::.cmp_chr_vec(D)) {Errors <- base::c(Errors, "[D] must be a complete character vec (?cmp_chr_vec).")}
  if (!uj:::.cmp_lgl_scl(U)) {Errors <- base::c(Errors, "[U] must be TRUE or FALSE.")}
  if (!uj::f0(base::is.null(N), T, uj:::.cmp_psw_vec(N))) {Errors <- base::c(Errors, "[N] must be NULL or a complete positive whole-number vec (?cmp_psw_vec).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, PKG = "uj")}
  X <- uj::av(...)
  for (d in D) {X <- uj::av(base::strsplit(base::as.character(X), d, fixed = T))}
  if (Trm) {X <- uj::trm(X)}
  if (Sqz) {X <- uj::sqz(X)}
  if (Drop) {X <- X[X != ""]}
  if (!base::is.null(N)) {X <- X[N]}
  if (U) {X <- base::unique(X)}
  X
}

#' @rdname ss
#' @export
ch <- function(..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL, U = FALSE) {
  X <- base::as.character(uj::av(...))
  if (Trm) {X <- stringr::str_trim(X, side = "both")}
  if (Sqz) {X <- stringr::str_squish(X)}
  X <- uj::ss("", X, Trm = TRUE, Sqz = TRUE, Drop = FALSE, N = N, U = U)
  X <- X[X != ""]
  if (Drop) {X <- X[X %in% base::c(letters, LETTERS, 0:9, " ")]}
  uj::ss("", ..., Trm = Trm, Sqz = Sqz, N = N, U = U)
}

#' @rdname ss
#' @export
chars <- ch

#' @rdname ss
#' @export
ss0 <- ch

#' @rdname ss
#' @export
ss_tb <- function(D, ..., Name = "string", Part = "part", Trm = TRUE, Sqz = TRUE, Drop = TRUE, U = FALSE, N = NULL) {
  X <- uj::av(...)
  Y <- base::lapply(X, ss, D = D, Trm = Trm, Sqz = Sqz, Drop = Drop, N = N, U = U)
  nDots <- base::length(X)
  if (nDots > 1) {
    if (!uj::recyclable_ns(base::lengths(Y))) {uj::stopperr("[...] args must be recyclable upon splitting and post-processing using optional args.", PKG = "uj")}
    Ns <- base::lengths(Y)
    if (base::length(base::unique(Ns)) > 1) {
      Reps <- base::max(Ns) / Ns
      if (base::any(Reps > 1)) {for (i in 1:nDots) {Y[i] <- base::rep.int(Y[[i]], Reps[i])}}
    }}
  Y <- base::matrix(Y, nrow = nDots, byrow = T)
  if (base::length(Part) == 1) {base::colnames(Y) <- base::paste0(Part, ".", 1:base::ncol(Y))} else {base::colnames(Y) <- Part}
  X <- base::matrix(X, ncol = 1)
  base::colnames(X) <- Name
  X <- base::cbind(X, Y)
  base::rownames(X) <- NULL
  tibble::as_tibble(X)
}

#' @rdname ss
#' @export
ss1 <- function(..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL, U = FALSE) {uj::ss(" ", ..., Trm = Trm, Sqz = Sqz, Drop = Drop, N = N, U = U)}

#' @rdname ss
#' @export
ss_p <- function(..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL, U = FALSE) {uj::ss("|", ..., Trm = Trm, Sqz = Sqz, Drop = Drop, N = N, U = U)}

#' @rdname ss
#' @export
ss_d <- function(..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL, U = FALSE) {uj::ss(".", ..., Trm = Trm, Sqz = Sqz, Drop = Drop, N = N, U = U)}

#' @rdname ss
#' @export
ss_b <- function(..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL, U = FALSE) {uj::ss("¦", ..., Trm = Trm, Sqz = Sqz, Drop = Drop, N = N, U = U)}

#' @rdname ss
#' @export
ss_pd <- function(..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL, U = FALSE) {uj::ss(c("|", "."), ..., Trm = Trm, Sqz = Sqz, Drop = Drop, N = N, U = U)}

#' @rdname ss
#' @export
ss_pb <- function(..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL, U = FALSE) {uj::ss(c("|", "¦"), ..., Trm = Trm, Sqz = Sqz, Drop = Drop, N = N, U = U)}

#' @rdname ss
#' @export
ss_db <- function(..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL, U = FALSE) {uj::ss(c(".", "¦"), ..., Trm = Trm, Sqz = Sqz, Drop = Drop, N = N, U = U)}

#' @rdname ss
#' @export
ss_pdb <- function(..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL, U = FALSE) {uj::ss(c("|", ".", "¦"), ..., Trm = Trm, Sqz = Sqz, Drop = Drop, N = N, U = U)}

#' @rdname ss
#' @export
uss <- function(d, ..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL) {uj::ss(d, ..., Trm = Trm, Sqz = Sqz, Drop = Drop, U = TRUE, N = N)}

#' @rdname ss
#' @export
uch <- function(..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL) {uj::ch(..., Trm = Trm, Sqz = Sqz, Drop = Drop, U = TRUE, N = N)}

#' @rdname ss
#' @export
uchars <- uch

#' @rdname ss
#' @export
uss0 <- function(..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL) {uj::ss0(..., Trm = Trm, Sqz = Sqz, Drop = Drop, U = TRUE, N = N)}

#' @rdname ss
#' @export
uss_tb <- function(d, ..., name = "string", part = "part", Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL) {uj::ss_tb(d, ..., name = name, part = part, Sqz = Sqz, Drop = Drop, U = TRUE, N = N)}

#' @rdname ss
#' @export
uss1 <- function(..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL) {uj::ss1(..., Trm = Trm, Sqz = Sqz, Drop = Drop, U = TRUE, N = N)}

#' @rdname ss
#' @export
uss_p <- function(..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL) {uj::ss_p(..., Trm = Trm, Sqz = Sqz, Drop = Drop, U = TRUE, N = N)}

#' @rdname ss
#' @export
uss_d <- function(..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL) {uj::ss_d(..., Trm = Trm, Sqz = Sqz, Drop = Drop, U = TRUE, N = N)}

#' @rdname ss
#' @export
uss_b <- function(..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL) {uj::ss_b(..., Trm = Trm, Sqz = Sqz, Drop = Drop, U = TRUE, N = N)}

#' @rdname ss
#' @export
uss_pd <- function(..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL) {uj::ss_pd(..., Trm = Trm, Sqz = Sqz, Drop = Drop, U = TRUE, N = N)}

#' @rdname ss
#' @export
uss_pb <- function(..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL) {uj::ss_pb(..., Trm = Trm, Sqz = Sqz, Drop = Drop, U = TRUE, N = N)}

#' @rdname ss
#' @export
uss_db <- function(..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL) {uj::ss_db(..., Trm = Trm, Sqz = Sqz, Drop = Drop, U = TRUE, N = N)}

#' @rdname ss
#' @export
uss_pdb <- function(..., Trm = TRUE, Sqz = TRUE, Drop = TRUE, N = NULL) {uj::ss_pdb(..., Trm = Trm, Sqz = Sqz, Drop = Drop, U = TRUE, N = N)}
