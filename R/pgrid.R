#' @encoding UTF-8
#' @family strings
#' @title `expand.grid` for `paste` and `paste0`
#' @description Generate all combinations of the values of 2 or more vectors and glue the combinations into a single character vector.
#' @details
#' \tabular{ll}{  `pgrid`    \tab Converts the `N` `...` arguments to character (with additional optional pre-processing) and create a character vector with each element
#'                                consisting of sub-strings from across `...` arguments pasted together using the 'paste' `.p`. See the *the* `.crossed` *argument* section for how the value
#'                                of `.crossed` affects the return value.           \cr   \tab   \cr
#'                `pgridN`   \tab Calls `pgrid` with `.crossed = FALSE`\eqn{^{(1)}} \cr
#'                `pgridX`   \tab Calls `pgrid` with `.crossed = TRUE`\eqn{^{(1)}}  \cr
#'                `pgrid0`   \tab Calls `pgrid` with `.p = ""` (blank).             \cr
#'                `pgrid1`   \tab Calls `pgrid` with `.p = " "` (space).            \cr   \tab     }
#'  \tabular{l}{  \eqn{^{(1)}} See *the* `.crossed` *argument*.                       }
#' @param ... Non-empty atomic objects.
#' @param .p A \link[=cmp_chr_scl]{complete character scalar} to use as the 'paste'.
#' @param .ch,.na Non-`NA` logical scalars indicating, respectively, whether to split each `...` arguments into its constituent characters after conversion to mode 'character' and whether `NA` values in `...` arguments are valid.
#' @param .crossed `TRUE` or `FALSE` indicating whether to construct the result to look like a fully-crossed model. See *the* `.crossed` *argument* section for details.
#' @section The `.crossed` Argument:
#' The following console excerpt shows the effect of the value of `.crossed`:
#' ```
#' > x <- c("A", "B", "C")
#' > y <- c(1, 2)
#' > z <- c("a", "b")
#' > p <- "."
#' > list(n = sort(pgrid(p, x, y, z, .crossed = FALSE)),
#'        y = sort(pgrid(p, x, y, z, .crossed = TRUE )))
#' $n
#' [1] "A.1.a" "A.1.b" "A.2.a" "A.2.b" "B.1.a" "B.1.b"
#' [7] "B.2.a" "B.2.b" "C.1.a" "C.1.b" "C.2.a" "C.2.b"
#'
#' $y
#'  [1] "1"     "1.a"   "1.b"   "2"     "2.a"   "2.b"
#'  [7] "a"     "A"     "A.1"   "A.1.a" "A.1.b" "A.2"
#' [13] "A.2.a" "A.2.b" "A.a"   "A.b"   "b"     "B"
#' [19] "B.1"   "B.1.a" "B.1.b" "B.2"   "B.2.a" "B.2.b"
#' [25] "B.a"   "B.b"   "C"     "C.1"   "C.1.a" "C.1.b"
#' [31] "C.2"   "C.2.a" "C.2.b" "C.a"   "C.b"
#' ```
#' @return A character vector.
#' @examples
#' x <- c("A", "B", "C")
#' y <- c(1, 2)
#' z <- c("a", "b")
#' pgrid(".", x, y, z, .crossed = F)
#' pgrid(".", x, y, z, .crossed = T)
#' pgridN(".", x, y, y)
#' pgridX(".", x, y, z)
#' pgrid0(x, y, z)
#' pgrid1(x, y, z)
#' @export
pgrid <- function(.p, ..., .ch = F, .crossed = F, .na = F) {
  .combo <- function(x) {
    x <- uj::av(x)
    if (.crossed) {x <- x[x != ""]}
    base::paste0(x, collapse = D)
  }
  dots <- base::list(...)
  errs <- NULL
  if (base::length(dots) == 0) {errs <- base::c(errs, "[...] is empty.")}
  if (!base::all(base::sapply(dots, uj::.cmp_vec))) {errs <- base::c(errs, "All arguments in [...] must be complete atomic vector+'s (?cmp_vec)")}
  if (!base::all(base::sapply(dots, base::length) > 0)) {errs <- base::c(errs, "[...] contains an empty element.")}
  if (!uj::.cmp_chr_scl(.p)) {errs <- base::c(errs, "[.p] must be a complete character scalar (?cmp_chr_scl).")}
  if (!uj::.cmp_lgl_scl(.ch)) {errs <- base::c(errs, "[.ch] must be TRUE or FALSE.")}
  if (!uj::.cmp_lgl_scl(.crossed)) {errs <- base::c(errs, "[.crossed] must be TRUE or FALSE.")}
  if (!uj::.cmp_lgl_scl(.na)) {errs <- base::c(errs, "[.crossed] must be TRUE or FALSE.")}
  if (base::isFALSE(.na) & base::any(base::is.na(uj::av(dots)))) {errs <- base::c(errs, "Arguments in [...] may not contain [NA] values when [.na = FALSE].")}
  if (!base::is.null(errs)) {uj::stopperr(errs)}
  call <- base::paste0("base::c(base::as.character(dots[[", 1:uj::N(dots), "]]), uj::f0(.crossed, '', NULL))")
  call <- base::paste0(call, collapse = ", ")
  call <- base::paste0("base::expand.grid(", call, ", stringsAsFactors = F)")
  y <- uj::run(call)
  y <- uj::av(base::apply(y, 1, .combo))
  if (.crossed) {y <- y[y != ""]}
  y
}

#' @rdname pgrid
#' @export
pgrid0 <- function(..., .ch = F, .crossed = F, .na = FALSE) {uj::pgrid("", ..., .ch = .ch, .crossed = .crossed, .na = .na)}

#' @rdname pgrid
#' @export
pgrid1 <- function(..., .ch = F, .crossed = F, .na = FALSE) {uj::pgrid(" ", ..., .ch = .ch, .crossed = .crossed, .na = .na)}

#' @rdname pgrid
#' @export
pgridN <- function(.p, ..., .ch = F, .na = FALSE) {uj::pgrid(.p, ..., .ch = .ch, .crossed = F, .na = .na)}

#' @rdname pgrid
#' @export
pgridX <- function(.p, ..., .ch = F, .na = FALSE) {uj::pgrid(.p, ..., .ch = .ch, .crossed = T, .na = .na)}
