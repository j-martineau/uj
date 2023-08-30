#' @encoding UTF-8
#' @family strings
#' @title `expand.grid` for `paste` and `paste0`
#' @description Generate all combinations of the values of 2 or more vectors and glue the combinations into a single character vector.
#' @details
#' \tabular{ll}{  `pgrid`    \tab Converts the `N` `...` arguments to character (with additional optional pre-processing) and create a character vector with each element
#'                                consisting of sub-strings from across `...` arguments pasted together using the 'paste' `.P`. See the *the* `.CROSSED` *argument* section for how the value
#'                                of `.CROSSED` affects the return value.           \cr   \tab   \cr
#'                `pgridN`   \tab Calls `pgrid` with `.CROSSED = FALSE`\eqn{^{(1)}} \cr
#'                `pgridX`   \tab Calls `pgrid` with `.CROSSED = TRUE`\eqn{^{(1)}}  \cr
#'                `pgrid0`   \tab Calls `pgrid` with `.P = ""` (blank).             \cr
#'                `pgrid1`   \tab Calls `pgrid` with `.P = " "` (space).            \cr   \tab     }
#'  \tabular{l}{  \eqn{^{(1)}} See *the* `.CROSSED` *argument*.                       }
#' @param ... Non-empty atomic objects.
#' @param .P A \link[=cmp_chr_scl]{complete character scalar} to use as the 'paste'.
#' @param .CH,.NA Non-`NA` logical scalars indicating, respectively, whether to split each `...` arguments into its constituent characters after conversion to mode 'character' and whether `NA` values in `...` arguments are valid.
#' @param .CROSSED `TRUE` or `FALSE` indicating whether to construct the result to look like a fully-crossed model. See *the* `.CROSSED` *argument* section for details.
#' @section The `.CROSSED` Argument:
#' The following console excerpt shows the effect of the value of `.CROSSED`:
#' ```
#' > x <- c("A", "B", "C")
#' > y <- c(1, 2)
#' > z <- c("a", "b")
#' > p <- "."
#' > list(n = sort(pgrid(p, x, y, z, .CROSSED = FALSE)),
#'        y = sort(pgrid(p, x, y, z, .CROSSED = TRUE )))
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
#' pgrid(".", x, y, z, .CROSSED = F)
#' pgrid(".", x, y, z, .CROSSED = T)
#' pgridN(".", x, y, y)
#' pgridX(".", x, y, z)
#' pgrid0(x, y, z)
#' pgrid1(x, y, z)
#' @export
pgrid <- function(.P, ..., .CH = F, .CROSSED = F, .NA = F) {
  .combo <- function(x) {
    x <- uj::av(x)
    if (.CROSSED) {x <- x[x != ""]}
    base::paste0(x, collapse = D)
  }
  Dots <- base::list(...)
  Errors <- NULL
  if (base::length(Dots) == 0) {Errors <- base::c(Errors, "[...] is empty.")}
  if (!base::all(base::sapply(Dots, uj:::.cmp_vec))) {Errors <- base::c(Errors, "All arguments in [...] must be complete atomic vector+'s (?cmp_vec)")}
  if (!base::all(base::sapply(Dots, base::length) > 0)) {Errors <- base::c(Errors, "[...] contains an empty element.")}
  if (!uj:::.cmp_chr_scl(.P)) {Errors <- base::c(Errors, "[.P] must be a complete character scalar (?cmp_chr_scl).")}
  if (!uj:::.cmp_lgl_scl(.CH)) {Errors <- base::c(Errors, "[.CH] must be TRUE or FALSE.")}
  if (!uj:::.cmp_lgl_scl(.CROSSED)) {Errors <- base::c(Errors, "[.CROSSED] must be TRUE or FALSE.")}
  if (!uj:::.cmp_lgl_scl(.NA)) {Errors <- base::c(Errors, "[.CROSSED] must be TRUE or FALSE.")}
  if (base::isFALSE(.NA) & base::any(base::is.na(uj::av(Dots)))) {Errors <- base::c(Errors, "Arguments in [...] may not contain [NA] values when [.NA = FALSE].")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = NULL)}
  Call <- base::paste0("base::c(base::as.character(Dots[[", 1:uj::N(Dots), "]]), uj::f0(.CROSSED, '', NULL))")
  Call <- base::paste0(Call, collapse = ", ")
  Call <- base::paste0("base::expand.grid(", Call, ", stringsAsFactors = F)")
  Y <- uj::run(Call)
  Y <- uj::av(base::apply(Y, 1, .combo))
  if (.CROSSED) {Y <- Y[Y != ""]}
  Y
}

#' @rdname pgrid
#' @export
pgrid0 <- function(..., .CH = F, .CROSSED = F, .NA = FALSE) {uj::pgrid("", ..., .CH = .CH, .CROSSED = .CROSSED, .NA = .NA)}

#' @rdname pgrid
#' @export
pgrid1 <- function(..., .CH = F, .CROSSED = F, .NA = FALSE) {uj::pgrid(" ", ..., .CH = .CH, .CROSSED = .CROSSED, .NA = .NA)}

#' @rdname pgrid
#' @export
pgridN <- function(.P, ..., .CH = F, .NA = FALSE) {uj::pgrid(.P, ..., .CH = .CH, .CROSSED = F, .NA = .NA)}

#' @rdname pgrid
#' @export
pgridX <- function(.P, ..., .CH = F, .NA = FALSE) {uj::pgrid(.P, ..., .CH = .CH, .CROSSED = T, .NA = .NA)}
