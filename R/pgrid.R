#' @name pgrid
#' @family strings
#' @title `expand.grid` for `paste` and `paste0`
#' @description \tabular{rl}{
#'        `pgrid`   \tab Converts the `n` `...` arguments to character (with additional optional pre-processing) and create a character vector with each element consisting of sub-strings from across `...` arguments pasted together using the 'paste' `p`. See the *value* section for how the value of `crossed` affects the return value.
#'   \cr            \tab   
#'   \cr `pgrid0`   \tab Calls `pgrid` with `p = ""` (blank).
#'   \cr `pgrid1`   \tab Calls `pgrid` with `p = " "` (space).
#'   \cr `pgridn`   \tab Calls `pgrid` with `crossed = FALSE`\eqn{^1}.
#'   \cr `pgridx`   \tab Calls `pgrid` with `crossed = TRUE`\eqn{^1}.
#' }
#' \eqn{^{1.}} See the section entitled *the* `crossed` *argument*.
#' @param ... Non-empty atomic objects.
#' @param p A \link[=cmp_chr_scl]{complete character scalar} to use as the 'paste'.
#' @param ch,na.err Non-`NA` logical scalars indicating, respectively, whether to split each `...` arguments into its constituent characters after conversion to mode 'character' and whether to throw an error if an argument in `...` contains an `NA` value.
#' @param crossed A non-`NA` logical scalar indicating whether to construct the result to look like a fully-crossed model. See the section entitled *the* `crossed` *argument* section for details.
#' @section The `crossed` Argument: The following console excerpt shows the effect of the value of `crossed`:
#' ```
#' > x <- c("A", "B", "C")
#' > y <- c(1, 2)
#' > z <- c("a", "b")
#' > p <- "."
#' > list(n = sort(pgrid(p, x, y, z, crossed = FALSE)),
#'        y = sort(pgrid(p, x, y, z, crossed = TRUE )))
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
#' pgrid(".", x, y, z, crossed = F)
#' pgrid(".", x, y, z, crossed = T)
#' pgridn(".", x, y, y)
#' pgridx(".", x, y, z)
#' pgrid0(x, y, z)
#' pgrid1(x, y, z)
#' @export
pgrid <- function(p, ..., ch = F, crossed = F, na.err = T) {
  combo <- function(xx) {
    xx <- av(xx)
    if (crossed) {xx <- xx[xx != ""]}
    paste0(xx, collapse = p)
  }
  dots <- list(...)
  errs <- c(f0(length(dots) > 0                    , NULL, "[...] is empty."),
            f0(all(sapply(dots, cmp_vec))          , NULL, "All arguments in [...] must be complete atomic vector+'s (?cmp_vec)"),
            f0(all(sapply(dots, length) > 0)       , NULL, "[...] contains an empty element."),
            f0(cmp_chr_scl(p)                      , NULL, "[p] must be a complete character scalar (?cmp_chr_scl)."),
            f0(isTF(ch)                            , NULL, "[ch] must be TRUE or FALSE."),
            f0(isTF(crossed)                       , NULL, "[crossed] must be TRUE or FALSE."),
            f0(isTF(na.err)                        , NULL, "[na.err] must be TRUE or FALSE."),
            f0(!isT(na.err) | !any(is.na(av(dots))), NULL, "Arguments in [...] may not contain [NA] values when [na.err = TRUE]."))
  if (!is.null(errs)) {stop(.errs(errs))}
  call <- paste0("c(as.character(dots[[", 1:length(dots), "]]), f0(crossed, '', NULL))")
  call <- paste0(call, collapse = ", ")
  call <- paste0("expand.grid(", call, ", stringsAsFactors = F)")
  out <- run(call)
  out <- av(apply(out, 1, combo))
  if (crossed) {out <- out[out != ""]}
  out
}

#' @rdname pgrid
#' @export
pgrid0 <- function(..., ch = F, crossed = F, na.err = T) {pgrid("", ..., ch = ch, crossed = crossed, na.err = na.err)}

#' @rdname pgrid
#' @export
pgrid1 <- function(..., ch = F, crossed = F, na.err = T) {pgrid(" ", ..., ch = ch, crossed = crossed, na.err = na.err)}

#' @rdname pgrid
#' @export
pgridn <- function(p, ..., ch = F, na.err = T) {pgrid(p, ..., ch = ch, crossed = F, na.err = na.err)}

#' @rdname pgrid
#' @export
pgridx <- function(p, ..., ch = F, na.err = T) {pgrid(p, ..., ch = ch, crossed = T, na.err = na.err)}
