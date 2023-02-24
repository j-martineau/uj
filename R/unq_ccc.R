#' @encoding UTF-8
#' @family properties
#' @title Unique + xclass combination properties
#' @description Functions checking for combinations of \link[=UNQ]{uniqueness} and \link[=ccc]{xclass}.
#' @details
#' \tabular{ll}{  `unq_ccc_funs`   \tab What \link[=UNQ]{unique} + \link[=ccc]{xclass} combination \link[=prop_funs]{property functions} are there?                          \cr   \tab   \cr
#'                `unq_{ccc}`      \tab Is `x` both unique and a match to the single xclass property `'{ccc}'` where `{ccc}` is a placeholder for any given xclass property? \cr   \tab   \cr
#'                `unq_ccc`        \tab Is `x` both unique and a match to the single xclass property in argument `ccc`?                                                                     }
#' @param x An R object.
#' @param ccc A character scalar single xclass property from \code{\link{ccc_props}()}.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return **A character vector** \cr\cr `unq_ccc_funs`
#' \cr\cr  **A logical scalar**   \cr\cr `unq_ccc, unq_{ccc}`
#' @examples
#' UnqArr <- array(1:27, dim = c(3, 3, 3))
#' UnqDtf <- data.frame(a = c("a", "b", "c"), b = 1:3)
#' UnqMat <- matrix(1:9, nrow = 3)
#' UnqMvc <- 1:2
#' UnqScl <- 1
#' UnqVec <- "a"
#' UnqVls <- list(a = letters, b = 1:3)
#'
#' unq_ccc_funs()
#' unq_ccc(UnqArr, "arr")
#' unq_ccc(UnqVls, "dtf")
#' c(unq_dtf(UnqDtf), unq_arr(UnqArr), unq_arr(rbind(UnqDtf, UnqDtf)))
#' c(unq_mat(UnqMat), unq_arr(UnqVls), unq_arr(rbind(UnqMat, UnqMat)))
#' c(unq_arr(UnqArr), unq_arr(UnqDtf), unq_arr(c(UnqArr, UnqArr)))
#' c(unq_gen(UnqArr), unq_arr(UnqDtf), unq_arr(c(UnqArr, UnqArr)))
#' c(unq_gen(UnqVec), unq_arr(UnqArr), unq_arr(c(UnqVec, UnqCec)))
#' c(unq_gen(UnqMvc), unq_arr(UnqScl), unq_arr(c(UnqMvc, UnqMvc)))
#' c(unq_gen(UnqScl), unq_arr(UnqMvc), unq_arr(c(UnqScl, UnqScl)))
#' c(unq_mvc(UnqMvc), unq_arr(UnqVec), unq_arr(c(UnqMvc, UnqMvc)))
#' c(unq_scl(UnqScl), unq_arr(UnqMvc), unq_arr(c(UnqScl, UnqScl)))
#' c(unq_vec(UnqVec), unq_arr(UnqDtf), unq_arr(c(UnqVec, UnqVec)))
#' c(unq_vls(UnqVls), unq_arr(UnqVec), unq_arr(c(UnqVls, UnqVls)))
#' @export
unq_ccc <- function(x, ccc, ...) {uj::f0(uj::cmp_ccc(x, ccc, ...), uj::UNQ(x, a = uj::notIN(base::tolower(ccc), "dtf", "vls"), nas = F), F)}

#' @rdname unq_ccc
#' @export
unq_ccc_funs <- function() {uj::p0('unq_', uj:::.cccs)}

#' @rdname unq_ccc
#' @export
unq_arr <- function(x, ...) {uj::unq_ccc(x, 'arr', ...)}

#' @rdname unq_ccc
#' @export
unq_dtf <- function(x, ...) {uj::unq_ccc(x, 'dtf', ...)}

#' @rdname unq_ccc
#' @export
unq_gen <- function(x, ...) {uj::unq_ccc(x, 'gen', ...)}

#' @rdname unq_ccc
#' @export
unq_mat <- function(x, ...) {uj::unq_ccc(x, 'mat', ...)}

#' @rdname unq_ccc
#' @export
unq_mvc <- function(x, ...) {uj::unq_ccc(x, 'mvc', ...)}

#' @rdname unq_ccc
#' @export
unq_scl <- function(x, ...) {uj::unq_ccc(x, 'scl', ...)}

#' @rdname unq_ccc
#' @export
unq_vec <- function(x, ...) {uj::unq_ccc(x, 'vec', ...)}

#' @rdname unq_ccc
#' @export
unq_vls <- function(x, ...) {uj::unq_ccc(x, 'vls', ...)}
