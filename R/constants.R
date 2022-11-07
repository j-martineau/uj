#' @name constants.
#' @title Character scalar and atomic vector constants.
#' @param ... unquoted, comma-separated list of the names of constants to
#'   return. If multiple constants are specified, they are coerced into a single
#'   atomic vector result.
#' @return An atomic vector of length 1 or greater.
#' @export
constants. <- function() {help("constants.", package = "uj")}

#' @describeIn constants. Get a \code{\link[idtf]{dtf}} containing all
#'   character scalar constants of package \code{uj}.
#' @export
scl_constants <- function() {uj::.chr.scl.constants}

#' @describeIn constants. Get a \code{\link[idtf]{dtf}} containing all
#'   vector constants of
#'   package \code{uj}.
#' @export
vec_constants <- function() {
  list(                                                                            # : a named list of all possible return values
    pt     = ggplot::.pt  ,                                                        # : | the size of a single point (ggplot constant)',
    L      = NA           ,                                                        # : | logical NA
    R      = NA_real_     ,                                                        # : | real-valued NA
    I      = NA_integer_  ,                                                        # : | integer NA
    C      = NA_character_,                                                        # : | character NA
    one    = 0.99999999,                                                           # : | almost one
    zero   = 0.00000001,                                                           # : | almost zero
    az     = letters,                                                              # : | lower-case letters
    AZ     = letters,                                                              # : | upper-case letters
    colors = v(dred, dblu, dmag, blk, dorn, fred, fblu, fmag, g40, forn),          # : | standard color palette
    lines  = c("solid", "42", "22", "11", "4111", "2111", "1114", "1112", "1441"), # : | standard linetype palette
    ldots  = c(3, 4, 8, 2, 6, 5, 0, 1),                                            # : | standard line-based dot shapes palette
    fdots  = c(21, 23, 24, 22, 25),                                                # : | standard fillable dot shapes palette
    edots  = c(5, 0, 2, 6, 1),                                                     # : | standard empty (outlined) dot shapes palette
    sdots  = c(16, 18, 17, 15),                                                    # : | standard solid dot shapes palette
    digits = as.character(0:9),                                                    # : | digits as character
    aAzZ   = c(letters, LETTERS),                                                  # : | English letters
    names  = c(letters, LETTERS, 0:9, "_", "."),                                   # : | valid for element names names (includes underscore)
    names0 = c(letters, LETTERS, 0:9,      "."),                                   # : | valid for element names names (excludes underscore)
    files  = c(letters, LETTERS, 0:9, "_", ".", " ", ",", "-", "(", ")"),          # : | valid for file names (includes underscore)
    files0 = c(letters, LETTERS, 0:9,      ".", " ", ",", "-", "(", ")"),          # : | valid for file names (excludes underscore)
    models = c(letters, LETTERS, 0:9, ".", " ", "×", "+", "*", ":"),               # : | valid for model specification
    labels = c(letters, LETTERS, 0:9, ".", " ", ",", "-", "(", ")"),               # : | valid for plotting labels
    vowels = c("a", "e", "i", "o", "u", "A", "E", "I", "O", "U")                   # : | English vowels
  )                                                                                # : END
}

#' @describeIn constants. Get one or more character scalar constants.
#' @export
v <- function(...) {
  x. <- as.character(match.call())
  x. <- x.[2:length(x.)]
  av(uj::.chr.scl.constants[x., "value"])
}

#' @describeIn constants. Get one or more atomic vector constants.
#' @export
l <- function(...) {                                                               # BODY
  x. <- as.character(match.call())                                                 # : get the function call elements as a vector
  x. <- x.[2:length(x.)]                                                           # : remove the name of the function
  av(vec_constants[x.])                                                            # : extract the specified elements and convert to a single atomic vector
}                                                                                  # END
