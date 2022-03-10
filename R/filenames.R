#' @name filepaths
#' @family files
#' @title Evaluate and manipulate paths to files or folders on disk.
#' @details \strong{\code{is_path}}
#'   \cr Checks whether \code{...} resolves to a valid path for an object (file
#'   or folder) when arguments in \code{...} are collapsed into a character
#'   scalar.
#'   \cr \cr
#'   \strong{\code{as_path}}
#'   \cr Collapses \code{...} into a path using the current platform file path
#'   separator (\code{.Platform$file.sep}).
#'   \cr \cr
#'   \strong{\code{object_path}}
#'   \cr Collapses \code{...} into a character scalar and expands the path to
#'   the object indicated by the result to account for relative paths.
#'   \cr \cr
#'   \strong{\code{parent_path}}
#'   \cr Calls \code{object_path} and extracts from the resulting character
#'   scalar just the path to the parent folder of the object specified by
#'   \code{...} (i.e., discarding the name of the object itself).
#'   \cr \cr
#'   \strong{\code{object_name}}
#'   \cr Calls \code{object_path} and extracts from the resulting character
#'   scalar just the name of the object, discarding the path to its parent
#'   folder.
#'   \cr \cr
#'   \strong{\code{parent_name}}
#'   \cr Calls \code{parent_path} and extracts from the resulting character
#'   scalar just the name of last folder in the path.
#'   \cr \cr
#'   \strong{\code{object_dirs}}
#'   \cr Calls \code{object_path} and splits the resulting character scalar into
#'   a character vector containing the name of the root folder, the names of any
#'   intermediate folders, the name of the parent folder of the object, and the
#'   name of the object.
#'   \cr \cr
#'   \strong{\code{parent_dirs}}
#'   \cr Calls \code{folder_path} and splits the resulting character scalar into
#'   a character vector containing the name of the root folder, the names of any
#'   intermediate folders, and the name of the parent folder of the object
#'   specified by \code{...}.
#'   \cr \cr
#'   In all cases, if (optionally) specified, all functions throw an error if
#'   \code{...} does not resolve to a valid object path.
#' @param ... Atomic arguments pasted to create
#' @param err Logical scalar indicating whether an error should be thrown if
#'   \code{...} is not a valid object path when collapsed to a character scalar.
#' @return \strong{\code{is_path}}: A logical scalar. \strong{\code{object_path,
#'   folder_path, object_name, folder_name}}: A character scalar.
#'   \strong{\code{object_dirs, folder_dirs}}: A character vector.
#' @export
is_path <- function(..., err = F) {
  x  <- list(...)
  VN <- length(x) > 0
  VL <- f0(!VN, T, all(lengths(x) > 0))
  VX <- f0(!VN | !VL, T, all(sapply(x, cmp_chr)))
  VE <- isTF(err)
  E <- NULL
  if (!VN) {E <- c(E, "\n  * [...] is empty.")}
  if (!VL) {E <- c(E, "\n  * An argument in [...] is empty.")}
  if (!VX) {E <- c(E, "\n  * Arguments in [...] must be non-NA character scalar/vectors.")}
  if (!VE) {E <- c(E, "\n  * [err] must be TRUE or FALSE.")}
  if (xdef(E)) {stop(E)}
  path <- file.path(...)
  Exists <- file.exists(path)                                                    # whether the file exists
  if (Exists) {return(T)}                                                        # if the resulting file already exists, return T
  R <- isERR(file.create(path, showWarnings = F))                                # can such a file be created?
  if ( R) {file.remove(path)}                                                    # remove any newly created file
  if (!R & err) {stop("\n  * [...] doesn't resolve to a valid file/folder path.")}
  R
}

#' @rdname filepaths
#' @export
as_path <- function(...) {if (is_path(..., err = T)) {file.path(...)}}

#' @rdname filepaths
#' @export
object_path  <- function(path, err = T) {f0(is_path(path, err = err), path.expand(path), "")}

#' @rdname filepaths
#' @export
parent_path  <- function(path, err = T) {dirname(object_path(path, err = err))}

#' @rdname filepaths
#' @export
object_name  <- function(path, err = T) {basename(object_path(path, err = err))}

#' @rdname filepaths
#' @export
parent_name  <- function(path, err = T) {object_name(parent_path(path, err = err))}

#' @rdname filepaths
#' @export
object_dirs <- function(path, err = T) {strsplit(object_path(path, err = err), .Platform$file.sep, fixed = T)[[1]]}

#' @rdname filepaths
#' @export
parent_dirs <- function(path, err = T) {strsplit(parent_path(path, err = err), .Platform$file.sep, fixed = T)[[1]]}
