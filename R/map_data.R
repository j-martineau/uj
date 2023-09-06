#' @title Map Variables in a Data Frame to Specified Roles
#' @description Utility to map variables in a data.frame to specific roles, with options to trim variables not assigned to roles, to rename variables to match variable roles (vs. attaching a variable map as an attribute), and to clear the console with each step in variable mapping.
#' @details Variable mapping is conducted interactively via the console for all variable roles in `roles` and `.roles` that do not have matching variable names in `x`. For variable roles in `roles` and `.roles` that have matching variable names in `x`, matching is done automatically.
#' @param x A \link[uj:rct_atm_dtf]{rectangular atomic data.frame} whose variables are to be mapped.
#' @param roles \link[uj::unq_str_mvc]{Unique string multivec} containing labels for *required* variable roles (that are also valid object names without back-quoting).
#' @param defs \link[uj::unq_str_mvc]{Unique string multivec} with `length(defs) == length(roles)` containing definitions/descriptions associated with corresponding values of `roles`.
#' @param .roles Optional \link[uj::unq_str_mvc]{unique string multivec} containing labels for *optional* variable roles (that are also valid object names without back-quoting).
#' @param .defs Optional \link[uj::unq_str_mvc]{unique string multivec} with `length(.defs) == length(.roles)` containing definitions/descriptions associated with corresponding values of `.roles`.
#' @param .TRIM,.RENAME,.CLEAR Logical scalar indicating, respectively, whether to trim un-mapped variables from `x`, whether to rename mapped variables using the associated values of `roles` and `.roles`, and whether to clear the console with each stop in the variable mapping process.
#' @return A \link[uj:rct_atm_dtf]{rectangular atomic data.frame}.
#' @examples
#' dg <- 1:9
#' ai <- letters[dg]
#' AI <- LETTERS[dg]
#' uf <- factor(ai)
#' of <- ordered(AI)
#' tf <- ai %in% c("a", "e", "i", "o", "u")
#'
#' dtf <- data.frame(dg = dg, ai = ai, AI = AI, uf = uf, of = of, tf = tf, stringsAsFactors = F)
#'
#' roles <- c("dg", "ai", "AI")
#' ROLES <- c("uf", "of", "tf")
#'
#' defs  <- c("digits 1 through 9", "lowercase a-i" , "uppercase A-I"         )
#' DEFS  <- c("unordered factor"  , "ordered factor", "sampled logical values")
#'
#' map_data(dtf, roles, defs)
#' map_data(dtf, roles, defs, .TRIM = F)
#' map_data(dtf, roles, defs, .roles = ROLES, .defs = DEFS)
#' @export
map_data <- function(x, roles, defs, .roles = NULL, .defs = NULL, .TRIM = TRUE, .RENAME = TRUE, .CLEAR = TRUE) {
  Errors <- NULL
  if (!uj::.atm_rct_dtf(x)) {Errors <- base::c(Errors, "[x] must be a rectangular atomic data.frame (?uj::atm_rct_dtf).")}
  if (!uj::.unq_str_vec(roles)) {Errors <- base::c(Errors, "[roles] must be a unique string vec (?uj::unq_str_vec).")}
  if (!uj::.unq_str_vec(defs)) {Errors <- base::c(Errors, "[defs] must be a unique string vec (?uj::unq_str_vec).")}
  if (!base::is.null(.roles)) {if (!uj::.unq_str_vec(.roles)) {Errors <- base::c(Errors, "[.roles] must be NULL or a unique string vec (?uj::unq_str_vec).")}}
  if (!base::is.null(.defs)) {if (!uj::.unq_str_vec(.defs)) {Errors <- base::c(Errors, "[.defs] must be NULL or a unique string vec (?uj::unq_str_vec).")}}
  if (!base::isTRUE(.TRIM) & !base::isFALSE(.TRIM)) {Errors <- base::c(Errors, "[.TRIM] must be TRUE or FALSE.")}
  if (!base::isTRUE(.RENAME) & !base::isFALSE(.RENAME)) {Errors <- base::c(Errors, "[.RENAME] must be TRUE or FALSE.")}
  if (!base::isTRUE(.CLEAR) & !base::isFALSE(.CLEAR)) {Errors <- base::c(Errors, "[.CLEAR] must be TRUE or FALSE.")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  available <- uj::cnames(x)
  if (!uj::unq_str_vec(available)) {Errors <- base::c(Errors, "Names of variables in [x] must be unique and non-blank.")}
  if (uj::N(roles) > uj::N(available)) {Errors <- base::c(Errors, "length(roles) > ncol(x).")}
  if (uj::N(roles) != uj::N(defs)) {Errors <- base::c(Errors, "length(defs) != length(roles).")}
  if (uj::N(.roles) != uj::N(.defs)) {Errors <- base::c(Errors, "length(.defs) != length(.roles).")}
  if (!base::is.null(Errors)) {uj::stopperr(Errors, .PKG = "uj")}
  map <- uj::tb(role = roles, name = NA)
  for (i in 1:uj::N(roles)) {
    role <- roles[i]
    if (uj::not_IN(role, available)) {
      def <- defs[i]
      name <- uj::choose1(available, "What variable of [x] contains ", def, " x?", .CLEAR = .CLEAR)
      available <- available[available != name]
    } else {name <- role}
    map$name[i] <- name
  }
  if (uj::N(available) > 0 & uj::N(.roles) > 0) {for (i in 1:uj::N(.roles)) {
    .role <- .roles[i]
    if  (uj::not_IN(.role, available)) {
      .def <- .defs[i]
      if (uj::YES("Does this dataset contain a variable filling the role ", .role, " >> ", .def, "?", .CLEAR = .CLEAR)) {
        .name <- uj::choose1(available, "What variable of [x] contains ", .def, " x?", .CLEAR = .CLEAR)
        available <- available[available != .name]
      } else {.name <- NULL}
    } else {.name <- .role}
    if (uj::DEF(.name)) {map <- base::rbind(map, uj::tb(role = .role, name = .name))}
  }}
  if (.TRIM) {x <- x[ , map$name]}
  if (.RENAME) {
    names <- uj::cnames(x)
    for (i in 1:uj::nr(map)) {
      role <- map$role[i]
      name <- map$name[i]
      if (uj::is_IN(name, names)) {names[names == name] <- role}
    }
    x <- uj::name_cols(x, names)
  } else {x <- uj::set_att(x, map = map)}
  x
}
