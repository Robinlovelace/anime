validate_lines <- function(x, error_call = rlang::caller_call()) {
  if (!wk::is_handleable(x)) {
    rlang::abort("Unable to process provided geometry as geoarrow linestring array")
  }
  geom_types <- unique(wk::wk_meta(x)[["geometry_type"]])
  if (!all(geom_types == 2L)) {
    rlang::abort(
      "Unexpected geometries. Expected linestrings.",
      footer = sprintf("Instead found %s", toString(wk::wk_geometry_type_label(geom_types))),
      call = error_call
    )
  }
  geoarrow::as_geoarrow_array(x)
}

#' Match two sets of lines
#' 
#' @param source A geoarrow object containing the source lines
#' @param target A geoarrow object containing the target lines
#' @param distance_tolerance The maximum distance between two points to be considered a match
#' @param angle_tolerance The maximum angle between two lines to be considered a match
#' @return A dataframe containing the matches
#' @export
anime <- function(source, target, distance_tolerance = 10, angle_tolerance = 5) {
  if (!rlang::is_bare_numeric(distance_tolerance, 1)) {
    rlang::abort("`distance_tolerance` must be a scalar numeric")
  }

  if (distance_tolerance <= 0) {
    rlang::abort("`distance_tolerance` must be a positive")
  }

  if (angle_tolerance <= 0) {
    rlang::abort("`angle_tolerance` must be a positive")
  }

  if (angle_tolerance >= 90) {
    rlang::abort("`angle_tolerance` must be less than 90 degrees")
  }

  if (!rlang::is_bare_numeric(angle_tolerance, 1)) {
    rlang::abort("`distance_tolerance` must be a scalar numeric")
  }

  source <- validate_lines(source)
  target <- validate_lines(target)

  init_anime(source, target, distance_tolerance, angle_tolerance)
}

#' @param x An `anime` object
#' @param ... Extensive variables to interpolate
#' @return A dataframe containing the matches
#' @export
as.data.frame.anime <- function(x, ...) {
  get_matches_(x)
}

#' Get the matches from an `anime` object
#' 
#' @param x An `anime` object
#' @export
get_matches <- function(x) {
  if (!inherits(x, "anime")) {
    rlang::abort("Expected an `anime` object")
  }

  res <- get_matches_(x)
  res[["target_id"]] <- res[["target_id"]] + 1L
  res[["source_id"]] <- res[["source_id"]] + 1L

  structure(res, class = c("tbl", "data.frame"))
}

#' @export
print.anime <- function(x, ...) {
  .info <- anime_print_helper(x)
  to_print <- c(
    "<anime>",
    sprintf("sources: %i", .info$source_fts),
    sprintf("targets: %i", .info$target_fts),
    sprintf("angle tolerance: %.1f", .info$angle_tolerance),
    sprintf("distance tolerance: %.1f", .info$distance_tolerance)
  )

  cat(to_print, sep = "\n")
  invisible(to_print)
}

#' Interpolate extensive variables
#'
#' @param x An `anime` object
#' @param ... Variables to interpolate
#' @export
interpolate_extensive <- function(x, ...) {
  if (!inherits(x, "anime")) {
    rlang::abort("Expected an `anime` object")
  }

  to_interpolate <- rlang::list2(...)
  if (!rlang::is_named2(to_interpolate)) {
    rlang::abort("All arguments passed to `...` must be named.")
  }

  for (var in to_interpolate) {
    if (!rlang::is_bare_numeric(var)) {
      rlang::abort("All arguments passed to `...` must be a numeric vector.")
    }
  }

  var_names <- names(to_interpolate)

  res <- vector("list", length(to_interpolate))

  for (i in seq_along(to_interpolate)) {
    res[[i]] <- interpolate_extensive_(as.double(to_interpolate[[i]]), x)
  }

  structure(
    as.data.frame(rlang::set_names(res, var_names)),
    class = c("tbl", "data.frame")
  )
}


#' Interpolate extensive variables
#'
#' @inheritParams interpolate_extensive
#' @export
interpolate_intensive <- function(x, ...) {
  if (!inherits(x, "anime")) {
    rlang::abort("Expected an `anime` object")
  }

  to_interpolate <- rlang::list2(...)
  if (!rlang::is_named2(to_interpolate)) {
    rlang::abort("All arguments passed to `...` must be named.")
  }

  for (var in to_interpolate) {
    if (!rlang::is_bare_numeric(var)) {
      rlang::abort("All arguments passed to `...` must be a numeric vector.")
    }
  }

  var_names <- names(to_interpolate)

  res <- vector("list", length(to_interpolate))

  for (i in seq_along(to_interpolate)) {
    res[[i]] <- interpolate_intensive_(as.double(to_interpolate[[i]]), x)
  }

  structure(
    as.data.frame(rlang::set_names(res, var_names)),
    class = c("tbl", "data.frame")
  )
}
