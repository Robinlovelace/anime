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
#' @param source a linestring geometry. Must be handleable by `wk`.
#' @param target a linestring geometry. Must be handleable by `wk`.
#' @param distance_tolerance the maximum distance between two linestrings to be considered a match.
#' @param angle_tolerance the maximum angle difference between two lines to be considered a match.
#' @return an object of class `anime`
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

#' @export
as.data.frame.anime <- function(x, ...) {
  get_matches(x)
}

#' Get Partial Matches
#'
#' Extract the partial matches from the `anime` object
#' as a `data.frame`.
#'
#' @param x an `anime` object as created with `anime()`.
#'
#' @returns
#' A data.frame with 5 columns:
#' - `target_id`: the 1-based index of the target linestring
#' - `source_id`: the 1-based index of the source linestring
#' - `shared_len`: the shared length between the `source` and `target` in the CRS's units
#' - `source_weighted`: the `shared_len` divided by the length of the source linestring
#' - `target_weighted`: the `shared_len` divided by the length of the target linestring
#' @export
get_matches <- function(x) {
  if (!inherits(x, "anime")) {
    rlang::abort("Expected an `anime` object")
  }

  res <- get_matches_(x)

  structure(res, class = c("tbl", "data.frame"))
}

#' @export
print.anime <- function(x, ...) {
  .info <- anime_print_helper(x)
  to_print <- c(
    "<anime>",
    sprintf("matches: %i", .info$n_matches),
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
#' Interpolate values from the source geometry to the target geometry.
#' Intensive properties are values which are independent of the geometry's size.
#' These are values such as a density or temperature.
#'
#' @param x a numeric variable with the same length as the source geometry
#' @param matches an `anime` object created with `anime()`
#'
#' @export
interpolate_extensive <- function(x, matches) {
  if (!inherits(matches, "anime")) {
    rlang::abort("Expected an `anime` object")
  }
  if (!rlang::is_bare_numeric(x)) {
    rlang::abort("`x` must be a numeric vector.")
  }

  # if (anyNA(x)) {
  #   rlang::abort("Cannot interpolate missing values.")
  # }

  interpolate_extensive_(as.double(x), matches)
}


#' Interpolate extensive variables
#'
#' Interpolate values from the source geometry to the target geometry.
#' Extensive properties are values which are dependent upon the geometry's size.
#' Extensive properties would be a population or length.
#'
#' @inheritParams interpolate_extensive
#' @export
interpolate_intensive <- function(x, matches) {
  if (!inherits(matches, "anime")) {
    rlang::abort("Expected an `anime` object")
  }
  if (!rlang::is_bare_numeric(x)) {
    rlang::abort("`x` must be a numeric vector.")
  }

  # if (anyNA(x)) {
  #   rlang::abort("Cannot interpolate missing values.")
  # }

  interpolate_intensive_(as.double(x), matches)
}

#' Filter matches in an anime object by destination and/or source overlap
#'
#' @param x An `anime` object as created with `anime()`.
#' @param min_overlap_dest Optional. A scalar numeric threshold (0 to 1) for the destination segment overlap.
#' @param min_overlap_source Optional. A scalar numeric threshold (0 to 1) for the source segment overlap.
#' @return The modified `anime` object in-place (invisibly).
#' @export
filter_matches <- function(x, min_overlap_dest = NULL, min_overlap_source = NULL) {
  if (!inherits(x, "anime")) {
    rlang::abort("Expected an `anime` object")
  }
  if (is.null(min_overlap_dest) && is.null(min_overlap_source)) {
    return(invisible(x))
  }
  if (!is.null(min_overlap_dest)) {
    if (!rlang::is_bare_numeric(min_overlap_dest, 1)) {
      rlang::abort("`min_overlap_dest` must be a scalar numeric")
    }
    min_overlap_dest <- as.double(min_overlap_dest)
  }
  if (!is.null(min_overlap_source)) {
    if (!rlang::is_bare_numeric(min_overlap_source, 1)) {
      rlang::abort("`min_overlap_source` must be a scalar numeric")
    }
    min_overlap_source <- as.double(min_overlap_source)
  }
  filter_matches_(x, min_overlap_dest, min_overlap_source)
  invisible(x)
}

#' Interpolate flow variables
#'
#' Interpolate network flow volume variables from the source geometry to the target geometry.
#' Flow variables (such as Annual Average Daily Traffic / AADT) are aggregated by multiplying
#' by the target/destination overlap ratio and summing. This naturally behaves as a length-weighted
#' average for serial matches and a sum for parallel matches.
#'
#' @param x A numeric variable with the same length as the source geometry.
#' @param matches An `anime` object created with `anime()`.
#' @return A numeric vector with the same length as the target geometry representing the interpolated flow.
#' @export
interpolate_flow <- function(x, matches) {
  if (!inherits(matches, "anime")) {
    rlang::abort("Expected an `anime` object")
  }
  if (!rlang::is_bare_numeric(x)) {
    rlang::abort("`x` must be a numeric vector.")
  }

  match_tbl <- get_matches(matches)
  if (nrow(match_tbl) == 0) {
    .info <- anime_print_helper(matches)
    return(numeric(.info$target_fts))
  }

  weighted_val <- x[match_tbl$source_id] * match_tbl$target_weighted
  weighted_val[is.na(weighted_val)] <- 0

  summed <- stats::aggregate(
    weighted_val,
    by = list(target_id = match_tbl$target_id),
    FUN = sum,
    na.rm = TRUE
  )

  .info <- anime_print_helper(matches)
  res <- numeric(.info$target_fts)
  res[summed$target_id] <- summed$x
  res
}
