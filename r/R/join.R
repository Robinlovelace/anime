#' Join attributes from a source network to a target network
#'
#' @param x An `sf` object or handleable geometry representing the target network.
#' @param y An `sf` object or handleable geometry representing the source network.
#' @param distance_tolerance The maximum distance between two linestrings to be considered a match.
#' @param angle_tolerance The maximum angle difference between two lines to be considered a match.
#' @param suffix A character vector of length 2 used to disambiguate non-joined duplicate variables.
#' @param match_strength A threshold for target_weighted to filter out weak matches (0 to 1). Default is 0.
#' @return The target object with joined attributes.
#' @export
anime_join <- function(x,
                       y,
                       distance_tolerance = 10,
                       angle_tolerance = 5,
                       suffix = c(".x", ".y"),
                       match_strength = 0) {

  if (!wk::is_handleable(x) || !wk::is_handleable(y)) {
    stop("x and y must be handleable by the wk package.")
  }

  # 1. Run the core anime matching
  # anime(source, target, ...) -> anime(y, x, ...)
  matches_ptr <- anime(y, x, distance_tolerance, angle_tolerance)
  match_tbl <- get_matches(matches_ptr)

  if (nrow(match_tbl) == 0) {
    warning("No matches found with current tolerances.")
    return(x)
  }

  # 2. Filter by match strength
  if (match_strength > 0) {
    match_tbl <- match_tbl[match_tbl$target_weighted >= match_strength, ]
  }

  if (nrow(match_tbl) == 0) {
    warning("No matches remained after filtering by match_strength.")
    return(x)
  }

  # 3. Handle data frames
  x_df <- as.data.frame(x)
  y_df <- as.data.frame(y)

  # Remove geometry columns from y_df to avoid duplication
  # Using a internal helper or wk logic
  is_y_geo <- vapply(y_df, wk::is_handleable, logical(1))
  y_df_clean <- y_df[, !is_y_geo, drop = FALSE]

  # Identify categorical vs numeric in y
  # We use the same logic as before: categorical = !numeric or character
  cat_cols <- names(y_df_clean)[vapply(y_df_clean, function(v) !is.numeric(v) || is.character(v), logical(1))]
  num_cols <- setdiff(names(y_df_clean), cat_cols)

  # Result object
  res_df <- x_df
  # Rename columns in x if they exist in y and use suffix
  shared_names <- intersect(names(res_df), names(y_df_clean))
  if (length(shared_names) > 0) {
    for (name in shared_names) {
      names(res_df)[names(res_df) == name] <- paste0(name, suffix[1])
    }
  }

  # 4. Perform Consensus Categorical Matching (Majority shared length wins)
  if (length(cat_cols) > 0) {
    for (col in cat_cols) {
      # Attach values from y to match_tbl
      # match_tbl$source_id is 1-based index
      m <- match_tbl
      m[[col]] <- y_df_clean[[col]][m$source_id]
      
      # Aggregate shared_len by target_id and category
      agg <- aggregate(shared_len ~ target_id + .data[[col]], data = m, sum)
      
      # Pick the max shared_len for each target_id
      # Order by target_id and descending shared_len
      winners <- agg[order(agg$target_id, -agg$shared_len), ]
      winners <- winners[!duplicated(winners$target_id), ]
      
      # Target column name (apply suffix if needed)
      new_col_name <- col
      if (new_col_name %in% names(res_df)) {
        new_col_name <- paste0(col, suffix[2])
      }
      
      # Join back to result. target_id is 1-based index of x.
      res_df[[new_col_name]] <- NA
      res_df[[new_col_name]][winners$target_id] <- winners[[col]]
    }
  }

  # 5. Perform Weighted Intensive Interpolation for numeric columns
  if (length(num_cols) > 0) {
    for (col in num_cols) {
      val <- as.numeric(y_df_clean[[col]])
      # Handling NAs by setting to 0 for the weighted sum (might need refinement)
      val[is.na(val)] <- 0
      interp_val <- interpolate_intensive(val, matches_ptr)

      new_col_name <- col
      if (new_col_name %in% names(res_df)) {
        new_col_name <- paste0(col, suffix[2])
      }
      
      res_df[[new_col_name]] <- interp_val
    }
  }

  # Convert back to original class if possible
  if (inherits(x, "sf")) {
    # Keep the geometry column name
    geom_col <- attr(x, "sf_column")
    # If it was renamed due to suffix, find it
    if (!(geom_col %in% names(res_df))) {
       # This shouldn't happen as we only rename shared non-geo columns
       # but safe to check
    }
    return(sf::st_as_sf(res_df, sf_column_name = geom_col))
  }

  res_df
}
