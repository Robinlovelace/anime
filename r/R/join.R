#' Join attributes from a source network to a target network
#'
#' @param source An `sf` object or handleable geometry representing the source network.
#' @param target An `sf` object or handleable geometry representing the target network.
#' @param distance_tolerance The maximum distance between two linestrings to be considered a match.
#' @param angle_tolerance The maximum angle difference between two lines to be considered a match.
#' @param prefix A string prefix for the new columns. Default is "".
#' @param match_strength A threshold for target_weighted to filter out weak matches. Default is 0.
#' @param columns Character vector of columns to transfer. If NULL, transfers all columns except geometries and internal IDs.
#' @param extensive Character vector of columns to treat as extensive (summed by source weight).
#' @param aadt Character vector of columns to treat as AADT (summed if parallel, averaged if series, using target weight).
#' @return The target object with joined attributes.
#' @export
anime_join <- function(source,
                       target,
                       distance_tolerance = 10,
                       angle_tolerance = 5,
                       prefix = "",
                       match_strength = 0,
                       columns = NULL,
                       extensive = NULL,
                       aadt = NULL) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package \"sf\" is required for anime_join to work. Please install it.", call. = FALSE)
  }

  if (!inherits(source, "sf") || !inherits(target, "sf")) {
    stop("source and target must be sf objects for anime_join to work automatically.")
  }

  extensive <- extensive %||% character()
  aadt <- aadt %||% character()

  matches_ptr <- anime(source, target, distance_tolerance, angle_tolerance)

  if (match_strength > 0) {
    filter_matches(matches_ptr, match_strength)
  }

  match_tbl <- get_matches(matches_ptr)

  if (nrow(match_tbl) == 0) {
    warning("No matches found with current tolerances or after filtering by match_strength.")
    return(target)
  }

  source_df <- sf::st_drop_geometry(source)

  if (is.null(columns)) {
    columns <- setdiff(
      names(source_df),
      c("source_id", "target_id", "row_number", "geometry", "geom")
    )
  } else {
    columns <- intersect(columns, names(source_df))
  }

  if (length(columns) == 0) {
    return(target)
  }

  is_cat <- vapply(
    source_df[columns],
    function(x) !is.numeric(x) || is.character(x),
    logical(1)
  )

  cat_cols <- columns[is_cat]
  num_cols <- setdiff(columns, cat_cols)

  source_df_internal <- source_df
  source_df_internal$row_idx_internal <- seq_len(nrow(source_df_internal))

  target_enriched <- target
  target_enriched$row_idx_internal <- seq_len(nrow(target_enriched))

  if (length(cat_cols) > 0) {
    for (col in cat_cols) {
      joined <- merge(
        match_tbl,
        source_df_internal[, c("row_idx_internal", col), drop = FALSE],
        by.x = "source_id",
        by.y = "row_idx_internal",
        all.x = TRUE,
        sort = FALSE
      )

      col_vals <- joined[[col]]
      keep <- !is.na(col_vals)

      if (!any(keep)) {
        next
      }

      joined <- joined[keep, c("target_id", "shared_len", col), drop = FALSE]

      key_target <- joined$target_id
      key_value <- as.character(joined[[col]])
      key <- paste(key_target, key_value, sep = "\r")

      summed <- stats::aggregate(
        joined$shared_len,
        by = list(key = key),
        FUN = sum
      )

      key_parts <- strsplit(summed$key, "\r", fixed = TRUE)
      target_id <- as.integer(vapply(key_parts, `[`, character(1), 1))
      value_chr <- vapply(key_parts, `[`, character(1), 2)

      totals <- data.frame(
        target_id = target_id,
        value = value_chr,
        total_shared = summed$x,
        stringsAsFactors = FALSE
      )

      ord <- order(
        totals$target_id,
        -totals$total_shared,
        totals$value
      )
      totals <- totals[ord, , drop = FALSE]

      first_idx <- !duplicated(totals$target_id)
      consensus <- totals[first_idx, c("target_id", "value"), drop = FALSE]

      new_col_name <- paste0(prefix, col)
      target_enriched[[new_col_name]] <- NA

      match_idx <- match(target_enriched$row_idx_internal, consensus$target_id)
      hit <- !is.na(match_idx)

      original_col <- source_df[[col]]
      if (is.factor(original_col)) {
        target_enriched[[new_col_name]] <- factor(target_enriched[[new_col_name]], levels = levels(original_col))
        target_enriched[[new_col_name]][hit] <- consensus$value[match_idx[hit]]
      } else if (is.logical(original_col)) {
        vals <- consensus$value[match_idx[hit]]
        target_enriched[[new_col_name]][hit] <- vals %in% "TRUE"
      } else {
        target_enriched[[new_col_name]][hit] <- consensus$value[match_idx[hit]]
      }
    }
  }

  if (length(num_cols) > 0) {
    for (col in num_cols) {
      val <- as.numeric(source_df[[col]])
      val[is.na(val)] <- 0

      new_col_name <- paste0(prefix, col, "_wt")

      if (col %in% aadt) {
        joined <- merge(
          match_tbl,
          data.frame(
            row_idx_internal = source_df_internal$row_idx_internal,
            value = val
          ),
          by.x = "source_id",
          by.y = "row_idx_internal",
          all.x = TRUE,
          sort = FALSE
        )

        joined$weighted_value <- joined$value * joined$target_weighted

        interp <- stats::aggregate(
          joined$weighted_value,
          by = list(target_id = joined$target_id),
          FUN = function(x) sum(x, na.rm = TRUE)
        )

        target_enriched[[new_col_name]] <- 0
        match_idx <- match(target_enriched$row_idx_internal, interp$target_id)
        hit <- !is.na(match_idx)
        target_enriched[[new_col_name]][hit] <- interp$x[match_idx[hit]]
      } else if (col %in% extensive) {
        target_enriched[[new_col_name]] <- interpolate_extensive(val, matches_ptr)
      } else {
        target_enriched[[new_col_name]] <- interpolate_intensive(val, matches_ptr)
      }
    }
  }

  target_enriched$row_idx_internal <- NULL
  target_enriched
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
