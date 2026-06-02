#' Join attributes from a source network to a destination network
#'
#' @param source An `sf` object or handleable geometry representing the source network.
#' @param dest An `sf` object or handleable geometry representing the destination network.
#' @param min_overlap_dest Optional. A scalar numeric threshold (0 to 1) for the destination segment overlap.
#' @param min_overlap_source Optional. A scalar numeric threshold (0 to 1) for the source segment overlap.
#' @param intensive Character vector of columns to treat as intensive (length-weighted average).
#' @param extensive Character vector of columns to treat as extensive (summed by source weight).
#' @param prefix A string prefix for the new columns. Default is "".
#' @param anime_opts A named list of matching options (e.g. `distance_tolerance` and `angle_tolerance`).
#' @return The destination object with joined attributes.
#' @export
anime_join <- function(source,
                       dest,
                       min_overlap_dest = NULL,
                       min_overlap_source = NULL,
                       intensive = NULL,
                       extensive = NULL,
                       prefix = "",
                       anime_opts = list()) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package \"sf\" is required for anime_join to work. Please install it.", call. = FALSE)
  }

  if (!inherits(source, "sf") || !inherits(dest, "sf")) {
    stop("source and dest must be sf objects for anime_join to work automatically.")
  }

  dist_tol <- anime_opts$distance_tolerance %||% 10
  angle_tol <- anime_opts$angle_tolerance %||% 5

  matches_ptr <- anime(source, dest, dist_tol, angle_tol)

  if (!is.null(min_overlap_dest) || !is.null(min_overlap_source)) {
    filter_matches(matches_ptr, min_overlap_dest, min_overlap_source)
  }

  match_tbl <- get_matches(matches_ptr)

  if (nrow(match_tbl) == 0) {
    warning("No matches found with current tolerances or after filtering by min_overlap.")
    return(dest)
  }

  source_df <- sf::st_drop_geometry(source)
  cols_to_check <- setdiff(
    names(source_df),
    c("source_id", "target_id", "row_number", "geometry", "geom")
  )

  is_num <- vapply(source_df[cols_to_check], is.numeric, logical(1))
  num_cols <- cols_to_check[is_num]

  if (length(num_cols) > 0 && is.null(intensive) && is.null(extensive)) {
    stop("For statistical safety, you must explicitly specify which numeric columns are 'intensive' or 'extensive'.")
  }

  intensive <- intensive %||% character()
  extensive <- extensive %||% character()

  intensive <- intersect(intensive, num_cols)
  extensive <- intersect(extensive, num_cols)

  cat_cols <- cols_to_check[!is_num]

  source_df_internal <- source_df
  source_df_internal$row_idx_internal <- seq_len(nrow(source_df_internal))

  dest_enriched <- dest
  dest_enriched$row_idx_internal <- seq_len(nrow(dest_enriched))

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
      dest_enriched[[new_col_name]] <- NA

      match_idx <- match(dest_enriched$row_idx_internal, consensus$target_id)
      hit <- !is.na(match_idx)

      original_col <- source_df[[col]]
      if (is.factor(original_col)) {
        dest_enriched[[new_col_name]] <- factor(dest_enriched[[new_col_name]], levels = levels(original_col))
        dest_enriched[[new_col_name]][hit] <- consensus$value[match_idx[hit]]
      } else if (is.logical(original_col)) {
        vals <- consensus$value[match_idx[hit]]
        dest_enriched[[new_col_name]][hit] <- vals %in% "TRUE"
      } else {
        dest_enriched[[new_col_name]][hit] <- consensus$value[match_idx[hit]]
      }
    }
  }

  if (length(intensive) > 0) {
    for (col in intensive) {
      val <- as.numeric(source_df[[col]])
      val[is.na(val)] <- 0
      new_col_name <- paste0(prefix, col, "_wt")
      dest_enriched[[new_col_name]] <- interpolate_intensive(val, matches_ptr)
    }
  }

  if (length(extensive) > 0) {
    for (col in extensive) {
      val <- as.numeric(source_df[[col]])
      val[is.na(val)] <- 0
      new_col_name <- paste0(prefix, col, "_wt")
      dest_enriched[[new_col_name]] <- interpolate_extensive(val, matches_ptr)
    }
  }

  dest_enriched$row_idx_internal <- NULL
  dest_enriched
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
