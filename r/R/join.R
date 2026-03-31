#' Join attributes from a source network to a target network
#'
#' @param source An `sf` object or handleable geometry representing the source network.
#' @param target An `sf` object or handleable geometry representing the target network.
#' @param distance_tolerance The maximum distance between two linestrings to be considered a match.
#' @param angle_tolerance The maximum angle difference between two lines to be considered a match.
#' @param prefix A string prefix for the new columns. Default is "".
#' @param match_strength A threshold for target_weighted to filter out weak matches (0 to 1). Default is 0.
#' @param columns Character vector of columns to transfer. If NULL, transfers all columns except geometries and internal IDs.
#' @param extensive Character vector of columns to treat as extensive (summed by source weight).
#' @param aadt Character vector of columns to treat as AADT (summed if parallel, averaged if series, using target weight).
#' @return The target object with joined attributes.
#' @importFrom sf st_drop_geometry st_as_sf
#' @importFrom dplyr mutate filter group_by summarise ungroup slice_max left_join select rename_with where everything n first
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
    if (!inherits(source, "sf") || !inherits(target, "sf")) {
        stop("source and target must be sf objects for anime_join to work automatically.")
    }

    # 1. Run the core anime matching
    matches_ptr <- anime(source, target, distance_tolerance, angle_tolerance)
    match_tbl <- get_matches(matches_ptr)

    if (nrow(match_tbl) == 0) {
        warning("No matches found with current tolerances.")
        return(target)
    }

    # 2. Filter by match strength
    if (match_strength > 0) {
        match_tbl <- match_tbl[match_tbl$target_weighted >= match_strength, ]
    }

    if (nrow(match_tbl) == 0) {
        warning("No matches remained after filtering by match_strength.")
        return(target)
    }

    # 3. Identify columns to transfer
    source_df <- sf::st_drop_geometry(source)
    if (is.null(columns)) {
        # Exclude common geom/id columns if they happen to exist
        columns <- setdiff(names(source_df), c("source_id", "target_id", "row_number", "geometry", "geom"))
    } else {
        columns <- intersect(columns, names(source_df))
    }

    # Identify categorical vs numeric
    cat_cols <- names(source_df[columns])[vapply(source_df[columns], function(x) !is.numeric(x) || is.character(x), logical(1))]
    num_cols <- setdiff(columns, cat_cols)

    # 4. Perform Consensus Categorical Matching (Majority shared length wins)
    # We use row indices for internal matching
    source_df_internal <- source_df
    source_df_internal$row_idx_internal <- seq_len(nrow(source_df_internal))

    # Initialize enriched target
    target_enriched <- target
    target_enriched$row_idx_internal <- seq_len(nrow(target_enriched))

    # Process categorical columns
    if (length(cat_cols) > 0) {
        # Robust approach: For each categorical column, find the consensus winner.
        for (col in cat_cols) {
            consensus <- match_tbl |>
                dplyr::left_join(source_df_internal[, c("row_idx_internal", col)], by = c("source_id" = "row_idx_internal")) |>
                dplyr::group_by(target_id, .data[[col]]) |>
                dplyr::summarise(total_shared = sum(shared_len), .groups = "drop") |>
                dplyr::group_by(target_id) |>
                dplyr::slice_max(total_shared, n = 1, with_ties = FALSE) |>
                dplyr::ungroup()

            # Rename to avoid collisions and add prefix
            new_col_name <- paste0(prefix, col)
            consensus <- consensus[, c("target_id", col)]
            names(consensus) <- c("row_idx_internal", new_col_name)

            target_enriched <- dplyr::left_join(target_enriched, consensus, by = "row_idx_internal")
        }
    }

    # 5. Perform Interpolation for numeric columns
    if (length(num_cols) > 0) {
        for (col in num_cols) {
            val <- as.numeric(source_df[[col]])
            val[is.na(val)] <- 0
            
            new_col_name <- paste0(prefix, col, "_wt")
            
            if (col %in% aadt) {
                # Smart AADT interpolation: sum(val * target_weighted)
                # This correctly averages segments in series and sums them in parallel.
                m <- match_tbl |>
                    dplyr::left_join(source_df_internal[, c("row_idx_internal", col)], by = c("source_id" = "row_idx_internal"))
                
                interp <- m |>
                    dplyr::group_by(target_id) |>
                    dplyr::summarise(v = sum(.data[[col]] * target_weighted, na.rm = TRUE), .groups = "drop")
                
                interp <- interp[, c("target_id", "v")]
                names(interp) <- c("row_idx_internal", new_col_name)
                target_enriched <- dplyr::left_join(target_enriched, interp, by = "row_idx_internal")
                target_enriched[[new_col_name]][is.na(target_enriched[[new_col_name]])] <- 0
            } else if (col %in% extensive) {
                target_enriched[[new_col_name]] <- interpolate_extensive(val, matches_ptr)
            } else {
                target_enriched[[new_col_name]] <- interpolate_intensive(val, matches_ptr)
            }
        }
    }

    # Clean up internal ID
    target_enriched$row_idx_internal <- NULL

    return(target_enriched)
}
