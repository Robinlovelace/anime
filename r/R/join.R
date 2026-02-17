#' Join attributes from a source network to a target network
#'
#' @param source A spatial object or data frame representing the source network.
#' @param target A spatial object or data frame representing the target network.
#' @param distance_tolerance The maximum distance between two linestrings to be considered a match.
#' @param angle_tolerance The maximum angle difference between two lines to be considered a match.
#' @param prefix A string prefix for the new columns. Default is "".
#' @param match_strength A threshold for target_weighted to filter out weak matches (0 to 1). Default is 0.
#' @param columns Character vector of columns to transfer. If NULL, transfers all columns except geometries and internal IDs.
#' @param extensive Character vector of columns to treat as extensive (summed by source weight).
#' @param aadt Character vector of columns to treat as AADT (summed if parallel, averaged if series, using target weight).
#' @return The target object with joined attributes.
#' @export
#' @examples
#' \dontrun{
#' # Example inspired by od2net Edinburgh tutorial
#' # See https://od2net.org for more context
#' # Using sf for geometry creation
#' library(sf)
#' 
#' source_lines = st_sf(
#'   osm_id = 1:2,
#'   flow = c(100, 200),
#'   highway = c("residential", "primary"),
#'   geometry = st_sfc(
#'     st_linestring(matrix(c(0,0, 10,0), ncol=2, byrow=TRUE)),
#'     st_linestring(matrix(c(0,1, 10,1), ncol=2, byrow=TRUE))
#'   ),
#'   crs = 27700
#' )
#' 
#' target_lines = st_sf(
#'   id = 1,
#'   geometry = st_sfc(
#'     st_linestring(matrix(c(0,0.1, 10,0.1), ncol=2, byrow=TRUE))
#'   ),
#'   crs = 27700
#' )
#' 
#' # Joining with AADT-style aggregation (summing parallel segments)
#' joined = anime_join(
#'   source = source_lines,
#'   target = target_lines,
#'   distance_tolerance = 1,
#'   columns = c("flow", "highway"),
#'   aadt = "flow"
#' )
#' }
anime_join <- function(source,
                       target,
                       distance_tolerance = 10,
                       angle_tolerance = 5,
                       prefix = "",
                       match_strength = 0,
                       columns = NULL,
                       extensive = NULL,
                       aadt = NULL) {

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

    # Helper to drop geometry without sf
    drop_geometry <- function(x) {
        # General removal of common geometry/spatial column names
        geom_cols <- c("geometry", "geom", "spatial", "shape", "geometry_column")
        if (inherits(x, "sf")) {
            geom_col <- attr(x, "sf_column")
            geom_cols <- unique(c(geom_cols, geom_col))
        }
        x_df <- as.data.frame(x)
        x_df <- x_df[, !(names(x_df) %in% geom_cols), drop = FALSE]
        x_df
    }

    # 3. Identify columns to transfer
    source_df <- drop_geometry(source)
    if (is.null(columns)) {
        # Exclude common geom/id columns if they happen to exist
        columns <- setdiff(names(source_df), c("source_id", "target_id", "row_number", "row_idx_internal"))
    } else {
        columns <- intersect(columns, names(source_df))
    }

    # Identify categorical vs numeric
    is_num <- vapply(source_df[columns], is.numeric, logical(1))
    cat_cols <- columns[!is_num]
    num_cols <- columns[is_num]

    # 4. Perform Consensus Categorical Matching (Majority shared length wins)
    # We use row indices for internal matching
    source_df_internal <- source_df
    source_df_internal$row_idx_internal <- seq_len(nrow(source_df_internal))

    # Initialize enriched target
    target_enriched <- as.data.frame(target)
    target_enriched$row_idx_internal <- seq_len(nrow(target_enriched))

    # Process categorical columns
    if (length(cat_cols) > 0) {
        for (col in cat_cols) {
            # Attach values from source to match_tbl
            m <- merge(match_tbl, source_df_internal[, c("row_idx_internal", col)], 
                       by.x = "source_id", by.y = "row_idx_internal", all.x = TRUE)
            
            # Aggregate shared_len by target_id and category
            # We use a formula that's safer for varying column names
            agg <- aggregate(m$shared_len, by = list(target_id = m$target_id, val = m[[col]]), sum)
            names(agg)[2] <- col
            names(agg)[3] <- "shared_len"
            
            # Pick the max shared_len for each target_id
            winners <- agg[order(agg$target_id, -agg$shared_len), ]
            winners <- winners[!duplicated(winners$target_id), ]
            
            new_col_name <- paste0(prefix, col)
            
            # Join back
            target_enriched <- merge(target_enriched, winners[, c("target_id", col)], 
                                     by.x = "row_idx_internal", by.y = "target_id", all.x = TRUE)
            names(target_enriched)[names(target_enriched) == col] <- new_col_name
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
                m <- merge(match_tbl, source_df_internal[, c("row_idx_internal", col)], 
                           by.x = "source_id", by.y = "row_idx_internal", all.x = TRUE)
                
                m$weighted_val <- m[[col]] * m$target_weighted
                interp <- aggregate(weighted_val ~ target_id, data = m, sum, na.rm = TRUE)
                
                target_enriched <- merge(target_enriched, interp, 
                                         by.x = "row_idx_internal", by.y = "target_id", all.x = TRUE)
                names(target_enriched)[names(target_enriched) == "weighted_val"] <- new_col_name
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

    # Convert back to original class if possible
    if (inherits(target, "sf")) {
        return(sf::st_as_sf(target_enriched))
    }

    return(target_enriched)
}
