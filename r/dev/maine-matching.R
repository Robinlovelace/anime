library(sf)
library(dplyr)
library(tigris)
library(anime)
library(ggplot2)

# get sample lines from package
targets_fp <- system.file("extdata", "maine-osm-targets.fgb", package = "anime")
sources_fp <- system.file("extdata", "maine-tigris-sources.fgb", package = "anime")

# read into sf objects
targets <- read_sf(targets_fp)
sources <- read_sf(sources_fp)

# perfomr matches
matches <- anime(
  sources,
  targets,
  distance_tolerance = 10,
  angle_tolerance = 5
)

matches

# view original geometries
plot(st_geometry(sources), lty = 2)
plot(st_geometry(targets), col = 1:nrow(targets), add = TRUE)


# extract matches as a data.frame
match_tbl <- get_matches(matches)
match_tbl

# find most matched source
most_matched_source <- count(match_tbl, source_id, sort = TRUE) |>
  slice(1) |>
  pull(source_id)

# find the matched targets in the sf object
matched_tars <- match_tbl |>
  filter(source_id == most_matched_source, shared_len > 0) |>
  inner_join(transmute(targets, target_id = row_number())) |>
  st_as_sf()

# visualize them
ggplot() +
  geom_sf(aes(color = shared_len), matched_tars, lwd = 2) +
  geom_sf(data = sources[most_matched_source, ], lty = 2) +
  scale_color_binned() +
  theme_void()

# TODO show interpolation
# these datasets dont have any numeric values to interolate
