## code to prepare `maine-matches` dataset goes here
maine_roads <- roads("Maine", "cumberland") |>
  st_transform(26983)

bbox <- st_point(c(-70.26190, 43.65672)) |>
  st_sfc(crs = 4326) |>
  st_transform(26983) |>
  st_buffer(100) |>
  st_bbox()

osm_rds <- osmextract::oe_get(bbox) |>
  st_transform(26983)

# crop results
sources <- st_crop(maine_roads, bbox)
targets <- st_crop(osm_rds, bbox)

sources <- st_make_valid(sources) |>
  st_cast("LINESTRING")

targets <- st_make_valid(targets) |>
  st_cast("LINESTRING")

sf::st_write(sources, "inst/extdata/maine-tigris-sources.fgb", delete_dsn = TRUE)
sf::st_write(targets, "inst/extdata/maine-osm-targets.fgb", delete_dsn = TRUE)
