library(sf)
library(dplyr)
library(tigris)
library(rnetmatch)

roads <- roads("Maine", "cumberland") |>
  st_transform(26983)
rds_bbox <- sf::st_bbox(roads)

osm_rds <- osmextract::oe_get(rds_bbox) |>
  st_transform(26983)

# crop results
targets <- st_crop(osm_rds, rds_bbox)

x <- st_make_valid(roads) |>
  st_cast("LINESTRING")

y <- st_make_valid(targets) |>
  st_cast("LINESTRING")


# look at casco street which is a really good example
# if you use DT of 10, you can get matches on both sides
# which is
matches <- rnet_match(x, y, 10, 5)

bm <- bench::mark(
  current = rnet_match(x, y, 10, 5),
  vec = rnet_match_vec(
    geoarrow::as_geoarrow_array(x),
    geoarrow::as_geoarrow_array(y),
    10, 5,
    T
  ),
  check = F
)

matches |>
  mutate(
    x_len = as.numeric(st_length(x)[i]),
    x_wt = shared_len / x_len,
    y_len = as.numeric(st_length(y)[j]),
    y_wt = shared_len / y_len,
    x_nm = x$FULLNAME[i],
    y_nm = y$name[j]
  ) |>
  # intensive vars weight
  arrange(-y_wt) |>
  as_tibble()

# casco street
plot(st_geometry(x)[13889], col = "red", lwd = 2)
plot(st_geometry(y)[8577], lty = 3, lwd = 5, add = TRUE)

#
plot(st_geometry(y)[c(19203, 21951,19211,19204, 19208)], lwd = 2)
plot(st_geometry(x)[78], col = "red", lwd = 1, add = TRUE)
rnet_aggregate(
  y,
  matches,
  intensive_vars = "z_order",
  extensive_vars = "z_order",
  categorical_vars = "highway",
  target_len = st_length(x),
)




