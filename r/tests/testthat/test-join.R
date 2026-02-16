test_that("anime_join works with base R implementation", {
  library(sf)
  source_lines = st_sf(
    id = 1:2,
    val_num = c(10, 20),
    val_cat = c("A", "B"),
    geometry = st_sfc(
      st_linestring(matrix(c(0,0, 10,0), ncol=2, byrow=TRUE)),
      st_linestring(matrix(c(0,1, 10,1), ncol=2, byrow=TRUE))
    ),
    crs = 27700
  )

  target_lines = st_sf(
    id = 1,
    geometry = st_sfc(
      st_linestring(matrix(c(0,0.1, 10,0.1), ncol=2, byrow=TRUE))
    ),
    crs = 27700
  )

  res = anime_join(
    source = source_lines,
    target = target_lines,
    distance_tolerance = 1,
    columns = c("val_num", "val_cat"),
    aadt = "val_num"
  )

  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 1)
  expect_equal(res$val_num_wt, 30)
  expect_equal(res$val_cat, "A")
})
