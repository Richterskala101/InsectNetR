test_that("get_gbif_ranges returns an sf object", {
  skip_on_cran()
  skip_if_offline()  # requires internet for GBIF API

  sp_vec <- c("Pseudochorthippus apicarius")  # small test species

  ranges <- get_gbif_ranges(
    species = sp_vec,
    res = 50,          # larger grid to reduce complexity
    limit = 200,       # keep test light
    region = "europe"
  )

  # Check class
  expect_s3_class(ranges, "sf")

  # Must have species column
  expect_true("species" %in% names(ranges))

  # Must only contain requested species
  expect_equal(unique(ranges$species), "Pseudochorthippus apicarius")

  # Must contain at least one polygon
  expect_gt(nrow(ranges), 0)
})
