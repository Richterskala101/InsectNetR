test_that("check_spatio_temporal_plausibility() returns correct flags", {


  # Predictions
  predictions <- data.frame(
    species = c("speciesA", "speciesB"),
    timestamp = lubridate::ymd_hms(c("2024-07-19 11:20:00", "2024-07-22 12:40:00"), tz = "UTC"),
    plot_id = c("plot1", "plot2")
  )

  # Temporal species info
  species_info_temporal <- data.frame(
    species = c("speciesA", "speciesB"),
    start_date = c("06-01", "07-01"),
    end_date   = c("08-31", "08-31"),
    start_hour = c(6, 8),
    end_hour   = c(20, 18)
  )

  # Plot info
  plot_info <- data.frame(
    plot_id = c("plot1", "plot2"),
    lat = c(50.25, 51.25),
    lon = c(10.25, 11.25)
  )

  # Spatial ranges as polygons
  polyA <- sf::st_polygon(list(matrix(c(
    9.0, 49.0,
    9.0, 51.0,
    11.0, 51.0,
    11.0, 49.0,
    9.0, 49.0
  ), ncol = 2, byrow = TRUE)))

  polyB <- sf::st_polygon(list(matrix(c(
    10.0, 50.0,
    10.0, 52.0,
    12.0, 52.0,
    12.0, 50.0,
    10.0, 50.0
  ), ncol = 2, byrow = TRUE)))

  species_ranges <- sf::st_sf(
    species = c("speciesA", "speciesB"),
    geometry = sf::st_sfc(list(polyA, polyB), crs = 4326)
  )

  # Run function
  result <- check_spatio_temporal_plausibility(
    predictions, species_info_temporal, species_ranges, plot_info, tz = "UTC"
  )

  # Check structure
  expect_true(all(c("season", "time_of_day", "range", "plausibility_score") %in% names(result)))

  # SpeciesA should be valid in all dimensions → score 3
  expect_true(result$plausibility_score[result$species == "speciesA"] == 3)

  # SpeciesB (plot2 at 12:40) is inside date window, inside polygon,
  # but outside time window (8–18 is ok, 12:40 is valid → so should be 3)
  expect_true(result$plausibility_score[result$species == "speciesB"] == 3)
})
