test_that("range violation is correctly flagged when point is outside species range", {
  library(terra)

  # Create raster with 1 presence cell
  r <- rast(nrows = 10, ncols = 10, xmin = 0, xmax = 20, ymin = 50, ymax = 60)
  values(r) <- 0
  cell_index <- cellFromRowCol(r, 5, 5)
  cell_coords <- xyFromCell(r, cell_index)
  values(r)[cell_index] <- 1
  names(r) <- "Bicolorana.bicolor"

  # Create a second location outside the presence area
  plot_info <- data.frame(
    plot_id = c("present", "absent"),
    lat = c(cell_coords[2], 55.0),
    lon = c(cell_coords[1], 15.0)
  )

  predictions <- data.frame(
    filename = c("20250701_120000.WAV", "20250701_120000.WAV"),
    offset = c("0-10", "0-10"),
    Bicolorana.bicolor = c(1, 1)
  )

  species_info_temporal <- data.frame(
    species = "Bicolorana.bicolor",
    season_start = 6,
    season_end = 8,
    peak_activity_start = 8,
    peak_activity_end = 20
  )

  result <- check_spatio_temporal_plausibility(
    predictions = predictions,
    species_info_temporal = species_info_temporal,
    species_ranges = r,
    plot_info = plot_info
  )

  expect_equal(result$Bicolorana.bicolor_range_violation, c(0L, 1L))
})
test_that("multiple species handled correctly", {
  # Create raster with two layers
  r <- rast(nrows = 10, ncols = 10, xmin = 0, xmax = 20, ymin = 50, ymax = 60)
  values(r) <- 0
  r1_cell <- cellFromRowCol(r, 3, 3)
  r2_cell <- cellFromRowCol(r, 8, 8)

  r1 <- r; r2 <- r
  values(r1)[r1_cell] <- 1
  values(r2)[r2_cell] <- 1
  names(r1) <- "Species.one"
  names(r2) <- "Species.two"
  r_stack <- c(r1, r2)

  # Get coords for presence points
  coords_r1 <- xyFromCell(r1, r1_cell)
  coords_r2 <- xyFromCell(r2, r2_cell)

  # Plot info (one for each species)
  plot_info <- data.frame(
    plot_id = c("r1", "r2"),
    lat = c(coords_r1[2], coords_r2[2]),
    lon = c(coords_r1[1], coords_r2[1])
  )

  predictions <- data.frame(
    filename = c("20250701_120000.WAV", "20250701_120000.WAV"),
    offset = c("0-10", "0-10"),
    Species.one = c(1, 0),
    Species.two = c(0, 1)
  )

  species_info_temporal <- data.frame(
    species = c("Species.one", "Species.two"),
    season_start = c(6, 6),
    season_end = c(8, 8),
    peak_activity_start = c(8, 8),
    peak_activity_end = c(20, 20)
  )

  result <- check_spatio_temporal_plausibility(
    predictions = predictions,
    species_info_temporal = species_info_temporal,
    species_ranges = r_stack,
    plot_info = plot_info
  )

  expect_equal(result$Species.one_range_violation, c(0L, NA_integer_))
  expect_equal(result$Species.two_range_violation, c(NA_integer_, 0L))
})
test_that("season wrapping year boundary is handled correctly", {
  predictions <- data.frame(
    filename = c("20251215_120000.WAV", "20250701_120000.WAV"),  # Dec and July
    offset = c("0-10", "0-10"),
    Winter.species = c(1, 1)
  )

  # Any location; all in-range
  plot_info <- data.frame(
    plot_id = c("20251215_120000", "20250701_120000"),
    lat = c(53.5, 53.5),
    lon = c(10.0, 10.0)
  )

  # All in-range
  r <- rast(nrows = 10, ncols = 10, xmin = 0, xmax = 20, ymin = 50, ymax = 60)
  values(r) <- 1
  names(r) <- "Winter.species"

  species_info_temporal <- data.frame(
    species = "Winter.species",
    season_start = 10,
    season_end = 2,
    peak_activity_start = 0,
    peak_activity_end = 23
  )

  result <- check_spatio_temporal_plausibility(
    predictions = predictions,
    species_info_temporal = species_info_temporal,
    species_ranges = r,
    plot_info = plot_info
  )

  # Only July sample is a season violation
  expect_equal(result$Winter.species_season_violation, c(0L, 1L))
})
