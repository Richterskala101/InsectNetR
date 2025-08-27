#' Flag spatio-temporal plausibility
#'
#' @param predictions data.frame with columns: species, timestamp, plot_id
#' @param species_info_temporal data.frame with temporal info: species, start_date, end_date, start_hour, end_hour
#' @param species_ranges sf object with spatial ranges (polygons) and column `species`
#' @param plot_info data.frame with plot_id and coordinates: lat, lon
#' @param tz time zone for time-of-day checks
#'
#' @return original data with logical flag columns: season, time_of_day, range, and plausibility_score (sum of flags)
#' @examples
#' library(lubridate)
#' library(sf)
#'
#' # Example predictions
#' predictions <- data.frame(
#'   species = c("speciesA", "speciesB"),
#'   timestamp = ymd_hms(c("2024-07-19 11:20:00", "2024-07-22 12:40:00"), tz = "UTC"),
#'   plot_id = c("plot1", "plot2")
#' )
#'
#' # Temporal species info
#' species_info_temporal <- data.frame(
#'   species = c("speciesA", "speciesB"),
#'   start_date = c("06-01", "07-01"),
#'   end_date   = c("08-31", "08-31"),
#'   start_hour = c(6, 8),
#'   end_hour   = c(20, 18)
#' )
#'
#' # Plot info with point coordinates
#' plot_info <- data.frame(
#'   plot_id = c("plot1", "plot2"),
#'   lat = c(50.25, 51.25),
#'   lon = c(10.25, 11.25)
#' )
#'
#' # Spatial ranges as polygons
#' polyA <- st_polygon(list(matrix(c(
#'   9.0, 49.0,
#'   9.0, 51.0,
#'   11.0, 51.0,
#'   11.0, 49.0,
#'   9.0, 49.0
#' ), ncol = 2, byrow = TRUE)))
#'
#' polyB <- st_polygon(list(matrix(c(
#'   10.0, 50.0,
#'   10.0, 52.0,
#'   12.0, 52.0,
#'   12.0, 50.0,
#'   10.0, 50.0
#' ), ncol = 2, byrow = TRUE)))
#'
#' species_ranges <- st_sf(
#'   species = c("speciesA", "speciesB"),
#'   geometry = st_sfc(list(polyA, polyB), crs = 4326)
#' )
#'
#' # Run plausibility check
#' result <- check_spatio_temporal_plausibility(
#'   predictions, species_info_temporal, species_ranges, plot_info, tz = "UTC"
#' )
#'
#' print(result[, c("species", "season", "time_of_day", "range", "plausibility_score")])
#'
#' @export
check_spatio_temporal_plausibility <- function(predictions,
                                               species_info_temporal,
                                               species_ranges,
                                               plot_info,
                                               tz = "UTC") {
  # Required packages
  requireNamespace("data.table", quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)
  requireNamespace("sf", quietly = TRUE)

  # Convert to data.table
  dt <- data.table::as.data.table(predictions)
  si <- data.table::as.data.table(species_info_temporal)
  pi <- data.table::as.data.table(plot_info)

  # Ensure timestamp is POSIXct
  if (!inherits(dt$timestamp, "POSIXct")) {
    dt$timestamp <- as.POSIXct(dt$timestamp, tz = tz)
  }

  # Extract date and hour
  dt$date <- as.Date(lubridate::with_tz(dt$timestamp, tzone = tz))
  dt$hour <- as.integer(lubridate::hour(lubridate::with_tz(dt$timestamp, tzone = tz)))

  # Merge temporal info + plot info
  dt <- merge(dt, si, by = "species", all.x = TRUE)
  dt <- merge(dt, pi, by = "plot_id", all.x = TRUE)

  # --- Temporal plausibility ---
  dt$season <- mapply(function(d, sd, ed) {
    sd_full <- as.Date(paste0(lubridate::year(d), "-", sd))
    ed_full <- as.Date(paste0(lubridate::year(d), "-", ed))
    if (sd_full <= ed_full) {
      d >= sd_full & d <= ed_full
    } else {
      d >= sd_full | d <= ed_full
    }
  }, dt$date, dt$start_date, dt$end_date)

  dt$time_of_day <- dt$hour >= dt$start_hour & dt$hour <= dt$end_hour

  # --- Spatial plausibility ---
  # Disable spherical geometry (use planar for bounding boxes)
  old_s2 <- sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)  # restore original setting

  sf_points <- sf::st_as_sf(dt, coords = c("lon", "lat"), crs = 4326)

  sf_joined <- sf::st_join(sf_points, species_ranges, join = sf::st_within, left = TRUE, suffix = c("", "_range"))

  # Range check
  sf_joined$range <- !is.na(sf_joined$species_range) & (sf_joined$species == sf_joined$species_range)

  # Deduplicate: keep only rows where species matched its polygon
  sf_joined <- sf_joined[sf_joined$range | is.na(sf_joined$species_range), ]

  # Finalize output
  out <- data.table::as.data.table(sf_joined)
  out$plausibility_score <- out$season + out$time_of_day + out$range

  return(out[])
}
