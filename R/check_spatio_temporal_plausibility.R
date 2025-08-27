#' Flag spatio-temporal plausibility
#'
#' @param predictions data.frame with columns: species, timestamp, plot_id
#' @param species_info_temporal data.frame with temporal info: species, start_date, end_date, start_hour, end_hour
#' @param species_ranges sf object with spatial ranges (polygons) and column `species`
#' @param plot_info data.frame with plot_id and coordinates: lat, lon
#' @param tz time zone for time-of-day checks
#'
#' @return original data with logical flag columns: season, time_of_day, range, and plausibility_score (sum of flags)
#' @export
check_spatio_temporal_plausibility <- function(predictions,
                                               species_info_temporal,
                                               species_ranges,
                                               plot_info,
                                               tz = "UTC") {

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
  dt <- data.table::merge.data.table(dt, si, by = "species", all.x = TRUE)
  dt <- data.table::merge.data.table(dt, pi, by = "plot_id", all.x = TRUE)

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
  # Disable spherical geometry (planar for bounding boxes)
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
