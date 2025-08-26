#' Flag spatio-temporal plausibility
#'
#' @param detections data.frame with columns: species, timestamp, plot_id. Could be created with reshape_predictions() function from raw data
#' @param species_info data.frame with species-specific metadata (e.g. range, season and time of day)
#' @param plot_info data.frame the coordinates of the plots/locations
#' @param tz time zone for time-of-day checks
#'
#' season, time_of_day, range are logical columns.
#' plausibility_score counts how many checks are TRUE (0–3).
#' species_info must include: species, start_date, end_date, start_hour, end_hour.
#' plot_info must include: plot_id, lat_min, lat_max, lon_min, lon_max.
#' detections must include: species, timestamp, plot_id.
#'
#' @return original data with logical flag columns. one column for every check, one summarizing column, counting number of flags.
#' @description
#' season = TRUE → detection occurs within the species’ known seasonal window
#' time_of_day = TRUE → detection occurs within the species’ known active hours
#' range = TRUE → detection occurs within the species’ known spatial range
#'
#' @examples
#' library(lubridate)
#' library(data.table)
#'
#' # minimal detections data
#' detections <- data.frame(
#'   species = c("speciesA", "speciesB"),
#'   timestamp = ymd_hms(c("2024-07-19 11:20:00", "2024-07-22 12:40:00")),
#'   plot_id = c("plot1", "plot2")
#' )
#'
#' # minimal species info
#' species_info <- data.frame(
#'   species = c("speciesA", "speciesB"),
#'   start_date = c("06-01", "07-01"),
#'   end_date   = c("08-31", "08-31"),
#'   start_hour = c(6, 8),
#'   end_hour   = c(20, 18)
#' )
#'
#' # minimal plot info
#' plot_info <- data.frame(
#'   plot_id = c("plot1", "plot2"),
#'   lat_min = c(50.0, 51.0),
#'   lat_max = c(50.5, 51.5),
#'   lon_min = c(10.0, 11.0),
#'   lon_max = c(10.5, 11.5)
#' )
#'
#' # run function
#' result <- check_spatio_tomporal_plausibility(detections, species_info, plot_info)
#' print(result)
#' @export
check_spatio_tomporal_plausibility <- function(detections, species_info, plot_info, tz = "UTC") {
  library(data.table)
  library(lubridate)

  # Convert to data.tables
  dt <- data.table::as.data.table(detections)
  si <- data.table::as.data.table(species_info)
  pi <- data.table::as.data.table(plot_info)

  # Ensure timestamp is POSIXct
  if (!inherits(dt$timestamp, "POSIXct")) {
    dt$timestamp <- as.POSIXct(dt$timestamp, tz = tz)
  }

  # Extract date and hour
  dt$date <- as.Date(lubridate::with_tz(dt$timestamp, tzone = tz))
  dt$hour <- as.integer(lubridate::hour(lubridate::with_tz(dt$timestamp, tzone = tz)))

  # Merge with species metadata
  dt <- merge(dt, si, by = "species", all.x = TRUE)

  # Merge with plot coordinates
  dt <- merge(dt, pi, by = "plot_id", all.x = TRUE)

  # Season check
  dt$season <- mapply(function(d, sd, ed) {
    sd_full <- as.Date(paste0(year(d), "-", sd))
    ed_full <- as.Date(paste0(year(d), "-", ed))
    if (sd_full <= ed_full) {
      d >= sd_full & d <= ed_full
    } else {
      d >= sd_full | d <= ed_full
    }
  }, dt$date, dt$start_date, dt$end_date)

  # Time-of-day check
  dt$time_of_day <- dt$hour >= dt$start_hour & dt$hour <= dt$end_hour

  # Range check: species occurrence vs plot point
  dt$range <- dt$lat >= dt$lat_min & dt$lat <= dt$lat_max &
    dt$lon >= dt$lon_min & dt$lon <= dt$lon_max

  # Plausibility score
  dt$plausibility_score <- dt$season + dt$time_of_day + dt$range

  return(dt)
}
