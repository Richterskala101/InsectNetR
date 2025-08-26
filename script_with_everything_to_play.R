' Parse datetime from filename (YYYYMMDD_HHMMSS or similar)
#' @param filename character
#' @param tz time zone (default "UTC")
#' @return POSIXct
#' @export
#'
#'
#'

predictions1 <- read.csv("B:/diverses/HearTheSpecies/Database/Test_Insect_Model/AEG18_RP/results.csv") |>
  mutate(filename = paste0("AEG18RP/", filename))

predictions2 <- read.csv("B:/diverses/HearTheSpecies/Database/Test_Insect_Model/AEG20_RP/results.csv") |>
  mutate(filename = paste0("AEG20RP/", filename))

predictions <- rbind(predictions1, predictions2)

parse_datetime <- function(df, filename_col = "filename") {
  lubridate::ymd_hms(basename(df[[filename_col]]))
}

parse_datetime <- function(filename, tz = "UTC") {
  x <- basename(filename)
  x <- tools::file_path_sans_ext(x)
  m <- regexpr("(\\d{8})[_-]?(\\d{6})", x, perl = TRUE)
  if (m[1] == -1) stop("Could not parse datetime from filename: ", filename)
  ymd <- substr(x, attr(m, "capture.start")[1], attr(m, "capture.start")[1] + attr(m, "capture.length")[1] - 1)
  hms <- substr(x, attr(m, "capture.start")[2], attr(m, "capture.start")[2] + attr(m, "capture.length")[2] - 1)
  lubridate::ymd_hms(paste0(ymd, " ", hms), tz = tz)
}


#' Reshape ML wide predictions to long tidy format
#'
#' @param df data.frame with filename, offset, and one column per species (confidence values)
#' @param tz character timezone (default "UTC")
#' @return data.table with columns filename, offset, species, conf, timestamp, Plot_ID
#' @export
reshape_predictions <- function(df, tz = "UTC") {
  df %>%
    tidyr::pivot_longer(
      cols = -c(filename, offset, prediction, output),
      names_to = "species",
      values_to = "conf"
    ) %>%
    dplyr::mutate(
      timestamp = parse_datetime(filename) + readr::parse_number(offset),
      Plot_ID = stringr::str_split(filename, "_", simplify = TRUE)[,1]
    )
}


# ── R/plausibility.R ──────────────────────────────────────────────────────────
#' Flag spatio-temporal plausibility
#'
#' @param dets data.frame with columns: species, timestamp, lat, lon
#' @param species_info data.frame with species-specific metadata
#' @param tz time zone for time-of-day checks
#' @return original data with logical flag columns
#' @export
check_spatio_tomporal_plausibility <- function(dets, species_info, tz = "UTC") {
  dt <- data.table::as.data.table(dets)
  si <- data.table::as.data.table(species_info)
  dt[, date := lubridate::as_date(timestamp, tz = tz)]
  dt[, hour := as.integer(lubridate::hour(lubridate::with_tz(timestamp, tzone = tz)))]
  out <- merge(dt, si, by = "species", all.x = TRUE)
  out[, in_season := {
    sd <- paste0(lubridate::year(date), "-", start_date)
    ed <- paste0(lubridate::year(date), "-", end_date)
    d <- as.Date(date)
    s <- as.Date(sd)
    e <- as.Date(ed)
    ifelse(s <= e, d >= s & d <= e, d >= s | d <= e)
  }]
  out[, in_hours := hour >= start_hour & hour <= end_hour]
  out[, in_bbox := lat >= lat_min & lat <= lat_max & lon >= lon_min & lon <= lon_max]
  out[, plausible := in_season & in_hours & in_bbox]
  out
}
