library(openxlsx)
confusable_species <- read.xlsx("C:/Users/darend/Downloads/Heuschrecken Verwechslungen Matrix.xlsx") |>
  dplyr::mutate(Verwechslung.1 = ifelse(Verwechslung.1 == "x", NA, Verwechslung.1)) |>
  dplyr::rename(species = Art,
         confusion_species_1 = Verwechslung.1,
         confusion_species_2 = Verwechslung.2,
         confusion_species_3 = Verwechslung.3)

usethis::use_data(confusable_species, compress = "xz")

temporal_phenology <- read.xlsx(("B:/diverses/HearTheSpecies/Database/Insect_Acoustics/most_Traits_all_species.xlsx")) |>
  dplyr::rename(species = Species_Name,
                genus = Genus,
                season_start = Season_Start,
                season_end = Season_End) #|>
  #dplyr::mutate() |>

library(dplyr)
library(tidyr)

# Define function with time_blocks inside
extract_activity_windows <- function(row) {
  time_blocks <- data.frame(
    col = c("SA-09", "09-12", "12-15", "15-18", "18-SU", "SU-24", "24-SA"),
    start_hour = c(6, 9, 12, 15, 18, 21, 0),
    end_hour = c(9, 12, 15, 18, 21, 24, 6),
    stringsAsFactors = FALSE
  )

  values <- as.numeric(row[time_blocks$col])

  peak_idx <- which(values == 3)
  inter_idx <- which(values %in% c(1, 2))

  peak_start <- if (length(peak_idx) > 0) min(time_blocks$start_hour[peak_idx]) else NA
  peak_end   <- if (length(peak_idx) > 0) max(time_blocks$end_hour[peak_idx]) else NA

  inter_only_idx <- setdiff(inter_idx, peak_idx)
  inter_start <- if (length(inter_only_idx) > 0) min(time_blocks$start_hour[inter_only_idx]) else NA
  inter_end   <- if (length(inter_only_idx) > 0) max(time_blocks$end_hour[inter_only_idx]) else NA

  return(tibble(
    peak_activity_start = peak_start,
    peak_activity_end = peak_end,
    intermediate_activity_start = inter_start,
    intermediate_activity_end = inter_end
  ))
}

# Apply the function row-wise
temporal_species_phenology <- temporal_phenology %>%
  rowwise() %>%
  mutate(
    activity_info = list(extract_activity_windows(cur_data()))
  ) %>%
  unnest_wider(activity_info) %>%
  ungroup() |>
  mutate(intermediate_activity_start =
           if_else(is.na(intermediate_activity_start), peak_activity_start, intermediate_activity_start),
         intermediate_activity_end =
           if_else(is.na(intermediate_activity_end), peak_activity_end, intermediate_activity_end),
         season_start = as.numeric(season_start),
         season_end = as.numeric(season_end)) |>
  select(genus, species, season_start, season_end,
         peak_activity_start, peak_activity_end,
         intermediate_activity_start, intermediate_activity_end)



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
