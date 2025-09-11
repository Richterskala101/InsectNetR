#' Flag spatio-temporal violations (wide format, fully robust)
#'
#' Checks temporal (season / time-of-day) and spatial (range polygon) plausibility
#' for presence/absence columns in a wide predictions table (like `bin_preds`),
#' and directly flags violations.
#'
#' Violations are coded as:
#' - 1 = problematic / implausible
#' - 0 = plausible / no issue
#'
#' @param predictions data.frame with at least `filename`, `offset`, and species columns (0/1).
#' @param species_info_temporal data.frame with species temporal info:
#'   `genus` + `species` or `species` and `season_start`, `season_end`, `peak_activity_start`, `peak_activity_end`.
#' @param species_ranges sf object with polygons and a column `species`.
#' @param plot_info data.frame with columns `plot_id`, `lat`, `lon`.
#' @param tz time zone for timestamps (default `"UTC"`).
#'
#' @return A data.frame with original columns + violation flags per species:
#'   `<species>_season_violation`, `<species>_time_violation`, `<species>_range_violation`,
#'   `<species>_violation_score`.
#' @export
check_spatio_temporal_plausibility <- function(predictions,
                                               species_info_temporal,
                                               species_ranges,
                                               plot_info,
                                               tz = "UTC") {

  normalize <- function(x) tolower(gsub("[._ ]+", "_", as.character(x)))
  df <- as.data.frame(predictions, stringsAsFactors = FALSE)
  species_cols <- setdiff(colnames(df), c("filename", "offset"))

  # Parse timestamp from filename + offset
  basename_noext <- sub("\\.WAV$|\\.wav$", "", basename(df$filename))
  base_ts <- as.POSIXct(basename_noext, format = "%Y%m%d_%H%M%S", tz = tz)
  start_offset <- suppressWarnings(as.numeric(sub("^(\\d+\\.?\\d*)-.*", "\\1", df$offset)))
  start_offset[is.na(start_offset)] <- 0
  df$timestamp <- base_ts + start_offset
  df$date <- as.Date(lubridate::with_tz(df$timestamp, tzone = tz))
  df$hour <- as.integer(lubridate::hour(lubridate::with_tz(df$timestamp, tzone = tz)))
  df$plot_id <- dirname(df$filename)

  # Merge plot coordinates
  df <- merge(df, plot_info, by = "plot_id", all.x = TRUE, sort = FALSE)

  # If coordinates column is named "long", rename to "lon"
  if (!"lon" %in% names(df) && "long" %in% names(df)) {
    df$lon <- df$long
  }

  # Normalize species names in input tables
  si <- as.data.frame(species_info_temporal, stringsAsFactors = FALSE)
  if (all(c("genus", "species") %in% names(si))) {
    si$species_full <- paste(si$genus, si$species)
  } else if ("species" %in% names(si)) {
    si$species_full <- si$species
  } else {
    stop("species_info_temporal must contain either 'species' or both 'genus' and 'species'.")
  }
  si$species_norm <- normalize(si$species_full)
  si$start_month <- si$season_start
  si$end_month <- si$season_end
  si$start_hour <- si$peak_activity_start
  si$end_hour <- si$peak_activity_end

  species_ranges$species_norm <- normalize(species_ranges$species)

  # Convert to spatial points
  old_s2 <- sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)
  sf_points <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

  for (sp in species_cols) {
    sp_norm <- normalize(sp)

    # --- Skip species not detected anywhere ---
    detected <- df[[sp]] > 0
    if (!any(detected, na.rm = TRUE)) {
      df[[paste0(sp, "_season_violation")]] <- NA_integer_
      df[[paste0(sp, "_time_violation")]]   <- NA_integer_
      df[[paste0(sp, "_range_violation")]]  <- NA_integer_
      df[[paste0(sp, "_violation_score")]]  <- NA_integer_
      next
    }

    # --- season ---
    si_idx <- match(sp_norm, si$species_norm)
    if (length(si_idx) == 0 || is.na(si_idx)) {
      warning("Species '", sp, "' not found in temporal info; marking season/time as violation")
      season_violation <- rep(NA_integer_, nrow(df))
      season_violation[detected] <- 1L
      time_violation <- season_violation
    } else {
      start_d <- si$start_date[si_idx]
      end_d   <- si$end_date[si_idx]
      if (length(start_d) == 0 || length(end_d) == 0 || is.na(start_d) || is.na(end_d)) {
        warning("Species '", sp, "' missing start/end date; marking season as violation")
        season_violation <- rep(NA_integer_, nrow(df))
        season_violation[detected] <- 1L
      } else {
        season_flag <- vapply(df$date, FUN.VALUE = logical(1), FUN = function(d) {
          yr <- lubridate::year(d)
          sd_full <- as.Date(paste0(yr, "-", start_d))
          ed_full <- as.Date(paste0(yr, "-", end_d))
          if (sd_full <= ed_full) {
            d >= sd_full & d <= ed_full
          } else {
            d >= sd_full | d <= ed_full
          }
        })
        season_violation <- rep(NA_integer_, nrow(df))
        season_violation[detected] <- as.integer(!season_flag[detected])
      }

      # --- time ---
      start_h <- si$start_hour[si_idx]
      end_h   <- si$end_hour[si_idx]
      if (length(start_h) == 0 || length(end_h) == 0 || is.na(start_h) || is.na(end_h)) {
        warning("Species '", sp, "' missing start/end hour; marking time as violation")
        time_violation <- rep(NA_integer_, nrow(df))
        time_violation[detected] <- 1L
      } else {
        time_flag <- if (start_h <= end_h) {
          df$hour >= start_h & df$hour <= end_h
        } else {
          df$hour >= start_h | df$hour <= end_h
        }
        time_violation <- rep(NA_integer_, nrow(df))
        time_violation[detected] <- as.integer(!time_flag[detected])
      }
    }

    # --- Range violation ---
    sp_polys <- species_ranges[species_ranges$species_norm == sp_norm, ]
    if (nrow(sp_polys) == 0) {
      warning("Species '", sp, "' not found in species_ranges; marking range as violation")
      range_violation <- rep(NA_integer_, nrow(df))
      range_violation[detected] <- 1L
    } else {
      within_mat <- sf::st_within(sf_points, sp_polys, sparse = FALSE)
      if (is.matrix(within_mat)) {
        range_flag <- rowSums(within_mat, na.rm = TRUE) > 0
      } else {
        range_flag <- as.logical(within_mat)
      }
      range_violation <- rep(NA_integer_, nrow(df))
      range_violation[detected] <- as.integer(!range_flag[detected])
    }

    # --- Assign flags ---
    df[[paste0(sp, "_season_violation")]] <- season_violation
    df[[paste0(sp, "_time_violation")]]   <- time_violation
    df[[paste0(sp, "_range_violation")]]  <- range_violation
    df[[paste0(sp, "_violation_score")]]  <- season_violation + time_violation + range_violation
  }

  rownames(df) <- NULL
  return(df)
}

