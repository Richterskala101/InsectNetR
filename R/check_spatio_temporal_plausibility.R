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
                                               species_ranges,  # terra::SpatRaster only
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

  # --- Merge plot coordinates (robust, preserves order) ---
  plot_info_df <- as.data.frame(plot_info, stringsAsFactors = FALSE)
  if (!"lon" %in% names(plot_info_df) && "long" %in% names(plot_info_df)) {
    plot_info_df$lon <- plot_info_df$long
  }

  if (!"plot_id" %in% names(plot_info_df)) {
    stop("plot_info must contain a 'plot_id' column")
  }

  # If lat/lon are missing in plot_info, fill with NA but continue
  if (!"lat" %in% names(plot_info_df)) plot_info_df$lat <- NA_real_
  if (!"lon" %in% names(plot_info_df)) plot_info_df$lon <- NA_real_

  idx <- match(df$plot_id, plot_info_df$plot_id)
  df$lat <- plot_info_df$lat[idx]
  df$lon <- plot_info_df$lon[idx]

  # Normalize species names in species_info_temporal
  si <- as.data.frame(species_info_temporal, stringsAsFactors = FALSE)
  if ("species" %in% names(si)) {
    if (any(grepl("\\.", si$species))) {
      si$species_full <- si$species
    } else if ("genus" %in% names(si)) {
      si$species_full <- paste(si$genus, si$species)
    } else {
      stop("species_info_temporal must contain full species names or genus + species.")
    }
  } else if (all(c("genus", "species") %in% names(si))) {
    si$species_full <- paste(si$genus, si$species)
  } else {
    stop("species_info_temporal must contain either 'species' or both 'genus' and 'species'.")
  }
  si$species_norm <- normalize(si$species_full)
  si$start_month <- si$season_start
  si$end_month <- si$season_end
  si$start_hour <- si$peak_activity_start
  si$end_hour <- si$peak_activity_end

  # Normalize species names in raster layers
  raster_species_norm <- normalize(names(species_ranges))

  # Prepare terra points from df coords
  # (terra::vect tolerates NA coordinates; extract will return NA values)
  pts_vect <- terra::vect(df[, c("lon", "lat")], geom = c("lon", "lat"), crs = "EPSG:4326")

  for (sp in species_cols) {
    sp_norm <- normalize(sp)

    # Skip species not detected anywhere
    detected <- !is.na(df[[sp]]) & df[[sp]] > 0
    if (!any(detected, na.rm = TRUE)) {
      df[[paste0(sp, "_season_violation")]] <- NA_integer_
      df[[paste0(sp, "_time_violation")]]   <- NA_integer_
      df[[paste0(sp, "_range_violation")]]  <- NA_integer_
      df[[paste0(sp, "_violation_score")]]  <- NA_integer_
      next
    }

    # --- season and time violations ---
    si_idx <- match(sp_norm, si$species_norm)
    if (is.na(si_idx)) {
      warning("Species '", sp, "' not found in temporal info; marking season/time as violation")
      season_violation <- rep(NA_integer_, nrow(df))
      season_violation[detected] <- 1L
      time_violation <- season_violation
    } else {
      # Season check
      start_m <- si$start_month[si_idx]
      end_m <- si$end_month[si_idx]
      if (is.na(start_m) || is.na(end_m)) {
        warning("Species '", sp, "' missing start/end month; marking season as violation")
        season_violation <- rep(NA_integer_, nrow(df))
        season_violation[detected] <- 1L
      } else {
        season_flag <- vapply(df$date, FUN.VALUE = logical(1), FUN = function(d) {
          yr <- lubridate::year(d)
          sd_full <- as.Date(paste0(yr, "-", sprintf("%02d", start_m), "-01"))
          ed_full <- as.Date(paste0(yr, "-", sprintf("%02d", end_m), "-01")) +
            lubridate::days(lubridate::days_in_month(as.Date(paste0(yr, "-", sprintf("%02d", end_m), "-01")))) - 1
          if (sd_full <= ed_full) {
            d >= sd_full & d <= ed_full
          } else {
            d >= sd_full | d <= ed_full
          }
        })
        season_violation <- rep(NA_integer_, nrow(df))
        season_violation[detected] <- as.integer(!season_flag[detected])
      }

      # Time check
      start_h <- si$start_hour[si_idx]
      end_h <- si$end_hour[si_idx]
      if (is.na(start_h) || is.na(end_h)) {
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

    # --- Range violation using raster (robust) ---
    layer_idx <- match(sp_norm, raster_species_norm)
    if (is.na(layer_idx)) {
      warning("Species '", sp, "' not found in species_ranges raster layers; marking range as violation")
      range_violation <- rep(NA_integer_, nrow(df))
      range_violation[detected] <- 1L
    } else {
      ex <- terra::extract(species_ranges[[layer_idx]], pts_vect)
      # 'ex' is typically a data.frame with ID (col1) and values (col2)
      if (is.data.frame(ex) || is.matrix(ex)) {
        vals <- ex[, ncol(ex)]
      } else {
        vals <- as.vector(ex)
      }

      # compute violation only for detected presences; leave others as NA
      range_violation <- ifelse(!is.na(df[[sp]]) & df[[sp]] > 0,
                                as.integer(is.na(vals) | vals != 1),
                                NA_integer_)
    }

    # --- Assign flags and score ---
    df[[paste0(sp, "_season_violation")]] <- season_violation
    df[[paste0(sp, "_time_violation")]]   <- time_violation
    df[[paste0(sp, "_range_violation")]]  <- range_violation

    # Violation score: sum of available flags; if all three NA -> NA
    any_flag_present <- !(is.na(season_violation) & is.na(time_violation) & is.na(range_violation))
    score_vals <- (ifelse(is.na(season_violation), 0L, season_violation) +
                     ifelse(is.na(time_violation), 0L, time_violation) +
                     ifelse(is.na(range_violation), 0L, range_violation))
    df[[paste0(sp, "_violation_score")]] <- ifelse(any_flag_present, score_vals, NA_integer_)
  }

  rownames(df) <- NULL
  return(df)
}
