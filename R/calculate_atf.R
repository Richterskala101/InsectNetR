#' Calculate Aggregated Time-series Features (ATF) up to 12 hours
#'
#' Vectorized and data.table-based reimplementation of Singer et al. style ATF.
#' Uses interval joins (foverlaps) to compute stats in windows around each
#' validated detection, limiting predictor windows to a maximum span.
#'
#' @param birdnet data.frame/data.table with at least: Plot_ID, german, timestamp (POSIXct), conf
#' @param species character vector of target species (german names)
#' @param validated data.frame with columns: detID, german, Plot_ID, timestamp, conf (or will be merged in)
#' @param windows_sec integer vector of window sizes in seconds (default: c(9,15,21,27, 1200,2400,3600,4800,7200,14400,28800,43200))
#' @param max_hours numeric maximum predictor span in hours (default 12). Any window > max_hours*3600 is dropped.
#' @return data.table of ATF (one row per detection)
#'
#' @references
#' Singer, D., Hagge, J., Kamp, J., Hondong, H. & Schuldt, A. (2024) Aggregated time-series features boost species-specific differentiation of true and false positives in passive acoustic monitoring of bird assemblages, Remote Sensing in Ecology and Conservation, https://doi.org/10.1002/rse2.385
#'
#' @export
calculate_atf <- function(birdnet, species, validated,
                          windows_sec = c(9,15,21,27, 1200,2400,3600,4800, 7200,14400,28800,43200),
                          max_hours = 12) {
  requireNamespace("data.table")
  requireNamespace("stats")
  bn <- data.table::as.data.table(birdnet)
  val <- data.table::as.data.table(validated)
  bn <- bn[german %in% species]
  val <- val[german %in% species]

  # keep only windows within max_hours
  windows_sec <- sort(unique(as.integer(windows_sec)))
  windows_sec <- windows_sec[windows_sec <= max_hours * 3600]

  # ensure keys and interval columns for foverlaps
  # represent points as zero-length intervals for foverlaps
  bn[, `:=`(start = timestamp, end = timestamp)]
  data.table::setkey(bn, Plot_ID, german, start, end)

  # if detID missing, compute a robust ID at second resolution
  if (!"detID" %in% names(val)) {
    val[, detID := paste0(Plot_ID, "_", format(timestamp, "%Y%m%d_%H%M%S"))]
  }

  # reduce to validated detections per plot/species
  val <- val[Plot_ID %in% bn$Plot_ID]

  # build interval table
  ints <- .build_intervals(val[, .(Plot_ID, german, detID, timestamp, conf)], intervals = windows_sec)

  # interval overlap join
  ov <- data.table::foverlaps(bn, ints, by.x = c("Plot_ID","german","start","end"),
                              by.y = c("Plot_ID","german","from","to"), type = "within", nomatch = 0L)

  # aggregate per detID x window
  sum_by <- ov[, .(
    n = .N,
    avgconf = mean(conf, na.rm = TRUE),
    medconf = stats::median(conf, na.rm = TRUE),
    maxconf = max(conf, na.rm = TRUE),
    minconf = min(conf, na.rm = TRUE),
    ndets10 = sum(conf >= 0.10, na.rm = TRUE),
    ndets20 = sum(conf >= 0.20, na.rm = TRUE),
    ndets30 = sum(conf >= 0.30, na.rm = TRUE),
    ndets40 = sum(conf >= 0.40, na.rm = TRUE),
    ndets50 = sum(conf >= 0.50, na.rm = TRUE),
    ndets60 = sum(conf >= 0.60, na.rm = TRUE),
    ndets70 = sum(conf >= 0.70, na.rm = TRUE),
    ndets80 = sum(conf >= 0.80, na.rm = TRUE),
    ndets90 = sum(conf >= 0.90, na.rm = TRUE),
    ndets99 = sum(conf >= 0.99, na.rm = TRUE)
  ), by = .(detID, int_label)]

  # cast to wide
  wide <- data.table::dcast(sum_by, detID ~ int_label, value.var = c("n","avgconf","medconf","maxconf","minconf",
                                                                     "ndets10","ndets20","ndets30","ndets40","ndets50",
                                                                     "ndets60","ndets70","ndets80","ndets90","ndets99"))

  # merge back core columns
  out <- merge(val[, .(detID, Plot_ID, german, timestamp, conf)], wide, by = "detID", all.x = TRUE)

  # replace NA (no neighbors) with zeros for counts and with own-conf for min/max/median/mean fallbacks
  count_cols <- grep("^n_|^ndets", names(out), value = TRUE)
  stat_cols  <- grep("^(avgconf|medconf|maxconf|minconf)_", names(out), value = TRUE)
  if (length(count_cols)) out[, (count_cols) := lapply(.SD, function(z) ifelse(is.na(z), 0, z)), .SDcols = count_cols]
  if (length(stat_cols)) out[, (stat_cols) := lapply(.SD, function(z) ifelse(is.na(z), conf, z)), .SDcols = stat_cols]

  data.table::setorder(out, german, Plot_ID, timestamp)
  out
}
