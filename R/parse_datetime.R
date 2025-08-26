#' Parse datetime from filename (YYYYMMDD_HHMMSS or similar)
#' @param filename A character, ususaly the name of the file, including the date and time of recording
#' @param tz time zone (default "UTC")
#' @return a POSIXct object/string
#' @export
#'
#' @examples
#' # example code
#' predictions <- filename = c("path/to/20240719_112000.WAV", "another/path/20240722_124000.WAV")
#' datetimes <- parse_datetime(predictions)
#' print(datetimes)
#'

parse_datetime <- function(filenames) {
  lubridate::ymd_hms(basename(filenames))
}
