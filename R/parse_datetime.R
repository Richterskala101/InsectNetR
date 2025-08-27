#' Parse datetime from filenames (YYYYMMDD_HHMMSS or similar)
#' @param filenames A character, ususaly the name of the file, including the date and time of recording
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
