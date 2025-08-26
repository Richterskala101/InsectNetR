parse_datetime <- function(df, filename_col = "filename") {
  lubridate::ymd_hms(basename(df[[filename_col]]))
}
