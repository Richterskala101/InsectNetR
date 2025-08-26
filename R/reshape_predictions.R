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
