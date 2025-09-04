#' Create Validation Segments from Model Predictions
#'
#' This function reads a CSV file containing machine learning predictions
#' for audio files, selects top segments per species, and exports fixed-length
#' WAV clips into species-specific folders for manual validation.
#'
#' @param input_csv Path to the CSV file with model predictions. The file must
#'   contain at least `offset`, `filename`, and one or more numeric prediction
#'   columns (species scores).
#' @param audio_folder Directory containing the source audio files referenced
#'   in the `filename` column of the CSV.
#' @param output_folder Directory where extracted segments will be written.
#'   Subfolders are created per species.
#' @param segment_length Numeric, length of each segment in seconds.
#' @param n_per_class Integer, maximum number of segments to extract per species.
#' @param output_csv Optional path to a CSV file summarizing the generated
#'   validation segments. If `NULL`, no summary is written. Default: `NULL`.
#'
#' @details
#' The `offset` column in `input_csv` must encode start–end times in seconds
#' using the format `"start - >end"` (e.g. `"12.5-16.5"`).
#' Species columns are detected automatically as numeric columns that are not
#' `offset`, `prediction`, `start`, `end`, or `filename`.
#'
#' Segment filenames are created as: `<original_filename>_<score>.wav`, where
#' `score` is the rounded prediction score.
#'
#' @return
#' Invisibly returns a `data.frame` summarizing the generated segments, with
#' columns:
#' \describe{
#'   \item{file}{The extracted WAV filename}
#'   \item{species}{The predicted species label (from the column name)}
#'   \item{score}{The prediction score used in the filename}
#' }
#'
#' In addition, the function creates directories in `output_folder` (one per
#' species) and writes extracted WAV segments into them. If `output_csv` is
#' provided, it also writes a summary CSV file.
#' @examples
#' \dontrun{
#' create_validation_segments(
#'   input_csv = "results.csv",
#'   audio_folder = "audio/",
#'   output_folder = "segments/",
#'   segment_length = 4,
#'   n_per_class = 30,
#'   output_csv = "segments/validation_segments.csv"
#' )
#' }
#'
#' @export
create_validation_segments <- function(input_csv,
                                       audio_folder,
                                       output_folder,
                                       segment_length = 4,
                                       n_per_class = 30,
                                       output_csv = NULL) {
  preds <- utils::read.csv(input_csv)  |>
    dplyr::mutate(
      start = as.numeric(sub("^(\\d+\\.?\\d*)-.*", "\\1", offset)),
      end   = as.numeric(sub(".*-(\\d+\\.?\\d*)$", "\\1", offset))
    ) |>
    dplyr::filter(!is.na(start), !is.na(end))

  skip_cols <- c("offset", "prediction", "start", "end", "filename")
  species_cols <- preds |>
    dplyr::select(-dplyr::any_of(skip_cols)) |>
    dplyr::select_if(is.numeric) |>
    names()

  fs::dir_create(output_folder)

  for (sp in species_cols) {
    message("Processing species: ", sp)

    top_rows <- preds |>
      dplyr::filter(.data[[sp]] > 0) |>
      dplyr::arrange(dplyr::desc(.data[[sp]])) |>
      dplyr::slice_head(n = n_per_class)

    if (nrow(top_rows) == 0) {
      message("  -> no segments with positive score; skipping.")
      next
    }

    sp_dir <- file.path(output_folder, sp)
    fs::dir_create(sp_dir)

    for (i in seq_len(nrow(top_rows))) {
      row <- top_rows[i, ]

      wav_path <- file.path(audio_folder, row$filename)
      if (!file.exists(wav_path)) {
        warning("Missing file: ", wav_path); next
      }
      wav <- tuneR::readWave(wav_path)
      sr  <- wav@samp.rate

      from_samp <- as.integer(row$start * sr)
      to_samp   <- from_samp + as.integer(segment_length * sr)
      if (to_samp > length(wav@left)) {
        warning("Segment exceeds bounds for ", row$filename); next
      }

      seg <- tuneR::extractWave(wav, from = from_samp, to = to_samp, xunit = "samples")

      base  <- tools::file_path_sans_ext(basename(row$filename))
      score <- round(row[[sp]], 2)
      out   <- sprintf("%s_%.2f.wav", base, score)

      tuneR::writeWave(seg, file.path(sp_dir, out))
    }
  }

  filelist <- list.files(output_folder, recursive = TRUE)
  summary_df <- data.frame(filelist) |>
    dplyr::mutate(
      file    = basename(filelist),
      species = stringr::str_extract(filelist, "^[^/]+"),
      score   = stringr::str_extract(filelist, "[0-9]+\\.[0-9]+(?=\\.wav$)")
    )

  if (!is.null(output_csv)) {
    utils::write.csv(summary_df, output_csv, row.names = FALSE, quote = FALSE)
  }

  invisible(summary_df)
}
