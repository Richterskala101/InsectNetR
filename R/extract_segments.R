#' Extract Top-Scoring Audio Segments per Class
#'
#' This function reads a model output CSV, selects the top-scoring segments for
#' each species/class, extracts fixed-length audio snippets from WAV files, and
#' saves them into class-specific folders.
#'
#' @param input_csv Path to the model output CSV file.
#' @param audio_folder Directory containing full-length audio files referenced
#'   in the model output.
#' @param output_folder Directory where extracted segments will be saved.
#' @param segment_length Length of each extracted segment in seconds.
#' @param n_per_class Maximum number of segments to extract per species/class.
#'
#' @return A data.frame summarizing all extracted segments. Also writes WAV
#'   files into \code{output_folder} and a CSV summary
#'   \code{validation_segments.csv}.
#'
#' @export
extract_segments <- function(
    input_csv,
    audio_folder,
    output_folder,
    segment_length = 4,
    n_per_class = 30
) {

  # ---- Validate input paths ----
  if (!file.exists(input_csv))
    stop("input_csv does not exist: ", input_csv)

  if (!dir.exists(audio_folder))
    stop("audio_folder does not exist: ", audio_folder)

  fs::dir_create(output_folder)

  message("Reading prediction table: ", input_csv)

  # ---- Read + preprocess data ----
  preds <- utils::read.csv(input_csv, stringsAsFactors = FALSE) |>
    dplyr::mutate(
      start = as.numeric(sub("^(\\d+\\.?\\d*)-.*", "\\1", offset)),
      end   = as.numeric(sub(".*-(\\d+\\.?\\d*)$", "\\1", offset))
    ) |>
    dplyr::filter(!is.na(start), !is.na(end))

  # Identify species score columns (numeric predictors)
  skip_cols <- c("offset", "prediction", "start", "end", "filename")

  species_cols <- preds |>
    dplyr::select(-dplyr::all_of(skip_cols)) |>
    dplyr::select(where(is.numeric)) |>
    names()

  if (length(species_cols) == 0)
    stop("No numeric species score columns found in predictions.")

  # ---- Loop over species/classes ----
  for (sp in species_cols) {
    message("\nProcessing species: ", sp)

    top_rows <- preds |>
      dplyr::filter(.data[[sp]] > 0) |>
      dplyr::arrange(dplyr::desc(.data[[sp]])) |>
      dplyr::slice_head(n = n_per_class)

    if (nrow(top_rows) == 0) {
      message("  → No positive scores; skipping.")
      next
    }

    sp_dir <- file.path(output_folder, sp)
    fs::dir_create(sp_dir)

    # ---- Extract each segment ----
    for (i in seq_len(nrow(top_rows))) {
      row <- top_rows[i, ]

      wav_path <- file.path(audio_folder, row$filename)
      if (!file.exists(wav_path)) {
        warning("Missing file: ", wav_path)
        next
      }

      wav <- tuneR::readWave(wav_path)
      sr  <- wav@samp.rate

      # Compute sample indices
      from_samp <- as.integer(row$start * sr)
      to_samp   <- from_samp + as.integer(segment_length * sr)

      if (to_samp > length(wav@left)) {
        warning("Segment exceeds bounds for ", row$filename)
        next
      }

      seg <- tuneR::extractWave(
        wav,
        from = from_samp,
        to   = to_samp,
        xunit = "samples"
      )

      # Build filename: <original>_<score>.wav
      base  <- tools::file_path_sans_ext(basename(row$filename))
      score <- round(row[[sp]], 2)
      out   <- sprintf("%s_%.2f.wav", base, score)

      tuneR::writeWave(seg, file.path(sp_dir, out))
    }
  }

  # ---- Build summary table ----
  filelist <- list.files(output_folder, recursive = TRUE, full.names = TRUE)

  out_df <- data.frame(
    file_path = filelist,
    file      = basename(filelist),
    species   = stringr::str_extract(filelist, "(?<=/)[^/]+(?=/)"),
    score     = stringr::str_extract(basename(filelist), "[0-9]+\\.[0-9]+(?=\\.wav$)"),
    stringsAsFactors = FALSE
  )

  summary_csv <- file.path(output_folder, "validation_segments.csv")

  utils::write.csv(out_df, summary_csv, row.names = FALSE, quote = FALSE)

  message("Extraction complete.")
  message("Summary written to: ", summary_csv)

  return(out_df)
}
