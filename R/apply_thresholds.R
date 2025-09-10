#' Apply Confidence Score Thresholds to Predictions
#'
#' This function thresholds model predictions using either a uniform cutoff or
#' species-specific thresholds. Predictions greater than or equal to the chosen
#' threshold become `1`, and values below become `0`.
#'
#' @param predictions A `data.frame` containing model predictions, where the first
#'   two columns are metadata (e.g., filename, offset) and all subsequent columns
#'   are species confidence scores.
#' @param threshold Either a single numeric value (uniform threshold applied to all
#'   species) or a `data.frame` with two columns: `species` (character) and
#'   `cutoff` (numeric), defining species-specific thresholds.
#' @param metadata_cols Integer. The number of leading metadata columns in
#'   `predictions` that should be preserved (default: `2`).
#'
#' @return A `data.frame` of the same shape as `predictions`, with confidence
#'   scores replaced by `0` or `1` according to the thresholding rules.
#' @examples
#' \dontrun{
#' # get data to try
#' data(predictions)
#'
#' # Uniform threshold
#' bin_preds <- apply_thresholds(predictions, threshold = 0.5)
#' head(bin_preds)
#'
#' # load example species-specific thresholds
#' data(custom_thresholds)
#' bin_preds <- apply_thresholds(predictions, threshold = thresholds)
#' head(bin_preds)
#' }
#' @export
apply_thresholds <- function(predictions, threshold, metadata_cols = 2) {

  # Extract species names from predictions
  species_cols <- colnames(predictions)[(metadata_cols + 1):ncol(predictions)]

  # If uniform threshold
  if (is.numeric(threshold) && length(threshold) == 1) {
    preds_bin <- predictions
    preds_bin[, species_cols] <- lapply(preds_bin[, species_cols, drop = FALSE],
                                        function(x) as.integer(x >= threshold))
    return(preds_bin)
  }

  # If species-specific thresholds
  if (is.data.frame(threshold) &&
      all(c("species", "cutoff") %in% colnames(threshold))) {

    # Normalize species names for matching
    normalize_name <- function(x) {
      gsub("[[:punct:]_ ]+", ".", x) |> tolower()
    }

    species_pred_norm <- normalize_name(species_cols)
    species_thr_norm <- normalize_name(threshold$species)

    # Initialize output
    preds_bin <- predictions

    for (i in seq_along(species_cols)) {
      sp_col <- species_cols[i]
      sp_norm <- species_pred_norm[i]

      # Match threshold
      idx <- match(sp_norm, species_thr_norm)
      if (!is.na(idx)) {
        thr_val <- threshold$cutoff[idx]
      } else {
        stop(paste("No threshold found for species:", sp_col))
      }

      preds_bin[[sp_col]] <- as.integer(preds_bin[[sp_col]] >= thr_val)
    }

    return(preds_bin)
  }

  stop("`threshold` must be either a numeric value or a data.frame with 'species' and 'cutoff' columns.")
}
