#' Exemplary predictions of Orthoptera on audio recordings
#'
#'
#' @format A data frame with 137968 rows and 31 columns:
#' \describe{
#'   \item{filename}{Relative file path to the original WAV audio recording (character).}
#'   \item{offset}{Time segment of the audio file used for prediction, formatted as `"start-end"` in seconds (character).}
#'   \item{Bicolorana.bicolor}{Probability that the segment contains a stridulation of *Bicolorana bicolor* (numeric).}
#'   \item{Chorthippus.albomarginatus}{Probability of *Chorthippus albomarginatus*.}
#'   \item{Chorthippus.apricarius}{Probability of *Chorthippus apricarius*.}
#'   \item{Chorthippus.biguttulus}{Probability of *Chorthippus biguttulus*.}
#'   \item{Chorthippus.brunneus}{Probability of *Chorthippus brunneus*.}
#'   \item{Chorthippus.dorsatus}{Probability of *Chorthippus dorsatus*.}
#'   \item{Chorthippus.mollis}{Probability of *Chorthippus mollis*.}
#'   \item{Chrysochraon.dispar}{Probability of *Chrysochraon dispar*.}
#'   \item{Conocephalus.dorsalis}{Probability of *Conocephalus dorsalis*.}
#'   \item{Conocephalus.fuscus}{Probability of *Conocephalus fuscus*.}
#'   \item{Euthystira.brachyptera}{Probability of *Euthystira brachyptera*.}
#'   \item{Gomphocerippus.rufus}{Probability of *Gomphocerippus rufus*.}
#'   \item{Gryllotalpa.gryllotalpa}{Probability of *Gryllotalpa gryllotalpa*.}
#'   \item{Gryllus.campestris}{Probability of *Gryllus campestris*.}
#'   \item{Isophya.kraussii}{Probability of *Isophya kraussii*.}
#'   \item{Leptophyes.punctatissima}{Probability of *Leptophyes punctatissima*.}
#'   \item{Nemobius.sylvestris}{Probability of *Nemobius sylvestris*.}
#'   \item{Omocestus.viridulus}{Probability of *Omocestus viridulus*.}
#'   \item{Phaneroptera.falcata}{Probability of *Phaneroptera falcata*.}
#'   \item{Pholidoptera.griseoaptera}{Probability of *Pholidoptera griseoaptera*.}
#'   \item{Platycleis.albopunctata}{Probability of *Platycleis albopunctata*.}
#'   \item{Pseudochorthippus.montanus}{Probability of *Pseudochorthippus montanus*.}
#'   \item{Pseudochorthippus.parallelus}{Probability of *Pseudochorthippus parallelus*.}
#'   \item{Roeseliana.roeselii}{Probability of *Roeseliana roeselii*.}
#'   \item{Ruspolia.nitidula}{Probability of *Ruspolia nitidula*.}
#'   \item{Stenobothrus.lineatus}{Probability of *Stenobothrus lineatus*.}
#'   \item{Stethophyma.grossum}{Probability of *Stethophyma grossum*.}
#'   \item{Tettigonia.cantans}{Probability of *Tettigonia cantans*.}
#'   \item{Tettigonia.viridissima}{Probability of *Tettigonia viridissima*.}
#' }
#' @examples
#' # load the data
#' data(predictions)
#' # View the first few rows
#' head(predictions)
#'
"predictions"
#' Confusable Orthoptera Species
#'
#' A dataset listing species of grasshoppers whose stridulations (songs/acoustic signals)
#' are often confused with each other. This is useful for suggesting alternative
#' species when a prediction is incorrect or has low confidence.
#'
#' @format A data frame with 29 rows and 4 variables:
#' \describe{
#'   \item{species}{Character. The focal species.}
#'   \item{confusion_species_1}{Character. The first species commonly confused with `species`. May be `NA`.}
#'   \item{confusion_species_2}{Character. The second species commonly confused with `species`. May be `NA`.}
#'   \item{confusion_species_3}{Character. The third species commonly confused with `species`. May be `NA`.}
#' }
#'
#' @details
#' This dataset can be used to provide suggestions of likely species when a
#' classification model predicts incorrectly or with low confidence. The columns
#' `confusion_species_1`, `confusion_species_2`, and `confusion_species_3` list
#' species that are acoustically similar to the focal species.
#'
#' @examples
#' # load the data
#' data(confusable_species)
#' # View the first few rows
#' head(confusable_species)
#'
#' # Suggest possible confusions for a species
#' subset(confusable_species, species == "Chorthippus biguttulus")
#' @references
#' Ragge, D. R., & Reynolds, W. J. (1998). *The songs of the grasshoppers and crickets of Western Europe*. Colchester: Brill.
#'
#' Roesti, C., & Keist, B. (2009). *Die Stimmen der Heuschrecken*. Bern: Haupt.
#'
"confusable_species"
#' Temporal Phenology and Stridulation Activity of Orthopteran Species
#'
#' A dataset summarizing seasonal and daily activity patterns of 28 Orthopteran species (grasshoppers and bush-crickets) based on literature (e.g. Ragge & Reynolds 1998; Roesti & Keist 2009)
#' It includes both coarse seasonal activity (months) and fine-scale diurnal activity windows (by hour), split into peak and intermediate activity periods.
#'
#' @format A tibble with 28 rows and 8 variables:
#' \describe{
#'   \item{genus}{Genus of the species (character).}
#'   \item{species}{Species name (character).}
#'   \item{season_start}{Start of seasonal activity (month number, numeric).}
#'   \item{season_end}{End of seasonal activity (month number, numeric).}
#'   \item{peak_activity_start}{Start of peak daily stridulation activity (hour of day, numeric).}
#'   \item{peak_activity_end}{End of peak daily stridulation activity (hour of day, numeric).}
#'   \item{intermediate_activity_start}{Start of intermediate daily activity (hour of day, numeric).}
#'   \item{intermediate_activity_end}{End of intermediate daily activity (hour of day, numeric).}
#' }
#'
#' @details
#' Daily activity time blocks were originally categorized into 7 periods and converted to continuous hour ranges assuming a fixed sunrise at 6:00 and sunset at 21:00.
#' Values in `peak_activity_*` correspond to periods with highest stridulation intensity, while `intermediate_activity_*` includes times of moderate activity.
#'
#' @examples
#' data(temporal_species_phenology)
#' head(temporal_species_phenology)
#' @references
#' Ragge, D. R., & Reynolds, W. J. (1998). *The songs of the grasshoppers and crickets of Western Europe*. Colchester: Brill.
#'
#' Roesti, C., & Keist, B. (2009). *Die Stimmen der Heuschrecken*. Bern: Haupt.
"temporal_species_phenology"
#' Species-specific Thresholds (Dummy Data)
#'
#' A dataset containing dummy cutoff thresholds for all species present
#' in the example \code{predictions} dataset. Threshold values are randomly
#' sampled between 0.2 and 0.8 for demonstration purposes.
#'
#' @format A data frame with \eqn{n} rows (one per species) and 2 variables:
#' \describe{
#'   \item{species}{Character. Species names corresponding to the prediction columns.}
#'   \item{cutoff}{Numeric. Dummy threshold values between 0.2 and 0.8.}
#' }
#'
#' @examples
#' data(custom_thresholds)
#' head(custom_thresholds)
"custom_thresholds"
