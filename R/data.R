#' Exemplary predictions of Orthoptera on audio recordings
#'
#'
#' @format A data frame with 137968 rows and 34 columns:
#' \describe{
#'   \item{country}{Country name}
#'   \item{iso2, iso3}{2 & 3 letter ISO country codes}
#'   \item{year}{Year}
#'   ...
#' }
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
#' # View the first few rows
#' head(confusable_species)
#'
#' # Suggest possible confusions for a species
#' subset(confusable_species, species == "Chorthippus biguttulus")
#' @references
#' Ragge, D. R., & Reynolds, W. J. (2023). *The songs of the grasshoppers and crickets of Western Europe*. Brill.
#'
#' Roesti, C., & Keist, B. (2009). *Die Stimmen der Heuschrecken*. Bern: Haupt.
#'
"confusable_species"
