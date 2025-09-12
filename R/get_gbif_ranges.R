#' Build GBIF-based species ranges at selected resolution
#'
#' Retrieves occurrence data from GBIF via \pkg{rgbif} and aggregates them
#' into a gridded species range map at the specified resolution.
#'
#' The result is an `terra` raster stack suitable for spatial plausibility checks.
#' A citation for the used datsets can also be exported. Please cite the data accordingly.
#'
#' @param species Character vector of species names, or a column from a data.frame. Can either be formated like
#' c("Bicolorana bicolor", "Chorthippus albomarginatus") or c("Bicolorana.bicolor","Chorthippus.albomarginatus") to match with model output nomeclature.
#' @param res Numeric. Grid resolution in kilometers. Default is 10.
#' @param limit Integer. Maximum number of GBIF records per species. Default is 200.
#' @param region Character. One of "world", "europe", "asia", "americas".
#'   Defines the bounding box used for the GBIF query. Default is "world".
#' @param collect_citations Logical. If TRUE, dataset citations are retrieved
#'   and attached as an attribute of the returned sf object.
#'
#' @return An `terra` raster stack object with a `species` occurence raster stacked. If `collect_citations=TRUE`,
#'   a `citations` attribute is attached (data.frame with datasetKey, title, citation).
#'
#' @examples
#' \dontrun{
#' sp_vec <- c("Tettigonia viridissima", "Barbitistes serricauda")
#'
#' # Retrieve ranges for Europe only
#' ranges_eu <- get_gbif_ranges(
#'   species = species_vec,
#'   res = 50,
#'   limit = 100,
#'   region = "europe",
#'   collect_citations = TRUE)
#'
#' # Plot result
#' plot(ranges_eu)
#'
#' # plot citations
#'
#' }
#'
#' @export
get_gbif_ranges <- function(species,
                                    res = 10,
                                    limit = 200,
                                    region = "world",
                                    collect_citations = FALSE) {

  region_bboxes <- list(
    world = c(-180, 180, -90, 90),
    europe = c(-25, 45, 34, 72),
    asia = c(25, 180, -10, 80),
    americas = c(-130, -30, -60, 75)
  )

  if (!region %in% names(region_bboxes)) {
    stop("Invalid region. Choose one of: ", paste(names(region_bboxes), collapse = ", "))
  }

  bbox <- region_bboxes[[region]]

  deg_res <- res / 111
  base_raster <- terra::rast(
    res = deg_res,
    extent = terra::ext(bbox[1], bbox[2], bbox[3], bbox[4]),
    crs = "EPSG:4326"
  )
  # After species input processing:

  if (is.data.frame(species)) {
    if (ncol(species) != 1) stop("If species is a data.frame, it must be a single column")
    species <- species[[1]]
  }

  # Replace dots with spaces (e.g. "Bicolorana.bicolor" -> "Bicolorana bicolor")
  species <- gsub("\\.", " ", species)

  species <- unique(species)
  species <- species[!is.na(species)]
  process_one_species <- function(species_name) {
    message("Processing species: ", species_name)

    gbif_data <- tryCatch(
      rgbif::occ_search(
        scientificName = species_name,
        hasCoordinate = TRUE,
        limit = limit,
        decimalLongitude = paste0(bbox[1], ",", bbox[2]),
        decimalLatitude = paste0(bbox[3], ",", bbox[4])
      ),
      error = function(e) {
        warning("Failed to download GBIF data for ", species_name, ": ", e$message)
        return(NULL)
      }
    )

    if (is.null(gbif_data) || nrow(gbif_data$data) == 0) {
      message("No occurrence data found for ", species_name)
      return(NULL)
    }

    if (collect_citations) {
      citations <- tryCatch(
        rgbif::derived_dataset(species_name),
        error = function(e) NULL
      )
    }

    # Remove rows with missing coords
    occ_df <- gbif_data$data[!is.na(gbif_data$data$decimalLongitude) & !is.na(gbif_data$data$decimalLatitude), ]
    if (nrow(occ_df) == 0) {
      message("No valid coordinates for ", species_name)
      return(NULL)
    }

    # Create SpatVector directly from data.frame coords
    occ_vect <- terra::vect(occ_df, geom = c("decimalLongitude", "decimalLatitude"), crs = "EPSG:4326")

    raster_layer <- terra::rasterize(occ_vect, base_raster, field = 1, fun = "sum", background = 0)
    raster_layer[raster_layer > 0] <- 1
    names(raster_layer) <- make.names(species_name)

    if (collect_citations && exists("citations")) {
      attr(raster_layer, "citations") <- citations
    }

    return(raster_layer)
  }

  if (is.data.frame(species)) {
    if (ncol(species) != 1) stop("If species is a data.frame, it must be a single column")
    species <- species[[1]]
  }

  species <- unique(species)
  species <- species[!is.na(species)]

  raster_list <- lapply(species, process_one_species)
  raster_list <- Filter(Negate(is.null), raster_list)

  if (length(raster_list) == 0) {
    stop("No valid raster layers generated. Check species names and GBIF availability.")
  }

  species_stack <- terra::rast(raster_list)

  if (collect_citations) {
    all_citations <- lapply(raster_list, function(x) attr(x, "citations"))
    all_citations <- Filter(Negate(is.null), all_citations)
    if (length(all_citations)) {
      attr(species_stack, "citations") <- all_citations
    }
  }

  return(species_stack)
}
