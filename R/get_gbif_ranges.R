#' Build GBIF-based species ranges at ~10 km resolution
#'#' Build GBIF-based species ranges at ~10 km resolution
#' Retrieves occurrence data from GBIF via \pkg{rgbif} and aggregates them
#' into a gridded species range map at the specified resolution.
#'
#' The result is an `sf` polygon object suitable for spatial plausibility checks.
#' A citation for the used datsets can also be exported. Please cite the data accordingly.
#'
#' @param species Character vector of species names, or a column from a data.frame.
#' @param res Numeric. Grid resolution in kilometers. Default is 10.
#' @param limit Integer. Maximum number of GBIF records per species. Default is 200.
#' @param region Character. One of "world", "europe", "asia", "americas".
#'   Defines the bounding box used for the GBIF query. Default is "world".
#' @param collect_citations Logical. If TRUE, dataset citations are retrieved
#'   and attached as an attribute of the returned sf object.
#'
#' @return An `sf` polygon object with a column `species`. If `collect_citations=TRUE`,
#'   a `citations` attribute is attached (data.frame with datasetKey, title, citation).
#'
#' @examples
#' \dontrun{
#' library(sf)
#' sp_vec <- c("Tettigonia viridissima", "Barbitistes serricauda")
#'
#' # Retrieve ranges for Europe only
#' ranges_eu <- get_gbif_ranges(sp_vec, res = 10, region = "europe", return_citation = TRUE)
#'
#' # Plot result
#' plot(st_geometry(ranges_eu), col = "lightblue", border = "darkblue")
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
  if (is.data.frame(species)) {
    stop("Please pass a character vector, not a data.frame. Use df$species_col instead.")
  }
  species <- unique(as.character(species))

  # --- Region bounding boxes ---
  region_bbox <- list(
    world    = sf::st_bbox(c(xmin = -180, ymin = -90, xmax = 180, ymax = 90), crs = 4326),
    europe   = sf::st_bbox(c(xmin = -31, ymin = 34, xmax = 40, ymax = 72), crs = 4326),
    asia     = sf::st_bbox(c(xmin = 26, ymin = -10, xmax = 180, ymax = 81), crs = 4326),
    americas = sf::st_bbox(c(xmin = -170, ymin = -60, xmax = -25, ymax = 83), crs = 4326)
  )
  if (!region %in% names(region_bbox)) {
    stop("Unknown region. Use one of: ", paste(names(region_bbox), collapse = ", "))
  }
  wkt_region <- sf::st_as_text(sf::st_as_sfc(region_bbox[[region]]))

  ranges_list <- list()
  dataset_keys <- character()

  for (sp in species) {
    message("Processing species: ", sp, " in region: ", region)

    # 1. Taxon key
    key <- tryCatch({
      out <- rgbif::name_backbone(name = sp, rank = "species")
      out$usageKey
    }, error = function(e) NA)
    if (is.na(key)) {
      warning("No GBIF key for ", sp)
      next
    }

    # 2. Occurrences
    occ <- tryCatch({
      rgbif::occ_search(
        taxonKey = key,
        hasCoordinate = TRUE,
        geometry = wkt_region,
        limit = limit,
        fields = c("decimalLatitude", "decimalLongitude", "datasetKey")
      )$data
    }, error = function(e) NULL)

    if (is.null(occ) || nrow(occ) == 0) next

    # 🧼 Clean missing coordinates
    occ <- occ[!is.na(occ$decimalLatitude) & !is.na(occ$decimalLongitude), ]
    if (nrow(occ) == 0) next  # skip if nothing left after cleaning
    dataset_keys <- c(dataset_keys, unique(occ$datasetKey))

    # 3. sf conversion
    # 3. sf conversion
    occ <- occ[!is.na(occ$decimalLatitude) & !is.na(occ$decimalLongitude), ]
    if (nrow(occ) == 0) {
      warning("All coordinates missing for ", sp)
      next
    }

    pts <- tryCatch({
      sf::st_as_sf(occ,
                   coords = c("decimalLongitude", "decimalLatitude"),
                   crs = 4326, remove = FALSE)
    }, error = function(e) {
      warning("Failed to convert to sf for ", sp)
      return(NULL)
    })
    if (is.null(pts)) next

    pts_proj <- tryCatch({
      sf::st_transform(pts, 6933) # Equal Area projection
    }, error = function(e) {
      warning("Projection failed for ", sp)
      return(NULL)
    })
    if (is.null(pts_proj) || nrow(pts_proj) == 0 || all(sf::st_is_empty(pts_proj))) {
      warning("No valid projected points for ", sp)
      next
    }

    grid <- tryCatch({
      sf::st_make_grid(pts_proj, cellsize = res * 1000, square = TRUE)
    }, error = function(e) {
      warning("Grid creation failed for ", sp)
      return(NULL)
    })
    if (is.null(grid)) next

    grid_sf <- sf::st_sf(geometry = grid)
    grid_sel <- grid_sf[lengths(sf::st_intersects(grid_sf, pts_proj)) > 0, ]

    grid_sel <- sf::st_transform(grid_sel, 4326)
    grid_sel$species <- sp

    ranges_list[[sp]] <- grid_sel
  }

  if (length(ranges_list) == 0) {
    stop("No valid ranges produced.")
  }

  ranges_all <- do.call(rbind, ranges_list)

  # --- Collect citations if requested ---
  if (collect_citations && length(dataset_keys) > 0) {
    dataset_keys <- unique(dataset_keys)
    citations <- lapply(dataset_keys, function(k) {
      cit <- tryCatch(rgbif::dataset_get(k), error = function(e) NULL)
      if (is.null(cit)) return(NULL)
      data.frame(datasetKey = k,
                 title = cit$title %||% NA_character_,
                 citation = cit$citation$text %||% NA_character_,
                 stringsAsFactors = FALSE)
    })
    citations <- do.call(rbind, citations)
    attr(ranges_all, "citations") <- citations
  }

  return(ranges_all)
}
