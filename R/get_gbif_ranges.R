#' Build GBIF-based species ranges at ~10 km resolution
#' Retrieves occurrence data from GBIF via \pkg{rgbif} and aggregates them
#' into a gridded species range map at the specified resolution.
#' The result is an `sf` polygon object suitable for spatial plausibility checks.
#'
#' @param species Character vector of species names, or a column from a data.frame.
#' @param res Numeric. Grid resolution in kilometers. Default is 10.
#' @param limit Integer. Maximum number of GBIF records per species. Default is 200.
#' @param region Character. One of `"world"`, `"europe"`, `"asia"`, `"americas"`.
#'   Defines the bounding box used for the GBIF query. Default is `"world"`.
#'
#' @return An `sf` polygon object with a column `species`.
#' @examples
#' \dontrun{
#' library(sf)
#' sp_vec <- c("Pseudochorthippus apicarius", "Barbitistes constrictus")
#'
#' # Retrieve ranges for Europe only
#' ranges_eu <- get_gbif_ranges(sp_vec, res = 10, region = "europe")
#'
#' # Plot result
#' plot(st_geometry(ranges_eu), col = "lightblue", border = "darkblue")
#' }
#'
#' @export
get_gbif_ranges <- function(species,
                                  res = 10,
                                  limit = 200,
                                  region = "world") {
  if (is.data.frame(species)) {
    stop("Please pass a character vector, not a data.frame. Use df$species_col instead.")
  }
  species <- unique(as.character(species))

  # --- Define region bounding boxes (EPSG:4326) ---
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

  for (sp in species) {
    message("Processing species: ", sp, " in region: ", region)

    # 1. Find GBIF taxon key
    key <- tryCatch({
      out <- rgbif::name_backbone(name = sp, rank = "species")
      out$usageKey
    }, error = function(e) NA)
    if (is.na(key)) {
      warning("No GBIF key for ", sp)
      next
    }

    # 2. Get occurrences with coordinates + region filter
    occ <- tryCatch({
      rgbif::occ_search(
        taxonKey = key,
        hasCoordinate = TRUE,
        geometry = wkt_region,
        limit = limit,
        fields = c("decimalLatitude", "decimalLongitude")
      )$data
    }, error = function(e) NULL)
    if (is.null(occ) || nrow(occ) == 0) next

    # 3. Convert to sf in WGS84
    pts <- sf::st_as_sf(occ,
                        coords = c("decimalLongitude", "decimalLatitude"),
                        crs = 4326, remove = FALSE)

    # 4. Reproject to metric CRS
    pts_proj <- sf::st_transform(pts, 6933) # Equal Area projection

    # 5. Create grid at res km
    grid <- sf::st_make_grid(
      pts_proj,
      cellsize = res * 1000,
      what = "polygons",
      square = TRUE
    )
    grid_sf <- sf::st_sf(geometry = grid)

    # 6. Keep only occupied cells
    grid_sel <- grid_sf[lengths(sf::st_intersects(grid_sf, pts_proj)) > 0, ]

    # 7. Back to WGS84
    grid_sel <- sf::st_transform(grid_sel, 4326)
    grid_sel$species <- sp

    ranges_list[[sp]] <- grid_sel
  }

  if (length(ranges_list) == 0) {
    stop("No valid ranges produced.")
  }

  ranges_all <- do.call(rbind, ranges_list)
  return(ranges_all)
}
