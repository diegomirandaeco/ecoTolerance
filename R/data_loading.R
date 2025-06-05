# data_loading.R

#' Load Brazilian Roads Shapefile
#'
#' This function loads the roads shapefile stored in the package's inst/extdata directory.
#'
#' @return An sf object containing roads data.
#' @export
load_roads <- function() {
  roads_path <- system.file("extdata", "shapefiles", "Rodovias_Brasil.shp",
                            package = "ecoTolerance")
  roads <- sf::st_read(roads_path, quiet = TRUE)
  # Optionally transform the CRS
  roads <- sf::st_transform(roads, crs = 4326)
  return(roads)
}

#' Load Human Footprint GeoTIFF
#'
#' This function loads the human footprint GeoTIFF stored in the package's inst/extdata directory.
#'
#' @return A RasterLayer object containing human footprint data.
#' @export
load_human_footprint <- function() {
  footprint_path <- system.file("extdata", "geotiffs", "human_footprint_brazil.tif",
                                package = "ecoTolerance")
  footprint <- raster::raster(footprint_path)
  return(footprint)
}
