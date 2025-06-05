# R/data_processing.R

#' Process occurrence data
#'
#' This function processes species occurrence data by optionally removing duplicates and filtering out
#' points that are closer than a specified distance (buffer).
#'
#' @param data A data frame containing at least the columns "species", "longitude", and "latitude".
#' @param remove_duplicates Logical. If TRUE, remove exact duplicates of species + coordinates.
#' @param buffer_km The buffer size in kilometers used to filter out close points.
#'
#' @return An sf object with the processed occurrences.
#' @export
#' @examples
#' # Example data
#' occ_data <- data.frame(
#'   species = c("SpA", "SpA", "SpB", "SpB"),
#'   longitude = c(-50, -50.0005, -51, -51.001),
#'   latitude = c(-10, -10.0005, -11, -11.001)
#' )
#'
#' processed <- process_occurrences(
#'   data = occ_data,
#'   remove_duplicates = TRUE,
#'   buffer_km = 1
#' )
#' processed
process_occurrences <- function(data, remove_duplicates = TRUE, buffer_km = 1) {
  require(dplyr)
  require(sf)

  # Check necessary columns
  if (!all(c("species", "longitude", "latitude") %in% colnames(data))) {
    stop('Data must have "species", "longitude", and "latitude" columns.')
  }

  # Convert to sf
  data_sf <- sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

  # Remove duplicates if requested
  if (remove_duplicates) {
    data_sf <- data_sf %>%
      distinct(species, geometry, .keep_all = TRUE)
  }

  # Filter out points within a given buffer
  if (buffer_km > 0) {
    buffer_m <- buffer_km * 1000

    data_sf <- data_sf %>%
      group_by(species) %>%
      mutate(to_keep = {
        keep_vec <- rep(TRUE, n())
        for (i in seq_len(n())) {
          if (!keep_vec[i]) next
          dist_vals <- as.numeric(sf::st_distance(geometry[i], geometry))
          keep_vec <- keep_vec & (dist_vals > buffer_m | dist_vals == 0)
        }
        keep_vec
      }) %>%
      ungroup() %>%
      dplyr::filter(to_keep) %>%          # use dplyr:: explicitly
      dplyr::select(-to_keep)            # use dplyr:: explicitly
  }

  return(data_sf)
}
