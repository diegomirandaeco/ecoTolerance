# R/compute_indices.R

#' Compute both RTI and HFTI
#'
#' This main function processes occurrence data (removing duplicates, filtering close points),
#' then calculates the Highway Tolerance Index (RTI) and the Human Footprint Tolerance Index (HFTI).
#'
#' @param data A data frame with columns "species", "longitude", and "latitude".
#' @param remove_duplicates Logical. If TRUE, remove exact duplicates of species + coordinates.
#' @param buffer_km Numeric. Buffer size in kilometers for removing nearby points.
#' @param ref_dist Numeric. Reference distance in kilometers for RTI.
#' @param divisor Numeric. Divisor for the human footprint values in HFTI.
#'
#' @return A list containing:
#' \describe{
#'   \item{RTI}{A data frame with median RTI values per species.}
#'   \item{HFTI}{A data frame with median HFTI values per species.}
#'   \item{indices}{A data frame combining both RTI and HFTI by species.}
#'   \item{processed_data}{The sf object of occurrences with RTI and HFTI values.}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Example usage
#' library(sf)
#' sample_data <- data.frame(
#'   species = c("SpA", "SpA", "SpB", "SpB"),
#'   longitude = c(-50, -50.0005, -51, -51.001),
#'   latitude = c(-10, -10.0005, -11, -11.001)
#' )
#'
#' result <- compute_indices(
#'   data = sample_data,
#'   remove_duplicates = TRUE,
#'   buffer_km = 1,
#'   ref_dist = 3.5,
#'   divisor = 50
#' )
#'
#' print(result$RTI)
#' print(result$HFTI)
#' print(result$indices)
#' }
compute_indices <- function(data,
                            remove_duplicates = TRUE,
                            buffer_km = 1,
                            ref_dist = 3.5,
                            divisor = 50) {

  # 1. Process occurrences
  processed_data <- process_occurrences(
    data,
    remove_duplicates = remove_duplicates,
    buffer_km = buffer_km
  )

  # 2. Load roads and footprint
  roads <- load_roads()
  footprint <- load_human_footprint()

  # 3. Calculate RTI
  rti_res <- calculate_RTI(processed_data, roads, ref_dist = ref_dist)

  # 4. Calculate HFTI
  hfti_res <- calculate_HFTI(
    rti_res$data,
    footprint,
    divisor = divisor,
    buffer_m = 1000  # define 1 km
  )


  # 5. Merge RTI and HFTI medians by species
  combined_indices <- dplyr::left_join(rti_res$RTI, hfti_res$HFTI, by = "species")

  return(list(
    RTI = rti_res$RTI,
    HFTI = hfti_res$HFTI,
    indices = combined_indices,
    processed_data = hfti_res$data
  ))
}
