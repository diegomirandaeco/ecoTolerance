#' Calculate Human Footprint Tolerance Index (HFTI)
#'
#' This function calculates the HFTI for each species occurrence based on
#' the average human footprint within a 1 km buffer around each occurrence.
#'
#' @param occurrences An sf object containing processed occurrences.
#' @param footprint A RasterLayer containing human footprint values.
#' @param divisor A numeric value to divide the footprint by (default = 50).
#' @param buffer_m Numeric. Buffer size in meters for footprint extraction (default = 1000).
#'
#' @return A list with:
#' \describe{
#'   \item{data}{The sf object of occurrences with a new column `HFTI_value`.}
#'   \item{HFTI}{A data frame with the median HFTI per species.}
#' }
#' @export
#' @examples
#' \dontrun{
#' footprint <- load_human_footprint()
#' processed <- process_occurrences(occ_data)
#' hfti_result <- calculate_HFTI(processed, footprint, divisor = 50, buffer_m = 1000)
#' }
calculate_HFTI <- function(occurrences, footprint, divisor = 50, buffer_m = 1000) {
  require(raster)
  require(dplyr)

  # Convert sf to Spatial (raster::extract não lida diretamente com sf)
  occurrences_sp <- as(occurrences, "Spatial")

  # Extrai a MÉDIA do raster num buffer de 1 km (ou outro valor em buffer_m)
  fp_values <- raster::extract(
    x = footprint,            # o raster
    y = occurrences_sp,       # pontos de ocorrência (SpatialPointsDataFrame / SpatialPoints)
    buffer = buffer_m,        # raio do buffer em metros
    fun = mean,               # calcular média
    na.rm = TRUE
  )

  # Aplica a fórmula: HFTI = (média no buffer) / divisor
  HFTI_values <- fp_values / divisor

  # Adiciona aos dados
  occurrences$HFTI_value <- HFTI_values

  # Mediana do HFTI por espécie
  HFTI_df <- occurrences %>%
    st_set_geometry(NULL) %>%
    group_by(species) %>%
    summarise(HFTI = median(HFTI_value, na.rm = TRUE))

  return(list(data = occurrences, HFTI = HFTI_df))
}
