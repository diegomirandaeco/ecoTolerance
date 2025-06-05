# R/calculate_RTI.R

#' Calculate Road Tolerance Index (RTI)
#'
#' Esta função calcula o RTI para cada ocorrência de espécie com base
#' na distância mínima até a estrada mais próxima. A fórmula usada é:
#' \deqn{ RTI = \frac{ref\_dist}{dist + ref\_dist} }
#'
#' Se \code{dist} = 0, então RTI = 1. Conforme \code{dist} aumenta, RTI
#' aproxima-se de 0.
#'
#' @param occurrences Um objeto \code{sf} contendo as ocorrências já
#'   processadas (output de \code{process_occurrences()}).
#' @param roads Um objeto \code{sf} contendo a rede de estradas.
#' @param ref_dist Distância de referência em quilômetros (padrão = 3.5).
#'
#' @return Uma lista com:
#' \describe{
#'   \item{data}{O objeto \code{sf} de ocorrências, porém com uma nova  
#'                coluna `RTI_value` com o índice calculado.}
#'   \item{RTI}{Um data frame com a mediana de RTI por espécie.}
#' }
#' @export
#' @examples
#' \dontrun{
#' roads <- load_roads()
#' # Suponha que 'processed' seja um objeto sf com ocorrências
#' rti_res <- calculate_RTI(processed, roads, ref_dist = 3.5)
#' }
calculate_RTI <- function(occurrences, roads, ref_dist = 3.5) {
  # Não use require(); vamos chamar sf:: e dplyr:: diretamente
  # Calcular distância entre cada ponto e a estrada mais próxima (em metros)
  dist_matrix <- sf::st_distance(occurrences, roads)
  # Converter para quilômetros (as distâncias retornadas vêm em metros)
  min_dist_km <- apply(dist_matrix, 1, min) / 1000

  # Fórmula: RTI = ref_dist / (dist + ref_dist)
  RTI_values <- ref_dist / (min_dist_km + ref_dist)

  # Adicionar nova coluna ao objeto sf
  occurrences$RTI_value <- RTI_values

  # Calcular a mediana de RTI por espécie
  RTI_df <- occurrences %>%
    sf::st_set_geometry(NULL) %>%       # Remover geometria para agrupar
    dplyr::group_by(species) %>%
    dplyr::summarise(RTI = stats::median(RTI_value, na.rm = TRUE))

  return(list(data = occurrences, RTI = RTI_df))
}
