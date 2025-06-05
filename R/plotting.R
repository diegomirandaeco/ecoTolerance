# R/plotting.R

#' Generate and save maps & density plots for RTI and HFTI of all species
#'
#' Esta função recebe o resultado de \code{compute_indices()}, um shapefile
#' de área de estudo e o diretório de saída. Para cada espécie presente em
#' \code{result$processed_data}, ela gera:
#'   1) Mapa de RTI (points coloridos por \code{RTI_value})
#'   2) Mapa de HFTI (points coloridos por \code{HFTI_value})
#'   3) Gráfico de densidade de RTI (com linha vertical na mediana)
#'   4) Gráfico de densidade de HFTI (com linha vertical na mediana)
#' Além disso, salva um arquivo CSV com RTI e HFTI médios por espécie.
#'
#' @param result Lista retornada por \code{compute_indices()}, contendo:
#'   \itemize{
#'     \item{\code{result$processed_data}}{ Um objeto \code{sf} com as
#'       ocorrências e colunas \code{RTI_value} e \code{HFTI_value}. }
#'     \item{\code{result$indices}}{ Data frame com RTI e HFTI médios por espécie. }
#'   }
#' @param area_shapefile Caminho (string) para o shapefile da área de
#'   estudo (por exemplo, limite do Brasil). Deve ser um arquivo `.shp`.
#' @param out_dir Caminho (string) para o diretório onde as imagens e o CSV
#'   serão salvos. Caso não exista, será criado automaticamente.
#' @param map_crs CRS desejado para os mapas (padrão = 4326, WGS84).
#' @param point_size Tamanho dos pontos nos mapas (padrão = 1.5).
#' @param density_fill Cor de preenchimento dos gráficos de densidade
#'   (padrão = "steelblue").
#' @param density_alpha Transparência do preenchimento dos gráficos de
#'   densidade (padrão = 0.35).
#'
#' @return Invisível. A função retorna NULL após salvar todos os arquivos
#'   em \code{out_dir}.
#' @export
#' @importFrom ggplot2 ggplot geom_sf aes scale_color_distiller theme_minimal
#' @importFrom ggplot2 ggtitle labs theme element_text element_line geom_density
#' @importFrom ggplot2 geom_vline scale_color_viridis_c
#' @importFrom ggplot2 ggsave
#' @importFrom sf st_read st_transform st_drop_geometry
#' @importFrom dplyr filter
#' @importFrom utils write.csv
generate_all_reports <- function(result,
                                 area_shapefile,
                                 out_dir,
                                 map_crs = 4326,
                                 point_size = 1.5,
                                 density_fill = "steelblue",
                                 density_alpha = 0.35) {
  # 1. Verificar se 'result' tem os elementos essenciais
  if (!all(c("processed_data", "indices") %in% names(result))) {
    stop("O objeto 'result' deve ser a lista retornada por compute_indices().")
  }

  # 2. Criar o diretório de saída, se não existir
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # 3. Salvar o CSV com RTI e HFTI médios por espécie
  csv_path <- file.path(out_dir, "indices_por_especie.csv")
  utils::write.csv(result$indices, csv_path, row.names = FALSE)
  message("Salvo CSV com índices em: ", csv_path)

  # 4. Carregar o shapefile da área de estudo
  area_sf <- sf::st_read(area_shapefile, quiet = TRUE)
  # Reprojetar para o CRS desejado, se necessário
  if (sf::st_crs(area_sf)$epsg != map_crs) {
    area_sf <- sf::st_transform(area_sf, crs = map_crs)
  }

  # 5. Extrair o objeto sf com as ocorrências já contendo RTI e HFTI
  occ_sf <- result$processed_data
  # Reprojetar ocorrências para o mesmo CRS dos mapas
  if (sf::st_crs(occ_sf)$epsg != map_crs) {
    occ_sf <- sf::st_transform(occ_sf, crs = map_crs)
  }

  # 6. Obter a lista de espécies únicas
  especies <- unique(occ_sf$species)

  # 7. Iterar sobre cada espécie
  for (sp in especies) {
    # 7.1 Filtrar apenas as ocorrências desta espécie
    dados_sp <- dplyr::filter(occ_sf, species == sp)
    # 7.2 Obter os valores individuais de RTI e HFTI (em formato “tibble” normal)
    dados_nogeo <- sf::st_drop_geometry(dados_sp)

    # 7.3 Calcular valores medianos de RTI e HFTI (para a linha vertical)
    mediana_rti  <- stats::median(dados_nogeo$RTI_value,  na.rm = TRUE)
    mediana_hfti <- stats::median(dados_nogeo$HFTI_value, na.rm = TRUE)

    # 7.4 Definir nomes de arquivo padronizados (retirar caracteres especiais)
    # Substituir espaços e barras por sublinhado
    safe_name <- gsub("[^[:alnum:]_]", "_", sp)
    # 7.5 Caminhos para salvar imagens
    mapa_rti_file     <- file.path(out_dir, paste0(safe_name, "_mapa_RTI.png"))
    mapa_hfti_file    <- file.path(out_dir, paste0(safe_name, "_mapa_HFTI.png"))
    dens_rti_file     <- file.path(out_dir, paste0(safe_name, "_densidade_RTI.png"))
    dens_hfti_file    <- file.path(out_dir, paste0(safe_name, "_densidade_HFTI.png"))

    # 7.6 GERAR e SALVAR mapa de RTI
    p_map_rti <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = area_sf, fill = "gray90", color = "gray50") +
      ggplot2::geom_sf(data = dados_sp, aes(color = RTI_value), size = point_size) +
      ggplot2::scale_color_distiller(palette = "RdYlBu", direction = -1, name = "RTI") +
      ggplot2::theme_minimal() +
      ggplot2::ggtitle(paste0("Road Tolerance Index (RTI) - ", sp)) +
      ggplot2::theme(
        plot.title   = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title   = ggplot2::element_text(size = 16),
        axis.text    = ggplot2::element_text(size = 14),
        legend.title = ggplot2::element_text(size = 16, face = "bold"),
        legend.text  = ggplot2::element_text(size = 13),
        legend.key.size = ggplot2::unit(0.5, "cm")
      )
    ggplot2::ggsave(filename = mapa_rti_file, plot = p_map_rti, width = 8, height = 6, dpi = 300)
    message("Salvo mapa RTI para ", sp, " em: ", mapa_rti_file)

    # 7.7 GERAR e SALVAR mapa de HFTI
    p_map_hfti <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = area_sf, fill = "gray90", color = "gray50") +
      ggplot2::geom_sf(data = dados_sp, aes(color = HFTI_value), size = point_size) +
      ggplot2::scale_color_distiller(palette = "RdYlBu", direction = -1, name = "HFTI") +
      ggplot2::theme_minimal() +
      ggplot2::ggtitle(paste0("Human Footprint Tolerance Index (HFTI) - ", sp)) +
      ggplot2::theme(
        plot.title   = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title   = ggplot2::element_text(size = 16),
        axis.text    = ggplot2::element_text(size = 14),
        legend.title = ggplot2::element_text(size = 16, face = "bold"),
        legend.text  = ggplot2::element_text(size = 13),
        legend.key.size = ggplot2::unit(0.5, "cm")
      )
    ggplot2::ggsave(filename = mapa_hfti_file, plot = p_map_hfti, width = 8, height = 6, dpi = 300)
    message("Salvo mapa HFTI para ", sp, " em: ", mapa_hfti_file)

    # 7.8 GERAR e SALVAR gráfico de densidade de RTI
    p_den_rti <- ggplot2::ggplot(dados_nogeo, aes(x = RTI_value)) +
      ggplot2::geom_density(fill = density_fill, alpha = density_alpha) +
      ggplot2::geom_vline(xintercept = mediana_rti, colour = "red",
                          linetype = "dashed", size = 1) +
      ggplot2::labs(
        title = paste0("Density of RTI - ", sp),
        x = "Road Tolerance Index (RTI)",
        y = "Density"
      ) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        plot.title   = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title   = ggplot2::element_text(size = 16),
        axis.text    = ggplot2::element_text(size = 14)
      )
    ggplot2::ggsave(filename = dens_rti_file, plot = p_den_rti, width = 8, height = 6, dpi = 300)
    message("Salvo gráfico densidade RTI para ", sp, " em: ", dens_rti_file)

    # 7.9 GERAR e SALVAR gráfico de densidade de HFTI
    p_den_hfti <- ggplot2::ggplot(dados_nogeo, aes(x = HFTI_value)) +
      ggplot2::geom_density(fill = density_fill, alpha = density_alpha) +
      ggplot2::geom_vline(xintercept = mediana_hfti, colour = "red",
                          linetype = "dashed", size = 1) +
      ggplot2::labs(
        title = paste0("Density of HFTI - ", sp),
        x = "Human Footprint Tolerance Index (HFTI)",
        y = "Density"
      ) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        plot.title   = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title   = ggplot2::element_text(size = 16),
        axis.text    = ggplot2::element_text(size = 14)
      )
    ggplot2::ggsave(filename = dens_hfti_file, plot = p_den_hfti, width = 8, height = 6, dpi = 300)
    message("Salvo gráfico densidade HFTI para ", sp, " em: ", dens_hfti_file)
  }

  return(invisible(NULL))
}
