% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{generate_all_reports}
\alias{generate_all_reports}
\title{Generate and save maps & density plots for RTI and HFTI of all species}
\usage{
generate_all_reports(
  result,
  area_shapefile,
  out_dir,
  map_crs = 4326,
  point_size = 1.5,
  density_fill = "steelblue",
  density_alpha = 0.35
)
}
\arguments{
\item{result}{Lista retornada por \code{compute_indices()}, contendo:
\itemize{
\item{\code{result$processed_data}}{ Um objeto \code{sf} com as
ocorrências e colunas \code{RTI_value} e \code{HFTI_value}. }
\item{\code{result$indices}}{ Data frame com RTI e HFTI médios por espécie. }
}}

\item{area_shapefile}{Caminho (string) para o shapefile da área de
estudo (por exemplo, limite do Brasil). Deve ser um arquivo \code{.shp}.}

\item{out_dir}{Caminho (string) para o diretório onde as imagens e o CSV
serão salvos. Caso não exista, será criado automaticamente.}

\item{map_crs}{CRS desejado para os mapas (padrão = 4326, WGS84).}

\item{point_size}{Tamanho dos pontos nos mapas (padrão = 1.5).}

\item{density_fill}{Cor de preenchimento dos gráficos de densidade
(padrão = "steelblue").}

\item{density_alpha}{Transparência do preenchimento dos gráficos de
densidade (padrão = 0.35).}
}
\value{
Invisível. A função retorna NULL após salvar todos os arquivos
em \code{out_dir}.
}
\description{
Esta função recebe o resultado de \code{compute_indices()}, um shapefile
de área de estudo e o diretório de saída. Para cada espécie presente em
\code{result$processed_data}, ela gera:
\enumerate{
\item Mapa de RTI (points coloridos por \code{RTI_value})
\item Mapa de HFTI (points coloridos por \code{HFTI_value})
\item Gráfico de densidade de RTI (com linha vertical na mediana)
\item Gráfico de densidade de HFTI (com linha vertical na mediana)
Além disso, salva um arquivo CSV com RTI e HFTI médios por espécie.
}
}
