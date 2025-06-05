test_that("generate_all_reports cria arquivos no diretório correto", {
  # Criar dados de exemplo bem simples
  sample_data <- data.frame(
    species  = c("SpA", "SpA"),
    longitude = c(-50, -50.1),
    latitude  = c(-10, -10.05)
  )
  # Computar índices (usa dados mínimos)
  result <- compute_indices(
    data = sample_data,
    remove_duplicates = TRUE,
    buffer_km = 0,
    ref_dist = 1.5,
    divisor = 50
  )

  # Shapefile mínimo: criamos um polígono retângulo de teste
  library(sf)
  rect <- st_as_sf(
    data.frame(id = 1),
    geometry = st_sfc(st_polygon(list(rbind(c(-51, -11), c(-49, -11), c(-49, -9), c(-51, -9), c(-51, -11))))),
    crs = 4326
  )
  # Salvar temporariamente esse shapefile para testar
  tmp_shp_dir <- tempdir()
  st_write(rect, dsn = file.path(tmp_shp_dir, "area_test.shp"), quiet = TRUE)

  # Diretório temporário para salvar relatórios
  tmp_out <- file.path(tempdir(), "reports_test")
  if (dir.exists(tmp_out)) unlink(tmp_out, recursive = TRUE)

  generate_all_reports(result, 
                       area_shapefile = file.path(tmp_shp_dir, "area_test.shp"),
                       out_dir = tmp_out)

  # Verificar se o CSV e pelo menos um arquivo PNG foram criados
  expect_true(file.exists(file.path(tmp_out, "indices_por_especie.csv")))
  # Como só há uma espécie, testar se pelo menos um dos arquivos _mapa_RTI.png existe
  files <- list.files(tmp_out, pattern = "mapa_RTI.png", full.names = TRUE)
  expect_true(length(files) == 1 && file.exists(files))
})
