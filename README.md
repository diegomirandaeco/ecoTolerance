# ecoTolerance

**Ecological Tolerance Indices (RTI & HFTI) for Species Occurrence Data**

---

## Visão Geral

O **ecoTolerance** is an R package designed to:

1. Calculate **Road Tolerance Index (RTI)** for each species occurrence point, using the distance to the nearest road.
2. Calculate the **Human Footprint Tolerance Index (HFTI)** for each occurrence point, by extracting the human footprint value within a surrounding buffer.
3. Generate, for each species, cartographic maps (points colored by RTI or HFTI) and density distribution plots of the indices.
4. Offer a simple way to use either **internal data** (roads and raster for Brazil) or **external data** (shapefiles and rasters from any region).

---

## Instalação

```r
# If you don't have devtools yet:
install.packages("devtools")

# In the root of the package directory:
devtools::document()  # Gera documentação (NAMESPACE, .Rd)
devtools::install()   # Instala o pacote
