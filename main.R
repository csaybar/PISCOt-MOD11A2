library(rgee)
library(raster)
source("functions.R")

ee_reattach()
ee_Initialize()

# 1. Search Data
# ee_dataset() %>%
#   ee_search_title("MOD11A2","006",logical_operator = "AND") %>%
#   ee_search_display()

# Read dataset
mod11a2 <- ee$ImageCollection("MODIS/006/MOD11A2")$
  filter(ee$Filter$date('2001-01-01', '2019-12-31'))$
  filter(ee$Filter$calendarRange(1,field = "month"))
nimg <- mod11a2$size()$getInfo()
# Raw Temperature MODIS LST day 1km
mod11a2_raw <- mod11a2
mod11a2_raw_npixels <- mod11a2_raw$map(count_pixels)$sum()
mod11a2_composite_raw <- mod11a2$mean()$
  select("LST_Day_1km")$
  multiply(0.02)$
  subtract(273.15)

# QA quality Temperature MODIS LST day 1km
mod11a2_clean <- mod11a2$map(mod11A2_clean)
mod11a2_clean_npixels <- mod11a2_clean$map(count_pixels)$sum()

# at least 10% of pixels
mask <- mod11a2_clean_npixels$gte(round(nimg*0.10))
mod11a2_composite_clean <- mod11a2_clean$mean()$updateMask(mask)

# GeoVIZ
lst_viz <- list(min = 15 , max = 30, palette = temperature_palette)
npixel_viz <- list(min = 0 , max = nimg, palette = temperature_palette)

# Display maps
Map$setCenter(-74.16747, -12.42585, 7)
Map$addLayer(mod11a2_composite_raw, lst_viz, name = "raw_mod11") +
Map$addLayer(mod11a2_raw_npixels, npixel_viz, name = "raw_npixels") +
Map$addLayer(mod11a2_composite_clean, lst_viz, name = "clean_mod11") +
Map$addLayer(mod11a2_clean_npixels, npixel_viz, name = "clean_npixels")

# Donwload image
pisco_area <- ee$Geometry$Rectangle(-75, -10, -65, 10)
mod11a2 <- ee_image_as_raster(
  image = mod11a2_composite_raw,
  region = pisco_area,
  dsn = "mod11a2_12.tif",
  via = "getInfo" # for large image use drive
)
plot(mod11a2)


