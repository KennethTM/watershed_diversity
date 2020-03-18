library(sf);library(raster);library(rgdal);library(gdalUtils);library(tidyverse);library(readxl)
library(rgrass7);library(link2GI)

#Projection, as EPSG number, used for spatial analysis (UTM ZONE 32)
dk_epsg <- 25832

#Path for spatial vector database
gis_database <- paste0(getwd(), "/data_processed/gis_database.sqlite")