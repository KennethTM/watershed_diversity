library(sf);library(raster);library(rgdal);library(gdalUtils);library(tidyverse);library(readxl);library(lubridate)
library(rgrass7);library(link2GI);library(patchwork);library(lwgeom)
library(mapview)
library(lme4)
library(MuMIn);library(ggeffects);library(corrplot)

#Projection, as EPSG number, used for spatial analysis (UTM ZONE 32)
dk_epsg <- 25832

#Path for spatial vector database
gis_database <- paste0(getwd(), "/data_processed/gis_database.sqlite")

#Figure sizing. For most journals the figures should be 39 mm, 84 mm, 129 mm, or 174 mm wide and not higher than 234 mm.
#ggplot theme
theme_pub <- theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "black"), 
        strip.background = element_rect(fill = "white"))
theme_set(theme_pub)
