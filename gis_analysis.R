source("libs_and_funcs.R")

#Load gis layers from database
#Show layers
st_layers(gis_database)

dk_lakes <- st_read(dsn = gis_database, layer = "dk_lakes") %>% 
  mutate(lake_area = as.numeric(st_area(geometry)))
dk_basins <- st_read(dsn = gis_database, layer = "dk_basins")

fish_species_lakes <- st_read(dsn = gis_database, layer = "fish_species_lakes")
fish_species_basin <- st_read(dsn = gis_database, layer = "fish_species_basin")

#Count species richness in each basin
fish_basin_species_count <- dk_basins %>% 
  st_join(fish_species_basin) %>% 
  group_by(basin_id) %>% 
  summarise(n_spec_basin = ifelse(any(is.na(fish_id)), 0, length(unique(fish_id)))) %>%
  ungroup() %>% 
  mutate(basin_area = as.numeric(st_area(GEOMETRY)))

#Count number of fish caught for each sampling
fish_lake_species_count <- fish_species_lakes %>%
  group_by(system, site_id) %>%
  summarise(n_spec_lake = ifelse(any(is.na(fish_id)), 0, length(unique(fish_id)))) %>%
  ungroup()



#Quick exploration of initial data
#Join lake fish data with basin species richness and lake area
lake_basin_richness <- fish_lake_species_count %>% 
  st_join(fish_basin_species_count) %>% 
  st_join(dk_lakes) %>% 
  na.omit() %>% 
  st_drop_geometry()

model_df <- lake_basin_richness %>% 
  mutate(lake_basin_area_ratio = lake_area/basin_area,
         lake_basin_spec_ratio = n_spec_lake/n_spec_basin,
         basin_area_log10 = log10(basin_area),
         lake_area_log10 = log10(lake_area)) %>% 
  select(system, n_spec_lake, n_spec_basin, lake_basin_spec_ratio, basin_area_log10, lake_area_log10, lake_basin_area_ratio, elevation) %>% 
  na.omit()

#Binomial glm model
mod_0 <- glm(cbind(n_spec_lake, n_spec_basin) ~ basin_area_log10 + lake_area_log10 + elevation, data = model_df, family = "binomial")
summary(mod_0)

#https://stackoverflow.com/questions/26385617/proportion-modeling-betareg-errors 
y.transf.betareg <- function(y){
  n.obs <- sum(!is.na(y))
  (y * (n.obs - 1) + 0.5) / n.obs
}

#Beta regression, transform response to avoid exact 0 and 1's
library(betareg)
mod_1 <- betareg(y.transf.betareg(lake_basin_spec_ratio) ~ basin_area_log10 + lake_area_log10 + elevation, data=model_df)
summary(mod_1)

