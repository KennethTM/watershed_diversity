source("libs_and_funcs.R")

#Load gis layers from database
#Show layers
st_layers(gis_database)

dk_lakes <- st_read(dsn = gis_database, layer = "dk_lakes") %>% 
  mutate(lake_area = as.numeric(st_area(geometry)))

dk_basins <- st_read(dsn = gis_database, layer = "dk_basins")

fish_species_lakes <- st_read(dsn = gis_database, layer = "fish_species_lakes")

fish_species_basin <- st_read(dsn = gis_database, layer = "fish_species_basin") %>% 
  select(-system, -site_id)

#Fish atlas data
fish_atlas <- read_delim(paste0(getwd(), "/data_raw/Atlas_data_danish_latin_id_basin.csv"), delim = " ") %>% 
  select(basin_id, fish_id = ID) %>% 
  na.omit() %>% 
  distinct() %>% 
  left_join(dk_basins) %>% 
  st_as_sf()

#Count species richness in each basin
fish_basin_species_count <- dk_basins %>% 
  st_join(fish_species_basin) %>% 
  rbind(fish_atlas) %>% 
  group_by(basin_id) %>% 
  summarise(n_spec_basin = ifelse(all(is.na(fish_id)), 0, length(unique(fish_id)))) %>%
  ungroup() %>% 
  mutate(basin_area = as.numeric(st_area(GEOMETRY))) %>% 
  st_cast("MULTILINESTRING") %>% 
  mutate(basin_circum = as.numeric(st_length(GEOMETRY))) %>% 
  st_cast("MULTIPOLYGON")

#Count number of fish caught for each sampling
fish_lake_species_count <- fish_species_lakes %>%
  group_by(system, site_id) %>%
  summarise(n_spec_lake = ifelse(all(is.na(fish_id)), 0, length(unique(fish_id)))) %>%
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



#exp plot
fig_hist <- model_df %>% 
  ggplot(aes(lake_basin_spec_ratio, fill = system)) +
  geom_histogram()

fig_lake <- model_df %>% 
  ggplot(aes(y=lake_basin_spec_ratio, x=lake_area_log10, col = system)) +
  geom_point()+
  ylab("andel af arter i sø ifht opland")+
  xlab("log10 lake area")

fig_basin <- model_df %>% 
  ggplot(aes(y=lake_basin_spec_ratio, x=basin_area_log10, col = system)) +
  geom_point()+
  ylab("andel af arter i sø ifht opland")+
  xlab("log10 basin area")

fig_elev <- model_df %>% 
  ggplot(aes(y=lake_basin_spec_ratio, x=elevation, col = system)) +
  geom_point()+
  ylab("andel af arter i sø ifht opland")+
  xlab("sø elevation over hav")

fig_hist+fig_lake+fig_basin+fig_elev+plot_layout(ncol = 2, guides = "collect")
