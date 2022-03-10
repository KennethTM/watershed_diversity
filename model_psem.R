source("libs_and_funcs.R")

library(piecewiseSEM);library(lme4)

basins_sal <- readRDS(paste0(getwd(), "/data_processed/basins_sal.rds"))

model_data_raw <- read_csv(paste0(getwd(), "/data_processed/lake_species_all.csv")) %>% 
  left_join(basins_sal) %>% 
  filter(!is.na(basin_id)) %>% 
  mutate(lake_stream_connect = factor(ifelse(lake_stream_connect > 0, 1, 0)),
         ice_covered = factor(ice_covered),
         basin_elev_range = basin_elev_max_m - basin_elev_min_m) %>% 
  select(-lake_area_m2, -basin_elev_max_m, -basin_elev_min_m) %>% 
  mutate(lake_age = ifelse(is.na(year_established), 9999, year_sample - year_established),
         lake_age_bins = cut(lake_age, 
                             breaks = c(-1, 10, 20, 200, 10000), 
                             labels = c("0-10", "10-20", ">20", "Unknown")),
         lake_age_bins = factor(lake_age_bins, levels = c("Unknown", "0-10", "10-20", ">20"))) %>% 
  select(-lake_age, -year_established, -year_sample)

#plot distributions
model_data_trans <- model_data_raw %>% 
  mutate_at(vars(basin_area_m2, basin_circum_m, basin_lake_area_m2, basin_slope_prc, 
                 basin_stream_length_m, bathy_area, bathy_vol, bathy_zmax, bathy_zmean, 
                 chla_ug_l, salinity, secchi_depth_m, shoreline_m, tn_mg_l, tp_mg_l), list(log10)) 

model_data_trans %>% 
  select(-gml_id, -site_id, -site_name, -n_spec_lake, -n_spec_basin,
         -lake_stream_connect, -ice_covered, -lake_age_bins) %>% 
  gather(variable, value) %>% 
  ggplot(aes(value))+
  geom_density()+
  facet_wrap(variable~., scales = "free")

#corr analysis for basin vars
basin_corr <- model_data_raw %>% 
  select(basin_lake_area_m2, basin_stream_length_m, basin_arti, basin_agri, basin_elev_mean_m,
         basin_slope_prc, basin_area_m2, basin_circum_m, salinity, basin_elev_range) %>% 
  select(-basin_area_m2, -basin_circum_m, -basin_elev_range, -basin_stream_length_m)

#iteratively drop vars until vif < 3
corvif(basin_corr)

#corr analysis for lake vars
lake_corr <- model_data_raw %>% 
  select(lake_elev_m, shoreline_m, bathy_area, bathy_vol, bathy_zmean, bathy_zmax,
         alk_mmol_l, chla_ug_l, tn_mg_l, ph_ph, tp_mg_l, secchi_depth_m) %>% 
  select(-bathy_zmean, -bathy_vol)
  
#iteratively drop vars until vif < 3
corvif(lake_corr)



#drop observations with missing
model_data <- model_data_trans %>% 
  filter(!is.na(tn_mg_l),
         !is.na(ph_ph),
         !is.na(bathy_area),
         !is.na(alk_mmol_l),
         !is.na(secchi_depth_m),
         !is.na(basin_arti)) %>% 
  select(-basin_area_m2, -basin_circum_m, -basin_elev_range, -basin_stream_length_m,
         -bathy_zmean, -bathy_vol) %>% 
  mutate(lake_stream_connect = ifelse(lake_stream_connect == "0", 0, 1),
         ice_covered = as.numeric(ice_covered)-1,
         lake_age_bins = ifelse(lake_age_bins == "Unknown", 1, 0))

summary(model_data)

#specify model
psem_mod <- psem(
  glmer(n_spec_basin ~ basin_lake_area_m2+basin_arti+basin_agri+basin_elev_mean_m+basin_slope_prc+salinity+(1|basin_id), data = model_data, family = "poisson", nAGQ=0),
  
  glmer(n_spec_lake ~ n_spec_basin+ice_covered+lake_stream_connect+lake_age_bins+lake_elev_m+shoreline_m+bathy_area+bathy_zmax
        +alk_mmol_l+chla_ug_l+tn_mg_l+ph_ph+tp_mg_l+secchi_depth_m+(1|basin_id), data = model_data, family = "poisson", nAGQ=0)
)
psem_sum <- summary(psem_mod)
psem_sum
