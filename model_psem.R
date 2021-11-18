source("libs_and_funcs.R")

library(piecewiseSEM);library(lme4)

model_data_raw <- read_csv(paste0(getwd(), "/data_processed/lake_species_all.csv")) %>% 
  filter(!is.na(basin_id)) %>% 
  mutate(lake_stream_connect = factor(ifelse(lake_stream_connect > 0, 1, 0)),
         ice_covered = factor(ice_covered),
         basin_elev_range = basin_elev_max_m - basin_elev_min_m) %>% 
  select(-lake_area_m2, -basin_elev_max_m, -basin_elev_min_m) 

#corr analysis for predictors
model_predictors <- model_data_raw %>% 
  filter(is.na(year_established)) %>% 
  select(-contains("_id"), -site_name, -contains("n_spec_"), -contains("year"), -lake_stream_connect, -ice_covered)

#do intercorrelation analysis
model_predictors_cor <- cor(model_predictors, use="pairwise.complete.obs")
corrplot.mixed(model_predictors_cor)

#remove vars
model_preds_to_remove <- c("bathy_zmean", "bathy_vol", "shoreline_m", "tn_mg_l", 
                           "basin_stream_length_m", "basin_arti", "basin_elev_mean_m", 
                           "basin_circum_m", "basin_area_m2", "basin_elev_range")

model_predictors_cor_sub <- model_predictors %>% 
  select(-all_of(model_preds_to_remove)) %>% 
  cor(use="pairwise.complete.obs")
corrplot.mixed(model_predictors_cor_sub)

#plot distributions
model_predictors %>% 
  select(-all_of(model_preds_to_remove)) %>% 
  mutate_at(vars(basin_lake_area_m2, bathy_area, bathy_zmax, chla_ug_l, secchi_depth_m, tp_mg_l, basin_slope_prc), list(log10)) %>% 
  mutate_at(vars(basin_agri, lake_elev_m), list(~log10(. + 2))) %>% 
  gather(variable, value) %>% 
  ggplot(aes(value))+
  geom_density()+
  facet_wrap(variable~., scales = "free")

#drop observations with missing
model_data <- model_data_raw %>% 
  select(-all_of(model_preds_to_remove)) %>% 
  filter(!is.na(bathy_area),
         !is.na(secchi_depth_m),
         !is.na(alk_mmol_l),
         !is.na(basin_agri),
         !is.na(tp_mg_l),
         !is.na(ph_ph)) %>% 
  mutate_at(vars(basin_lake_area_m2, bathy_area, bathy_zmax, chla_ug_l, secchi_depth_m, tp_mg_l, basin_slope_prc), list(log10))

summary(model_data)

#split data
model_data_newlakes <- model_data %>% 
  filter(!is.na(year_established))

model_data_natural <- model_data %>% 
  filter(is.na(year_established))

#specify model
psem_mod <- psem(
  glmer(n_spec_basin ~ ice_covered+basin_lake_area_m2+basin_agri+basin_slope_prc+(1|basin_id), data = model_data_natural, family = "poisson", nAGQ=0),
  glmer(n_spec_lake ~ ice_covered+n_spec_basin+lake_elev_m+bathy_area+bathy_zmax+alk_mmol_l+chla_ug_l+ph_ph+tp_mg_l+secchi_depth_m+lake_stream_connect+(1|basin_id), data = model_data_natural, family = "poisson", nAGQ=0)
)
summary(psem_mod)


