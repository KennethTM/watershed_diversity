source("0_libs_and_funcs.R")

#Prepare environmental data for further analysis:
#Chemistry
#Secchi depth
#Bathymetry

dk_border <- st_read(gis_database, layer = "dk_border")

#Submerged macrophyte and depth rawdata from www.odaforalle.au.dk (or https://miljoedata.miljoeportal.dk/)
#Time period 01-01-1990 to 01-10-2020
depth_area <- read_xlsx(paste0(getwd(), "/data_raw/odaforalle_depth_area_01102020.xlsx")) %>% 
  clean_names()

#Lake plant cover
cover_to_pct <- tribble(~total_dækningsgrad, ~plant_cover,
                        "Ej oplyst", NA,
                        "0%", 0,
                        ">0-5%", 2.5,
                        "5-25%", 15.5,
                        "25-50%", 38,
                        "50-75%", 63,
                        "75-95%", 85.5,
                        "95-100%", 98)

#Lake coordinates
#Correct wrong coordinates for Skenkelsø
lake_coords <- depth_area %>%
  select(observationsstednr, observationsstednavn, xutm_euref89_zone32, yutm_euref89_zone32) %>%
  distinct() %>%
  mutate(xutm_euref89_zone32 = ifelse(observationsstednr == "52000929", 696273, xutm_euref89_zone32),
         yutm_euref89_zone32 = ifelse(observationsstednr == "52000929", 6188507, yutm_euref89_zone32))

#Lake bathymetry data
lake_depth_area <- depth_area %>% 
  select(observationsstednr, observationsstednavn, startdato,
         dybden_fra_i_meter, dybden_til_i_meter, arealet_i_m2) %>% 
  mutate_at(vars(dybden_fra_i_meter, dybden_til_i_meter, arealet_i_m2), 
            list(~parse_number(., locale = locale(decimal_mark = ",")))) %>% 
  na.omit()

lake_bathy <- lake_depth_area %>% 
  group_by(observationsstednr, observationsstednavn, startdato) %>% 
  arrange(observationsstednr, observationsstednavn, startdato, desc(dybden_fra_i_meter)) %>% 
  mutate(area_accum = cumsum(arealet_i_m2)) %>% 
  nest() %>% 
  mutate(bathy_fun = map(data, approx_bathy),
         bathy_n = map_int(data, nrow)) %>%
  mutate(bathy_zmax = map_dbl(data, ~max(.$dybden_til_i_meter)),
         bathy_area = map_dbl(data, ~sum(.$arealet_i_m2)),
         bathy_vol_approx = map2_dbl(bathy_fun, bathy_zmax, ~approx_bathy_integrate(.x, 0, .y)),
         bathy_vol = ifelse(bathy_n == 1, pi*(bathy_area/pi)*bathy_zmax/3, bathy_vol_approx), #when only zmax and area is available, assume cone volume
         bathy_zmean = bathy_vol/bathy_area) %>% 
  ungroup() %>% 
  select(-bathy_vol_approx)

lake_level_stats_sf <- lake_bathy %>% 
  group_by(observationsstednr, observationsstednavn) %>% 
  summarise_at(vars("bathy_area", "bathy_vol", "bathy_zmean", "bathy_zmax"), list(~.x[which.max(bathy_n)])) %>% 
  ungroup() %>% 
  left_join(lake_coords) %>% 
  filter(!is.na(xutm_euref89_zone32)) %>% 
  rename(site_name = observationsstednavn, site_id = observationsstednr) %>% 
  st_as_sf(coords = c("xutm_euref89_zone32", "yutm_euref89_zone32"), crs = dk_epsg) %>% 
  st_crop(dk_border)

st_write(lake_level_stats_sf, dsn = gis_database, layer = "lake_bathy", delete_layer = TRUE)

#Chemistry and secchi data
lake_secchi_raw <- read_xlsx(paste0(getwd(), "/data_raw/odaforalle_secchi_lake_09112020.xlsx")) %>% 
  clean_names()

lake_chem_raw <- read_xlsx(paste0(getwd(), "/data_raw/odaforalle_chemistry_lake_09112020.xlsx")) %>% 
  clean_names()

#Combine data and keep summer observations
lake_secchi_chem_raw <- bind_rows(lake_chem_raw, lake_secchi_raw) %>% 
  mutate(date = ymd(startdato), 
         year = year(date), 
         month = month(date),
         xutm_euref89_zone32 = as.numeric(coalesce(xutm_euref89_zone32, xutm_euref89_zone32_zone32)), 
         yutm_euref89_zone32 = as.numeric(coalesce(yutm_euref89_zone32, yutm_euref89_zone32_zone32)),
         parameter_corr = ifelse(parameter == "Chlorophyl (ukorrigeret)", "Chlorophyl A",
                                 ifelse(parameter == "Phosphor,total PO4", "Phosphor, total-P", parameter)),
         var_unit = paste0(parameter_corr, "_", enhed),
         avg_sample_depth_m_chem = parse_number(gennemsnitsdybde_i_m, locale = locale(decimal_mark = ",")),
         avg_sample_depth_m = ifelse(is.na(avg_sample_depth_m_chem), 0, avg_sample_depth_m_chem),
         summer = ifelse(month %in% c(5:9), "summer", "not summer")) %>% 
  filter(summer == "summer",
         yutm_euref89_zone32 > 700000) 

lake_chem <- lake_secchi_chem_raw %>%
  select(observationsstednr, observationsstednavn,
         avg_sample_depth_m, var_unit, value = resultat, year, date,
         xutm_euref89_zone32, yutm_euref89_zone32) %>%
  group_by(observationsstednr, observationsstednavn, 
           xutm_euref89_zone32, yutm_euref89_zone32,
           var_unit, year, date, avg_sample_depth_m) %>%
  summarise(value_mean_dups = mean(value)) %>%
  summarise(value_surface = value_mean_dups[which.min(avg_sample_depth_m)]) %>%
  summarise(value_mean_year = mean(value_surface)) %>%
  ungroup() %>%
  spread(var_unit, value_mean_year) %>%
  rename(alk_mmol_l = `Alkalinitet,total TA_mmol/l`, chla_ug_l = `Chlorophyl A_µg/l`, tn_mg_l = `Nitrogen,total N_mg/l`,
         ph_ph = pH_pH, tp_mg_l = `Phosphor, total-P_mg/l`, secchi_depth_m = Sigtdybde_m,
         site_id = observationsstednr, site_name = observationsstednavn)

lake_chem_sf <- lake_chem %>% 
  st_as_sf(coords = c("xutm_euref89_zone32", "yutm_euref89_zone32"), crs = dk_epsg) %>% 
  st_crop(dk_border)

st_write(lake_chem_sf, dsn = gis_database, layer = "lake_chem", delete_layer = TRUE)
