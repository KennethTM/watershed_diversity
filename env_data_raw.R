source("libs_and_funcs.R")

#Prepare environmental data for further analysis:
#Chemistry
#Secchi depth
#Plant and bathymetry

#Submerged macrophyte and depth rawdata from www.odaforalle.au.dk
#Time period 01-01-1990 to 01-10-2020
depth_area <- read_xlsx(paste0(getwd(), "/data_raw/odaforalle_depth_area_01102020.xlsx")) %>% 
  clean_names()
plant_water <- read_xlsx(paste0(getwd(), "/data_raw/odaforalle_plant_water_01102020.xlsx")) %>% 
  clean_names()

#lake plant cover
cover_to_pct <- tribble(~total_dækningsgrad, ~plant_cover,
                        "Ej oplyst", NA,
                        "0%", 0,
                        ">0-5%", 2.5,
                        "5-25%", 15.5,
                        "25-50%", 38,
                        "50-75%", 63,
                        "75-95%", 85.5,
                        "95-100%", 98)

#lake coordinates
#correct wrong coordinates for skenkelsø
lake_coords <- plant_water %>% 
  select(observationsstednr, observationsstednavn, xutm_euref89_zone32, yutm_euref89_zone32) %>% 
  distinct() %>% 
  mutate(xutm_euref89_zone32 = ifelse(observationsstednr == "52000929", 696273, xutm_euref89_zone32),
         yutm_euref89_zone32 = ifelse(observationsstednr == "52000929", 6188507, yutm_euref89_zone32)) 

#lake bathymetry data
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
  ungroup()

lake_depth_area_layer <- lake_depth_area %>% 
  left_join(lake_bathy %>% 
              select(observationsstednr, observationsstednavn, startdato, bathy_fun, bathy_n, bathy_area, bathy_zmean)) %>% 
  mutate(layer_area = pmap_dbl(list(bathy_fun, dybden_fra_i_meter, dybden_til_i_meter), approx_bathy_layer_area),
         layer_area = ifelse(bathy_n == 1 & is.na(layer_area), bathy_area, layer_area)) %>% #if bathy only contains one depth use total area
  select(-arealet_i_m2, -bathy_fun, -bathy_n, -bathy_area, -bathy_zmean)

#Lake relative plant cover
lake_plant_water <- plant_water %>% 
  left_join(cover_to_pct) %>% 
  select(observationsstednr, observationsstednavn, startdato, transektnummer, punktnummer, 
         vanddybden_i_m, gsnit_plantehøjde_m, plant_cover) %>% 
  distinct() %>% 
  mutate_at(vars(vanddybden_i_m, gsnit_plantehøjde_m), 
            list(~parse_number(., locale = locale(decimal_mark = ",")))) %>% 
  mutate(vanddybden_i_m = ifelse(vanddybden_i_m == 0, 0.01, vanddybden_i_m))

lake_level_stats <- lake_plant_water %>% 
  left_join(lake_depth_area) %>% 
  mutate(is_within_depth = ifelse(vanddybden_i_m >= dybden_fra_i_meter & vanddybden_i_m < dybden_til_i_meter, "within", "outside")) %>% 
  filter(is_within_depth == "within") %>% 
  group_by(observationsstednr, observationsstednavn, startdato, dybden_fra_i_meter, dybden_til_i_meter, transektnummer) %>% 
  summarise(rpa_trans = mean(plant_cover)) %>% 
  summarise(rpa_depth_int = mean(rpa_trans)) %>%
  left_join(lake_depth_area_layer) %>% 
  mutate(rpa_area = (rpa_depth_int/100) * layer_area) %>%
  group_by(observationsstednr, observationsstednavn, startdato) %>% 
  summarise(lake_rpa = sum(rpa_area)) %>% 
  left_join(lake_bathy %>% select(observationsstednr, observationsstednavn, startdato, bathy_zmax, bathy_zmean, bathy_area, bathy_vol)) %>% 
  mutate(lake_rpa_perc = lake_rpa/bathy_area*100) %>% 
  ungroup()

lake_level_stats_sf <- lake_level_stats %>% 
  left_join(lake_coords) %>% 
  mutate(year = year(ymd(startdato))) %>% 
  select(site_id = observationsstednr, site_name = observationsstednavn,
         year, lake_rpa, lake_rpa_perc, contains("bathy_"),
         xutm_euref89_zone32, yutm_euref89_zone32) %>% 
  filter(!is.na(xutm_euref89_zone32)) %>% 
  st_as_sf(coords = c("xutm_euref89_zone32", "yutm_euref89_zone32"), crs = dk_epsg)

st_write(lake_level_stats_sf, dsn = gis_database, layer = "lake_plants", delete_layer = TRUE)

#chemistry and secchi data
lake_secchi_raw <- read_xlsx(paste0(getwd(), "/data_raw/odaforalle_secchi_lake_09112020.xlsx")) %>% 
  clean_names()

lake_chem_raw <- read_xlsx(paste0(getwd(), "/data_raw/odaforalle_chemistry_lake_09112020.xlsx")) %>% 
  clean_names()

#combine data and keep summer observations
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
  st_as_sf(coords = c("xutm_euref89_zone32", "yutm_euref89_zone32"), crs = dk_epsg)

st_write(lake_chem_sf, dsn = gis_database, layer = "lake_chem", delete_layer = TRUE)

# #Add gml_id to chemistry and plant data
# #Export files as kml and edit interactively in Google Earth
# dk_lakes_subset <- st_read(gis_database, layer = "dk_lakes_subset")
# lake_chem <- st_read(gis_database, layer = "lake_chem")
# lake_plant <- st_read(gis_database, layer = "lake_plants")
# 
# lake_chem %>% 
#   select(site_id, site_name) %>% 
#   distinct() %>% 
#   st_write(paste0(getwd(), "/data_raw/files_to_fix/lake_chem_raw.kml"))
# 
# lake_plant %>% 
#   select(site_id, site_name) %>% 
#   distinct() %>% 
#   st_write(paste0(getwd(), "/data_raw/files_to_fix/lake_plant_raw.kml"))
# 
# dk_lakes_subset %>% 
#   st_write(paste0(getwd(), "/data_raw/files_to_fix/dk_lakes_subset.kml"))
# 
# #Import edited chem and plant files and add gml_ids to chem and plant tables
# chem_newlakes <- read_csv(paste0(getwd(), "/data_processed/chem_newlakes.csv"))
# 
# dk_lakes_subset %>% 
#   st_join(lake_chem %>% 
#   select(site_id, site_name) %>% 
#   distinct()) %>% st_drop_geometry() %>% View()
