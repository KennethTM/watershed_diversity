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
         bathy_vol = map2_dbl(bathy_fun, bathy_zmax, ~approx_bathy_integrate(.x, 0, .y)),
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
         ph_ph = pH_pH, tp_mg_l = `Phosphor, total-P_mg/l`, secchi_depth_m = Sigtdybde_m)

lake_chem_sf <- lake_chem %>% 
  st_as_sf(coords = c("xutm_euref89_zone32", "yutm_euref89_zone32"), crs = dk_epsg)

st_write(lake_chem_sf, dsn = gis_database, layer = "lake_chem", delete_layer = TRUE)







# #Secchi depth and chemistry data
# #Note: lake id's in secchi and chemistry data do not match that of fish data
# lake_secchi_raw <- read_xlsx(paste0(getwd(), "/data_raw/", "odaforalle_secchi_lake.xlsx"))
# lake_chem_raw <- read_xlsx(paste0(getwd(), "/data_raw/", "odaforalle_chemistry_lake.xlsx"))
# 
# #Combine data, average across depth and calculate mean across year
# lake_secchi_chem <- bind_rows(lake_chem_raw, lake_secchi_raw) %>% 
#   mutate(system = "lake", date = ymd(Startdato), year = year(date), month = month(date),
#          Xutm_Euref89_Zone32 = as.numeric(coalesce(Xutm_Euref89_Zone32, Xutm_Euref89_Zone32_ZONE32)), 
#          Yutm_Euref89_Zone32 = as.numeric(coalesce(Yutm_Euref89_Zone32, Yutm_Euref89_Zone32_ZONE32)),
#          Parameter_cor = ifelse(Parameter == "Chlorophyl (ukorrigeret)", "Chlorophyl A", Parameter),
#          var_unit = paste0(Parameter_cor, "_", Enhed),
#          avg_sample_depth_m_chem = parse_number(`GennemsnitsDybde i m`, locale = locale(decimal_mark = ",")),
#          avg_sample_depth_m = ifelse(is.na(avg_sample_depth_m_chem), 0, avg_sample_depth_m_chem),
#          summer = ifelse(month %in% c(5:9), "summer", "not summer")) %>% 
#   filter(summer == "summer") %>% 
#   select(system, lake_name = ObservationsStedNavn, site_id_2 = ObservationsStedNr, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, 
#          avg_sample_depth_m, var_unit, value = Resultat, year, date) %>% 
#   group_by(system, lake_name, site_id_2, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, var_unit, year, date, avg_sample_depth_m) %>% 
#   summarise(value_mean_dups = mean(value)) %>% 
#   summarise(value_surface = value_mean_dups[which.min(avg_sample_depth_m)]) %>% 
#   summarise(value_mean_year = mean(value_surface)) %>% 
#   ungroup() %>% 
#   spread(var_unit, value_mean_year) %>% 
#   rename(alk_mmol_l = `Alkalinitet,total TA_mmol/l`, chla_ug_l = `Chlorophyl A_µg/l`, tn_mg_l = `Nitrogen,total N_mg/l`, 
#          ph_ph = pH_pH, tp_mg_l = `Phosphor, total-P_mg/l`, secchi_depth_m = Sigtdybde_m) %>% 
#   filter(Yutm_Euref89_Zone32 > 700000,
#          year >= 2006) 
# 
# lake_secchi_chem_sf <- lake_secchi_chem %>% 
#   st_as_sf(coords = c("Xutm_Euref89_Zone32", "Yutm_Euref89_Zone32"), crs = dk_epsg)
# 
# #Write raw secchi_chem stations to .sqlite file and align coordinates with lake polygons
# # lake_secchi_chem_sf %>%
# #   select(site_id_2, name) %>%
# #   distinct(.keep_all = TRUE) %>%
# #   st_write(paste0(getwd(), "/data_raw/lake_secchi_chem_raw.sqlite"), delete_dsn = TRUE)
# 
# st_write(lake_secchi_chem_sf, dsn = gis_database, layer = "lake_secchi_chem", delete_layer = TRUE)
# 
# #Lake submerged macrophyte and mean depth data
# #Downloaded from miljøportalen, not all open as odaforalle
# sv_files <- list.files(paste0(getwd(), "/data_raw/veg_data"), full.names = TRUE)
# names(sv_files) <- str_sub(basename(sv_files), end = -5)
# 
# #Load all downloaded files
# sv_list1 <- lapply(sv_files[-c(7, 8, 12)], function(path){read_table(path, locale = locale(decimal_mark = ",", encoding = "ISO-8859-1"))})
# sv_list2 <- lapply(sv_files[c(7, 8, 12)], function(path){read_table2(path, locale = locale(decimal_mark = ",", encoding = "ISO-8859-1"))})
# sv_list <- c(sv_list1, sv_list2)
# 
# #Join nescessary files
# #Raw data
# lake_plants_raw <- sv_list$sv_bertotal %>% 
#   select(-X10) %>% 
#   left_join(select(sv_list$std_enhed, enhed_std = kode, enhed = betegn)) %>% 
#   left_join(select(sv_list$sv_berparam, param_std = kode, param = betegn)) %>% 
#   left_join(sv_list$sv_dybdeomr) %>% 
#   left_join(sv_list$sv_tilsyn2)
# 
# #Joined with station info
# lake_plants_stat <- lake_plants_raw %>% 
#   filter(dybdeomr_nr == 0 & bertype == 0) %>% 
#   mutate(system = "lake", date = dmy(startdato), year = year(date),
#          param_unit = paste0(param, "_", enhed)) %>% 
#   select(system, recopl_id, year, dybdeomr_id, resultat, param_unit) %>% 
#   spread(param_unit, resultat) %>% 
#   left_join(select(sv_list$so_cstation3, navn, recopl_id, utm_zone, utm_x, utm_y))
# 
# #Extract max depth from bathymetry data
# max_depth <- lake_plants_raw %>%
#   group_by(recopl_id) %>%
#   summarise(max_depth_m = max(til_dybde, na.rm = TRUE))
# 
# #Collect and clean plant data
# lake_plants <- lake_plants_stat %>% 
#   mutate(zmean_m = Søvolumen_m3/Søreal_m2) %>% 
#   left_join(max_depth) %>% 
#   select(system, year, site_id_3 = recopl_id, name = navn, utm_zone, utm_x, utm_y, area_m2 = Søreal_m2,
#          zmean_m, max_depth_m, volume_m3 = Søvolumen_m3, mean_plant_height_m = `Middel plantehøjde_m`,
#          plant_area_m2 = `Plantedækket areal_m2`, plant_vol_m3 = `Plantefyldt volumen_m3`,
#          plant_area_perc = `Relativ plantedækket areal_pct.`, plant_vol_perc = `Relativ plantefyldt volumen_pct.`) %>% 
#   distinct()
# 
# #Convert all coords to utm zone 32
# lake_plants_zone32 <- lake_plants %>% 
#   filter(utm_zone == "U32")
# 
# lake_plants_zone33_to_32 <- lake_plants %>% 
#   filter(utm_zone == "U33") %>% 
#   st_as_sf(coords=c("utm_x", "utm_y"), crs = dk_epsg+1) %>% 
#   st_transform(dk_epsg) %>% 
#   bind_cols(as.data.frame(st_coordinates(.))) %>% 
#   st_drop_geometry() %>% 
#   rename(utm_x=X, utm_y=Y)
# 
# #Convert to spatial and write to database
# lake_plants_sf <- bind_rows(lake_plants_zone32, lake_plants_zone33_to_32) %>% 
#   select(-utm_zone) %>% 
#   filter(between(utm_x, 4*10^5, 9*10^5),
#          between(utm_y, 6010000, 6400000)) %>% 
#   st_as_sf(coords = c("utm_x", "utm_y"), crs = dk_epsg)
# 
# # lake_plants_sf %>%
# #   select(site_id_3, name) %>%
# #   distinct(.keep_all = TRUE) %>%
# #   st_write(paste0(getwd(), "/data_raw/lake_plants_raw.sqlite"), delete_dsn = TRUE)
# 
# st_write(lake_plants_sf, dsn = gis_database, layer = "lake_plants", delete_layer = TRUE)
# 
# #lake_plants and lake_secchi_chem exported as raw .sqlite files and edited by EK so lake stations align with lake_subset
# #Write the edited files to database
# ogr2ogr(paste0(getwd(), "/data_raw/lake_secchi_chem_raw_EK_Fixed.sqlite"),
#         gis_database,
#         nln = "lake_secchi_chem_edit",
#         update = TRUE,
#         overwrite = TRUE,
#         a_srs = paste0("EPSG:", dk_epsg))
# 
# ogr2ogr(paste0(getwd(), "/data_raw/lake_plants_raw_EK_Fixed.sqlite"),
#         gis_database,
#         nln = "lake_plants_edit",
#         update = TRUE,
#         overwrite = TRUE,
#         a_srs = paste0("EPSG:", dk_epsg))
