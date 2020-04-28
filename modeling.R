source("libs_and_funcs.R")

#Load data
all_model_data <- readRDS(paste0(getwd(), "/data_processed/all_model_data.rds"))
summary(all_model_data)

#Set up data
model_df <- all_model_data %>% 
  st_drop_geometry() %>% 
  filter(!is.na(basin_id)) %>% 
  select(-year_established, -year, -gml_id, -n_spec_lake, -system, -site_id, -basin_id, -lake_stream_connect) %>% 
  select(-lake_area_m2, -lake_circum_m, -basin_area_m2, -basin_circum_m,
         -lake_circum_log10, -basin_circum_log10,
         -plant_vol_perc, -dist_to_sea_m)

#Examine correlations
cont_preds <- model_df %>% 
  select(-basin_id_fact, -lake_stream_connect_binary, -n_spec_basin, -spec_proportion)

cond_preds_cor <- cor(cont_preds, use = "pairwise.complete.obs")
corrplot.mixed(cond_preds_cor)

#Calculate variance inflation factors (VIF) (Zuur MEE 2009)
#Drop variables such that maximum VIF is not higher than 3 
source("HighstatLib.r")

corvif(cont_preds)

#Binomial model
model_df_scale<- cont_preds %>% 
  mutate_all(list(scale)) %>% 
  bind_cols(select(model_df, basin_id_fact, lake_stream_connect_binary, n_spec_basin, spec_proportion)) %>% 
  select(-plant_area_perc, -zmean_m) %>% #Removed as it is not important
  na.omit()

#Fit ordinary glm
global_glm <- glm(spec_proportion~elevation+lake_dev_ind+
                   secchi_depth_m+pH_pH+alk_meq_l+chla_ug_l+tp_mg_l+lake_basin_area_ratio+
                   basin_area_log10+lake_area_log10+lake_stream_connect_binary,
                 weights=n_spec_basin, 
                 data=model_df_scale, 
                 family="binomial")
summary(global_glm)

#Fit glmm with basin_id as random
global_glmm <- glmer(spec_proportion~elevation+lake_dev_ind+
                    secchi_depth_m+pH_pH+alk_meq_l+chla_ug_l+tp_mg_l+lake_basin_area_ratio+
                    basin_area_log10+lake_area_log10+lake_stream_connect_binary+(1|basin_id_fact),
                  weights=n_spec_basin, 
                  data=model_df_scale, 
                  family="binomial")
summary(global_glmm)

#Test for inclusion of random effect
#p-value 0.0519. Use random effect as LRT test is conservative
anova(global_glmm, global_glm)

#Refit glmm with better accuracy 
glmm_0 <- update(global_glmm, nAGQ = 4)
  
summary(glmm_0)

#Check for overdspersion (residual deviance / df) should approximate 1 
#https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/10-0340.1
deviance(glmm_0)/df.residual(glmm_0) #deviance 242.9 in summary - same conclusion

#Another function, same conclusion
#https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(glmm_0)

#Backward elimination of variables untill all terms are significant
drop1(glmm_0, test = "Chisq")
glmm_1 <- update(glmm_0, . ~ . -lake_dev_ind)

drop1(glmm_1, test = "Chisq")
glmm_2 <- update(glmm_1, . ~ . -chla_ug_l)

drop1(glmm_2, test = "Chisq")
glmm_3 <- update(glmm_2, . ~ . -lake_basin_area_ratio)

drop1(glmm_3, test = "Chisq")
glmm_4 <- update(glmm_3, . ~ . -secchi_depth_m)

drop1(glmm_4, test = "Chisq")
glmm_5 <- update(glmm_4, . ~ . -tp_mg_l)

drop1(glmm_5, test = "Chisq")

#Validate model
plot(glmm_5)

conf_intervals <- confint(glmm_5, method = "profile")

deviance(glmm_5)/df.residual(glmm_5) 

#Plots
preds <- ggpredict(glmm_5, terms = "alk_meq_l")
plot(preds)





#include squared terms
tmp <- update(glmm_5, . ~ . + I(pH_pH^2)+I(lake_area_log10^2)-alk_meq_l) #lake_area^2 improves model, problematic with both alk and ph
drop1(tmp, test = "Chisq")







ph <- ggeffect(tmp, terms = "pH_pH [all]")
elev <- ggeffect(tmp, terms = "elevation [all]")
basin_area <- ggeffect(tmp, terms = "basin_area_log10 [all]")
lake_area <- ggeffect(tmp, terms = "lake_area_log10 [all]")
connect <- ggeffect(tmp, terms = "lake_stream_connect_binary")

ph_plot <- ggplot(data = tbl_df(ph), aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "coral")+
  geom_line() +
  geom_point(data = model_df_scale, aes(pH_pH, spec_proportion), alpha = 0.2)+
  xlab("pH")

elev_plot <- ggplot(data = tbl_df(elev), aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "coral")+
  geom_line() +
  geom_point(data = model_df_scale, aes(elevation, spec_proportion), alpha = 0.2)+
  xlab("Elevation")

basin_area_plot <- ggplot(data = tbl_df(basin_area), aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "coral")+
  geom_line() +
  geom_point(data = model_df_scale, aes(basin_area_log10, spec_proportion), alpha = 0.2)+
  xlab("Basin area")

lake_area_plot <- ggplot(data = tbl_df(lake_area), aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "coral")+
  geom_line() +
  geom_point(data = model_df_scale, aes(lake_area_log10, spec_proportion), alpha = 0.2)+
  xlab("Lake area")

connect_plot <- ggplot(data = tbl_df(connect), aes(x, predicted)) +
  geom_linerange(aes(x = x, ymin = conf.low, ymax = conf.high), col = "coral", size = 2)+
  geom_point(aes(y=predicted))+
  geom_jitter(data = model_df_scale, aes(lake_stream_connect_binary, spec_proportion), alpha = 0.2)+
  xlab("Stream connect")

glmer_plot <- ph_plot+elev_plot+basin_area_plot+lake_area_plot+connect_plot+plot_layout(ncol = 2)

ggsave(paste0(getwd(), "/figures/glmer_plot.png"), glmer_plot, units = "mm", width = 174, height = 200)
