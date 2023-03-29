library(fasterize);library(sf);library(raster);library(rgdal);
library(tidyverse);library(readxl);library(lubridate);library(rgrass7);
library(link2GI);library(patchwork);library(lwgeom);
library(exactextractr);library(mapview);library(corrplot);
library(vegan);library(viridisLite);
library(piecewiseSEM);library(lme4);library(emmeans);library(FSA)
library(pairwiseAdonis);library(rmapshaper)

#library(gdalUtils) #No longer available, use 'gdalUtilities' package instead

#Load libraries and define useful functions and constants used in other scripts

#Danish projection, as EPSG number, used for spatial analysis (UTM ZONE 32)
dk_epsg <- 25832

#Path for spatial vector database
gis_database <- paste0(getwd(), "/data_processed/gis_database.sqlite")

#Figure sizing. For most journals the figures should be 39 mm, 84 mm, 129 mm, or 174 mm wide and not higher than 234 mm.
#ggplot theme
theme_pub <- theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "black"), 
        panel.border = element_rect(fill = NA, colour = "black"),
        strip.background = element_rect(fill = "white"))
theme_set(theme_pub)

#Function for counting fish species caught per sampling, if na no fish caught
keep_na_for_zero_catch <- function(spec_col){
  if(all(is.na(spec_col[[1]]))){
    df <- tibble(col_name = NA)
    names(df) <- names(spec_col)
    return(df)
  }else{
    return(na.omit(spec_col))
  }
}

#Code to calculate variance inflation factor
#From Zuur et al MEE 2009
#VIF
myvif <- function(mod) {
  v <- vcov(mod)
  assign <- attributes(model.matrix(mod))$assign
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) stop("The model contains fewer than 2 terms")
  if (length(assign) > dim(v)[1] ) {
    diag(tmp_cor)<-0
    if (any(tmp_cor==1.0)){
      return("Sample size is too small, 100% collinearity is present")
    } else {
      return("Sample size is too small")
    }
  }
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/2Df)")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) {
    result <- data.frame(GVIF=result[, 1])
  } else {
    result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  }
  invisible(result)
}

corvif <- function(dataz) {
  dataz <- as.data.frame(dataz)
  #correlation part
  cat("Correlations of the variables\n\n")
  tmp_cor <- cor(dataz,use="complete.obs")
  print(tmp_cor)
  
  #vif part
  form    <- formula(paste("fooy ~ ",paste(strsplit(names(dataz)," "),collapse=" + ")))
  dataz   <- data.frame(fooy=1,dataz)
  lm_mod  <- lm(form,dataz)
  
  cat("\n\nVariance inflation factors\n\n")
  print(myvif(lm_mod))
}

clean_names <- function(df){
  col_names <- names(df)
  col_names <- make.names(col_names)
  col_names <- str_to_lower(col_names)
  col_names <- sub('[[:punct:]]+$', '', col_names)
  col_names <- gsub(".", "_", col_names, fixed = TRUE)
  col_names <- gsub("___", "_", col_names, fixed = TRUE)
  col_names <- gsub("__", "_", col_names, fixed = TRUE)
  
  names(df) <- col_names
  return(df)
}

#Functions for processing lake area-depth relationships
approx_bathy <- function(data){
  if(nrow(data) == 1){
    fun <- NULL
  }else if(min(data$dybden_fra_i_meter, na.rm = TRUE) != 0){
    fun <- NULL #One exception
  }else{
    depths <- c(max(data$dybden_til_i_meter), data$dybden_fra_i_meter)
    areas <- c(0, data$area_accum)
    fun <- approxfun(depths, areas) 
  }
  return(fun)
}

approx_bathy_integrate <- function(fun, from, to){
  ifelse(is.null(fun), NA, integrate(fun, from, to, stop.on.error = FALSE)$value)
}

approx_bathy_layer_area <- function(fun, from, to){
  ifelse(is.null(fun), NA, exec(fun, from) - exec(fun, to))
}

