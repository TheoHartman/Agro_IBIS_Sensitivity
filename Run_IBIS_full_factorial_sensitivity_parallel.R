#########################################################
# IBIS factorial sensitivity script (parallel)
#########################################################

#########################################################
# Working directory
#########################################################

file <- "/Users/theodore/sensitivity_km_ver/"
setwd(file)

library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)
library(readr)

#########################################################
# USER INPUT: crop type
#########################################################

crop_type <- "soybean"   # soybean, corn, wheat, miscanthus, sorghum

crop_index <- switch(crop_type,
                     soybean = 13,
                     corn = 14,
                     wheat = 15,
                     miscanthus = 16,
                     sorghum = 17)

print(paste("Running sensitivity analysis for:", crop_type))

#########################################################
# Parameter replacement function
#########################################################

replace_params <- function(template, replacements, outfile){
  
  tx <- readLines(template)
  
  for(i in seq_along(replacements)){
    pattern <- paste0("!", names(replacements)[i], "!")
    tx <- gsub(pattern, replacements[i], tx)
  }
  
  writeLines(tx, outfile)
}

#########################################################
# Parameter ranges (factorial design)
#########################################################

params <- expand.grid(
  
  arooti = c(.2,.35,.5),
  arootf = c(0.1,0.2,0.3),
  astemf = c(0.2,0.3,0.4),
  fleafi = c(.6,.75,.9),
  grnfill = c(0.5,0.6,0.7),
  hybgdd = c(1400,1500,1600),
  
  coefm = c(8,9,10),
  specla = c(40,45,50),
  gamma = c(0.010,0.020),
  vcmax = c(90,100,110,120),
  
  porosity = c(0.501),
  fc = c(0.33),
  wp = c(0.133),
  bexp = c(4.7),
  aep = c(0.21),
  shc = c(1.8889)
  
)

iterations <- nrow(params)

#########################################################
# Function to run one IBIS simulation
#########################################################

run_single <- function(run_num){
  
  vals <- params[run_num,]
  
  run_dir <- file.path(file, paste0("run_", run_num))
  dir.create(run_dir, showWarnings = FALSE)
  
  #####################################################
  # Copy files for separate directions
  #####################################################
  
  file.copy(c("ibis",
              "params_flag.crp",
              "params_flag.can",
              "params_flag.soi"),
            run_dir,
            overwrite = TRUE)
  
  setwd(run_dir)
  
  #####################################################
  # Write parameter files
  #####################################################
  
  replace_params("params_flag.crp",
                 c(AROOTI = vals$arooti,
                   AROOTF = vals$arootf,
                   ASTEMF = vals$astemf,
                   FLEAFI = vals$fleafi,
                   GRNFILL = vals$grnfill,
                   HYBGDD = vals$hybgdd),
                 "params.crp")
  
  replace_params("params_flag.can",
                 c(COEFM = vals$coefm,
                   SPECLA = vals$specla,
                   GAMMA = vals$gamma,
                   VCMAX = vals$vcmax),
                 "params.can")
  
  replace_params("params_flag.soi",
                 c(POROSITY = vals$porosity,
                   FC = vals$fc,
                   WP = vals$wp,
                   BEXP = vals$bexp,
                   AEP = vals$aep,
                   SHC = vals$shc),
                 "params.soi")
  
  #####################################################
  # Run IBIS
  #####################################################
  
  system("./ibis")
  
  #####################################################
  # Read outputs
  #####################################################
  
  biomass <- nc_open("output/daily/biomass.nc")
  
  cbiol <- ncvar_get(biomass,"cbiol")[crop_index,]
  cbior <- ncvar_get(biomass,"cbior")[crop_index,]
  cbiog <- ncvar_get(biomass,"cbiog")[crop_index,]
  cbios <- ncvar_get(biomass,"cbios")[crop_index,]
  
  nc_close(biomass)
  
  crops <- nc_open("output/yearly/crops.nc")
  yield <- ncvar_get(crops,"cropyld")[crop_index]
  nc_close(crops)
  
  setwd(file)
  
  #####################################################
  # Return results
  #####################################################
  
  list(
    params = vals,
    yield = yield,
    cbiol = cbiol,
    cbior = cbior,
    cbiog = cbiog,
    cbios = cbios
  )
}

#########################################################
# Parallel execution
#########################################################

ncores <- detectCores() - 1

results <- mclapply(
  1:iterations,
  run_single,
  mc.cores = ncores
)

#########################################################
# Combine outputs
#########################################################

ibis_data <- data.frame(matrix(ncol = 17, nrow = iterations))

cbiol_tot <- matrix(ncol = 365, nrow = iterations)
cbior_tot <- matrix(ncol = 365, nrow = iterations)
cbiog_tot <- matrix(ncol = 365, nrow = iterations)
cbios_tot <- matrix(ncol = 365, nrow = iterations)

for(i in 1:iterations){
  
  ibis_data[i,1:16] <- results[[i]]$params
  ibis_data[i,17] <- results[[i]]$yield
  
  cbiol_tot[i,] <- results[[i]]$cbiol
  cbior_tot[i,] <- results[[i]]$cbior
  cbiog_tot[i,] <- results[[i]]$cbiog
  cbios_tot[i,] <- results[[i]]$cbios
}

#########################################################
# Save outputs
#########################################################

write.csv(ibis_data,
          paste0("ibis_data_factorial_",crop_type,".csv"))

write.csv(cbios_tot,
          paste0("ibis_cbios_",crop_type,".csv"))

write.csv(cbiog_tot,
          paste0("ibis_cbiog_",crop_type,".csv"))

write.csv(cbiol_tot,
          paste0("ibis_cbiol_",crop_type,".csv"))

write.csv(cbior_tot,
          paste0("ibis_cbior_",crop_type,".csv"))

print("Parallel sensitivity analysis complete.")