#coefm optim function

#remove everything
rm(list=ls())

#Load Packages
library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)
library(readr)

#Set WD
setwd("/Users/theodore/PEST")

# Read Observations
obs = read.csv("sensitivity_compare_data.csv")

#Function to change value of coefm

parameters_can = function(coefm){
  
  setwd("/Users/theodore/PEST")
  tx = readLines("params_text_flag.can")
  
  if(length(coefm)==1){
    tx2 = gsub(pattern = "!COEFM!", replacement = coefm, x = tx)
  }else{
    tx2 = gsub(pattern = "!COEFM!", replacement = coefm[c_coefm], x = tx)
  }
  
  writeLines(tx2, con = "params_text.can")
  unlink(tx)
  unlink(tx2)
}


#function to run Agro-IBIS

run_IBIS = function(coefm){
  parameters_can(coefm)
  system("cp -r restart_after_CS_ROT_1971_2012_illinois/ restart")
  system("cp yearsrun_cp.dat yearsrun.dat")
  system("./ibis > text.txt")
  
  #Start code for output data
  
  setwd("/Users/theodore/PEST/output/yearly/")
  
  crops = nc_open('crops.nc')
  yield = ncvar_get(crops, "cropyld")
  nc_close(crops)
  
  #Get Biomass Output and place into matrix 
  setwd("/Users/theodore/PEST/output/daily")
  biomass = nc_open('biomass.nc')
  
  cbiol = ncvar_get(biomass, "cbiol")
  daily_biomass_l = cbiol[13,]
  
  cbiog = ncvar_get(biomass, "cbiog")
  daily_biomass_g = cbiog[13,]
  
  cbios = ncvar_get(biomass, "cbios")
  daily_biomass_s = cbios[13,]
  
  nc_close(biomass)
  
  #Create the sim matrix
  sim = data.frame("stem_bio_177" = daily_biomass_s[177], "stem_bio_191" = daily_biomass_s[191], "stem_bio_204" = daily_biomass_s[204], "stem_bio_219" = daily_biomass_s[219], "stem_bio_235" = daily_biomass_s[235], "leaf_bio_177" = daily_biomass_l[177], "leaf_bio_191" = daily_biomass_l[191], "leaf_bio_204" = daily_biomass_l[204], "leaf_bio_219" = daily_biomass_l[219],"leaf_bio_235" = daily_biomass_l[235],"grain_bio_177" = daily_biomass_l[177],"grain_bio_191" = daily_biomass_l[191],"grain_bio_204" = daily_biomass_l[204],"grain_bio_219" = daily_biomass_l[219],"grain_bio_235" = daily_biomass_l[235],"yield" = yield[13])
  return(sim)
}

#Relative residual sums of squares
rrss = function(obs,sim){
  ans = sum(((obs-sim))^2,na.rm = T)
  return(ans)
}

objf = function(coefm,obs){
  if (length(coefm) != 1)
    stop("vector of incorrect length")
  
  sim = run_IBIS(coefm)
  ans = rrss(obs,sim)
  ans
}

#op.nm = optim(c(9.0),objf, obs = obs, method = "Brent", lower = 1.0, upper = 15.0)
opti_ibis = optimize(objf,c(1,15),obs = obs)
