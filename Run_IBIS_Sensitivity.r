#Code to run IBIS parameter set factorially
#Updated TMH 1/14/20 for KMF
#Updated TMH 6/22/20 for TMH
#Updated TMH 11/2/20 for BP
#########################################################
# Change the path name for your working directory here
file = "/Users/theodore/sensitivity_km_ver/"
#########################################################

setwd(file)
library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)
library(readr)


#Set-up of the function which will control the search and replacement of the different variables given to the function
parameters_soi = function(file_soi,porosity = 0.501, fc = 0.33,wp=0.133,bexp=4.7, aep = 0.21, shc = 1.8889){
  
  tx = readLines(file_soi)
  
  if(length(porosity)==1){
    tx2 = gsub(pattern = "!POROSITY!", replacement = porosity, x = tx)
  }else{
    tx2 = gsub(pattern = "!POROSITY!", replacement = porosity[c_porosity], x = tx)
  }
  
  if(length(fc)==1){
    tx2 = gsub(pattern = "!FC!", replacement = fc, x = tx2)
  }else{
    tx2 = gsub(pattern = "!FC!", replacement = fc[c_fc], x = tx2)
  }
  
  
  if(length(wp)==1){
    tx2 = gsub(pattern = "!WP!", replacement = wp, x = tx2)
  }else{
    tx2 = gsub(pattern = "!WP!", replacement = wp[c_wp], x = tx2)
  }
  
  if(length(bexp)==1){
    tx2 = gsub(pattern = "!BEXP!", replacement = bexp, x = tx2)
  }else{
    tx2 = gsub(pattern = "!BEXP!", replacement = bexp[c_bexp], x = tx2)
  }
  
  if(length(aep)==1){
    tx2 = gsub(pattern = "!AEP!", replacement = aep, x = tx2)
  }else{
    tx2 = gsub(pattern = "!AEP!", replacement = aep[c_aep], x = tx2)
  }
  
  if(length(shc)==1){
    tx2 = gsub(pattern = "!SHC!", replacement = shc, x = tx2)
  }else{
    tx2 = gsub(pattern = "!SHC!", replacement = shc[c_shc], x = tx2)
  }
  
  writeLines(tx2, con = "params.soi")
  unlink(tx)
  unlink(tx2)
}

parameters_crp = function(file_crp,arooti = 0.35,arootf = 0.20,astemf=0.35,fleafi=0.47, grnfill = .49, hybgdd = 1600){
  
  tx = readLines(file_crp)
  
  if(length(arooti)==1){
    tx2 = gsub(pattern = "!AROOTI!", replacement = arooti, x = tx)
  }else{
    tx2 = gsub(pattern = "!AROOTI!", replacement = arooti[c_arooti], x = tx)
  }
  
  if(length(arootf)==1){
    tx2 = gsub(pattern = "!AROOTF!", replacement = arootf, x = tx2)
  }else{
    tx2 = gsub(pattern = "!AROOTF!", replacement = arootf[c_arootf], x = tx2)
  }
  
  
  if(length(astemf)==1){
    tx2 = gsub(pattern = "!ASTEMF!", replacement = astemf, x = tx2)
  }else{
    tx2 = gsub(pattern = "!ASTEMF!", replacement = astemf[c_astemf], x = tx2)
  }
  
  if(length(fleafi)==1){
    tx2 = gsub(pattern = "!FLEAFI!", replacement = fleafi, x = tx2)
  }else{
    tx2 = gsub(pattern = "!FLEAFI!", replacement = fleafi[c_fleafi], x = tx2)
  }
  
  if(length(grnfill)==1){
    tx2 = gsub(pattern = "!GRNFILL!", replacement = grnfill, x = tx2)
  }else{
    tx2 = gsub(pattern = "!GRNFILL!", replacement = grnfill[c_grnfill], x = tx2)
  }
  
  if(length(hybgdd)==1){
    tx2 = gsub(pattern = "!HYBGDD!", replacement = hybgdd, x = tx2)
  }else{
    tx2 = gsub(pattern = "!HYBGDD!", replacement = hybgdd[c_hybgdd], x = tx2)
  }
  
  writeLines(tx2, con = "params.crp")
  unlink(tx)
  unlink(tx2)
}


#Function to change can files
parameters_can = function(file_can,coefm = 10.75, specla = 70,gamma = 0.015, vcmax = 110.0){
  
  tx = readLines(file_can)
  
  if(length(coefm)==1){
    tx2 = gsub(pattern = "!COEFM!", replacement = coefm, x = tx)
  }else{
    tx2 = gsub(pattern = "!COEFM!", replacement = coefm[c_coefm], x = tx)
  }
  
  if(length(specla)==1){
    tx2 = gsub(pattern = "!SPECLA!", replacement = specla, x = tx2)
  }else{
    tx2 = gsub(pattern = "!SPECLA!", replacement = specla[c_specla], x = tx2)
  }
  
 if(length(gamma)==1){
    tx2 = gsub(pattern = "!GAMMA!", replacement = gamma, x = tx2)
  }else{
    tx2 = gsub(pattern = "!GAMMA!", replacement = gamma[c_gamma], x = tx2)
  }

 if(length(vcmax)==1){
    tx2 = gsub(pattern = "!VCMAX!", replacement = vcmax, x = tx2)
  }else{
    tx2 = gsub(pattern = "!VCMAX!", replacement = vcmax[c_vcmax], x = tx2)
  }



  writeLines(tx2, con = "params.can")
  unlink(tx)
  unlink(tx2)
}


#Function to compile and run IBIS
run_IBIS = function(){
  system("./ibis")
}


#####################################################
#Inputs for the parameter changing function
file_crp = "params_flag.crp"
file_can = "params_flag.can"
file_soi = "params_flag.soi"
######################################
#Set values to run on
######################################
arooti = c(.2,.35,.5)
arootf = c(0.1,0.2,0.3)
astemf = c(0.2,0.3,0.4)
fleafi = c(.6,.75,.9)
hybgdd = c(1400,1500,1600)
grnfill = c(0.5,0.6,0.7)
coefm = c(8,9,10)
specla = c(40,45,50)
vcmax = c(90,100,110,120)
gamma = c(0.010,0.020) 
porosity = c(0.501)
fc = c(0.33)
wp = c(0.133)
bexp = c(4.7)
aep = c(0.21)
shc = c(1.8889)
######################################

################################################## Change the variables in iterations to be the variables you include above
#Enter number of parameters in the runs
parameters = 16
outputs = 1 #Just yield in the main yearly output file. Biomass is in separate data frames because daily
iterations = (length(arooti)*length(arootf)*length(astemf)*length(fleafi)*length(hybgdd)*length(grnfill)*length(vcmax)*length(coefm)*length(specla)*length(gamma)*length(porosity)*length(fc)*length(wp)*length(bexp)*length(aep)*length(shc))
####################################################
#Initialize number counter for runs
run_num = 0

#Create data frame for the values
ibis_data = data.frame(matrix(ncol = (parameters + outputs), nrow = iterations))
cbiol_tot = matrix(ncol = 365, nrow = iterations)
cbior_tot = matrix(ncol = 365, nrow = iterations)
cbiog_tot = matrix(ncol = 365, nrow = iterations)
cbios_tot = matrix(ncol = 365, nrow = iterations)

#Run the model for all values listed above
for(c_arooti in 1:length(arooti)){
  for(c_arootf in 1:length(arootf)){
      for(c_astemf in 1:length(astemf)){
        for(c_fleafi in 1:length(fleafi)){
          for(c_grnfill in 1:length(grnfill)){
            for(c_hybgdd in 1:length(hybgdd)){
              for(c_coefm in 1:length(coefm)){
                  for(c_specla in 1:length(specla)){
                    for(c_gamma in 1:length(gamma)){
                      for(c_vcmax in 1:length(vcmax)){
                        for(c_porosity in 1:length(porosity)){
                          for(c_fc in 1:length(fc)){
                            for(c_wp in 1:length(wp)){
                              for(c_bexp in 1:length(bexp)){
                                for(c_aep in 1:length(aep)){
                                  for(c_shc in 1:length(shc)){

                     	 parameters_crp(file_crp,arooti,arootf,astemf,fleafi,grnfill,hybgdd)
                     	 parameters_can(file_can,coefm,specla,gamma,vcmax)
                     	 parameters_soi(file_soi,porosity,fc,wp,bexp,aep,shc)

                  
                      	run_IBIS()
                      	run_num = run_num + 1
                  
              	     	#Start code for data interpreting
                  
                     	 setwd(file.path(file,"output/daily/"))
                      	biomass = nc_open('biomass.nc')
                  
                      	cbiol = ncvar_get(biomass, "cbiol")
                      	cbiol_tot[run_num,] = cbiol[13,]
                  
                      	cbior = ncvar_get(biomass, "cbior")
                      	cbior_tot[run_num,] = cbior[13,]
                  
                      	cbiog = ncvar_get(biomass, "cbiog")
                      	cbiog_tot[run_num,] = cbiog[13,]
                  
                      	cbios = ncvar_get(biomass, "cbios")
                      	cbios_tot[run_num,] = cbios[13,]
                  
                      	nc_close(biomass)
                  
                      	setwd(file.path(file,"output/yearly/"))
                      	crops = nc_open('crops.nc')
                      	yield = ncvar_get(crops, "cropyld")
                      	nc_close(crops)
                  
                      	setwd(file)
                  
                      	#Put data into data frame
                      	ibis_data[run_num,1] = arooti[c_arooti]
                      	ibis_data[run_num,2] = arootf[c_arootf]
                      	ibis_data[run_num,3] = astemf[c_astemf]
                      	ibis_data[run_num,4] = fleafi[c_fleafi]
                      	ibis_data[run_num,5] = grnfill[c_grnfill]
                      	ibis_data[run_num,6] = hybgdd[c_hybgdd]
                      	ibis_data[run_num,7] = coefm[c_coefm]
                      	ibis_data[run_num,8] = specla[c_specla]
                      	ibis_data[run_num,9] = gamma[c_gamma]
			                  ibis_data[run_num,10] = vcmax[c_vcmax]
			                  ibis_data[run_num,11] = vcmax[c_porosity]
			                  ibis_data[run_num,12] = vcmax[c_fc]
			                  ibis_data[run_num,13] = vcmax[c_wp]
			                  ibis_data[run_num,14] = vcmax[c_bexp]
			                  ibis_data[run_num,15] = vcmax[c_aep]
			                  ibis_data[run_num,16] = vcmax[c_shc]
                      	ibis_data[run_num,17] = yield[13]

                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
            }
          }
        }
      }
  }
}

######################################################################
#setwd
setwd(file)

# Change the name of the csv file so it will not override the previous
write.csv(ibis_data,"ibis_data_factorial_km_ver.csv")
write.csv(cbios_tot,"ibis_data_factorial_km_ver_cbios.csv")
write.csv(cbiog_tot,"ibis_data_factorial_km_ver_cbiog.csv")
write.csv(cbiol_tot,"ibis_data_factorial_km_ver_cbiol.csv")
write.csv(cbior_tot,"ibis_data_factorial_km_ver_cbior.csv")

######################################################################
