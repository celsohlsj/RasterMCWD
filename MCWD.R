## MCWD (Maximum Cumulative Water Deficit) Script ##
# Reference: https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2006GL028946 #
# Library
library(raster)

setwd("Working-Directory") # Directory with Monthly Rainfall Rasters

# List of Monthly Rainfall Rasters
month.rainfall = list.files("./", pattern = '.tif$', full.names = T)

wd = stack(month.rainfall)-100 # 100 is the evapotranspiration in mm/month

# MCWD Function
mcwd.f = function(x){
  result= as.numeric(x)
  for(i in 1:length(result)){
    wdn = result[i]
    wdn1 = result[i-1]
    
    if(i==1){
      if(wdn>0){ result[i]=0}
      else{result[i]=wdn}
    }
    
    if(i!=1){
      cwd = wdn1+wdn
      if( cwd < 0){ result[i]=cwd}
      else{result[i]=0}
    }
  }
return(result)  
}


# Applying the Function
cwd = calc(wd, fun = mcwd.f)


# Determining the Annual MCDW
ano = 2006 # Start Year of the Temporal Series
for (i in seq(1,132,12)) { # Replace 132 by the Total Months of the Time Series
  cwd.a = cwd[[i:(i+11)]]
  mcwd.a = min(cwd.a)
  
  # Saving the Annual MCWD
  print(paste0(ano, " - ", Sys.time()))
  writeRaster(mcwd.a, paste0("./MCWD/MCWD_", ano, ".tif")) # Create a Folder Called "MCWD" in your Working Directory
  ano = ano+1
}
