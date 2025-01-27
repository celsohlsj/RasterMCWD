## MCWD (Maximum Cumulative Water Deficit) Script ##
# Reference: https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2006GL028946 #

# Libraries
library(terra)
library(parallel)

# Set working directory
setwd("directory-here") # Directory containing monthly rainfall rasters

# List of monthly rainfall rasters
monthly_rainfall_files <- list.files("./", pattern = '.tif$', full.names = TRUE)

# Load rasters and calculate water deficit
rainfall_stack <- rast(monthly_rainfall_files)
water_deficit <- rainfall_stack - 100 # 100 mm/month constant evapotranspiration

# MCWD function
mcwd_function <- function(values) {
  result <- as.numeric(values)
  for (i in seq_along(result)) {
    current_deficit <- result[i]
    previous_deficit <- ifelse(i == 1, 0, result[i - 1])
    
    if (i == 1) {
      result[i] <- ifelse(current_deficit > 0, 0, current_deficit)
    } else {
      cumulative_deficit <- previous_deficit + current_deficit
      result[i] <- ifelse(cumulative_deficit < 0, cumulative_deficit, 0)
    }
  }
  return(result)
}

# Apply the function in parallel
num_cores <- detectCores() - 1 # Detect available cores and use one less
cl <- makeCluster(num_cores)
clusterExport(cl, list("mcwd_function"))
water_deficit_mcwd <- app(water_deficit, mcwd_function, cores = num_cores)
stopCluster(cl)

# Save monthly MCWD data
if (!dir.exists("./MCWD")) dir.create("./MCWD")
writeRaster(water_deficit_mcwd, "./MCWD/MCWD_Monthly_1981_2020.tif", overwrite = TRUE)

# Calculating annual MCWD
start_year <- 1981 # Start year of the time series
total_months <- nlyr(water_deficit_mcwd) # Total number of months in the time series

for (i in seq(1, total_months, 12)) {
  annual_stack <- water_deficit_mcwd[[i:(i + 11)]]
  annual_mcwd <- app(annual_stack, min, na.rm = TRUE) # Calculate the annual minimum
  
  # Save the annual MCWD
  current_year <- start_year + ((i - 1) / 12)
  print(paste0("Year: ", current_year, " - ", Sys.time()))
  writeRaster(annual_mcwd, paste0("./MCWD/MCWD_", current_year, ".tif"), overwrite = TRUE)
}
