setwd("H:/Zambia/Zam_Dist07/Dist07")

library(terra)
library(geodata)
library(tidyr)
library(dplyr)

zam_districts <- terra::vect("ZAM.shp")
head(zam_districts)
plot(zam_districts)

# Population density
# Bring in Population density 2020 form worldpop -https://hub.worldpop.org/geodata/summary?id=29690
pop_density <- terra::rast("zmb_ppp_2020_UNadj.tif")
plot(pop_density)


# Download files from geodata
# set geodata path
geodata_path("H:/Zambia/Zam_Dist07/Dist07/geodata")

# Download from Geodata
# travel time to towns of 100k+
#travel <- geodata::travel_time(to="city", size=5, up=TRUE)
travel <- rast("H:/Zambia/Zam_Dist07/Dist07/geodata/travel/travel_time_to_cities_u5.tif")
# Crop to Zambia boundary
ttcity <- terra::crop(travel, zam_districts)
plot(ttcity)


# SOC
# path <- "H:/Zambia/Zam_Dist07/Dist07/geodata/SOC"
# SOC <- soil_af(var="SOC", depth=5, path=path)
SOC <- terra::rast("H:/Zambia/Zam_Dist07/Dist07/geodata/SOC/soil_af/af_soc_0-5cm_30s.tif")
# Crop to Zambia boundary
soc <- terra::crop(SOC, zam_districts)
plot(soc)

# PH
# path <- "H:/Zambia/Zam_Dist07/Dist07/geodata/pH"
# pH <- soil_af(var="pH", depth=5, path=path)
pH <- terra::rast("H:/Zambia/Zam_Dist07/Dist07/geodata/pH/soil_af/af_ph_0-5cm_30s.tif")
# Crop to Zambia boundary
PH <- terra::crop(pH, zam_districts)
plot(PH)

# sand
# path <- "H:/Zambia/Zam_Dist07/Dist07/geodata/sand"
# sand <- soil_af(var="sand", depth=5, path=path)
sand <- terra::rast("H:/Zambia/Zam_Dist07/Dist07/geodata/sand/soil_af/af_sand_0-5cm_30s.tif")
# Crop to Zambia boundary
Sand <- terra::crop(sand, zam_districts)
plot(Sand)

# create reference raster
zambia_extent <- ext(zam_districts) |> floor()
r <- crop(rast(res=1/12), zambia_extent)
## Harmonize rasters to national boundaries and common resolution
popd   <- resample(pop_density, r)
ttcity <- resample(ttcity, r)
soc <- resample(soc, r)
PH    <- resample(PH, r)
Sand   <- resample(Sand, r)


#Stack the rasters
rstack <- c(popd, ttcity, soc, PH, Sand)
names(rstack)
# Rename raster stack layers
names(rstack) <- c("Pop_dens", "travel_time_to_cities", "SOC", "pH", "sand_percentage")

# plot
plot(rstack[[5]])
plot(zam_districts, add=TRUE)

# Extract Pop_dens, travel_time_to_cities, SOC, pH, sand_percentage values to districts
r_values <- terra::extract(rstack, zam_districts, fun = mean, na.rm = TRUE, method = "bilinear")
r_values

# Add districts to the extracted data
r_values_Dist_merge <- cbind(zam_districts, r_values)
head(r_values_Dist_merge)


# Bring in monthly rainfall data from chirps, downloaded from here - https://data.chc.ucsb.edu/products/CHIRPS-2.0/africa_monthly/tifs/
# Chirps are monthly data from 2000 - 2023
chirps_path <- "H:/Zambia/Zam_Dist07/Dist07/Chirps"
chirps_files <- list.files(chirps_path, pattern = ".tif$", full.names = TRUE)
# Read all CHIRPS data files into a SpatRaster 
chirps_rasters <- rast(chirps_files)

#crop to Zambia boundary
Chirps_zam_cropped <- terra::crop(chirps_rasters, zam_districts)

#Replace -9999 with NA
zam_chirps_monthly <- classify(Chirps_zam_cropped, cbind(-9999,NA))
plot(zam_chirps_monthly)

# Extract mean monthly rainfall for each district
Rainfall_extract_values <- terra::extract(zam_chirps_monthly, r_values_Dist_merge, fun = mean, na.rm = TRUE, method = "bilinear")

# Add district to the extracted data
Rainfall_Dist_merge <- cbind(r_values_Dist_merge, Rainfall_extract_values)
head(Rainfall_Dist_merge)

# Extract column names from Rainfall_Dist_merge
rainfall_columns <- names(Rainfall_Dist_merge)
rainfall_columns

# Identify columns containing rainfall data
rainfall_layers <- grep("chirps-v2.0", rainfall_columns, value = TRUE)

# Extract year and month from the layer names
rainfall_metadata <- data.frame(
  OriginalName = rainfall_layers,
  Year = as.numeric(sub("chirps-v2.0\\.(\\d{4})\\..*", "\\1", rainfall_layers)),
  Month = as.numeric(sub("chirps-v2.0\\.\\d{4}\\.(\\d{2})", "\\1", rainfall_layers))
)

print(head(rainfall_metadata))

# Rename columns in the raster object
# Modify column names to include year and month, with a separator and a 10-character limit
names(Rainfall_Dist_merge) <- c(
  names(Rainfall_Dist_merge)[!grepl("chirps-v2.0", rainfall_columns)],
  paste0("Rai_", substr(rainfall_metadata$Year, 3, 4), "_", sprintf("%02d", rainfall_metadata$Month))
)

# Check renamed columns
names(Rainfall_Dist_merge)
head(Rainfall_Dist_merge)
# Rename ID_1 to ID and remove ID_2 and ID_3 columns
names(Rainfall_Dist_merge)[names(Rainfall_Dist_merge) == "ID_1"] <- "ID"
Rainfall_Dist_merge <- Rainfall_Dist_merge[, !(names(Rainfall_Dist_merge) %in% c("ID_2", "ID_3"))]
head(Rainfall_Dist_merge)

# save the shapefile with additional data
output_shapefile <- "H:/Zambia/Zam_Dist07/Dist07/NewZam/ZAM_Updated.shp"
# Save the SpatVector as a shapefile
writeVector(Rainfall_Dist_merge, output_shapefile, overwrite = TRUE)

# Save as CSV
write.csv(Rainfall_Dist_merge, "Zam_new.csv", row.names = FALSE)
