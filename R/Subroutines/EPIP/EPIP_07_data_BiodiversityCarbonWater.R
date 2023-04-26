######################################################################
library(sf)
library(raster)
library(pangaear)


#load root_geometry & download raster data:

source("./R/01_data_roots_geometry.R")

### data available from Zenodo: https://doi.org/10.5281/zenodo.5006332
zipfile <- "https://zenodo.org/record/5006332/files/BiodiversityCarbonWater.zip"
temp <- tempfile()
download.file(zipfile, temp)
unzip(zipfile = temp, exdir = "./data" )

if(!file.exists("./data/polygons/global_mining_polygons_v1.gpkg"))  {
  mine_polygons="10.1594/PANGAEA.910894"
  if("character" %in% class(mine_polygons)){
    dir.create("./data/mine_polygons", showWarnings = FALSE, recursive = TRUE)
    doi <- mine_polygons
    mine_polygons <- NULL
    pangaear::pg_data(doi = doi)
    unzip(pangaear::pg_cache$list(), exdir = "./data/polygons" )
    mine_polygons <- sf::read_sf("./data/polygons/global_mining_polygons_v1.gpkg", fid_column_name = "PID")
  }

  if(!"sf" %in% class(mine_polygons)){
    stop(paste0("mine_polygons is not a sf object or sf data could not be downloaded from PANGAEA ", doi))
  }
}
mine_polygons <- sf::read_sf("./data/polygons/global_mining_polygons_v1.gpkg", fid_column_name = "PID")

#############FUNCTION
get_raster_statistic_of_shapefile <- function(raster_path,indices)
{
  
  raster_data <- raster::raster(file.path(raster_path))
  statistic_vals <- c()
  ptm <- proc.time()
  for(yy in indices)#nrow(roots_geometry))
  {
    statistic_vals <- c(statistic_vals,extract(raster_data, st_as_sf(mine_polygons$geom[yy]), wkt = 'wkt', crs = 4326))
    #statistic_vals <- c(statistic_vals,extract(raster_data, st_as_sf(roots_geometry$geometry[yy]), wkt = 'wkt', crs = 4326))
  }
  print("Time needed for statistic calculation:")
  print(proc.time() - ptm)
  return(statistic_vals)
}

filename <- "minshort_speciestargets_biome.id__carbon__water__esh10km_repruns10_ranked.tif"

score_raw <- get_raster_statistic_of_shapefile(raster_path = paste0("./data/BiodiversityCarbonWater/10km/", filename),
                                               1:nrow(mine_polygons))

save(score_raw,file = "./data/Score_Data_carbon__water__esh10km_repruns10.R")

concordance_polygons_roots <- get_mine_polygons_root_concordance(
  root_path = "data/root_geoms/root_geoms.gpkg", mine_polygons="10.1594/PANGAEA.910894")

unique_poly_in_root <- unique(concordance_polygons_roots$GID_id)

root_score <- list("value" = vector("list", length = length(unique_poly_in_root) ),
                   "GID_id" = unique_poly_in_root )

for(i in 1:nr_poly_in_root )
{
  print(i)
  sel_PID <- concordance_polygons_roots %>% filter(GID_id == root_score$GID_id[i]) %>% pull(PID)
  root_score$value[[i]] <- unlist(score_raw[sel_PID])
}

save(root_score,file = "./data/score_in_root.R")

# filename <- "minshort_speciestargets_biome.id_withPA_carbon__water__esh10km_repruns10_ranked.tif"
# get_raster_statistic_of_shapefile(paste0(root_folder,"data/BiodiversityCarbonWater/10km/", filename),1:10)
# 
# filename <- "minshort_speciestargets_carbon__water__esh10km_repruns10_ranked.tif"
# get_raster_statistic_of_shapefile(paste0(root_folder,"data/BiodiversityCarbonWater/10km/", filename),1:10)
# 
# filename <- "minshort_speciestargetswithPA_carbon__water__esh10km_repruns10_ranked.tif"
# get_raster_statistic_of_shapefile(paste0(root_folder,"data/BiodiversityCarbonWater/10km/", filename),1:10)
