# Function for extracting NPP0 raster values for each polygon
get_NPP0_intensity_per_polygon <- function()
{
  if(! file.exists("./input/EPIP/NPP0/NPP0_of_polygons.R"))
  {
    # Loading mining polygons
    mine_polygons <- sf::read_sf("./input/EPIP/polygons/global_mining_polygons_v1.gpkg", fid_column_name = "PID")
    
    # Set path/filename of/to NPP0 raster data
    filename <- "tn0_all_gcm.asc"
    path <- paste0("./input/EPIP/NPP0/", filename)
    
    raster_data <- raster::raster(file.path(path))
    
    # plot(raster_data)
    # plot(st_as_sf(mine_polygons$geom[2]),add=T)
    # plot(raster::crop(raster_data, st_as_sf(mine_polygons$geom[2])))
    
    statistic_vals <- vector(mode = "numeric", length = length( 1:nrow(mine_polygons) ) )
    ptm <- proc.time()
    
    for(yy in 1:nrow(mine_polygons))
    {
      statistic_vals[yy] <- unlist( raster::extract(raster_data, st_centroid( st_as_sf(mine_polygons$geom[yy]) ) ,
                                                    method='bilinear', wkt = 'wkt', crs = 4326) )
    }
    
    save(statistic_vals, file = "./input/EPIP/NPP0/NPP0_of_polygons.R")
    
    print("Time needed for statistic calculation:")
    print(proc.time() - ptm)
  }else
  {
    load( file = "./input/EPIP/NPP0/NPP0_of_polygons.R" )
  }
    
  # Set NA to zero for mines that are not covered by the NPP/HANPP grid data
  statistic_vals[is.na(statistic_vals)] <- 0
  
  return(statistic_vals)
}