
#DISTANCE FUNCTION:
st_distance_par <- function(sf_df1, sf_df2, n_cores, ...){
  
  # Create a vector to split the data set up by.
  split_vector <- sort(rep(1:n_cores, each = nrow(sf_df1) / n_cores, length.out = nrow(sf_df1)))
  split_results <- mclapply(split(sf_df1, split_vector), st_distance, sf_df2, mc.cores = n_cores)
  
  # Combine results back together. Method of combining depends on the output from the function.
  if ( class(split_results[[1]]) == 'list' ){
    result <- do.call("c", split_results)
    names(result) <- NULL
  } else {
    result <- do.call("rbind", split_results)
  }
  
  result <- apply(result, 1, min)
  
  # Return result
  return(result)
}

#CREATE OBJECTS WITH VALID WDPA POLYGONS IF NECESSARY:
#load wdpa shp files in global env if they do not already exist
file_path0 <- "WDPA_WDOECM_Mar2022_Public_e05cdb98a28bc5c1f76cc4919558750e5cb5dfe8ebdf74a197e38cf76ee489b7_shp_0/WDPA_WDOECM_Mar2022_Public_e05cdb98a28bc5c1f76cc4919558750e5cb5dfe8ebdf74a197e38cf76ee489b7_shp-polygons.shp"
file_path1 <- "WDPA_WDOECM_Mar2022_Public_e05cdb98a28bc5c1f76cc4919558750e5cb5dfe8ebdf74a197e38cf76ee489b7_shp_1/WDPA_WDOECM_Mar2022_Public_e05cdb98a28bc5c1f76cc4919558750e5cb5dfe8ebdf74a197e38cf76ee489b7_shp-polygons.shp"
file_path2 <- "WDPA_WDOECM_Mar2022_Public_e05cdb98a28bc5c1f76cc4919558750e5cb5dfe8ebdf74a197e38cf76ee489b7_shp_2/WDPA_WDOECM_Mar2022_Public_e05cdb98a28bc5c1f76cc4919558750e5cb5dfe8ebdf74a197e38cf76ee489b7_shp-polygons.shp"
filepaths <- c(file_path0,file_path1,file_path2)
folder_path <- "./data/WDPA/"

# if 0 then all polygons of the wdpa shp files are considered
ind_shps <- 1:50
ind_shps <- 0

###############################################################################
for(p in 1:3)
{
  if (exists(paste0("valids",p-1))==F) 
  {
    # if 0 then all indices of the filtered snl points are considered
    if(sum(ind_shps==0)==1){wdpa <- sf::read_sf(paste0(folder_path,filepaths[p]))}
    else{wdpa <- sf::read_sf(paste0(folder_path,filepaths[p]))[ind_shps,]}
    # clean data
    wdpa <- wdpa %>% 
      dplyr::filter(MARINE != 2) %>% # excluding entirely maritime areas
      dplyr::filter(STATUS != "Proposed") # excluding protected areas that are not yet implemented, 
    assign(paste0("valids",p-1), sf::st_make_valid(sf::st_as_sf(wdpa$geometry)))
  }
}

#EXAMPLE FUNCTION PARAMETERS:
indices <- 0 # if 0 then all indices of the filtered snl points are considered
SNL_path_gpkg <- paste0("./data/SNL/CKAN/snl_mines.gpkg")
SNL_prim_commodity <- 'Iron Ore'
SNL_prod_year <- 2016
ncores <- 6
class_borders <- c(1,5,10,50,500)*10^3
sf::sf_use_s2(FALSE)

#############FUNCTION
get_SNL_distance <- function(class_borders,indices,ncores=1,SNL_path_gpkg,SNL_prim_commodity,SNL_prod_year)
{
  ptm <- proc.time()
  ###SNL data
  SNL_points <- sf::read_sf(SNL_path_gpkg)
  
  print(ptm)
  snl_data <- list()
  cat("\nload", SNL_prim_commodity, "...")
  load(paste0("./data/SNL/CKAN/", SNL_prim_commodity, "_production.Rdata"))
  snl_data[[SNL_prim_commodity]] <- x %>% dplyr::filter(value > 0, year == SNL_prod_year) 
  rm(x)
  snl_data <- do.call("rbind", snl_data) %>%
    dplyr::rename(SNL_id = snl_id) %>%
    dplyr::mutate(SNL_id = as.numeric(SNL_id))
  names(snl_data)[1] <- 'snl_id'
  ###JOINS
  SNL_points$snl_id <- as.numeric(SNL_points$snl_id)
  SNL_points <- left_join(snl_data,data.frame(SNL_points))
  print(paste("# of NA coordinates:",sum(is.na(SNL_points$mine))))
  SNL_points <- SNL_points[is.na(SNL_points$mine)==F,]
  snl_p <- sf::st_as_sf(SNL_points$geom)
  
  ##load or compute conc (concmine object should only contain the columns with GID_id and SNL_id)
  ##i did small changes to the code, becaue write_csv and read_csv did not work for me
  source(paste0("./R/03_data_SNL_root_concordance.R"))
  root_path <- paste0("./data/root_geoms/")
  concmine <- get_snl_root_concordance(root_path,SNL_path_gpkg)
  concmine$GID_id <- as.character(concmine$GID_id)
  
  # if 0 then all indeices of the filtered snl points are considered
  if (indices==0){indices <- 1:nrow(SNL_points)}
  
  #compute distances
  res0 <- st_distance_par(snl_p[indices,],valids0,ncores)
  res1 <- st_distance_par(snl_p[indices,],valids1,ncores)
  res2 <- st_distance_par(snl_p[indices,],valids2,ncores)
  res <- cbind(res0,res1,res2)  
  res <- sapply(1:nrow(res),function(x){min(res[x,])})
  classes <- sapply(1:length(res), function(k) min(which(res[k]<class_borders)))
  res <- data.frame("SNL_id"=SNL_points$snl_id[indices],"min_dist"=res,"class"=classes,"value"=SNL_points$value)
  
  ##aggregation for the root_regions
  res0 <- left_join(res,concmine)
  res0$class <- as.character(res0$class) 
  res1 <- aggregate(value ~ class+GID_id, data = res0, FUN=sum)
  #create df with GID-id and classes as columns
  res2 <- data.frame()
  for (k in unique(res1$GID_id))
  {
    vectemp <- class_borders*0
    restemp <- res1[res1$GID_id==k,]
    vectemp[as.numeric(restemp$class)] <- restemp$value
    res2 <- rbind(res2,c(k,vectemp))
    res2[,1] <- as.character(res2[,1])
    for(i in 2:ncol(res2))
    {
      res2[,i] <- as.numeric(res2[,i])
    }
    #we have to add the results for the first GID-id again becuase of some errors due to factor using in the background
    if(k==unique(res1$GID_id[1])){res2[1,] <- c(k,vectemp)}
  }
  names(res2) <- c("GID_id", 1:length(class_borders))
  
  ##aggregation f?r countries
  res3 <- cbind("ISO3"=substr(res2$GID_id,1,3),res2)[,-2]
  res3$ISO3 <- as.character(res3$ISO3)
  res4 <- aggregate(. ~ ISO3, data = res3, FUN=sum)
  
  print("Time needed for statistic calculation:")
  print(proc.time() - ptm)
  return(list(res, res2, res4))
}


DPA_raw <- get_SNL_distance(class_borders,indices,ncores,SNL_path_gpkg,SNL_prim_commodity,SNL_prod_year)

save(DPA_raw, file = str_c("./data/WDPA/Distance_protected_areas_",SNL_prod_year,".R") )

#rm(valids0)
#rm(valids1)
#rm(valids2)

#alu iron und copper
#root region auf nat ebene auch aggregieren


