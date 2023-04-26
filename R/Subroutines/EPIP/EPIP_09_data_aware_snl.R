
get_SNL_awareness <- function(aware_path,SNL_path_gpkg,SNL_prim_commodity,SNL_prod_year)
{
  if(! file.exists(str_c("./input/EPIP/AWARE/SNL_AWARE_",SNL_prim_commodity, "_", SNL_prod_year,".R")) )
  {
    ptm <- proc.time()
    ###GPKG files
    aware_polygons <- sf::read_sf(aware_path)
    SNL_points <- sf::read_sf(SNL_path_gpkg)
    
    ###SNL data
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
    
    
    valids <- cbind(1:nrow(aware_polygons),sf::st_make_valid(sf::st_as_sf(aware_polygons$geometry)))
    joi <- sf::st_join(sf::st_as_sf(SNL_points$geom),valids, join = sf::st_intersects,left=TRUE)
    final <- aware_polygons[joi$X1.nrow.aware_polygons.,]
    final <- cbind('year'=as.numeric(SNL_points$year),'snl_id'=as.numeric(SNL_points$snl_id),'aware_id'=as.numeric(final$Group.1),
                   'aware_mean'=as.numeric(final$mean), 'snl_value'=as.numeric(SNL_points$value))
    
    print("Time needed for statistic calculation:")
    print(proc.time() - ptm)
    save(final, file = str_c("./input/EPIP/AWARE/SNL_AWARE_",SNL_prim_commodity, "_", SNL_prod_year,".R") )
  }else
  {
    load(file = str_c("./input/EPIP/AWARE/SNL_AWARE_",SNL_prim_commodity, "_", SNL_prod_year,".R") )
  }
  
  return(final)
}

#########PLOT POLYGON AND POINT IN ONE PLOT
# plot(vals[232,2])
# plot(SNL_points$geom[1],pch=18,col="red", add=T)


