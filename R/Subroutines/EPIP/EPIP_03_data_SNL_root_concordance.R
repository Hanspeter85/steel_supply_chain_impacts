
# mapping the SNL production accounts + commodities to the root 

get_snl_root_concordance <- function(root_path, snl_path){
  
  if(!file.exists(str_c(root_path, "/concordance_SNL_roots.csv"))){
    
    # read root classifications ----------------------------------------------------
    roots_geometry <- st_read(file.path(root_path, "root_geoms.gpkg"))
    
    # load SNL general data from Geoserver ------------------------------------
    
    # fineprint_geoserver <- "http://fineprint.wu.ac.at:8080/geoserver/"
    # snl_wfs_request <- "snl-2018/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=snl-2018:snl_metals&outputFormat=application/json"
    # snl_mines_wfs <- stringr::str_glue(fineprint_geoserver, snl_wfs_request)
    # snl_mines <- httr::GET(url = snl_mines_wfs, httr::authenticate(Sys.getenv("GS_USER"), Sys.getenv("GS_PASS"))) %>%
    #   sf::st_read(quiet = TRUE) %>%
    #   sf::st_transform(crs = sf::st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    
    # use old data since geoserver is down:
    # path_mnt <- "/home/geoserver/snl/metals/2018/v0"
    # list.files(path_mnt)
    snl_mines_commodities <- sf::st_read(snl_path, stringsAsFactors = FALSE) %>% 
      mutate(snl_id = as.numeric(snl_id)) %>% 
      sf::st_transform(crs = st_crs(roots_geometry)) %>%
      dplyr::select(SNL_id = snl_id) %>%
      st_make_valid() %>%
      filter(st_is_valid(geom))
    
    # intersect ---------------------------------------------------------------
    commodity_roots_intersects <- st_intersects(roots_geometry, snl_mines_commodities)
    # commodity_in_roots <- sf::st_join(roots_geometry, snl_mines_commodities, join = st_intersects)
    concordance_commodity_roots <- tibble(GID_id = roots_geometry$GID_id[lengths(commodity_roots_intersects)>0],
                                          SNL_id = commodity_roots_intersects[lengths(commodity_roots_intersects)>0]) %>% 
      unnest(cols = SNL_id) %>% 
      dplyr::mutate(SNL_id = snl_mines_commodities$SNL_id[SNL_id]) %>%
      left_join(st_drop_geometry(snl_mines_commodities))
    
    write_csv(concordance_commodity_roots, str_c(root_path, "/concordance_SNL_roots.csv"))
 
  } else {
    concordance_commodity_roots <- read_csv(str_c(root_path, "/concordance_SNL_roots.csv"))
  }
  
  return(concordance_commodity_roots)
  
}


