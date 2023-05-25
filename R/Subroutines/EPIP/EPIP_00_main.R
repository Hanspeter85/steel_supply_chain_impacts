
Build_Extension_AREA_HANPP_AWARE <- function()
{
  filepath <- str_c("./output/Extensions/AREA_HANPP_AWARE_",job$year,".xlsx")
  
  if( !file.exists(filepath) )
  {
    # Loading functions
    source("./R/Subroutines/EPIP/EPIP_01_data_roots_geometry.R")
    source("./R/Subroutines/EPIP/EPIP_02_data_polygons.R")
    source("./R/Subroutines/EPIP/EPIP_03_data_SNL_root_concordance.R")
    source("./R/Subroutines/EPIP/EPIP_10_data_npp0.R")
    source("./R/Subroutines/EPIP/EPIP_04_data_biomes.R")
    source("./R/Subroutines/EPIP/EPIP_09_data_aware_snl.R")
    source("./R/Subroutines/EPIP/EPIP_03_data_SNL_commodities.R")
    source("./R/Subroutines/EPIP/EPIP_03_data_SNL_production.R")
    # source("./R/11_wdpa_distance.R")
    # source("./R/Subroutines/EPIP_05_data_wb_roots_geometry.R")
    # source("./R/Subroutines/99_map_roots.R")
    
    ## Load basic geometry data and concordances
    
    # Loading geometry file of root regions
    root_geom <- create_root(root_path = "input/EPIP/root_geoms/root_geoms.gpkg")
    
    # Loading mining polygons
    mine_polygons <- sf::read_sf("input/EPIP/polygons/global_mining_polygons_v1.gpkg", fid_column_name = "PID") 
    
    # Loading concordance to link polygons and root regions
    conco_poly2root <- get_mine_polygons_root_concordance( root_path = "input/EPIP/root_geoms/root_geoms.gpkg", 
                                                           mine_polygons="10.1594/PANGAEA.910894")
    # Loading concordance to link SNL data to root regions
    conco_SNL2root <- get_snl_root_concordance( root_path = "input/EPIP/root_geoms", 
                                                snl_path =  "/home/geoserver/snl/metals/2018/v0/S.gpkg")
    
    ## Feed data to compile impact layers
    
    # Load NPP0 intensities for mining polygons
    NPP_intens <- get_NPP0_intensity_per_polygon()
    
    # Linking biomes with root regions 
    root_biomes <- get_biomes_area(root_path = "input/EPIP/root_geoms/root_geoms.gpkg", # "data/root_geoms_wb/root_geoms.gpkg"
                                   biome_path = "input/EPIP/ecoregions/Ecoregions2017.shp",
                                   mine_polygons="10.1594/PANGAEA.910894")
    
    # Linking SNL production with AWARE water data
    aware <- get_SNL_awareness(aware_path = "./input/EPIP/AWARE/aware_hydrobasins.geojson",
                               SNL_path_gpkg = "./input/EPIP/SNL/CKAN/snl_mines.gpkg",
                               SNL_prim_commodity = "Iron Ore",
                               SNL_prod_year = 2017)
    
    tmp <- left_join( as.data.frame( aware ), conco_SNL2root, by = c("snl_id" = "SNL_id") )
    tmp <- left_join( tmp, root_biomes[,c(2,3)] )
    aware <- tmp %>% select(GID_0,snl_value, aware_mean)
    
    # Calculate weighted national aware index
    aware["aware_x_snl_value"] <- aware$aware_mean * aware$snl_value
    
    aware <- aware %>% select(GID_0,snl_value,aware_x_snl_value) %>% group_by(GID_0) %>% 
      summarise(snl_value = sum(snl_value), aware_x_snl_value = sum(aware_x_snl_value))
    
    # aware["AWARE_index"] <- aware$aware_x_snl_value / aware$snl_value
    
    aware_in_root <- data.frame("GID_0" = root$region$RootCountryAbbreviation,
                                "SNL_value" = 0,
                                "AWARE_index" = 0 )
    
    aware_in_root$SNL_value[aware_in_root$GID_0 %in% aware$GID_0] <- aware$snl_value 
    aware_in_root$AWARE_index[aware_in_root$GID_0 %in% aware$GID_0] <- aware$AWARE_index 
    aware_in_root["aware_x_snl_value"] <- aware_in_root$SNL_value * aware_in_root$AWARE_index
    
    aware_in_base <- t( as.matrix(aware_in_root[,c("SNL_value","aware_x_snl_value")]) ) %*% Conco$Root_2_base
    colnames(aware_in_base) <- base$region$Abbrev
    aware_in_base <- as.data.frame( t( aware_in_base ) )
    aware_in_base["AWARE_index"] <- aware_in_base$aware_x_snl_value / aware_in_base$SNL_value
    aware_in_base <- aware_in_base$AWARE_index
    aware_in_base[is.na(aware_in_base)] <- 0
    
    # roots_geometry_wb <- create_wb_root(root_path = "data/root_geoms_wb/root_geoms.gpkg")
    # 
    # # pass the PANGAEA doi 10.1594/PANGAEA.910894 or an sf object 
    # mine_polygons_root_concordance <-
    #   get_mine_polygons_root_concordance(root_path = "data/root_geoms_wb/root_geoms.gpkg",
    #                                      mine_polygons="10.1594/PANGAEA.910894")
    # mine_polygons_root_concordance <- 
    #   get_mine_polygons_root_concordance(root_path = "data/root_geoms_wb/root_geoms.gpkg",
    #                                      mine_polygons = st_read("data/global_mining_polygons_v2.geojson", fid_column_name = "FID"))
    # 
    # # Polygon IDs must be unique
    # !any(duplicated(mine_polygons_root_concordance$PID)) 
    
    # add further properties --------------------------------------------------
    
    
    # from SNL: commodities
    snl_root_commodities <-  get_SNL_commodities(root_path = "input/EPIP/root_geoms", 
                                         snl_path =  "/home/geoserver/snl/metals/2018/v0/S.gpkg") # "data/SNL/mining_commodities.gpkg" 
    
    # from SNL: production; example iron ore 2017
    snl_root_production <-  get_SNL_production(root_path = "input/EPIP/root_geoms", 
                                                 snl_path =  "/home/geoserver/snl/metals/2018/v0/S.gpkg", # "data/SNL/mining_commodities.gpkg" 
                                                 production_commodities = c("Iron Ore"),
                                                 production_year = 2017)
    
    # write_csv(snl_root_production_iron_2017, "data/SNL/SNL_in_root_iron.csv") 
    # p_dat <- readr::read_csv("data/SNL/SNL_in_root_iron.csv")
    # p <- map_roots(root_data = p_dat, geoms = roots_geometry)
    # ggplot2::ggsave("production_worldmap_iron_ore_2017.png", plot = p, device = "png", path = "./img",
    #                 scale = 1, width = 320, height = 180, units = "mm")
    
    
    ### Create extension for iron-steel gPIOT
    
    NPP <- data.frame("PID" = 1:21060, "value" = NPP_intens)
    
    Land <- conco_poly2root %>% filter(GID_id %in% unique(snl_root_production$GID_id) )
    Land["NPP_intens"] <- NPP$value[Land$PID]
    Land["HANPP"] <- Land$AREA * Land$NPP_intens
    
    # Add country code for aggregation
    Land <- left_join( Land, root_biomes[,c(2,3)] )
    
    Land <- Land %>% select(AREA, HANPP, GID_0) %>% group_by(GID_0) %>% 
      summarise(AREA = sum(AREA), HANPP = sum(HANPP))
    
    Land_in_root <- data.frame("GID_0" = root$region$RootCountryAbbreviation,
                               "HANPP" = 0,
                               "AREA" = 0 )
    
    Land_in_root$HANPP[Land_in_root$GID_0 %in% Land$GID_0] <- Land$HANPP
    Land_in_root$AREA[Land_in_root$GID_0 %in% Land$GID_0] <- Land$AREA
    
    Land_in_base <- t( as.matrix(Land_in_root[,c("HANPP","AREA")]) ) %*% Conco$Root_2_base
    colnames(Land_in_base) <- base$region$Abbrev
    
    Land_in_base <- as.data.frame( t(Land_in_base) )
    # Land_in_base["C_intens"] <- Land_in_base$HANPP / Land_in_base$AREA
    
    # Now transform the matrix into use-able format with full sector detail
    index <- match( str_c(1:32,"Mining"), str_c(Code$Z$RegionCode,Code$Z$SectorName)  )
    
    E <- matrix(0,nrow = 960, ncol = 3)
    colnames(E) <- c( colnames(Land_in_base), "AWARE" )
    E <- as.data.frame(E)
    
    E$HANPP[index] <- Land_in_base$HANPP
    E$AREA[index] <- Land_in_base$AREA
    E$AWARE[index] <- aware_in_base
    
    E <- as.data.frame( t(E) )
    
    write.xlsx(x = E, file = filepath, rowNames = TRUE)
      
    # create EPIP -------------------------------------------------------------
    
    # function to select needed pressures/impacts and regions
    # build_epip <- function(stressors = c("area", "...")){
    #   
    #   # load root (not spatial)
    #   root <- readr::read_csv("data/root_mod.csv")
    #   
    #   # load concordance
    #   conc <- readr::read_csv("data/conc_root_poly.csv")
    #   
    #   # add area
    #   if("area" %in% stressors){
    #     # read area
    #     mps <- sf::read_sf("data/polygons/mine_polygons.shp") %>% dplyr::select(AREA, pid) %>% sf::st_drop_geometry()
    #     # merge to root
    #     root <- root %>% 
    #       dplyr::left_join(conc, by =  c("GID_id" = "root_id") ) %>%
    #       dplyr::right_join(mps, by = c("polygon_id" = "pid") ) %>% 
    #       dplyr::mutate(AREA = ifelse(is.na(AREA), 0, AREA))
    #   }
    #   
    #   return(root)
    #   
    # }
    # 
    # # compile case-specific EPIP matrix, e.g.:
    # epip_area <- build_epip(stressors = "area")
    # 
    # write.csv(epip_area,"data/epip_area.csv", row.names = FALSE)
    
    # A few simple checks -----------------------------------------------------
    
    # sum(epip_area$AREA)
    # sum(mps$AREA)
    # 
    # length( unique(root$GID_id) )
    # length( unique(conc$root_id) )
    # length( unique(mps$pid) )
    
    # make spatial: join with geometries --------------------------------------
    
    # only here, at the end, data will be joined with roots geometry for geographical visualization, open data access, static maps, etc.
    
  }else
  {
    E <- read.xlsx(xlsxFile = filepath, rowNames = TRUE)
  }
  return(E)
}