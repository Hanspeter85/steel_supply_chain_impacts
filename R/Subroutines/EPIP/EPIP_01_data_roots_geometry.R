
### Script to create roots geometry (see data flow chart)


# GADM data ---------------------------------------------------------------

# This part of the code is taken from Hanspeter's GADM repo https://github.com/fineprint-global/GADM/blob/master/R/Main.R 
# See https://gadm.org/metadata.html for variable names
# The GADM GeoPackage can be downloaded from https://gadm.org/download_world.html

create_root <- function(root_path){

if(! file.exists(root_path)){
  
  dir.create(dirname(root_path), showWarnings = FALSE, recursive = TRUE)
  
  # Download file
  
  if (!file.exists("./data/GADM/gadm36_levels.gpkg")){
    if (!file.exists("data/GADM/gadm36_levels_gpkg.zip")){
      dir.create("data/GADM", showWarnings = FALSE, recursive = TRUE)
      timeout <- getOption('timeout')
      options(timeout=600) # 10 min
      download.file("https://biogeo.ucdavis.edu/data/gadm3.6/gadm36_levels_gpkg.zip", destfile = "data/GADM/gadm36_levels_gpkg.zip")
      options(timeout=timeout)
    }
    unzip("data/GADM/gadm36_levels_gpkg.zip", files = "gadm36_levels.gpkg", exdir = "data/GADM")
  }
  
  # Set file path/name to GeoPackage
  file <- "./data/GADM/gadm36_levels.gpkg"
  
  # Explore layers
  sf::st_layers(file)
  
  # Import GeoPackage
  data <- src_sqlite(file)
  
  ########################################################################
  # Run through all layers and read number of unique subnational regions #
  # This code section creates overview files for each of the layers ######
  ########################################################################

  for( l in 0:5 )
  {
    print(l)

    # Create a table for specific layer (0 = ISO region codes)
    df <- as.data.frame( tbl(data, paste0("level",l) ) )

    # Drop geo info cause otherwise not possible to write data frame to xlsx
    # @Sebastian: I think this is what you need for aggregating the spatial data
    df <- df[,colnames(df) != "geom"]

    # Write layer l to file
    write.xlsx(df, file = paste0("./data/GADM/processed/level",l,".xlsx"), overwrite = TRUE)

    if(l == 0)
    {
      # Empty object to store number of regions per country on each layer
      regions <- data.frame(df,
                            "Layer1" = 0,
                            "Layer2" = 0,
                            "Layer3" = 0,
                            "Layer4" = 0,
                            "Layer5" = 0)

      # Transform strings to factors to preserve order of entries
      regions$GID_0 <- as.factor(regions$GID_0)
      regions$NAME_0 <- as.factor(regions$NAME_0)

    }else
    {
      # Count subregions per country and transform to data frame
      tmp <- as.data.frame( table(df$GID_0) )

      # Write number of subregions in coresponding column.
      regions[ regions$GID_0 %in% tmp$Var1, l+3] <- tmp$Freq
    }
  }

  # Write overview table to file
  write.xlsx(regions, file = paste0("./data/GADM/Layers_Overview.xlsx"), overwrite = TRUE)
  
  
  ########################################################################
  ### Create regional root using layers as selected in input/xlsx file ###
  ########################################################################
  
  # Apply concordance_source2root_nations.xlsx:
  # This concordance maps the GADM countries to the national root classification. 
  # In this step, for example, many islands and overseas territories are aggregated to the mother land.
  
  # Load Source-2-Root Concordance for aggregating GADM countries to the national root (213) of the PIOLab
  S2R <- read.xlsx( xlsxFile =  "./config/concordance_source2root_nations.xlsx", 
                    sheet = 1, 
                    rows = 4:260,
                    cols = 6:218 )
  
  S2R <- as.matrix(S2R)
  
  # Load root region list
  regions_Root <- read.xlsx( xlsxFile =  "./config/concordance_source2root_nations.xlsx", 
                     sheet = 1, 
                     rows = 1:3,
                     cols = 6:218,
                     colNames = FALSE )
  
  regions_Root <- as.data.frame( t(regions_Root) )
  colnames(regions_Root) <- c("Index", "Code", "Name")
  regions_Root$Index <- 1:nrow(regions_Root)
  
  # Subnational entities for root: Selection of layers for each of the GADM countries
  select <- read.xlsx( xlsxFile =  "./config/Layer_Selection.xlsx", sheet = 1 )
  
  select$GID_0 <- as.factor(select$GID_0)
  select$NAME_0 <- as.factor(select$NAME_0)
  
  # Object to store numbers of subnational regions
  regions_GADM <- cbind( select[,1:3], "Regions" = 0 )
  
  # Run through layers 0...4 and read number of unique subnational regions. Layer 5 is not in use.
  for( l in 0:4 )
  {
    print(l)
    
    # Create a table for specific layer (0 = ISO region codes)
    df <- as.data.frame( tbl(data, paste0("level",l) ) )
    
    # Drop geo info cause otherwise not possible to write data frame to xlsx 
    df <- df[,colnames(df) != "geom"]
    
    # Count subregions per country and transform to data frame
    tmp <- as.data.frame( table(df$GID_0) )
    
    SelReg <- select %>% filter(Selection == l) %>% pull(GID_0)
    
    filter <- tmp[tmp$Var1 %in% SelReg, ]
    
    regions_GADM$Regions[ regions_GADM$GID_0 %in% filter$Var1] <- filter$Freq
  }
  
  regions_Root["number"] <- colSums( regions_GADM$Regions * S2R )
  
  # Set root region for Australia to the newest SA2 classification 2016 (AusIELab)
  regions_Root$number[ regions_Root$Code == "AUS" ] <- 2310
   
  sum(regions_Root$number)
  
  colnames(regions_Root) <- c("index", "GID_0", "name", "number")
  
  write.xlsx( regions_Root, paste0(dirname(root_path), "./Root_classification_regions_overview.xlsx"), overwrite = TRUE)
  
  ########################################################################
  ### get the geometries ###
  ########################################################################
  
  # detect selected level of each country
  l0 <- select$GID_0[select$Selection == 0]
  l1 <- select$GID_0[select$Selection == 1]
  l2 <- select$GID_0[select$Selection == 2]
  l3 <- select$GID_0[select$Selection == 3]
  l4 <- select$GID_0[select$Selection == 4]
  
  # read spatial data for each layer and subset to respective countries
  level0 <- sf::read_sf("./data/GADM/gadm36_levels.gpkg", layer="level0") %>% 
    dplyr::filter(GID_0 %in% l0) %>%
    dplyr::mutate(GID_id = GID_0, NAME_1 = NA, NAME_2 = NA, NAME_3 = NA, NAME_4 = NA) %>% 
    dplyr::select(GID_id, GID_0, NAME_0, NAME_1, NAME_2, NAME_3, NAME_4)
  
  level1 <- sf::read_sf("./data/GADM/gadm36_levels.gpkg", layer="level1") %>% 
    dplyr::filter(GID_0 %in% l1) %>% 
    dplyr::mutate(GID_id = GID_1, NAME_2 = NA, NAME_3 = NA, NAME_4 = NA) %>% 
    dplyr::select(GID_id, GID_0, NAME_0, NAME_1, NAME_2, NAME_3, NAME_4)
  
  level2 <- sf::read_sf("./data/GADM/gadm36_levels.gpkg", layer="level2") %>% 
    dplyr::filter(GID_0 %in% l2) %>%
    dplyr::mutate(GID_id = GID_2, NAME_3 = NA, NAME_4 = NA) %>% 
    dplyr::select(GID_id, GID_0, NAME_0, NAME_1, NAME_2, NAME_3, NAME_4)

  level3 <- sf::read_sf("./data/GADM/gadm36_levels.gpkg", layer="level3") %>% 
    dplyr::filter(GID_0 %in% l3) %>%
    dplyr::mutate(GID_id = GID_3, NAME_4 = NA) %>%
    dplyr::select(GID_id, GID_0, NAME_0, NAME_1, NAME_2, NAME_3, NAME_4)

  level4 <- sf::read_sf("./data/GADM/gadm36_levels.gpkg", layer="level4") %>% 
    dplyr::filter(GID_0 %in% l4) %>%
    dplyr::mutate(GID_id = GID_4) %>% 
    dplyr::select(GID_id, GID_0, NAME_0, NAME_1, NAME_2, NAME_3, NAME_4)
  
  # Create one object for all levels and transform GID_id to remove dots
  roots_geometry <- rbind(level0, level1, level2, level3, level4) %>% 
    mutate( GID_id = gsub("_1","",GID_id)) %>% mutate( GID_id = gsub("[.]","_",GID_id) )
  format(object.size(roots_geometry), units = "GB")
  
  # create concordance between GADM and root country region 
  S2R_conc <- data.frame("GADM" = rep( regions_GADM$GID_0 , 213),
                         "root" = rep(regions_Root$GID_0, each = 256 ),
                         "value" = c(S2R) ) %>% filter(value == 1) %>% select(GADM, root)
  
  S2R_conc <- data.frame(lapply(S2R_conc, as.character), stringsAsFactors=FALSE)
  tmp <- S2R_conc %>% count(root) %>% filter(n > 1) %>% select(root)
  S2R_conc <- S2R_conc %>% filter(root %in%  tmp$root, GADM != root)
  
  # Remove regions that we need to allocate to another country and transform labels accordingly
  
  keep <- roots_geometry %>% filter(!GID_0 %in% S2R_conc$GADM)
  
  new <- roots_geometry %>% filter(GID_0 %in% S2R_conc$GADM)
  new$NAME_2 <- new$NAME_1
  new$NAME_1 <- new$NAME_0
  new$NAME_0 <- NA
  new <- left_join(new, S2R_conc, by = c('GID_0' = 'GADM')) %>% mutate(GID_id = paste0(root,"_",GID_id)) %>% 
    mutate(GID_0 = root)
  new <- left_join(new, regions_Root, by = 'GID_0') %>% mutate(NAME_0 = name) %>% 
    select(GID_id, GID_0, NAME_0, NAME_1, NAME_2, NAME_3, NAME_4)
  
  roots_geometry <- rbind(keep, new)
  
  
  # Australia ---------------------------------------------------------------
  
  # download from https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.001July%202016?OpenDocument#Data
  
  if (!file.exists("./data/australia/SA2_2016_AUST.shp")){
    if (!file.exists("data/australia/aust_geom.zip")){
      dir.create("data/australia", showWarnings = FALSE, recursive = TRUE)
      timeout <- getOption('timeout')
      options(timeout=600) # 10 min
      download.file("https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa2_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&A09309ACB3FA50B8CA257FED0013D420&0&July%202016&12.07.2016&Latest", destfile = "data/australia/aust_geom.zip")
      options(timeout=timeout)
    }
    unzip("data/australia/aust_geom.zip", exdir = "data/australia")
  }
  
  roots_geometry_aus <- sf::read_sf("./data/australia/SA2_2016_AUST.shp") %>%
    dplyr::mutate(GID_0 = "AUS", GID_id = paste0("AUS_",SA2_5DIG16), NAME_0 = "Australia", 
                  NAME_1 = STE_NAME16, NAME_2 = GCC_NAME16, NAME_3 = SA4_NAME16, NAME_4 = SA2_NAME16) %>%
    dplyr::select(geometry, GID_0, GID_id, NAME_0, NAME_1, NAME_2, NAME_3, NAME_4) %>%
    sf::st_transform(crs = sf::st_crs(roots_geometry))
  colnames(roots_geometry_aus) <- c("geometry", "GID_0", "GID_id", "NAME_0", "NAME_1", "NAME_2", "NAME_3", "NAME_4")
  colnames(roots_geometry_aus) <- c("geom", "GID_0", "GID_id", "NAME_0", "NAME_1", "NAME_2", "NAME_3", "NAME_4")
  st_geometry(roots_geometry_aus) <- "geom"
  
  
  # merge -------------------------------------------------------------------
  
  roots_geometry_mod <- roots_geometry %>% dplyr::filter(GID_0 != "AUS")
  roots_geometry <- rbind(roots_geometry_mod, roots_geometry_aus)
  
  roots_geometry <- roots_geometry[order(roots_geometry$GID_0),] %>% 
    mutate("index" = 1:nrow(roots_geometry)) %>% 
    select(index, GID_id, GID_0, NAME_0, NAME_1, NAME_2, NAME_3, NAME_4, geom)
  
  colnames(roots_geometry) 
    
  
  # export ------------------------------------------------------------------
  sf::st_write(roots_geometry, root_path)
  write.csv(roots_geometry %>% sf::st_drop_geometry(),stringi::stri_replace(root_path, replacement = ".csv", fixed = ".gpkg"), row.names = FALSE)

} else {
  roots_geometry <- st_read(root_path)
}

# still decide for one:
# roots_geometry <- sf::st_read("data/root_geoms/root_geoms.shp")
  
  return(roots_geometry)  
  
}


# # # plot --------------------------------------------------------------------
# 
# library(ggplot2)
# 
# roots_geometry_mod <- roots_geometry
# 
# # zoom table
# zoom_bbox <- tibble::tribble(
#   ~region,                    ~x_lim,            ~y_lim,
#   "Chile",          c(-76.00, -67.00), c(-57.00, -14.00),
#   "Latin America",  c( -88.00, -31.00), c(-58.00, 13.00),
#   "Peru",           c(-82.00, -68.00), c(-19.00, 00.50),
#   "Mexico",         c(-119.00, -84.00), c(13.00, 33.00),
#   "Minas Gerais",   c(-52.00, -39.00), c(-23.00, -13.00),
#   "Brasil",         c(-74.00, -34.00), c(-34.00, 6.00),
#   "Southern Africa",c(15.00, 35.00), c(-35.00, -15.00),
#   "Australia",      c(110.00, 156.00), c(-44.00, -8.00),
#   "Robinson World", c(-11631775, 14842336), c(-5839280, 8459648),
# ) %>%
#   dplyr::mutate(geometry = lapply(seq_along(region), function(i) sf::st_multipoint(matrix(c(x_lim[[i]], y_lim[[i]]), nrow = 2))),
#                 group = 1,
#                 geometry = lapply(geometry, sf::st_bbox),
#                 geometry = lapply(geometry, sf::st_as_sfc),
#                 geometry = lapply(geometry, sf::st_geometrycollection),
#                 geometry = sf::st_sfc(geometry)) %>%
#   sf::st_sf() %>%
#   sf::st_collection_extract()
# 
# lim <- zoom_bbox %>%
#   dplyr::filter(region == "Robinson World")
# 
# # p <- roots_geometry_gadm %>%
# #   sf::st_transform(crs = sf::st_crs("+proj=robin")) %>%
# #   ggplot2::ggplot() +
# #   ggplot2::geom_sf(size = .05) +
# #   ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]])
# # 
# # # save
# # ggplot2::ggsave(paste0("EPIP_geoms_world_GADM.png"),
# #                 plot = p, device = "png",
# #                 path = "./img",
# #                 scale = 1, width = 320, height = 180, units = "mm")
# 
# p_mod <- roots_geometry_mod %>%
#   sf::st_transform(crs = sf::st_crs("+proj=robin")) %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(size = .05) +
#   ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]])
# 
# # save
# ggplot2::ggsave(paste0("EPIP_geoms_world_mod.png"),
#                 plot = p_mod, device = "png",
#                 path = "./img",
#                 scale = 1, width = 320, height = 180, units = "mm")
# 
# # Australien Zoom
# lim <- zoom_bbox %>%
#   dplyr::filter(region == "Australia")
# 
# # p <- roots_geometry_gadm %>%
# #   ggplot2::ggplot() +
# #   ggplot2::geom_sf(size = .05) +
# #   ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]])
# # 
# # # save
# # ggplot2::ggsave(paste0("EPIP_geoms_AUS_GADM.png"),
# #                 plot = p, device = "png",
# #                 path = "./img",
# #                 scale = 1, width = 320, height = 180, units = "mm")
# 
# p_mod <- roots_geometry_mod %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(size = .05) +
#   ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]])
# 
# # save
# ggplot2::ggsave(paste0("EPIP_geoms_AUS_mod.png"),
#                 plot = p_mod, device = "png",
#                 path = "./img",
#                 scale = 1, width = 320, height = 180, units = "mm")







