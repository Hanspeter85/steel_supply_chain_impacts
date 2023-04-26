
# disaggregating land use into different states i.e. biomes for example

get_biomes_area <- function(root_path, biome_path, mine_polygons="10.1594/PANGAEA.910894"){
  
  if(! file.exists(str_c(dirname(root_path), "/biome_in_roots.csv"))){
    
    # root classifications ----------------------------------------------------
    roots_geometry <- st_read(root_path)
    
    # ecoregions --------------------------------------------------------------
    # group into biomes (instead of ecoregions) to make the file smaller
    if(! file.exists(str_c(dirname(biome_path), "/biomes.gpkg"))){
      st_read(biome_path) %>% 
        st_make_valid() %>% 
        dplyr::group_by(BIOME_NAME) %>% 
        summarise() %>% 
        sf::st_write(str_c(dirname(biome_path), "/biomes.gpkg"), delete_dsn = TRUE)
    } 
    
    # intersect ---------------------------------------------------------------
    
    # # old: intersect full root geoms
    # if(! file.exists(str_c(dirname(biome_path), "/biome_in_roots_raw.gpkg"))){
    #   sf::st_intersection(roots_geometry, sf::read_sf(str_c(dirname(biome_path), "/biomes.gpkg"))) %>% 
    #     st_make_valid() %>% 
    #     rename_at(vars(matches("^geometry$")), function(x) "geom") %>% 
    #     filter(st_is_valid(geom)) %>% 
    #     dplyr::mutate(AREA = sf::st_area(geom)) %>% 
    #     sf::st_write(str_c(dirname(biome_path), "/biome_in_roots_raw.gpkg"), delete_dsn = TRUE)
    # } 
    # # old: This is more efficient as geometries are not need in any of the further steps 
    # biome_in_roots <- st_read(str_c(dirname(biome_path), "/biome_in_roots_raw.gpkg"), 
    #                           query = "SELECT 'index', GID_id, GID_0, NAME_0, NAME_1, NAME_2, NAME_3, NAME_4, BIOME_NAME, AREA FROM biome_in_roots_raw") %>% 
    #   as_tibble() %>% 
    #   dplyr::group_by(GID_id) %>%
    #   dplyr::mutate(list_of_biomes = paste0(BIOME_NAME, collapse = ",")) %>%
    #   dplyr::filter(AREA == max(AREA)) %>%
    #   # dplyr::select(GID_id, list_of_biomes)
    #   dplyr::mutate(primary_biome = as.character(BIOME_NAME))  %>%
    #   dplyr::select(GID_id, list_of_biomes, primary_biome) %>%
    #   dplyr::mutate(primary_biome = ifelse(primary_biome == "N/A", NA, primary_biome))
    # 
    # biome_in_roots <- dplyr::left_join(roots_geometry, biome_in_roots, by = "GID_id")
    
    
    # new: intersect mine polygon centroids with biomes and then add root info
    if(! file.exists(str_c(dirname(biome_path), "/polyogn_in_biomes_raw.gpkg"))){
      if("character" %in% class(mine_polygons)){
        dir.create("data/mine_polygons", showWarnings = FALSE, recursive = TRUE)
        doi <- mine_polygons
        mine_polygons <- NULL
        pangaear::pg_data(doi = doi)
        unzip(pangaear::pg_cache$list(), exdir = "data/polygons/")
        mine_polygons <- sf::read_sf("data/polygons/global_mining_polygons_v1.gpkg", fid_column_name = "PID") 
      }
      sf::st_join(sf::st_centroid(mine_polygons), sf::read_sf(str_c(dirname(biome_path), "/biomes.gpkg")), 
                  join = st_nearest_feature) %>% # join with nearest feature because some centroids are in no biome
        sf::st_make_valid() %>% 
        filter(st_is_valid(geom)) %>% 
        sf::st_write(str_c(dirname(biome_path), "/polyogn_in_biomes_raw.gpkg"), delete_dsn = TRUE)
    }
    
    biome_in_roots <- st_read(str_c(dirname(biome_path), "/polyogn_in_biomes_raw.gpkg"), 
                              query = "SELECT PID, BIOME_NAME FROM polyogn_in_biomes_raw") %>% 
      as_tibble() %>%
      dplyr::mutate(PID = as.numeric(as.character(PID))) %>%
      dplyr::left_join(concordance_polygons_roots, by = "PID") %>%
      dplyr::group_by(GID_id, BIOME_NAME) %>%
      dplyr::summarise(AREA = sum(AREA)) %>%
      dplyr::mutate(list_of_biomes = paste0(BIOME_NAME, collapse = ",")) %>%
      dplyr::filter(AREA == max(AREA)) %>%
      dplyr::mutate(primary_biome = as.character(BIOME_NAME))  %>%
      dplyr::select(GID_id, list_of_biomes, primary_biome) %>%
      dplyr::mutate(primary_biome = ifelse(primary_biome == "N/A", NA, primary_biome))

    # maps --------------------------------------------------------------------
    
    # # make the data spatial
    # map_data <- dplyr::left_join(roots_geometry, biome_in_roots, by = "GID_id")
    # 
    # library(ggplot2)
    # library(viridis)
    # 
    # # zoom table
    # zoom_bbox <- tibble::tribble(
    #   ~region,                    ~x_lim,            ~y_lim,
    #   "Chile",          c(-76.00, -67.00),  c(-57.00, -14.00),
    #   "South America",  c( -88.00, -31.00), c(-58.00, 13.00),
    #   "Peru",           c(-82.00, -68.00),  c(-19.00, 00.50),
    #   "Mexico",         c(-119.00, -84.00), c(13.00, 33.00),
    #   "Minas Gerais",   c(-52.00, -39.00),  c(-23.00, -13.00),
    #   "Brasil",         c(-74.00, -34.00),  c(-34.00, 6.00),
    #   "Africa",         c(-20, 54.00),      c(-35.00, 39),
    #   "Southern Africa",c(15.00, 35.00),    c(-35.00, -15.00),
    #   "Oceania",        c(92, 160.00),      c(-48.00, 20.00),
    #   "Australia",      c(110.00, 156.00),  c(-44.00, -8.00),
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
    # # primary biome
    # p <- map_data %>%
    #   sf::st_transform(crs = sf::st_crs("+proj=robin")) %>%
    #   ggplot2::ggplot() +
    #   ggplot2::geom_sf(aes(fill = primary_biome), size = .05) +
    #   ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
    #   ggplot2::theme(legend.position = "bottom",
    #                  legend.title = element_blank())
    # # save
    # ggplot2::ggsave("primary_biome_worldmap_new.png",
    #                 plot = p, device = "png",
    #                 path = "./img",
    #                 scale = 1, width = 380, height = 180, units = "mm")
    
    
    # # check root geoms with multiple biomes
    # biomes <- sf::read_sf(str_c(dirname(biome_path), "/biomes.gpkg"))
    # library(ggplot2)
    # 
    # lim <- zoom_bbox %>%
    #   dplyr::filter(region == "Australia")
    # p <- roots_geometry %>% dplyr::filter(GID_id %in% c("AUS_11062", "AUS_11092", "AUS_11093", "AUS_11098")) %>% 
    #   ggplot2::ggplot() +
    #   ggplot2::geom_sf(color = "red") +
    #   ggplot2::geom_sf(data = biomes, aes(fill = BIOME_NAME), size = .05, alpha = 0.2) +
    #     ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
    #     ggplot2::theme(legend.position = "bottom",
    #                    legend.title = element_blank())
    # ggplot2::ggsave("biome_Australia_check.png",
    #                 plot = p, device = "png",
    #                 path = "./img",
    #                 scale = 1, width = 380, height = 180, units = "mm")
    # 
    # lim <- zoom_bbox %>%
    #   dplyr::filter(region == "Brasil")
    # p <- roots_geometry %>% dplyr::filter(GID_id %in% c("BRA_11_26", "BRA_11_60", "BRA_11_68", "BRA_12_34", "BRA_12_54", "BRA_12_63")) %>% 
    #   ggplot2::ggplot() +
    #   ggplot2::geom_sf(color = "red") +
    #   ggplot2::geom_sf(data = biomes, aes(fill = BIOME_NAME), size = .05, alpha = 0.2) +
    #   ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
    #   ggplot2::theme(legend.position = "bottom",
    #                  legend.title = element_blank())
    # ggplot2::ggsave("biome_Brazil_check.png",
    #                 plot = p, device = "png",
    #                 path = "./img",
    #                 scale = 1, width = 380, height = 180, units = "mm")
    
    
    # save csv ----------------------------------------------------------------
    # biome_in_roots <- biome_in_roots %>% 
    #   sf::st_drop_geometry() %>%
    #   as_tibble()
    write_csv(biome_in_roots, str_c(dirname(root_path), "/biome_in_roots.csv"))
    
  } else {
    biome_in_roots <- read_csv(str_c(dirname(root_path), "/biome_in_roots.csv"))
  }
  
  return(biome_in_roots)
  
}


