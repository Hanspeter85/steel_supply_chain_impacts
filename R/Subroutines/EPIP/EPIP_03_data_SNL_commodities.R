
# mapping the SNL commodities to the root 

get_SNL_commodities <- function(root_path, snl_path){
  
  if(! file.exists(file.path(root_path, "commodity_in_roots.csv"))) {
  
  # get SNL to root concordance
  if(! file.exists(file.path(root_path, "/concordance_commodity_roots.csv"))) {
    stop("SNL to root concordance does not exist. Run get_snl_root_concordance() first!")
  } else {
    concordance_SNL_roots <- read_csv(file.path(root_path, "/concordance_SNL_roots.csv"))
  }
  
  # read SNL commodities
  snl_mines_commodities <- sf::st_read(snl_path, stringsAsFactors = FALSE) %>% 
    sf::st_drop_geometry() %>%
    mutate(snl_id = as.numeric(snl_id)) %>% 
    dplyr::select(SNL_id = snl_id, list_of_commodities)
  
  # merge into root regions
  commodity_in_roots <- concordance_SNL_roots %>% dplyr::left_join(snl_mines_commodities, by = "SNL_id")
  
  # summarise
  commodity_in_roots <- commodity_in_roots %>% 
      group_by(GID_id) %>% 
      summarise(commodities = str_c(list_of_commodities, collapse = ","),
                n_mines = n()) %>%
    dplyr::ungroup()
    
  # remove multiple counts of commodities
    rem_dup.one <- function(x){
      paste(unique(trimws(unlist(strsplit(x,split="(?!')[ [:punct:]]",fixed=F,perl=T)))),collapse = ", ")
    }
    commodity_in_roots <- commodity_in_roots %>%
      dplyr::group_by(GID_id) %>%
      dplyr::mutate(commodities = rem_dup.one(commodities)) %>%
      dplyr::mutate(commodities = gsub("Iron, Ore", "Iron Ore", commodities)) %>%
      dplyr::mutate(commodities = gsub(", ,", ", ", commodities))
  
    # # maps --------------------------------------------------------------------
    # 
    # NEEDS TO BE UPDATED FOR PRODUCING MAPS (merge with geoms, new version has no spatial format any more)
    # 
    # library(stringr)
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
    # 
    # # coal
    # p_dat <- commodity_in_roots %>%
    #   dplyr::mutate(d = stringr::str_detect(commodities, "Coal", negate = FALSE))
    # p <- p_dat %>%
    #   sf::st_transform(crs = sf::st_crs("+proj=robin")) %>%
    #   ggplot2::ggplot() +
    #   ggplot2::geom_sf(aes(fill = d), size = .05) +
    #   ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
    #   ggplot2::theme(legend.position = "bottom")
    # # save
    # ggplot2::ggsave("commodity_worldmap_coal.png",
    #                 plot = p, device = "png",
    #                 path = "./img",
    #                 scale = 1, width = 320, height = 180, units = "mm")
    # 
    # 
    # # iron ore
    # p_dat <- commodity_in_roots %>%
    #   dplyr::mutate(d = stringr::str_detect(commodities, "Iron Ore", negate = FALSE))
    # p <- p_dat %>%
    #   sf::st_transform(crs = sf::st_crs("+proj=robin")) %>%
    #   ggplot2::ggplot() +
    #   ggplot2::geom_sf(aes(fill = d), size = .05) +
    #   ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
    #   ggplot2::theme(legend.position = "bottom")
    # # save
    # ggplot2::ggsave("commodity_worldmap_iron_ore.png",
    #                 plot = p, device = "png",
    #                 path = "./img",
    #                 scale = 1, width = 320, height = 180, units = "mm")
    # 
    # # diamonds
    # p_dat <- commodity_in_roots %>%
    #   dplyr::mutate(d = stringr::str_detect(commodities, "Diamonds", negate = FALSE))
    # p <- p_dat %>%
    #   sf::st_transform(crs = sf::st_crs("+proj=robin")) %>%
    #   ggplot2::ggplot() +
    #   ggplot2::geom_sf(aes(fill = d), size = .05) +
    #   ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
    #   ggplot2::theme(legend.position = "bottom")
    # # save
    # ggplot2::ggsave("commodity_worldmap_diamonds.png",
    #                 plot = p, device = "png",
    #                 path = "./img",
    #                 scale = 1, width = 320, height = 180, units = "mm")
    
    # save csv ----------------------------------------------------------------
    
    write_csv(commodity_in_roots, file.path(root_path, "commodity_in_roots.csv"))
    
  } else {
    commodity_in_roots <- read_csv(file.path(root_path, "commodity_in_roots.csv"))
  }
  
  return(commodity_in_roots)
  
}


