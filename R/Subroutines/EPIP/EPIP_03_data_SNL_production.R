
# mapping the SNL production accounts to the root 

get_SNL_production <- function(root_path, snl_path, production_commodities, production_year){
  
    # get SNL to root concordance
    if(! file.exists(file.path(root_path, "/concordance_SNL_roots.csv"))) {
      stop("SNL to root concordance does not exist. Run get_snl_root_concordance() first!")
    } else {
      concordance_SNL_roots <- read_csv(file.path(root_path, "/concordance_SNL_roots.csv"))
    }
    
    # load SNL production data from CKAN ----------------------------------------
    # TBC, this is more complex
    # decide on which production measure we need. SNL reports content and, in some cases, ores.
    # In the mining geographies publication, we converted content to ore via UNEP conversion factors
    
    
    # # THIS DOES NOT WORK ATM, TO BE FIXED. FOR NOW, ACCESS DATA FROM ELSEWHERE
    # # Connect to CKAN using your API key.
    # fineprintutils::ck_setup()
    # 
    # # filter for snl datasets and download them
    # if(!"CKAN" %in% dir(str_c(getwd(), "/data/SNL"))){
    #   fineprintutils::ck_get_datasets() %>%
    #     dplyr::filter(name.organization == "snl") %>%
    #     dplyr::filter(version == "v1") %>%
    #     fineprintutils::ck_download(destpath = str_c(getwd(), "/data/SNL/CKAN"))
    # }
    
    # production data ---------------------------------------------------------
    
    snl_data <- list()
    for (com in production_commodities){
      cat("\nload", com, "...")
      load(paste0(str_c(getwd(), "/input/EPIP/SNL/CKAN/"), com, "_production.Rdata"))
      snl_data[[com]] <- x %>% dplyr::filter(value > 0, year == production_year) 
    }
    rm(x)
    snl_data <- do.call("rbind", snl_data) %>%
      dplyr::rename(SNL_id = snl_id) %>%
      dplyr::mutate(SNL_id = as.numeric(SNL_id))
    # here, we need checks on units, content vs ore etc. in case we consider multiple commodities
    
    # merge into root regions
    # THIS CREATES 183 NAs, SNL IDs all between 82,018 and 84,765, this is because the used space table S is not as recent as the production data!
    production_in_roots <- snl_data %>% dplyr::left_join(concordance_SNL_roots, by = "SNL_id")
    # View(production_data %>% dplyr::filter(is.na(GID_id)))
    
    # summarise
    production_in_roots <- production_in_roots %>% dplyr::filter(!is.na(GID_id)) %>%
      dplyr::group_by(GID_id, commodity, year) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()
    
    # # maps --------------------------------------------------------------------
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
    # 
    # roots_geometry <- create_root(root_path = "data/root_geoms/root_geoms.gpkg")
    # p_dat <- roots_geometry %>% dplyr::left_join(production_in_roots, by = "GID_id") %>%
    #   dplyr::mutate(value = value / 1000000) # megatonnes
    # summary(p_dat$value)
    # 
    # p <- p_dat %>%
    #   sf::st_transform(crs = sf::st_crs("+proj=robin")) %>%
    #   ggplot2::ggplot() +
    #   ggplot2::geom_sf(aes(fill = value), size = .05) +
    #   ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
    #   viridis::scale_fill_viridis(direction = -1, na.value="white", begin = 0.2) +
    #   ggplot2::theme(legend.position = "bottom")
    # # save
    # ggplot2::ggsave("production_worldmap_iron_ore_2019.png",
    #                 plot = p, device = "png",
    #                 path = "./img",
    #                 scale = 1, width = 320, height = 180, units = "mm")

  write_csv(production_in_roots, str_c(dirname(root_path), "/production_in_roots/", 
                                       production_commodities,"_",production_year,"_in_roots.csv"))
    
  return(production_in_roots)
  
}


