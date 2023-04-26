
### Script to insert polygons to root geometries (see data flow chart)

# get data ----------------------------------------------------------------

get_mine_polygons_root_concordance <- function(root_path, mine_polygons="10.1594/PANGAEA.910894"){

  if(!file.exists(str_c(dirname(root_path), "/concordance_root_poly.csv")))  {
    # mine_polygons: either the PANGAEA doi of the data set or an sf object 
    
    if("character" %in% class(mine_polygons)){
      dir.create("data/mine_polygons", showWarnings = FALSE, recursive = TRUE)
      doi <- mine_polygons
      mine_polygons <- NULL
      pangaear::pg_data(doi = doi)
      unzip(pangaear::pg_cache$list(), exdir = "data/polygons/")
      mine_polygons <- sf::read_sf("data/polygons/global_mining_polygons_v1.gpkg", fid_column_name = "PID") 
    }
    
    if(!"sf" %in% class(mine_polygons)){
      stop(paste0("mine_polygons is not a sf object or sf data could not be downloaded from PANGAEA ", doi))
    }
    
    mine_polygons %>% 
      rename_at(vars(matches("^geometry$")), function(x) "geom") %>% 
      rename_at(vars(matches("^FID$")), function(x) "PID") %>% 
      st_make_valid() %>% 
      filter(st_is_valid(geom)) %>% 
      sf::st_write(str_c(dirname(root_path), "/mine_polygons.gpkg"), delete_dsn = TRUE) 
    
    mine_points <- sf::read_sf(str_c(dirname(root_path), "/mine_polygons.gpkg"), fid_column_name = "PID") %>% 
      sf::st_centroid()
    
    root_geoms <- st_read(root_path)
    
    # intersect ---------------------------------------------------------------
    # Attention: Executing the next lines might take some time!
    mine_intersects <- st_intersects(root_geoms, mine_points)
    mine_concordance <- tibble(GID_id = root_geoms$GID_id[lengths(mine_intersects)>0],
                               PID = mine_intersects[lengths(mine_intersects)>0])
    
    # Summary matches:
    n_p <- nrow(mine_points)
    n_pm <- length(unique(unlist(mine_concordance$PID)))
    n_r <- nrow(root_geoms)
    n_rm <- nrow(mine_concordance)
    
    mine_concordance <- unnest(mine_concordance, cols = PID) %>%
      mutate(PID = mine_points$PID[PID], AREA = mine_points$AREA[mine_points$PID %in% PID])
    
    # Summary area:
    a_p <- sum(mine_points$AREA, na.rm = TRUE)
    a_pm <- sum(mine_concordance$AREA, na.rm = TRUE)
    
    print(paste0("Polygons mateched to root ", n_pm,"/", n_p, " (", round(n_pm / n_p * 100, digits = 2),"%)"))
    print(paste0("Area mateched to root ", round(a_pm, 2),"/", round(a_p, 2), " sq. km (", round( a_pm / a_p * 100, digits = 2),"%)"))
    print(paste0("Roots mateched ", n_rm ,"/", n_r, " (", round( n_rm / n_r * 100, digits = 2),"%)"))
    
    write_csv(mine_concordance, str_c(dirname(root_path), "/concordance_root_poly.csv")) 
  } else {
    mine_concordance <- read_csv(str_c(dirname(root_path), "/concordance_root_poly.csv")) 
  }

  return(mine_concordance)
  
}

# # map ---------------------------------------------------------------------
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
# p <- poly_in_roots %>% 
#   dplyr::mutate(AREA = ifelse(AREA == 0, NA, AREA)) %>%
#   sf::st_transform(crs = sf::st_crs("+proj=robin")) %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(aes(fill = AREA), size = .05, ) +
#   viridis::scale_fill_viridis(direction = -1) +
#   ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
#   ggplot2::theme(legend.position = "none")
# 
# # save
# ggplot2::ggsave(paste0("mining_areas_world.png"),
#                 plot = p, device = "png",
#                 path = "./img",
#                 scale = 1, width = 320, height = 180, units = "mm")
# 
# # South America
# lim <- zoom_bbox %>%
#   dplyr::filter(region == "South America")
# 
# p <- poly_in_roots %>% 
#   dplyr::mutate(AREA = ifelse(AREA == 0, NA, AREA)) %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(aes(fill = AREA), size = .05, ) +
#   viridis::scale_fill_viridis(direction = -1) +
#   ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
#   ggplot2::theme(legend.position = "none")
# 
# # save
# ggplot2::ggsave(paste0("mining_areas_south_america.png"),
#                 plot = p, device = "png",
#                 path = "./img",
#                 scale = 1, width = 180, height = 240, units = "mm")
# 
# # Africa
# lim <- zoom_bbox %>%
#   dplyr::filter(region == "Africa")
# 
# p <- poly_in_roots %>% 
#   dplyr::mutate(AREA = ifelse(AREA == 0, NA, AREA)) %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(aes(fill = AREA), size = .05, ) +
#   viridis::scale_fill_viridis(direction = -1) +
#   ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
#   ggplot2::theme(legend.position = "none")
# 
# # save
# ggplot2::ggsave(paste0("mining_areas_africa.png"),
#                 plot = p, device = "png",
#                 path = "./img",
#                 scale = 1, width = 180, height = 180, units = "mm")
# 
# 
# # Oceania
# lim <- zoom_bbox %>%
#   dplyr::filter(region == "Oceania")
# 
# p <- poly_in_roots %>% 
#   dplyr::mutate(AREA = ifelse(AREA == 0, NA, AREA)) %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(aes(fill = AREA), size = .05, ) +
#   viridis::scale_fill_viridis(direction = -1) +
#   ggplot2::coord_sf(datum = NA, xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) +
#   ggplot2::theme(legend.position = "none")
# 
# # save
# ggplot2::ggsave(paste0("mining_areas_oceania.png"),
#                 plot = p, device = "png",
#                 path = "./img",
#                 scale = 1, width = 180, height = 180, units = "mm")














