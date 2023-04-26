### Script to create roots geometry (see data flow chart)

# World Bank ---------------------------------------------------------------
# Requires the official geometries from the World Bank 
# 
# 
create_wb_root <- function(root_path){
  
  if(! file.exists(root_path)){
    
    dir.create(dirname(root_path), showWarnings = FALSE, recursive = TRUE)
    
    # Check if WB geom file exists -- Direct download from onedrive did not work
    if (!file.exists("data/WB/TH boundaries.zip") | !file.exists("data/WB/WB_disputed_areas_Admin0_10m.zip")){
      stop("You must provide the WB geometry files. Download Admin0 and 1: https://1drv.ms/u/s!Ao9KqGg3WGJVgZ1lbGrFJFOtGr_GYA?e=1eIOl5 Disputed territories: https://1drv.ms/u/s!Ao9KqGg3WGJVgZUkQpUo47YB19TcnA?e=OuBvZj")
    } else {
      unzip("data/WB/TH boundaries.zip", exdir = "data/WB")
      unzip("data/WB/WB_disputed_areas_Admin0_10m.zip", exdir = "data/WB")
    }

    # read and correct WB geometries
    if(!file.exists(stri_c(dirname(root_path), "/wb_geom_adm1.gpkg"))){
      wb_geom <- st_read("data/WB/ADM1_TH.shp") %>% 
        mutate(index = row_number()) %>% 
        select("index", "ADM0_CODE", "ADM0_NAME", "ADM1_CODE", "ADM1_NAME", "DISP_AREA", "STATUS") 
      
      # try to correct bad geometries 
      bad_wb_geom <- filter(wb_geom, !st_is_valid(geometry)) %>% 
        st_transform("+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs") %>%
        st_simplify() %>%
        st_transform(st_crs(wb_geom))
      
      bad_wb_geom <- bad_wb_geom %>% 
        st_make_valid()
        
      bad_wb_geom1 <- bad_wb_geom
      st_precision(bad_wb_geom1) <- 0.00001
      
      bad_wb_geom[!st_is_valid(bad_wb_geom$geometry),] <- 
        st_set_precision(bad_wb_geom[!st_is_valid(bad_wb_geom$geometry),], 0.00001) %>% 
        st_make_valid() 
      
        st_geometry(bad_wb_geom) %>% 
        plot()
      
      # merge corrected geometries
      wb_geom[wb_geom$index %in% bad_wb_geom$index,] <- bad_wb_geom
      
      st_write(wb_geom, stri_c(dirname(root_path), "/wb_geom_adm1.gpkg"), delete_dsn = TRUE)
    } 
    
    wb_geom <- st_read(stri_c(dirname(root_path), "/wb_geom_adm1.gpkg"))
  
    # library(stringdist) # some help for partial string matching
    # country_match <- amatch(wb_geom$ADM0_NAME, gdam_adm0$NAME_0,maxDist=50,nomatch=0)
    # the output was manually corrected and saved to config/wb_gadm_concordance.csv
    wb_gadm_concordance <- read_csv("config/wb_gadm_concordance.csv", show_col_types = FALSE)
    
    wb_geom <- left_join(wb_geom, wb_gadm_concordance, by = c("ADM0_NAME" = "ADM0_NAME"))
    
    wb_root_geom <- filter(wb_geom, DISP_AREA == "NO") %>% 
      mutate(NAME_1 = ADM1_NAME,
             NAME_2 = NA,
             NAME_3 = NA,
             NAME_4 = NA) %>% 
      group_by(GID_0) %>% 
      mutate(GID_id = str_c(GID_0, str_pad(row_number(), width = 4, pad = "0"), sep = "_")) %>% 
      ungroup() %>% 
      select(index, GID_id, GID_0, NAME_0, NAME_1, NAME_2, NAME_3, NAME_4, geom)
    
    st_write(wb_root_geom, dsn = root_path, delete_dsn = TRUE)
    
  } else {
    wb_root_geom <- st_read(root_path)
  }
  
  return(wb_root_geom)
  
}
