
# Load packages
library(openxlsx)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(tibble)
library(viridis)
library(stringr)
library(ggsci)
library(dichromat)
library(scales)
library(ggpubr)
library(cowplot)
# library(scales)
# library(ggridges)
# library(hrbrthemes)
# library(ggpubr)
# library(ggExtra)
# library(sf)
# library(RSQLite)
# library(readr)
# library(dbplyr)
# library(stringi)
# library(pangaear)
# library(raster)
# library(parallel)

# Set parameters to select raw data files (version and/or year) for the construction of PIOTs
job <<- list("date" = "20201218",
             "phase" = "666",
             "loop" = "666",
             "year" = 2014,
             "RegAgg" = "032",
             "IEdatafeed" = "Ind30Pro39v1")

# Set path to folder with GitHub repositories:
github <- "C:/Users/hwieland/Desktop/GitHub"

# Set paths to important folders
path <<- list("input" = paste0(github,"/steel_supply_chain_impacts/input/"),
              "output" = paste0(github,"/PIOLab/Analysis/output/"),
              "run" = paste0(github,"/PIOLab/Analysis/input/AISHA_runs/"),
              "repo" = paste0(github,"/steel_supply_chain_impacts"),
              "SI" = paste0(github,"/steel_supply_chain_impacts/output/SI"),
              "concordance" = paste0(github,"/steel_supply_chain_impacts/input/Concordances/"),
              "subroutines" = paste0(github,"/steel_supply_chain_impacts/R/Subroutines"),
              "MISO2" = "C:/Users/hwieland/Data/MISO2/")

## Load functions and general data frames into workspace
source( paste0(path$subroutines,"/Load_Routines.R") )

## Create supply use tables from raw data and compile IO model
SUT <<- Load_SUT(type = "Results")                    
IOT <<- Build_IOT(SUT = SUT,mode = "ixi")                          

## Build extensions
source( paste0(path$subroutines,"/Build_Extension.R") )

# Calculate footprints
source("./R/Subroutines/Footprint_calculation.R")

# Create ew-MFA data set with all accounts including stocks
source("./R/Subroutines/Compile_full_ewMFA_dataset.R")

# Remove unnecessary objects
remove(job, github, Stock_data, pop, tmp, Conco, Conco_EXIO2MISO, EXIO_reg_list, num, 
       pop_agg, Results, IOT, SUT, root, path, base, Code, agg_key_biome, enduse_order, region_agg)

# Remove unnecessary funtions
remove(Build_Extension_AREA_HANPP_AWARE, Build_Extension_Biodiversity_Carbon_Water_Score, Build_Extension_Biomes,
       Build_IOT, Calc_ewMFA, Calc_FP, Load_IOCodes, Load_population_data, Load_SUT, MEME_decomposition, Plot_HeadlineIndicators,
       Run_Hypothetical_Extraction_Method)






# Run IDA for China and Europe
source("./R/Subroutines/Calc_IDA.R")

































res_agg$destination_region_group <- factor( res_agg$destination_region_group, levels = region_ranking)

# Data frame to consistently link world regions to specific symbols and colors in the scatter plot
world_scatter <<- data.frame("index" = 1:10,
                             "name" = region_ranking,
                             "symbol" = 1:10,
                             "color" = viridis_pal()(10),
                             stringsAsFactors = FALSE)

res_agg_sel <- res_agg
res_agg_sel$value[res_agg_sel$stressor == "HANPP"] <- res_agg_sel$value[res_agg_sel$stressor == "HANPP"]/1000
res_agg_sel$value[res_agg_sel$stressor == "Extraction"] <- res_agg_sel$value[res_agg_sel$stressor == "Extraction"]/1000000
res_agg_sel$stressor[res_agg_sel$stressor == "Extraction"] <- "Extraction [Mt/y]"
res_agg_sel$stressor[res_agg_sel$stressor == "AREA"] <- "AREA [km2/y]"
res_agg_sel$stressor[res_agg_sel$stressor == "HANPP"] <- "HANPP [ktC/y]"


ggplot(res_agg_sel, aes(destination_region_group, value, fill = stressor)) + 
  geom_col(position = "dodge")


res_agg_sel <- res_agg %>% filter(stressor %in% c("Extraction", "AREA")) %>% spread(stressor, value)




# Scatter <- function()
# {
  
  scatter <- ggplot( data = res_agg_sel, aes(x = AREA, y = Extraction, color = destination_region_group ) ) +
    geom_point(alpha = 0.8, size=4, aes(shape = destination_region_group) ) +
    scale_shape_manual(values = world_scatter$symbol ) +
    scale_color_manual(values = world_scatter$color) +
    scale_x_continuous( expand = c(intersec,intersec),
                        limits = c(lim[1],lim[2]),
                        breaks = lim[1]:lim[2]) + 
    scale_y_continuous( expand = c(intersec,intersec),
                        limits = c(lim[1],lim[2]),
                        breaks = lim[1]:lim[2] ) +
    labs( x= paste0("",x_lab," [log10 tonnes]"),
          y = paste0(y_lab," [log10 tonnes]") ) +
    theme( panel.grid.minor = element_blank(), 
           legend.position = "top",
           text = element_text(size=14),
           legend.text=element_text(size=14),
           legend.title=element_blank(),
           legend.background = element_rect(fill="gray90", size=.5) ) +
    geom_abline(intercept = 0.15, slope = 1, linetype = 3, alpha = 0.5) + 
    geom_abline(intercept = -0.15, slope = 1, linetype = 3, alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, linetype = 2, alpha = 0.5)
  
#   return(scatter)
# }















