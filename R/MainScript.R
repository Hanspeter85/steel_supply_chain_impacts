
# Load packages
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
# library(scales)
# library(ggridges)
library(viridis)
# library(hrbrthemes)
# library(ggpubr)
# library(ggExtra)
library(stringr)
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
github <- "C:/Users/hpwie/OneDrive/Github_repositories"


# Set paths to important folders
path <<- list("input" = paste0(github,"/steel_supply_chain_impacts/input/"),
              "output" = paste0(github,"/PIOLab/Analysis/output/"),
              "run" = paste0(github,"/PIOLab/Analysis/input/AISHA_runs/"),
              "repo" = paste0(github,"/steel_supply_chain_impacts"),
              "SI" = paste0(github,"/steel_supply_chain_impacts/output/SI"),
              "concordance" = paste0(github,"/steel_supply_chain_impacts/input/Concordances/"),
              "subroutines" = paste0(github,"/steel_supply_chain_impacts/R/Subroutines") )

## Load functions and general data frames into workspace
source( paste0(path$subroutines,"/Load_Routines.R") )

## Create supply use tables from raw data and compile IO model
SUT <<- Load_SUT(type = "Results")                    
IOT <<- Build_IOT(SUT = SUT,mode = "ixi")                          
IOT_HEM <<- Run_Hypothetical_Extraction_Method()

## Load pre-processed extensions and create single data frame
# Disaggregate iron extraction into biomes
E1 <- Build_Extension_Biomes()
E1 <- t( ( t(E1) / colSums(E1) ) * IOT$e[,4] )
E1[is.na(E1)] <- 0
print( str_c("Sum of iron ore extraction by biome in 2014: ", sum(E1) ," t/year") )
# Load other extensions
E2 <- Build_Extension_AREA_HANPP_AWARE()
E2 <- as.data.frame(E2)
print( str_c("Sum of HANPP of iron ore extraction in 2014: ", rowSums(E2[1,]) ," tC/year") )
print( str_c("Sum of direct land use for iron ore extraction in 2014: ", rowSums(E2[2,]) ," km2/year") )
E3 <- as.data.frame( Build_Extension_Biodiversity_Carbon_Water_Score() )
colnames(E3) <- "Biodiversity_Carbon_Water"
# integrate extensions and discern flow- from index-based extensions
E_flow <- t( rbind(E1, E2)[-15,] )
E_index <- as.matrix( cbind( E3, t( E2[3,] ) ) )
remove(E1,E2,E3)

# Set ranking of regions for visualization
region_agg <- c("China","Europe","United States","Australia","Asia and Pacific (nec)", 
                "Middle East","Brazil","South America (nec)","Japan","Canada","Russia","India","Africa")

## Calculate all footprints on sectoral from-to level
source( paste0(path$subroutines,"/Footprint_calculation.R") )

# Load and aggregate population data
pop <- read.xlsx(paste0(path$repo,"/input/EXIOBASE/EXIOBASE population data.xlsx"),sheet = 3) %>%
  select(EXIOcode,as.character(job$year))
pop <- pop[1:49,]                               # Clean pop data
pop <- colSums(Conco$EXIO_2_base * pop[,2])     # Aggregate to base classification
pop_agg <- data.frame("region" = base$region$Region_new, "value" = pop) %>% group_by(region) %>% summarise(value = sum(value))
pop_agg <- as.data.frame(pop_agg)

Results <- Footprint_calculation(IOT_model = IOT)
Results_HEM <- Footprint_calculation(IOT_model = IOT_HEM)
sum(Results$value)
sum(Results_HEM$value)



Results_compare <- cbind(Results,Results_HEM$value)
colnames(Results_compare)[c(6,11)] <- c("Baseline","HEM_scenario")

write.xlsx(x = Results_compare, file = "./output/Sector_footprints_HEM_scenario.xlsx")

# Aggregate to region groups
# Results <- Results %>% select(final_demand,destination_region_group, value, stressor) %>% group_by(final_demand,destination_region_group, stressor) %>% 
#   summarise(value = sum(value))
# 
# Results_HEM <- Results_HEM %>% select(final_demand,destination_region_group, value, stressor) %>% group_by(final_demand,destination_region_group, stressor) %>% 
#   summarise(value = sum(value))

## Calculate traditional ewMFA indicators
ewMFA <- cbind( base$region[,c(2,3,5)],Calc_ewMFA(IOT = IOT, Code = Code) )
ewMFA["Net-trade"] <- ewMFA$Import - ewMFA$Export
# Read RMC account
tmp <- Results %>% filter(stressor == "RMC") %>% select(destination_region, value) %>% 
  group_by(destination_region) %>% summarise(value = sum(value)) 
# Add RMC to other variables
ewMFA <- left_join(ewMFA, tmp, by = c("Name" = "destination_region"))
colnames(ewMFA)[9] <- "RMC"

## Calculate traditional ewMFA indicators with HEM scenario
ewMFA_HEM <- cbind( base$region[,c(2,3,5)],Calc_ewMFA(IOT = IOT_HEM, Code = Code) )
ewMFA_HEM["Net-trade"] <- ewMFA_HEM$Import - ewMFA_HEM$Export
# Read RMC account
tmp <- Results_HEM %>% filter(stressor == "RMC") %>% select(destination_region, value) %>% 
  group_by(destination_region) %>% summarise(value = sum(value)) 
# Add RMC to other variables
ewMFA_HEM <- left_join(ewMFA_HEM, tmp, by = c("Name" = "destination_region"))
colnames(ewMFA_HEM)[9] <- "RMC"

ewMFA <- melt(ewMFA,id.vars = colnames(ewMFA)[1:3])
ewMFA_HEM <- melt(ewMFA_HEM,id.vars = colnames(ewMFA_HEM)[1:3])

ewMFA_comparison_HEM_scenario <- ewMFA
colnames(ewMFA_comparison_HEM_scenario)[5] <- "Baseline"
ewMFA_comparison_HEM_scenario <- cbind(ewMFA_comparison_HEM_scenario,"HEM_scenario" = ewMFA_HEM$value)

write.xlsx(x = ewMFA_comparison_HEM_scenario, file = "./output/ewMFA_HEM_scenario.xlsx")

# The region list
region_agg

reg_order <- c(10,9,4,2,3,11,6,7,8,5,13,12)

r <- 1
for(r in 1:length(reg_order))
{
  tmp <- MEME_decomposition(reg_1 = 1, reg_2 = reg_order[r])
  if(r == 1) result_IDA <- tmp
  if(r != 1) result_IDA <- cbind(result_IDA,tmp$value)
  
}

colnames(result_IDA)[2:13] <- region_agg[reg_order]

write.xlsx(x = result_IDA, file = "./output/IDA_eHANPP_per_cap_result_2014_China_vs_all_other_regions.xlsx")



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















