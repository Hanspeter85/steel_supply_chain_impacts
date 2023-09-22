
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
              "subroutines" = paste0(github,"/steel_supply_chain_impacts/R/Subroutines"),
              "MISO2" = paste0(github,"/MatStocks/model_input_data/EndUseShare_codes/output/MISO2_v01_stock_results/"))

## Load functions and general data frames into workspace
source( paste0(path$subroutines,"/Load_Routines.R") )

## Create supply use tables from raw data and compile IO model
SUT <<- Load_SUT(type = "Results")                    
IOT <<- Build_IOT(SUT = SUT,mode = "ixi")                          
# IOT_HEM <<- Run_Hypothetical_Extraction_Method()

## Build extensions
source( paste0(path$subroutines,"/Build_Extension.R") )

# Set ranking of regions for visualization
region_agg <- c("China","Europe","United States","Australia","Asia and Pacific (nec)", 
                "Middle East","Brazil","South America (nec)","Japan","Canada","Russia","India","Africa")

# Calculate footprints
Results <- Footprint_calculation(IOT_model = IOT)
# Results_HEM <- Footprint_calculation(IOT_model = IOT_HEM)

# Aggregate results into region groups (region_agg)
Results_agg <- Results %>% mutate(source = ifelse(source_region_group == destination_region_group, "Domestic", "Import")) %>% 
  select(final_demand,value,unit,stressor,destination_region_group, source) %>% 
  group_by(source, stressor,destination_region_group,final_demand) %>% summarise(value = sum(value)) 


# Set factor levels for industries to always maintain the same order
Results_agg$final_demand <- factor(x = Results_agg$final_demand, levels = base$industry$Name[20:29]) 

# write.xlsx(x = Results_agg, file = "./output/Footprint_indicators_13_world_regions.xlsx")




### Compile ew-MFA indicator of stocks and flows
# Traditional ew-MFA indicators (DE, DMC)
ewMFA <- cbind( base$region[,c(2,3,5)],Calc_ewMFA(IOT = IOT, Code = Code) )

ewMFA["Net-trade"] <- ewMFA$Import - ewMFA$Export

# Add Read RMC account
tmp <- Results %>% filter(stressor == "RMC") %>% select(destination_region, value) %>% 
  group_by(destination_region) %>% summarise(value = sum(value)) 

ewMFA <- left_join(ewMFA, tmp, by = c("Name" = "destination_region"))
colnames(ewMFA)[9] <- "RMC"

## Add steel consumption (GAS)
tmp <- Results %>% filter(stressor == "Steel_GAS") %>% select(destination_region, value) %>% 
  group_by(destination_region) %>% summarise(value = sum(value)) 
colnames(tmp) <- c("Name","GAS")
ewMFA <- left_join(ewMFA,tmp)

## Add population
ewMFA["Population"] <- pop

## Add steel stocks from MISO2
Stock_data <- read.csv(file = paste0(path$MISO2,"global_data_v0_6_no_uncert_enduse.csv"))
colnames(Stock_data)[6:202] <- 1820:2016
Stock_data <- Stock_data %>% filter(name == "S10_stock_enduse", material == "iron_steel") %>% select(region, "2014")

## Load region concordances to aggregate MISO2 data
Conco_EXIO2MISO <- read.csv( file = paste0(path$concordance,"Region_concordance_EXIOBASE_MISO.csv") )
Conco_EXIO2MISO <- Conco_EXIO2MISO %>% select(MISO_names, EXIOBASE_iso2)
EXIO_reg_list <- read.xlsx( xlsxFile = paste0(path$concordance,"RegionAggregator_49_to_32.xlsx") )[,1:4]

Stock_data <- left_join(Stock_data, Conco_EXIO2MISO, by = c("region" = "MISO_names"))
Stock_data <- Stock_data %>% select(EXIOBASE_iso2, "2014")
colnames(Stock_data) <- c("region", "value") 
Stock_data <- Stock_data %>% group_by(region) %>% summarise(value = sum(value))
Stock_data <- Stock_data[match(EXIO_reg_list$Abbrev,Stock_data$region),]

# MISO2 stock results are in kilo tons and need to be transformed into tons
Stock_data$value <- Stock_data$value * 1000

# Aggregate to 32 region PIOT classification
Stock_data <- colSums( Stock_data$value * Conco$EXIO_2_base )

ewMFA["Stocks"] <- Stock_data  

ewMFA <- Agg( as.matrix(ewMFA[,4:12]), aggkey = ewMFA$Region_new, dim = 1 )
ewMFA <- as.data.frame(ewMFA)

# write.xlsx(x = ewMFA, file = "./output/ewMFA_indicators_13_world_regions.xlsx", rowNames = TRUE)




### Index Decomposition Analysis for China
# Note: Maybe the decomposition does not work correctly because of new column for source region of flow. Needs to be checked.

# Define order of regions that are compared with China
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















