
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
library(data.table)


# Set parameters to select raw data files (version and/or year) for the construction of PIOTs
job <<- list("date" = "20201218",
             "phase" = "666",
             "loop" = "666",
             "year" = 2014,
             "RegAgg" = "032",
             "IEdatafeed" = "Ind30Pro39v1")

# Set path to folder with GitHub repositories:
github <- "C:/Users/hwieland/Desktop/GitHub"
# github <- "C:/Github_repositories/"

# Set paths to important folders
path <<- list("input" = paste0(github,"/steel_supply_chain_impacts/input/"),
              "output" = paste0(github,"/PIOLab/Analysis/output/"),
              "run" = paste0(github,"/PIOLab/Analysis/input/AISHA_runs/"),
              "repo" = paste0(github,"/steel_supply_chain_impacts"),
              "SI" = paste0(github,"/steel_supply_chain_impacts/output/SI"),
              "concordance" = paste0(github,"/steel_supply_chain_impacts/input/Concordances/"),
              "subroutines" = paste0(github,"/steel_supply_chain_impacts/R/Subroutines"))

# Set path to MISO2 data
path[["MISO2"]] <- "C:/Users/hwieland/Data/MISO2/"
# path[["MISO2"]] <- "C:/Users/hpwie/BOKU/Data/MISO2/documents-export-2024-3-27/"

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

# Run IDA for India, China and Europe
source("./R/Subroutines/Calc_IDA.R")

# Remove unnecessary objects
remove(job, github, Stock_data, pop, tmp, Conco, Conco_EXIO2MISO, EXIO_reg_list, num, pop_agg, result_IDA,
       Results, IOT, SUT, root, path, base, Code, agg_key_biome, enduse_order, region_agg, supply)

# Remove unnecessary functions
remove(Build_Extension_AREA_HANPP_AWARE, Build_Extension_Biodiversity_Carbon_Water_Score, Build_Extension_Biomes,
       Build_IOT, Calc_ewMFA, Calc_FP, Load_IOCodes, Load_population_data, Load_SUT, Plot_HeadlineIndicators,
       Run_Hypothetical_Extraction_Method)


# Create figures
source("./R/Subroutines/Plot_figure_1.R")
source("./R/Subroutines/Plot_figure_2.R")
source("./R/Subroutines/Plot_figure_3.R")
source("./R/Subroutines/Plot_figure_4.R")

source("./R/Subroutines/Plot_figure_5.R")

# Write data of figures for SI to file
write.xlsx(data_SI, "./output/Figure_data_for_SI.xlsx")










