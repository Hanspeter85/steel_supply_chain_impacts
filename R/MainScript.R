
# Load packages
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(scales)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(ggpubr)
library(ggExtra)
library(stringr)
library(reshape2)
library(sf)
library(RSQLite)
library(readr)
library(dbplyr)
library(stringi)
library(pangaear)
library(raster)
library(parallel)

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

# Load functions and general data frames into workspace
source( paste0(path$subroutines,"/Load_Routines.R") )

# Create supply use tables from raw data and compile IO model
SUT <<- Load_SUT(type = "Results")                    
IOT <<- Build_IOT(SUT,"ixi")                          

E1 <- Build_Extension_Biodiversity_Carbon_Water_Score()
E2 <- Build_Extension_Biomes()
E3 <- Build_Extension_AREA_HANPP_AWARE()

