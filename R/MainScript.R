
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
              "concordance" = paste0(github,"/steel_supply_chain_impacts/ConcordanceLibrary"),
              "subroutines" = paste0(github,"/steel_supply_chain_impacts/R/Subroutines") )

# Load functions into workspace
source( paste0(path$subroutines,"/Load_Routines.R") )

# Read IO model codes
Code <<- Load_IOCodes() 

# Create supply use tables from raw data
SUT <<- Load_SUT(type = "Results")       

# Create PIOTs
IOT <<- Build_IOT(SUT,"ixi")      # Compile IO model






# Calculate ewMFA indicators from gPIOT
ewMFA <- Calc_ewMFA(IOT, Code)

# Calculate footprints for selected stressor (i.e. boundary input/output flow)
FP <- Calc_FP("Crude Ore", 1)

# Read domestic variables/tables for specific region
SUT_sel <- Check_DomesticSUT("China")

Diagnostics()
Plot_HeadlineIndicators()
Plot_Ratios()
