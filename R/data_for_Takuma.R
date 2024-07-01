### This script extracts and transforms multipliers for Takuma Watari's work where
### he is interested in the scrap content of intermediate/traded products

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
IOT <<- Build_IOT(SUT = SUT,mode = "pxp")


## Full flow table (Z) + Final demand (Y) + boundary in and outputs (as negatives)
Z <- IOT$Z
colnames(Z) <- str_c(Code$Z$RegionName,"_",Code$Z$SectorName)
rownames(Z) <- str_c(Code$Z$RegionName,"_",Code$Z$SectorName)

# Remove own-flows (because of manufacturing)
diag(Z) <- 0

y <- Agg(IOT$y, aggkey = Code$Y$RegionName, dim = 2 )
colnames(y) <- str_c( colnames(y), "_FinalDemand")

v <- t(IOT$e)
rownames(v) <- str_c( Code$V$Boundary,"_", Code$V$Entity  )

# Change sign to negative inputs to have balances
v[c(6,7),] <- -v[c(6,7),]

Full_IOT <- rbind(Z, v) 
Full_IOT <- cbind(Full_IOT, rbind(y, matrix(0, nrow = 7, ncol = 32)) )

# Remove very small numbers
Full_IOT[Full_IOT < 10^-3 & Full_IOT > -10^-3] <- 0

# Check process balances to exclude multipliers for processes where there are very small flows (<1000t)

output <- data.frame("value" = rowSums(Full_IOT[1:1248,]) ,
                  "region" = Code$Z$RegionName,
                  "sector" = Code$Z$SectorName,
                  "SectorIndex" = Code$Z$SectorIndex) %>% 
  filter(value > 1)

rownames(output) <- NULL

## PIVOT list of multipliers (filter out flows where there is less than 1000t production)
L <- IOT$L
colnames(L) <- 1:dim(L)[2]

df <- as.data.frame(L) %>% 
  bind_cols(Code$Z) %>% 
  pivot_longer(names_to = "index_to", cols = as.character(1:dim(L)[2])) %>% 
  rename("RegionCode_From" = "RegionCode",
         "RegioName_From" = "RegionName", 
         "SectorCode_From" = "SectorCode",
         "SectorName_From" = "SectorName",
         "Index_From" = "SectorIndex",
         "Index_To" = "index_to") %>% 
  select(-Index, -EntityCode, -EntityName) %>%
  mutate(Index_To = as.numeric(Index_To)) %>%
  filter(Index_To %in% output$SectorIndex,
         Index_From %in% output$SectorIndex) %>% 
  left_join(Code$Z, by = c("Index_To" = "SectorIndex")) %>% 
  rename("RegionCode_To" = "RegionCode",
         "RegionName_To" = "RegionName", 
         "SectorCode_To" = "SectorCode",
         "SectorName_To" = "SectorName") %>% 
  select(-Index, -EntityCode, -EntityName) %>% 
  mutate("Source" = ifelse(RegionCode_From == RegionCode_To, "Domestic", "Trade")) %>% 
  filter(value >= 10^-6) %>% 
  group_by(RegionCode_To, 
           RegionName_To, 
           SectorCode_To,
           SectorName_To,
           SectorCode_From, 
           SectorName_From,
           Source) %>% 
  summarise(value = sum(value))

## Aggregate and filter for multipliers relevant for Takuma
InputSteelMaking <- data.frame("SectorName_From" = c("Steel scrap", "Forming scrap", "Pig iron", "Sponge iron"),
                                "InputSteelMaking" = c("Scrap","Scrap", "Primary iron", "Primary iron"))

df_L_takuma <- df %>% 
  filter(SectorName_From %in% InputSteelMaking$SectorName_From) %>% 
  left_join(InputSteelMaking) %>% 
  group_by(InputSteelMaking, Source, RegionName_To, SectorName_To) %>% 
  summarise(value = sum(value), .groups = 'drop') %>% 
  filter(SectorName_To %in% base$product$Name[7:36]) %>% 
  rename("region" = "RegionName_To",
         "source" = "Source",
         "product" = "SectorName_To",
         "multiplier" = "value",
         "UpstreamInputSteelMaking" = "InputSteelMaking")
  


## PIVOT list of actual flows (Z)
Z <- IOT$Z
colnames(Z) <- 1:dim(Z)[2]

# Remove own-flows (because of manufacturing)
diag(Z) <- 0

df_Z <- as.data.frame(Z) %>% 
  bind_cols(Code$Z) %>% 
  pivot_longer(names_to = "index_to", cols = as.character(1:dim(L)[2])) %>% 
  rename("RegionCode_From" = "RegionCode",
         "RegionName_From" = "RegionName", 
         "SectorCode_From" = "SectorCode",
         "SectorName_From" = "SectorName",
         "Index_From" = "SectorIndex",
         "Index_To" = "index_to") %>% 
  mutate(Index_To = as.numeric(Index_To)) %>%
  select(-Index, -EntityCode, -EntityName) %>%
  filter(Index_To %in% output$SectorIndex,
         Index_From %in% output$SectorIndex) %>% 
  left_join(Code$Z, by = c("Index_To" = "SectorIndex")) %>% 
  rename("RegionCode_To" = "RegionCode",
         "RegionName_To" = "RegionName", 
         "SectorCode_To" = "SectorCode",
         "SectorName_To" = "SectorName") %>% 
  select(-Index, -EntityCode, -EntityName) %>% 
  mutate("Source" = ifelse(RegionCode_From == RegionCode_To, "Domestic", "Trade")) %>% 
  filter(value >= 1) %>% 
  group_by(RegionName_From, 
           RegionName_To, 
           SectorName_From,
           SectorName_To,
           Source) %>% 
  summarise(value = sum(value), .groups = 'drop')

# Transform Final Demand matrix for including trade in final products
colnames(y) <- 1:dim(y)[2]

df_y <- as.data.frame(y) %>% 
  bind_cols(Code$Z) %>% 
  pivot_longer(names_to = "RegionCode_To", cols = as.character(1:32)) %>% 
  rename("RegionCode_From" = "RegionCode",
         "RegionName_From" = "RegionName", 
         "SectorCode_From" = "SectorCode",
         "SectorName_From" = "SectorName",
         "Index_From" = "SectorIndex") %>% 
  mutate("SectorName_To" = "Final demand",
         RegionCode_To = as.numeric(RegionCode_To)) %>% 
  select(-Index, -EntityCode, -EntityName) %>% 
  left_join(base$region[,c("Code", "Name")], by = c("RegionCode_To" = "Code")) %>% 
  rename("RegionName_To" = "Name") %>% 
  mutate("Source" = ifelse(RegionCode_From == RegionCode_To, "Domestic", "Trade")) %>% 
  filter(value >= 10^-6) %>% 
  select(RegionName_From, RegionName_To, SectorName_From, SectorName_To, Source, value)

# Add Z and Y flows together in one data frame
df_z_y_takuma <- df_Z %>% 
  bind_rows(df_y) %>% 
  rename("region_from" = "RegionName_From",
         "region_to" = "RegionName_To",
         "product_from" = "SectorName_From",
         "product_to" = "SectorName_To")

# Calculate primary iron and scarp footprints of final demand



str(y)


fd <- df_z_y_takuma %>%
  filter(product_to == "Final demand",
         product_from %in% base$product$Name[27:36]) %>%
  rename("product" = "product_from",
         "source" = "Source",
         "finaldemand" = "value") %>%
  select(-product_to, -source)

# Check sums
fd %>% summarise(finaldemand = sum(finaldemand))

mp <- as.data.frame(L) %>%
  bind_cols(Code$Z) %>%
  pivot_longer(names_to = "Index_To", cols = as.character(1:dim(L)[2])) %>%
  rename("RegionCode_From" = "RegionCode",
         "RegionName_From" = "RegionName",
         "SectorCode_From" = "SectorCode",
         "SectorName_From" = "SectorName",
         "Index_From" = "SectorIndex") %>%
  select(-Index, -EntityCode, -EntityName) %>%
  mutate(Index_To = as.numeric(Index_To)) %>%
  filter(Index_To %in% output$SectorIndex,
         Index_From %in% output$SectorIndex) %>%
  left_join(Code$Z, by = c("Index_To" = "SectorIndex")) %>%
  rename("RegionCode_To" = "RegionCode",
         "RegionName_To" = "RegionName",
         "SectorCode_To" = "SectorCode",
         "SectorName_To" = "SectorName") %>%
  select(-Index, -EntityCode, -EntityName) %>%
  mutate("Source" = ifelse(RegionCode_From == RegionCode_To, "Domestic", "Trade")) %>%
  filter(value > 10^-6,
         SectorName_To %in% base$product$Name[27:36],
         SectorName_From %in% InputSteelMaking$SectorName_From) %>%
  left_join(InputSteelMaking) %>%
  group_by(InputSteelMaking,
           RegionName_From,
           RegionName_To,
           SectorName_To) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
#  filter(SectorName_To %in% base$product$Name[27:36]) %>%
  rename("product" = "SectorName_To",
         "multiplier" = "value",
         "UpstreamInputSteelMaking" = "InputSteelMaking",
         "region_from" = "RegionName_From",
         "region_to" = "RegionName_To")


fp <- fd %>%
  left_join(mp, by = c("region_from" = "region_to", "product" = "product"), relationship = "many-to-many") %>%
  mutate("footprint" = multiplier * finaldemand) %>%
  mutate("source" = ifelse(region_from == region_to, "Domestic", "Trade")) %>% 
  group_by(region_to, UpstreamInputSteelMaking, product) %>% 
  summarise(footprint = sum(footprint), .groups = 'drop') 
  
write.xlsx(fp, "./output/FP_upstream.xlsx")

fp %>% group_by(UpstreamInputSteelMaking) %>%
  summarise(footprint = sum(footprint))


# L_upstream <- Agg( L[Code$Z$SectorIndex[ Code$Z$SectorName %in% InputSteelMaking$SectorName_From],], 
#                    rep(c("Primary iron", "Primary iron", "Scrap", "Scrap"), 32), 
#                    1)
# 
# FP_upstream <- L_upstream %*% y
# rowSums(FP_upstream)

# Create root-to-base region concordance
colnames( Conco$Root_2_base ) <- 1:32

conco_takuma <- Conco$Root_2_base %>% 
  bind_cols(root$region) %>% 
  pivot_longer(names_to = "BaseRegionIndex", cols = as.character(1:32)) %>% 
  mutate(BaseRegionIndex = as.numeric(BaseRegionIndex)) %>% 
  rename("RootRegionName" = "Name",
         "RootRegionCode" = "Code") %>% 
  left_join(base$region[,1:2], by = c("BaseRegionIndex" = "Code")) %>% 
  filter(value == 1) %>% 
  rename("region" = "Name",
         "index" = "BaseRegionIndex") %>% 
  select(-value)



# Clean data frames for writing to folder
output_takuma <- output %>% 
  rename("product" = "sector") %>% 
  select(-SectorIndex)

regions_takuma  <- base$region %>% 
  rename("region" = "Name",
         "region_group" = "Region",
         "index" = "Code") %>% 
  select(-Region_new, -Abbrev)

products_takuma <- base$product %>% 
  rename("index" = "Code",
         "product" = "Name",
         "type" = "Type",
         "type_2" = "Type_2",
         "data_feed" = "feed") %>% 
  select(-Aggregate)

UpstreamInputSteelMaking <- InputSteelMaking %>% 
  rename("product" = "SectorName_From",
         "UpstreamInputSteelMaking" = "InputSteelMaking")
  
# Write files to folder
folder <- "./output/Multipliers for Takuma/"

list_files <- list("Multipliers" = df_L_takuma,
                   "InterIndustryFlows" = df_z_y_takuma,
                   "GrossProduction" = output_takuma,
                   "ConsumptionFootprints" = fp,
                   "FinalDemandSteel" = fd,
                   "ProductClassification" = products_takuma,
                   "UpstreamInputSteelMaking" = UpstreamInputSteelMaking,
                   "BaseRegionClassification" = regions_takuma,
                   "Root2BaseRegionsConcordance" = conco_takuma)

write.xlsx(list_files, 
           str_c(folder,"IronSteel_PIOT_2014_data_for_Takuma.xlsx"),
           rowNames = FALSE)


  


# Direct scrap content Austria / X

# Z <- IOT$Z
# colnames(Z) <- 1:dim(Z)[2]
# 
# tmp <- as.data.frame(Z) %>% 
#   bind_cols(Code$Z) %>% 
#   pivot_longer(names_to = "index_to", cols = as.character(1:dim(L)[2])) %>% 
#   rename("RegionCode_From" = "RegionCode",
#          "RegioName_From" = "RegionName", 
#          "SectorCode_From" = "SectorCode",
#          "SectorName_From" = "SectorName",
#          "Index_From" = "SectorIndex",
#          "Index_To" = "index_to") %>% 
#   select(-Index, -EntityCode, -EntityName) %>%
#   mutate(Index_To = as.numeric(Index_To)) %>% 
#   left_join(Code$Z, by = c("Index_To" = "SectorIndex")) %>% 
#   rename("RegionCode_To" = "RegionCode",
#          "RegionName_To" = "RegionName", 
#          "SectorCode_To" = "SectorCode",
#          "SectorName_To" = "SectorName") %>% 
#   select(-Index, -EntityCode, -EntityName) %>% 
#   filter(RegionCode_To == 17, 
#                SectorCode_To %in% c(4:6), 
#                SectorCode_From %in% c(2,37,38)) %>% 
#   group_by(RegionName_To, SectorName_From) %>% 
#   summarise(value = sum(value))
# 
# tmp$value <- 100* tmp$value / sum(tmp$value)

