### This script extracts and transforms multipliers for Takuma Watari's work where
### he is interested in the scrap content of intermediate/traded products



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
Full_IOT[Full_IOT < 10^-3] <- 0

## PIVOT list of multipliers
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
  left_join(Code$Z, by = c("Index_To" = "SectorIndex")) %>% 
  rename("RegionCode_To" = "RegionCode",
         "RegionName_To" = "RegionName", 
         "SectorCode_To" = "SectorCode",
         "SectorName_To" = "SectorName") %>% 
  select(-Index, -EntityCode, -EntityName) %>% 
  mutate("Source" = ifelse(RegionCode_From == RegionCode_To, "Domestic", "Trade")) %>% 
  filter(value > 10^-6) %>% 
  group_by(RegionCode_To, 
           RegionName_To, 
           SectorCode_To,
           SectorName_To,
           SectorCode_From, 
           SectorName_From,
           Source) %>% 
  summarise(value = sum(value))


## PIVOT list of actual flows (Z)
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
  left_join(Code$Z, by = c("Index_To" = "SectorIndex")) %>% 
  rename("RegionCode_To" = "RegionCode",
         "RegionName_To" = "RegionName", 
         "SectorCode_To" = "SectorCode",
         "SectorName_To" = "SectorName") %>% 
  select(-Index, -EntityCode, -EntityName) %>% 
  mutate("Source" = ifelse(RegionCode_From == RegionCode_To, "Domestic", "Trade")) %>% 
  filter(value > 10^-6) %>% 
  group_by(RegionCode_To, 
           RegionName_To, 
           SectorCode_To,
           SectorName_To,
           SectorCode_From, 
           SectorName_From,
           Source) %>% 
  summarise(value = sum(value))


# Direct scrap content Austria / X

Z <- IOT$Z
colnames(Z) <- 1:dim(Z)[2]

tmp <- as.data.frame(Z) %>% 
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
  left_join(Code$Z, by = c("Index_To" = "SectorIndex")) %>% 
  rename("RegionCode_To" = "RegionCode",
         "RegionName_To" = "RegionName", 
         "SectorCode_To" = "SectorCode",
         "SectorName_To" = "SectorName") %>% 
  select(-Index, -EntityCode, -EntityName) %>% 
  filter(RegionCode_To == 17, 
               SectorCode_To %in% c(4:6), 
               SectorCode_From %in% c(2,37,38)) %>% 
  group_by(RegionName_To, SectorName_From) %>% 
  summarise(value = sum(value))

tmp$value <- 100* tmp$value / sum(tmp$value)

# Write files to folder
folder <- "./output/Multipliers for Takuma/"

list_files <- list("Multipliers" = df,
                   "Full_IOT" = Full_IOT)

write.xlsx(list_files, 
           str_c(folder,"IronSteel_PIOT_pxp_2014_for_Takuma.xlsx"),
           rowNames = TRUE)

