



dim(IOT$L)


IOT <<- Build_IOT(SUT = SUT,mode = "pxp")

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

# Maybe also add the EoL scrarp onput

write.xlsx(df, str_c("./output/L_pxp_2014.xlsx"))
write.csv(df, str_c("./output/L_pxp_2014.csv"), row.names = TRUE)

getwd()


head(df)
