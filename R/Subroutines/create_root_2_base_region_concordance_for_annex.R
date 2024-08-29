


tmp <- Conco$Root_2_base |> as.data.frame()
colnames(tmp) <- 1:32  
tmp <- cbind(tmp, "root_region" = rownames(tmp))

fin <- tmp %>% 
  pivot_longer(cols = as.character(1:32), 
               names_to = "base_region") %>% 
  filter(value == 1) %>%
  mutate(root_region = as.numeric(root_region),
         base_region = as.numeric(base_region)) %>% 
  left_join(root$region, by = c("root_region" = "Code")) %>% 
  select(root_region, Name, RootCountryAbbreviation, base_region) %>% 
  left_join(base$region, by = c("base_region" = "Code")) %>% 
  select(-base_region, -Name.y, -Abbrev, -Region) %>% 
  rename("root_region_index" = "root_region",
         "root_region_name" = "Name.x",
         "root_region_abbreviation" = "RootCountryAbbreviation",
         "13_world_region_classification" = "Region_new")

write.xlsx(fin, "./output/13_world_region_classification_concordance.xlsx")
