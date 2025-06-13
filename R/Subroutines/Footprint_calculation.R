

  x <- rowSums(IOT$L %*% IOT$y)                       # Estimate new gross production
  Y <- Agg(IOT$y, rep(base$region$Name, each = 6),2)  # Aggregate columns in consuming regions
  
  E_flow_new <- E_flow[,c("AREA", "HANPP")]
  intens <- cbind("Extraction" = IOT$e[, 4], E_flow_new ) / x

  labels <- data.frame("stressor" = c("RMC","eLand","eHANPP"),
                       "unit" = c( "tons/year", "km2/year", "tC/year" ) )
  
  n <- list("reg" = 32,
            "sec" = 10,
            "stress" = 3)
  
  Result <- data.frame("index" = 1:(n$reg*n$reg*n$sec*n$stress),
                       "year" = 2014,
                       "source_region" = rep(base$region$Name, (n$sec * n$reg * n$stress)),
                       "destination_region" = rep(base$region$Name, each = (n$sec*n$reg) ),
                       "final_demand" = rep(base$industry$Name[20:29], each = n$reg),
                       "value" = NA,
                       "unit" = rep(labels$unit, each = n$reg*n$reg*n$sec),
                       "stressor" = rep(labels$stressor, each = n$reg*n$reg*n$sec) )
  
  # For code verification
  # i <- 1
  # r <- 1
  
  # calculate footprints
  for(i in 1:3)
  {
    MP <- IOT$L * intens[,i]  # Multipliers
    
    for(r in 1:32)
    {
      FP <- t( t(MP) * Y[,r] )# Footprints
      FP <- Agg(FP, Code$Z$SectorCode, 2)   # Aggregate columns into industries
      FP <- Agg(FP, Code$Z$RegionCode , 1)  # Aggregate rows into source regions
      
      FP <- FP[,20:29]
      
      Result$value[Result$stressor == labels$stressor[i] & Result$destination_region == base$region$Name[r] ] <- FP
    }
  }
  
  FP <- Result
  
  ## Write steel use (GAS) into results
  
  # Get index and labels for steel use in final/manufactured products
  index <- Code$Z[ Code$Z$SectorCode %in% 20:29, ]
  
  Result <- data.frame("index" = 1:(nrow(index)*n$reg),
                       "year" = 2014,
                       "source_region" = rep(index$RegionName, n$reg),
                       "destination_region" = rep(base$region$Name, each = nrow(index) ),
                       "final_demand" = rep(index$SectorName, n$reg),
                       "value" = NA,
                       "unit" = "tons/year",
                       "stressor" = "Steel_GAS")
  
  for(r in 1:32)
  {
    y_sel <- Y[index$SectorIndex, r]
    Result$value[Result$destination_region == base$region$Name[r]] <- y_sel
  }
  
  Results <- rbind(FP, Result)
  
  # add information about region groups
  Results <- left_join(Results, base$region[,c("Name", "Region_new")], by = c("destination_region" = "Name"))
  Results <- left_join(Results, base$region[,c("Name", "Region_new")], by = c("source_region" = "Name"))
  colnames(Results)[9:10] <- c("destination_region_group","source_region_group")
  
  
  ## Calculate RMC by type of biome
  # Set aggregation key for biomes
  agg_key_biome <- data.frame("stressor" = colnames(E_flow)[1:12],
                              "stressor_group" = c("Temperate Forests & Grasslands", "Deserts & Xeric Shrublands", "Mediterranean Forests & Woodlands", "Temperate Forests & Grasslands",
                                                   "Tropical/subtropical Forests & Grasslands", "Temperate Forests & Grasslands", "Tropical/subtropical Forests & Grasslands", "Tropical/subtropical Forests & Grasslands",
                                                   "Taiga & Tundra", "Montane Grasslands & Shrublands", "Tropical/subtropical Forests & Grasslands", "Taiga & Tundra") )
  
  E_flow_new <- E_flow[,1:12]/x
  
  Result <- data.frame("index" = 1:(n$reg*n$reg*n$sec*12),
                       "year" = 2014,
                       "source_region" = rep(base$region$Name, (n$sec * n$reg * 12)),
                       "destination_region" = rep(base$region$Name, each = (n$sec*n$reg) ),
                       "source_region_group" = rep(base$region$Region_new, (n$sec * n$reg * 12)),
                       "destination_region_group" = rep(base$region$Region_new, each = (n$sec*n$reg) ),
                       "final_demand" = rep(base$industry$Name[20:29], each = n$reg),
                       "value" = NA,
                       "unit" = labels$unit[1],
                       "stressor" = rep(agg_key_biome$stressor, each = (n$reg*n$reg*n$sec)),
                       "stressor_group" = rep(agg_key_biome$stressor_group, each = (n$reg*n$reg*n$sec)))
  
  # For code verification
  # i <- 1
  # r <- 1
  
  # calculate footprints
  for(i in 1:12)
  {
    MP <- IOT$L * E_flow_new[,i]  # Multipliers
    
    for(r in 1:32)
    {
      FP <- t( t(MP) * Y[,r] )# Footprints
      FP <- Agg(FP, Code$Z$SectorCode, 2)   # Aggregate columns into industries
      FP <- Agg(FP, Code$Z$RegionCode , 1)  # Aggregate rows into source regions
      
      FP <- FP[,20:29]
      
      Result$value[Result$stressor == colnames(E_flow_new)[i] & Result$destination_region == base$region$Name[r] ] <- FP
    }
  }
  
  Results_sector_biomes <- Result
  # write.xlsx(x = Result, file = "./output/Sector_footprints_by_biome.xlsx")
  
  remove(labels, MP, n, Y, i, r, x, intens, FP, Result, index, y_sel, E_flow, E_flow_new, E_index)

  # Set ranking of regions for visualization and for aggregation
  region_agg <- c("China","Europe","United States","Australia","Asia and Pacific (nec)", 
                  "Middle East","Brazil","South America (nec)","Japan","Canada","Russia","India","Africa")
  
  
  agg_key_enduse <- data.frame( "final_demand" = unique(Results_sector_biomes$final_demand),
                                "end_use" = c("Other products", "Stationary machinery", "Stationary machinery", "Stationary machinery",
                                              "Stationary machinery", "Other products", "Transport machinery", "Transport machinery",
                                              "Other products", "Construction materials") )
  
  enduse_order <- c("Other products", "Stationary machinery", "Transport machinery", "Construction materials")
  
  # Aggregate results into region groups (region_agg)
  # Set factor levels for industries to always maintain the same order
  # Add end use group and set ordering
  Results_agg <- Results %>% select(final_demand,value,unit,stressor,source_region_group, destination_region_group)  %>% 
    group_by(stressor,source_region_group, destination_region_group,final_demand) %>% summarise(value = sum(value)) %>% 
    mutate(source = ifelse(source_region_group == destination_region_group, "Domestic", "Import")) %>% 
    mutate(final_demand = factor(final_demand, levels = base$industry$Name[20:29])) %>% 
    left_join(agg_key_enduse, by = "final_demand") %>% 
    mutate(end_use = factor(end_use, levels = enduse_order))
  
  # write.xlsx(x = Results_agg, file = "./output/Footprint_indicators_13_world_regions.xlsx")
  
  
  # Aggregate biomes results and add end use group
  Results_agg_biome <- Results_sector_biomes %>% select(final_demand,value,unit,stressor,stressor_group,source_region_group, destination_region_group) %>%
    group_by(stressor,stressor_group,source_region_group, destination_region_group,final_demand) %>% summarise(value = sum(value)) %>% 
    mutate(source = ifelse(source_region_group == destination_region_group, "Domestic", "Import")) %>% 
    left_join(agg_key_enduse, by = "final_demand") %>% 
    mutate(end_use = factor(end_use, levels = enduse_order))
  
  # Full detail data sets for paper SI
  SI <- list()
  SI[["IOMF_eLand_eHANPP_GAS"]] <- Results %>% 
    left_join(agg_key_enduse, by = c("final_demand")) %>% 
    rename("end_use_group" = "end_use",
           "end_use" = "final_demand") %>% 
    select(source_region, destination_region, source_region_group, destination_region_group,
           end_use, end_use_group, stressor, unit, value) %>% 
    mutate(stressor = case_when(stressor == "RMC" ~ "IO-MF",
                                stressor == "eLand" ~ "eLand-steel",
                                stressor == "eHANPP" ~ "eHANPP-steel",
                                stressor == "Steel_GAS" ~ "Steel-GAS")) %>% 
    filter(value >= 1)
  
  SI[["IO-MF-biome"]] <- Results_sector_biomes %>% 
    left_join(agg_key_enduse, by = c("final_demand")) %>% 
    rename("end_use_group" = "end_use",
           "end_use" = "final_demand") %>% 
    select(source_region, destination_region, source_region_group, destination_region_group,
           end_use, end_use_group, stressor, stressor_group, unit, value) %>% 
    filter(value >= 1)
  
  
  
  remove(Results_sector_biomes)
  