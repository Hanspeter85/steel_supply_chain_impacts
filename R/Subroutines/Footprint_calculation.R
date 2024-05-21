

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
  
  
  
  # ## Next we compute index-based indicators
  # 
  # # Get indices for iron ore extraction and select relevant cells in extension
  # index <- Code$Z[ Code$Z$SectorCode == 1, ]
  # E_index_new <- E_index[index$SectorIndex,]
  # 
  # # Set AWARE indices for neighboring countries accordingly
  # E_index_new[8,2] <- E_index_new[29,2]   # Set Greece index to RoW Europe
  # E_index_new[13,2] <- E_index_new[29,2]   # Set Slovakia index to RoW Europe
  # E_index_new[14,2] <- E_index_new[29,2]   # Set UK index to RoW Europe
  # E_index_new[26,2] <- E_index_new[27,2]   # Set Indonesia index to RoW Asia
  # 
  # # Use average for all other regions (note that the missing indices refer to regions with less than 1t of extraction)
  # E_index_new <- as.data.frame(E_index_new)
  # E_index_new$AWARE[E_index_new$AWARE == 0] <- mean(E_index_new$AWARE[E_index_new$AWARE != 0])
  # 
  # # i <- 1
  # for(i in 1:2)
  # {
  #   # Calculate RMC
  #   MP <- IOT_model$L * intens[,1]
  #   RMC <- MP %*% Y
  #   RMC <- Agg(RMC, Code$Z$RegionCode, 1)
  #   
  #   # Multiply embodied flows with indices
  #   RMC_index <- RMC * E_index_new[,i]
  #   
  #   RMC <- Agg(RMC, aggkey = base$region$Region_new, 2)
  #   RMC <- Agg(RMC, aggkey = base$region$Region_new, 1)
  #   
  #   RMC_index <- Agg(RMC_index, aggkey = base$region$Region_new, 2)
  #   RMC_index <- Agg(RMC_index, aggkey = base$region$Region_new, 1)
  #   
  #   DE_index <- rowSums(RMC_index) / rowSums(RMC)
  #   RMC_index <- colSums(RMC_index) / colSums(RMC)
  #   
  #   if(i == 1)
  #   {
  #     Result_DE_index_regions <- data.frame("Region" = names(DE_index),
  #                                           "DE" = rowSums(RMC),
  #                                           "Biodiversity_Carbon_Water_index" = DE_index,
  #                                           "AWARE_index" = NA)
  #     
  #     Result_RMC_index_regions <- data.frame("Region" = names(RMC_index),
  #                                            "RMC" = colSums(RMC),
  #                                            "Biodiversity_Carbon_Water_index" = RMC_index,
  #                                            "AWARE_index" = NA)
  #   }else
  #   {
  #     Result_DE_index_regions$AWARE_index <- DE_index
  #     Result_RMC_index_regions$AWARE_index <- RMC_index
  #   }
  # }
  # 
  # # Add trade shares
  # Result_DE_index_regions["EX_share"] <- (rowSums(RMC)-diag(RMC))/rowSums(RMC)
  # Result_RMC_index_regions["IM_share"] <- (colSums(RMC)-diag(RMC))/colSums(RMC)
  # 
  # 
  # RMC_sec <- t( t(MP) * rowSums(Y) )
  # RMC_sec <- Agg(RMC_sec, Code$Z$RegionCode, 1)
  # RMC_sec <- Agg(RMC_sec, base$region$Region_new, 1)
  # RMC_sec <- Agg(RMC_sec, Code$Z$SectorCode, 2)
  # RMC_sec <- RMC_sec[,20:29]
  # colnames(RMC_sec) <- base$industry$Name[20:29]
  # 
  # list_of_tables <- list("DE_index" = Result_DE_index_regions,
  #                        "RMC_index" = Result_RMC_index_regions,
  #                        "RMC" = RMC,
  #                        "RMC_sec" = RMC_sec)
  # 
  # write.xlsx(list_of_tables, file = "./output/Index_assessment_iron_steel_2014_aggregated_regions.xlsx")
  
  
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
  
  
  # Aggregate results into region groups (region_agg)
  # Results_agg <- Results %>% mutate(source = ifelse(source_region_group == destination_region_group, "Domestic", "Import")) %>% 
  #   select(final_demand,value,unit,stressor,destination_region_group, source) %>% 
  #   group_by(source, stressor,destination_region_group,final_demand) %>% summarise(value = sum(value)) 
  
  # Aggregate results into region groups (region_agg)
  Results_agg <- Results %>% select(final_demand,value,unit,stressor,source_region_group, destination_region_group)  %>% 
    group_by(stressor,source_region_group, destination_region_group,final_demand) %>% summarise(value = sum(value)) %>% 
    mutate(source = ifelse(source_region_group == destination_region_group, "Domestic", "Import"))
  
  # Set factor levels for industries to always maintain the same order
  Results_agg$final_demand <- factor(x = Results_agg$final_demand, levels = base$industry$Name[20:29]) 
  # write.xlsx(x = Results_agg, file = "./output/Footprint_indicators_13_world_regions.xlsx")
  
  # Aggregate biomes results
  Results_agg_biome <- Results_sector_biomes %>% select(final_demand,value,unit,stressor,stressor_group,source_region_group, destination_region_group) %>%
    group_by(stressor,stressor_group,source_region_group, destination_region_group,final_demand) %>% summarise(value = sum(value)) %>% 
    mutate(source = ifelse(source_region_group == destination_region_group, "Domestic", "Import"))
    
  remove(Results_sector_biomes)
  