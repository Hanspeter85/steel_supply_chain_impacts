
Footprint_calculation <- function(IOT_model)
{
  x <- rowSums(IOT_model$L %*% IOT_model$y)                 # Estimate new gross production
  x_original <- rowSums(IOT$L %*% IOT$y)                    # Estimate original gross production
  Y <- Agg(IOT_model$y, rep(base$region$Name, each = 6),2)  # Aggregate columns in consuming regions
  
  E_flow_new <- ( E_flow[,c("AREA", "HANPP")]/x_original ) * x
  intens <- cbind("Extraction" = IOT_model$e[, 4], E_flow_new ) / x

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
    MP <- IOT_model$L * intens[,i]  # Multipliers
    
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
  
  ## Next we compute index-based indicators
  
  # Get indices for iron ore extraction and select relevant cells in extension
  index <- Code$Z[ Code$Z$SectorCode == 1, ]
  E_index_new <- E_index[index$SectorIndex,]
  
  # Set AWARE indices for neighboring countries accordingly
  E_index_new[8,2] <- E_index_new[29,2]   # Set Greece index to RoW Europe
  E_index_new[13,2] <- E_index_new[29,2]   # Set Slovakia index to RoW Europe
  E_index_new[14,2] <- E_index_new[29,2]   # Set UK index to RoW Europe
  E_index_new[26,2] <- E_index_new[27,2]   # Set Indonesia index to RoW Asia
  
  # Use average for all other regions
  E_index_new <- as.data.frame(E_index_new)
  E_index_new$AWARE[E_index_new$AWARE == 0] <- mean(E_index_new$AWARE[E_index_new$AWARE != 0])
  
  
  
  
  
  
  
  
  
  
  Result <- data.frame("index" = 1:(nrow(index)*n$reg),
                       "year" = 2014,
                       "source_region" = rep(index$RegionName, n$reg),
                       "destination_region" = rep(base$region$Name, each = nrow(index) ),
                       "final_demand" = rep(index$SectorName, n$reg),
                       "value" = NA,
                       "unit" = "tons/year",
                       "stressor" = "Steel_GAS")
  
  
  
  
  
  
  # add information about region groups
  Results <- left_join(Results, base$region[,c("Name", "Region_new")], by = c("destination_region" = "Name"))
  Results <- left_join(Results, base$region[,c("Name", "Region_new")], by = c("source_region" = "Name"))
  colnames(Results)[9:10] <- c("destination_region_group","source_region_group")
  
  # Change to factor levels to always kee the same order
  # Results$final_demand <- factor(x = Results$final_demand, levels = base$industry$Name[20:29])
  # Results$destination_region_group <- factor(x = Results$destination_region_group, region_agg)
  # Results$source_region_group <- factor(x = Results$source_region_group, region_agg)
  
  # write.xlsx(x = Results, file = "./output/Sector_footprints.xlsx")
  
  remove(labels, MP, n, Y, i, r, x, intens, FP, Result, index, y_sel)
  
  return(Results)
  
}