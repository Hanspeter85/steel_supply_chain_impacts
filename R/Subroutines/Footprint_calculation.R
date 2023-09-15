
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
  
  # add information about region groups
  Results <- left_join(Results, base$region[,c("Name", "Region_new")], by = c("destination_region" = "Name"))
  Results <- left_join(Results, base$region[,c("Name", "Region_new")], by = c("source_region" = "Name"))
  colnames(Results)[9:10] <- c("destination_region_group","source_region_group")
  
  
  
  ## Next we compute index-based indicators
  
  # Get indices for iron ore extraction and select relevant cells in extension
  index <- Code$Z[ Code$Z$SectorCode == 1, ]
  E_index_new <- E_index[index$SectorIndex,]
  
  # Set AWARE indices for neighboring countries accordingly
  E_index_new[8,2] <- E_index_new[29,2]   # Set Greece index to RoW Europe
  E_index_new[13,2] <- E_index_new[29,2]   # Set Slovakia index to RoW Europe
  E_index_new[14,2] <- E_index_new[29,2]   # Set UK index to RoW Europe
  E_index_new[26,2] <- E_index_new[27,2]   # Set Indonesia index to RoW Asia
  
  # Use average for all other regions (note that the missing indices refer to regions with less than 1t of extraction)
  E_index_new <- as.data.frame(E_index_new)
  E_index_new$AWARE[E_index_new$AWARE == 0] <- mean(E_index_new$AWARE[E_index_new$AWARE != 0])
  
  # i <- 1
  for(i in 1:2)
  {
    # Calculate RMC
    MP <- IOT_model$L * intens[,1]
    RMC <- MP %*% Y
    RMC <- Agg(RMC, Code$Z$RegionCode, 1)
    
    # Multiply embodied flows with indices
    RMC_index <- RMC * E_index_new[,i]
    
    RMC <- Agg(RMC, aggkey = base$region$Region_new, 2)
    RMC <- Agg(RMC, aggkey = base$region$Region_new, 1)
    
    RMC_index <- Agg(RMC_index, aggkey = base$region$Region_new, 2)
    RMC_index <- Agg(RMC_index, aggkey = base$region$Region_new, 1)
    
    DE_index <- rowSums(RMC_index) / rowSums(RMC)
    RMC_index <- colSums(RMC_index) / colSums(RMC)
    
    if(i == 1)
    {
      Result_DE_index_regions <- data.frame("Region" = names(DE_index),
                                            "DE" = rowSums(RMC),
                                            "Biodiversity_Carbon_Water_index" = DE_index,
                                            "AWARE_index" = NA)
      
      Result_RMC_index_regions <- data.frame("Region" = names(RMC_index),
                                             "RMC" = colSums(RMC),
                                             "Biodiversity_Carbon_Water_index" = RMC_index,
                                             "AWARE_index" = NA)
    }else
    {
      Result_DE_index_regions$AWARE_index <- DE_index
      Result_RMC_index_regions$AWARE_index <- RMC_index
    }
  }
  
  # Add trade shares
  Result_DE_index_regions["EX_share"] <- (rowSums(RMC)-diag(RMC))/rowSums(RMC)
  Result_RMC_index_regions["IM_share"] <- (colSums(RMC)-diag(RMC))/colSums(RMC)
  
  
  RMC_sec <- t( t(MP) * rowSums(Y) )
  RMC_sec <- Agg(RMC_sec, Code$Z$RegionCode, 1)
  RMC_sec <- Agg(RMC_sec, base$region$Region_new, 1)
  RMC_sec <- Agg(RMC_sec, Code$Z$SectorCode, 2)
  RMC_sec <- RMC_sec[,20:29]
  colnames(RMC_sec) <- base$industry$Name[20:29]
  
  list_of_tables <- list("DE_index" = Result_DE_index_regions,
                         "RMC_index" = Result_RMC_index_regions,
                         "RMC" = RMC,
                         "RMC_sec" = RMC_sec)
  
  write.xlsx(list_of_tables, file = "./output/Index_assessment_iron_steel_2014_aggregated_regions.xlsx")
  
  
  ## Calculate RMC by type of biome
  
  E_flow_new <- E_flow[,1:12]/x
  
  sum(E_flow_new)
  
  Result <- data.frame("index" = 1:(n$reg*n$reg*n$sec*12),
                       "year" = 2014,
                       "source_region" = rep(base$region$Name, (n$sec * n$reg * 12)),
                       "destination_region" = rep(base$region$Name, each = (n$sec*n$reg) ),
                       "source_region_group" = rep(base$region$Region_new, (n$sec * n$reg * 12)),
                       "destination_region_group" = rep(base$region$Region_new, each = (n$sec*n$reg) ),
                       "final_demand" = rep(base$industry$Name[20:29], each = n$reg),
                       "value" = NA,
                       "unit" = labels$unit[1],
                       "stressor" = rep(colnames(E_flow)[1:12], each = (n$reg*n$reg*n$sec)) )
  
  # For code verification
  # i <- 1
  # r <- 1
  
  # calculate footprints
  for(i in 1:12)
  {
    MP <- IOT_model$L * E_flow_new[,i]  # Multipliers
    
    for(r in 1:32)
    {
      FP <- t( t(MP) * Y[,r] )# Footprints
      FP <- Agg(FP, Code$Z$SectorCode, 2)   # Aggregate columns into industries
      FP <- Agg(FP, Code$Z$RegionCode , 1)  # Aggregate rows into source regions
      
      FP <- FP[,20:29]
      
      Result$value[Result$stressor == colnames(E_flow_new)[i] & Result$destination_region == base$region$Name[r] ] <- FP
    }
  }
  
  write.xlsx(x = Result, file = "./output/Sector_footprints_by_biome.xlsx")
  
  
  ## Compute Sankey of RME trade flows
  # Package
  library(networkD3)
  library(tidyverse)
  library(data.table)
  
  data <- RMC
  diag(data) <- 0
  data[rownames(data) == "Japan"] <- 0
  data <- cbind("rowname" = rownames(data), data)
  data <- as.data.table(data)
  
  # I need a long format
  data_long <- data %>%
    gather(key = 'key', value = 'value', -rowname) %>%
    filter(value > 0)
  
  colnames(data_long) <- c("source", "target", "value")
  data_long$target <- paste(data_long$target, " ", sep="")
  
  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  data_long$IDsource=match(data_long$source, nodes$name)-1 
  data_long$IDtarget=match(data_long$target, nodes$name)-1
  
  
  # prepare colour scale
  ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'
  
  # Make the Network
  sankeyNetwork(Links = data_long, Nodes = nodes,
                Source = "IDsource", Target = "IDtarget",
                Value = "value", NodeID = "name", 
                sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=27, nodePadding=20)
  
  
  remove(labels, MP, n, Y, i, r, x, intens, FP, Result, index, y_sel)
  
  return(Results)
  
}