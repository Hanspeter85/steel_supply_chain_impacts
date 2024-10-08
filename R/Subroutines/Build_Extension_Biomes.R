# This script calculates Iron Ore Footprints for contextual biodiversity risk

Build_Extension_Biomes <- function()
{
  filepath <- str_c("./output/Extensions/Biomes_",job$year,".xlsx")
  
  if( !file.exists(filepath) )
  {
    # Read biomes in root classification
    biome_in_root <- read.csv( str_c(path$repo,"/input/EPIP/root_geoms/biome_in_roots.csv"), sep = ",") %>%
      select(index, GID_id, GID_0, NAME_0, primary_biome)
    
    # Set preliminary biodiversity scores:                                     
    biomes <- c("Flooded Grasslands & Savannas", "Mangroves", "Tropical & Subtropical Moist Broadleaf Forests", "Tropical & Subtropical Grasslands, Savannas & Shrublands",
                "Deserts & Xeric Shrublands", "Tropical & Subtropical Dry Broadleaf Forests", "Montane Grasslands & Shrublands", 
                "Temperate Broadleaf & Mixed Forests", "Tropical & Subtropical Coniferous Forests", "Temperate Grasslands, Savannas & Shrublands",
                "Temperate Conifer Forests", "Mediterranean Forests, Woodlands & Scrub", "Boreal Forests/Taiga", "Tundra")
    
    # Read extraction in root classification
    extraction_in_root <- read.csv( str_c( "./input/EPIP/production_in_roots/Iron Ore_",job$year,"_in_roots.csv" ) ) %>% 
      select(GID_id, value)
      
    # Intersect extraction and biomes in root
    extraction_by_biome <- left_join(extraction_in_root, biome_in_root) %>% select(GID_0, primary_biome, value) %>% 
      group_by(GID_0, primary_biome) %>% summarise(value = sum(value))
        
    extraction_by_biome <- left_join( root$region %>% select(Code, RootCountryAbbreviation), 
                                      extraction_by_biome, by = c("RootCountryAbbreviation" = "GID_0"))
      
    extraction_by_biome <- reshape(extraction_by_biome, idvar = "Code", v.names = "value", 
                                   timevar = "primary_biome", direction = "wide")
      
    # Clean data frame
    rownames(extraction_by_biome) <- 1:221
    extraction_by_biome <- extraction_by_biome[, -which(names(extraction_by_biome) %in% c("RootCountryAbbreviation", "value.NA", "Code") ) ]
    colnames(extraction_by_biome) <- str_remove_all( colnames(extraction_by_biome), "value." )
    extraction_by_biome[is.na(extraction_by_biome)] <- 0
      
    ## Aggregate to base classification and calculate shares
    share <- t(extraction_by_biome) %*% Conco$Root_2_base
      
    # For regions where SNL reporst no extraction but IRP does, insert dummy valus for an even distribution
    share[, colSums(share) == 0] <- 1
      
    # Calculate shares for biomes
    share <- t( t(share) / colSums(share) )
    
    # Now transform the matrix into use-able format with full sector detail
    index <- match( str_c(1:32,"Mining"), str_c(Code$Z$RegionCode,Code$Z$SectorName)  )
    
    tmp <- matrix(0,nrow = nrow(share), ncol = 960)
    rownames(tmp) <- rownames(share)
    
    tmp[,index] <- share
    
    E <- t( t(tmp) * IOT$e[,4])
    E <- as.data.frame(E)
    
    write.xlsx(x = E, file = filepath, rowNames = TRUE)
    
  }else
  {
    E <- read.xlsx(xlsxFile = filepath, rowNames = TRUE)
  }
  
  return(E) 
}




#   ## Calculate footprints
#   x <- rowSums(IOT$L %*% IOT$y)  # Estimate new gross production
#   q <- IOT$e[, base$input$Code[base$input$Name == "Crude Ore"] ]/x
#   
#   MP <- IOT$L * q  # Material multipliers
#   FP <- MP %*% IOT$y  # Material fooptrints
#   
#   # FP <- Agg(FP, rep(Code$Y$index, each = 6),2)  # Aggregate columns in consuming regions
#   FP <- Agg(FP, rep(Code$Y$index, each = 6),2)  # Aggregate columns in consuming regions
#   FP <- Agg(FP, Code$Z %>% filter(EntityName == "Industry") %>% pull(RegionCode), 1)
#   FP <- Agg(FP, base$region$Region, 2)
#   
#   BFP <- share %*% FP/1000000
#   
#   order <- Bio_Score[ which( Bio_Score$biomes %in% rownames(BFP)  ), ]
#   
#   # colnames(BFP) <- base$region$Abbrev
#   
#   data <- melt(BFP)
#   colnames(data) <- c("Biomes", "Region", "Value")
#   data["Year"] <- year
#   
#   # if(year > 2008)
#   # {
#   #   data_final <- rbind(data_final,data)
#   # }else
#   # {
#   #   data_final <- data
#   # }
#   
#   HotSpot <- data.frame("Year" = year,
#                         "Region" = colnames(BFP),
#                         "Footprint" = colSums(BFP) )
#   
#   HotSpot["Biodiversity_Score"] <- rowSums( t(BFP * order$score) )/colSums(BFP) 
#   
#   rownames(HotSpot) <- NULL
#   
#   HotSpot <- HotSpot[ order(HotSpot$Biodiversity_Score), ]
#   
#   # if(year > 2008)
#   # {
#   #   HotSpot_final <- rbind(HotSpot_final,HotSpot)
#   # }else
#   # {
#   #   HotSpot_final <- HotSpot
#   # }
#   # ggplot(HotSpot, aes(x = Footprint, y=Biodiversity_Score) ) + 
#   #   geom_point()
# #}
# 
# # Reorder according to biodiversity score
# data$Biomes <- factor( data$Biomes, levels = order$biomes)
# data$Region <- factor( data$Region, levels = HotSpot$Region)
# 
# ggplot( data %>% filter(Year == 2016), 
#         aes(fill=Biomes, y=Value, x=Region)) + 
#   geom_bar(position="fill", stat="identity") +
#   scale_fill_viridis_d()
# 
# # Save plot: 
# ggsave(path = paste0(path$output), 
#        filename = paste0(job$year,"_Biome_Footprint_Regions.png"),
#        width = 12, height = 6.5 )
# 
# write.xlsx(HotSpot_final, file = str_c( path$output,"/Biodiversity_Hotspot_IronOre_2008-2017.xlsx"))
