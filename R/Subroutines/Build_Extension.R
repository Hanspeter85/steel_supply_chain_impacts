## Load pre-processed extensions and create single data frame

# Disaggregate iron extraction into biomes
E1 <- Build_Extension_Biomes()
E1 <- t( ( t(E1) / colSums(E1) ) * IOT$e[,4] )
E1[is.na(E1)] <- 0
print( str_c("Sum of iron ore extraction by biome in 2014: ", sum(E1) ," t/year") )

# Load other extensions
E2 <- Build_Extension_AREA_HANPP_AWARE()
E2 <- as.data.frame(E2)
print( str_c("Sum of HANPP of iron ore extraction in 2014: ", rowSums(E2[1,]) ," tC/year") )
print( str_c("Sum of direct land use for iron ore extraction in 2014: ", rowSums(E2[2,]) ," km2/year") )
E3 <- as.data.frame( Build_Extension_Biodiversity_Carbon_Water_Score() )
colnames(E3) <- "Biodiversity_Carbon_Water"

# integrate extensions and discern flow- from index-based extensions
E_flow <- t( rbind(E1, E2)[-15,] )
E_index <- as.matrix( cbind( E3, t( E2[3,] ) ) )

remove(E1,E2,E3)