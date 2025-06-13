
labels <- data.frame("stressor" = c("RMC","eLand","eHANPP"),
                     "unit" = c( "tons/year", "km2/year", "tC/year" ) )

n <- list("reg" = 32,
          "stress" = 3,
          "biome" = 12)

Result_EXIO <- list()

Result_EXIO[["FP"]] <- data.frame("index" = 1:(n$reg*n$reg*n$stress),
                                  "year" = 2014,
                                  "source_region" = rep(base$region$Name, (n$reg * n$stress)),
                                  "destination_region" = rep(base$region$Name, each = n$reg ),
                                  "value" = NA,
                                  "stressor" = rep(labels$stressor, each = (n$reg*n$reg) ) )

Result_EXIO[["biome"]] <- data.frame("index" = 1:(n$reg*n$reg*n$biome),
                                     "year" = 2014,
                                     "source_region" = rep(base$region$Name, (n$reg * n$biome)),
                                     "destination_region" = rep(base$region$Name, each = n$reg ),
                                     "value" = NA,
                                     "stressor" = rep(agg_key_biome$stressor, each = (n$reg*n$reg) ),
                                     "stressor_group" = rep(agg_key_biome$stressor_group, each = (n$reg*n$reg) ) )

x <- rowSums(EXIO$L %*% EXIO$Y)

intens <- data.frame("DE" = rowSums(E_flow_EXIO[,1:12]),
                     "HANPP" = E_flow_EXIO[,13],
                     "AREA" = E_flow_EXIO[,14]) %>% 
  as.matrix()/x

intens[is.na(intens)] <- 0
intens[intens == Inf] <- 0

# calculate footprints (MF, eLand, eHANPP)
i <- 1
for(i in 1:3)
{
  MP <- EXIO$L * intens[,i]
  
  FP <- MP %*% EXIO$Y
  FP <- Agg(FP, rep(as.character(1:32), each = 200) , 1)  # Aggregate rows into source regions
    
  Result_EXIO$FP$value[Result_EXIO$FP$stressor == labels$stressor[i]] <- c(FP)
}

intens <- E_flow_EXIO[,1:12]/x

intens[is.na(intens)] <- 0
intens[intens == Inf] <- 0


# For code verification
# i <- 1
# r <- 1

# calculate biome footprints
for(i in 1:12)
{
  MP <- EXIO$L * intens[,i]
  
  FP <- MP %*% EXIO$Y
  FP <- Agg(FP, rep(as.character(1:32), each = 200) , 1)  # Aggregate rows into source regions
  
  Result_EXIO$biome$value[Result_EXIO$biome$stressor == colnames(intens)[i]] <- c(FP)
}

### NEXT litegrate with PIOT result and make rocket plot like figure

CHECK <- Result_EXIO$biome %>% 
  group_by(stressor) %>% 
  summarize(EXIO = sum(value), .groups = 'drop') %>% 
  left_join(SI$`IO-MF-biome` %>% 
              group_by(stressor) %>% 
              summarize(PIOT = sum(value),.groups = 'drop'),
            by = c("stressor"))

