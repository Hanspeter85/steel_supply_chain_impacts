

MEME_decomposition <- function(reg_1, reg_2)#df is a dataframe with 4 columns: sector, Year, environmental_pressure, output
{
  # Read population data for selected regions 1 & 2
  pop_IDA <- c( pop_agg$value[pop_agg$region == region_agg[reg_1]], pop_agg$value[pop_agg$region == region_agg[reg_2]])
  
  # Create empty data frames for region data sets
  reg_data <- data.frame("index" = 1:2,
                         "region" = c(region_agg[reg_1], region_agg[reg_2]),
                         "POP" = pop_IDA,
                         "GAS" = NA,
                         "RMC" = NA,
                         "eLand" = NA,
                         "eHANPP" = NA,
                         "GAS_per_POP" = NA,
                         "RMC_per_GAS" = NA,
                         "Land_per_RMC" = NA,
                         "HANPP_per_Land" = NA,
                         "HANPP_per_POP" = NA)
  
  # Write values into data sets
  reg_data$GAS[1] <- sum( Results_agg$value[Results_agg$destination_region_group == region_agg[reg_1] & Results_agg$stressor == "Steel_GAS"] )
  reg_data$RMC[1] <- sum( Results_agg$value[Results_agg$destination_region_group == region_agg[reg_1] & Results_agg$stressor == "RMC"] )
  reg_data$eLand[1] <- sum( Results_agg$value[Results_agg$destination_region_group == region_agg[reg_1] & Results_agg$stressor == "eLand"] )
  reg_data$eHANPP[1] <- sum( Results_agg$value[Results_agg$destination_region_group == region_agg[reg_1] & Results_agg$stressor == "eHANPP"] )
  
  reg_data$GAS[2] <- sum( Results_agg$value[Results_agg$destination_region_group == region_agg[reg_2] & Results_agg$stressor == "Steel_GAS"] )
  reg_data$RMC[2] <- sum( Results_agg$value[Results_agg$destination_region_group == region_agg[reg_2] & Results_agg$stressor == "RMC"] )
  reg_data$eLand[2] <- sum( Results_agg$value[Results_agg$destination_region_group == region_agg[reg_2] & Results_agg$stressor == "eLand"] )
  reg_data$eHANPP[2] <- sum( Results_agg$value[Results_agg$destination_region_group == region_agg[reg_2] & Results_agg$stressor == "eHANPP"] )
  
  
  # Calculate shares and ratios
  reg_data$GAS_per_POP <- reg_data$GAS / reg_data$POP
  reg_data$RMC_per_GAS <- reg_data$RMC / reg_data$GAS
  reg_data$Land_per_RMC <- reg_data$eLand / reg_data$RMC
  reg_data$HANPP_per_Land <- reg_data$eHANPP / reg_data$eLand
  reg_data$HANPP_per_POP <- reg_data$eHANPP / reg_data$POP
  
  # Calculate differences between regions
  delta <- reg_data[2,3:12] - reg_data[1,3:12]
  
  # delta <- reg_data[,3:13]
  # delta[,2:11] <- reg_2_data[,4:13] - reg_1_data[,4:13]
  # pop_delta <- pop_IDA[2] - pop_IDA[1]
  
  ### eHANPP per capita decomposition
  L_term <- delta$HANPP_per_POP / log( reg_data$HANPP_per_POP[2] / reg_data$HANPP_per_POP[1] )
  
  effect_GAS_per_POP <- L_term * log( reg_data$GAS_per_POP[2] / reg_data$GAS_per_POP[1] )
  effect_RMC_per_GAS <- L_term * log( reg_data$RMC_per_GAS[2] / reg_data$RMC_per_GAS[1] )
  effect_Land_per_RMC <- L_term * log( reg_data$Land_per_RMC[2] / reg_data$Land_per_RMC[1] )
  effect_HANPP_per_Land <- L_term * log( reg_data$HANPP_per_Land[2] / reg_data$HANPP_per_Land[1] )
  
  effect_total <- effect_GAS_per_POP + effect_RMC_per_GAS + effect_Land_per_RMC + effect_HANPP_per_Land
  
  IDA_result <- data.frame("effect" = c("GAS_per_head",
                                        "RMC_per_GAS",
                                        "Land_per_RMC",
                                        "HANPP_per_Land",
                                        "TOTAL_delta"),
                           "value" = c(effect_GAS_per_POP,
                                       effect_RMC_per_GAS,
                                       effect_Land_per_RMC,
                                       effect_HANPP_per_Land,
                                       effect_total) )
  
  
  # Transform values into grams carbon per capita per year
  IDA_result$value <- IDA_result$value * 10^6
  
  return(IDA_result)
}