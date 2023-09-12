

MEME_decomposition <- function(reg_1, reg_2)#df is a dataframe with 4 columns: sector, Year, environmental_pressure, output
{
  # Read population data for selected regions 1 & 2
  pop_IDA <- c( pop_agg$value[pop_agg$region == region_agg[reg_1]], pop_agg$value[pop_agg$region == region_agg[reg_2]])
  
  # Create empty data frames for region data sets
  reg_1_data <- reg_2_data <- data.frame("index" = 1:10,
                                         "region" = NA,
                                         "sector" = Results_agg$final_demand[1:10],
                                         "GAS" = NA,
                                         "RMC" = NA,
                                         "eLand" = NA,
                                         "eHANPP" = NA,
                                         "GAS_per_POP" = NA,
                                         "GAS_sector_share" = NA,
                                         "RMC_per_GAS" = NA,
                                         "Land_per_RMC" = NA,
                                         "HANPP_per_Land" = NA,
                                         "HANPP_per_POP" = NA)
  
  # Write values into data sets
  reg_1_data$region <- region_agg[reg_1]
  reg_1_data$GAS <- Results_agg$value[Results_agg$destination_region_group == region_agg[reg_1] & Results_agg$stressor == "Steel_GAS"]
  reg_1_data$RMC <- Results_agg$value[Results_agg$destination_region_group == region_agg[reg_1] & Results_agg$stressor == "RMC"]
  reg_1_data$eLand <- Results_agg$value[Results_agg$destination_region_group == region_agg[reg_1] & Results_agg$stressor == "eLand"]
  reg_1_data$eHANPP <- Results_agg$value[Results_agg$destination_region_group == region_agg[reg_1] & Results_agg$stressor == "eHANPP"]
  
  reg_2_data$region <- region_agg[reg_2]
  reg_2_data$GAS <- Results_agg$value[Results_agg$destination_region_group == region_agg[reg_2] & Results_agg$stressor == "Steel_GAS"]
  reg_2_data$RMC <- Results_agg$value[Results_agg$destination_region_group == region_agg[reg_2] & Results_agg$stressor == "RMC"]
  reg_2_data$eLand <- Results_agg$value[Results_agg$destination_region_group == region_agg[reg_2] & Results_agg$stressor == "eLand"]
  reg_2_data$eHANPP <- Results_agg$value[Results_agg$destination_region_group == region_agg[reg_2] & Results_agg$stressor == "eHANPP"]
  
  
  # Calculate shares and ratios
  reg_1_data$GAS_per_POP <- reg_1_data$GAS / pop_agg$value[pop_agg$region == region_agg[reg_1]]
  reg_1_data$GAS_sector_share <- reg_1_data$GAS / sum(reg_1_data$GAS)
  reg_1_data$RMC_per_GAS <- reg_1_data$RMC / reg_1_data$GAS
  reg_1_data$Land_per_RMC <- reg_1_data$eLand / reg_1_data$RMC
  reg_1_data$HANPP_per_Land <- reg_1_data$eHANPP / reg_1_data$eLand
  reg_1_data$HANPP_per_POP <- reg_1_data$eHANPP / pop_agg$value[pop_agg$region == region_agg[reg_1]]
  
  reg_2_data$GAS_per_POP <- reg_2_data$GAS / pop_agg$value[pop_agg$region == region_agg[reg_2]]
  reg_2_data$GAS_sector_share <- reg_2_data$GAS / sum(reg_2_data$GAS)
  reg_2_data$RMC_per_GAS <- reg_2_data$RMC / reg_2_data$GAS
  reg_2_data$Land_per_RMC <- reg_2_data$eLand / reg_2_data$RMC
  reg_2_data$HANPP_per_Land <- reg_2_data$eHANPP / reg_2_data$eLand
  reg_2_data$HANPP_per_POP <- reg_2_data$eHANPP / pop_agg$value[pop_agg$region == region_agg[reg_2]]
  
  # Calculate differences between regions
  delta <- reg_2_data[,3:13]
  delta[,2:11] <- reg_2_data[,4:13] - reg_1_data[,4:13]
  pop_delta <- pop_IDA[2] - pop_IDA[1]
  
  ### eHANPP per capita decomposition
  L_term <- delta$HANPP_per_POP / log( reg_2_data$HANPP_per_POP / reg_1_data$HANPP_per_POP )
  
  effect_GAS_per_POP <- L_term * log( sum(reg_2_data$GAS_per_POP) / sum(reg_1_data$GAS_per_POP) )
  effect_GAS_sector_share <- L_term * log(reg_2_data$GAS_sector_share/reg_1_data$GAS_sector_share)
  effect_RMC_per_GAS <- L_term * log(reg_2_data$RMC_per_GAS/reg_1_data$RMC_per_GAS)
  effect_Land_per_RMC <- L_term * log(reg_2_data$Land_per_RMC/reg_1_data$Land_per_RMC)
  effect_HANPP_per_Land <- L_term * log(reg_2_data$HANPP_per_Land/reg_1_data$HANPP_per_Land)
  
  effect_total <- sum(effect_GAS_per_POP) + sum(effect_GAS_sector_share) + sum(effect_RMC_per_GAS) + sum(effect_Land_per_RMC) + sum(effect_HANPP_per_Land)
  
  IDA_result <- data.frame("effect" = c("GAS_per_head",
                                        "GAS_sector_share",
                                        "RMC_per_GAS",
                                        "Land_per_RMC",
                                        "HANPP_per_Land",
                                        "TOTAL_delta"),
                           "value" = c(sum(effect_GAS_per_POP),
                                       sum(effect_GAS_sector_share),
                                       sum(effect_RMC_per_GAS),
                                       sum(effect_Land_per_RMC),
                                       sum(effect_HANPP_per_Land),
                                       effect_total))
  
  
  return(IDA_result)
  
  ### Testing IDA with decomposing GAS into population and GAS-intensity
  # print( str_c( "The difference in GAS between ", region_agg[reg_2]," and ",region_agg[reg_1], " is ", sum(delta$GAS)/1000000, " Mt") )
  # 
  # L_term <- delta$GAS / log(reg_2_data$GAS / reg_1_data$GAS)
  # sum(L_term)
  # 
  # effect_pop <- L_term * log(pop_IDA[2]/pop_IDA[1])
  # effect_GAS_per_POP <- L_term * log(reg_2_data$GAS_per_POP/reg_1_data$GAS_per_POP)
  # 
  # effect_total <- sum(effect_pop) + sum(effect_GAS_per_POP)
  # effect_total/10^6
  ###
  
  
  ### Testing IDA with decomposing RMC into pop, GAS per pop and RMC per GAS
  # print( str_c( "The difference in RMC between ", region_agg[reg_2]," and ",region_agg[reg_1], " is ", sum(delta$RMC)/10^6," Megatons/year") )
  # 
  # L_term <- delta$RMC / log( reg_2_data$RMC / reg_1_data$RMC )
  # sum(L_term)
  # 
  # effect_pop <- L_term * log(pop_IDA[2]/pop_IDA[1])
  # effect_GAS_per_POP <- L_term * log(reg_2_data$GAS_per_POP/reg_1_data$GAS_per_POP)
  # sum(effect_GAS_per_POP)
  # effect_RMC_per_GAS <- L_term * log(reg_2_data$RMC_per_GAS/reg_1_data$RMC_per_GAS)
  # 
  # effect_total <- sum(effect_pop) + sum(effect_GAS_per_POP) + sum(effect_RMC_per_GAS)
  # effect_total/10^6
  # sum(delta$RMC)/10^6
  ###
  
  ### 2.Case: Testing IDA with decomposing RMC into pop, GAS per pop and RMC per GAS
  # print( str_c( "The difference in RMC between ", region_agg[reg_2]," and ",region_agg[reg_1], " is ", sum(delta$RMC)/10^6," Megatons/year") )
  
  # L_term <- delta$RMC / log( reg_2_data$RMC / reg_1_data$RMC )
  # 
  # effect_pop <- L_term * log(pop_IDA[2]/pop_IDA[1])
  # effect_GAS_per_POP <- L_term * log( sum(reg_2_data$GAS_per_POP) / sum(reg_1_data$GAS_per_POP) )
  # effect_GAS_sector_share <- L_term * log(reg_2_data$GAS_sector_share/reg_1_data$GAS_sector_share)
  # effect_RMC_per_GAS <- L_term * log(reg_2_data$RMC_per_GAS/reg_1_data$RMC_per_GAS)
  # 
  # effect_total <- sum(effect_pop) + sum(effect_GAS_per_POP) + sum(effect_GAS_sector_share) + sum(effect_RMC_per_GAS)
  # effect_total/10^6
  # sum(delta$RMC)/10^6
  ###
  
  ### Full eHANPP decomposition
  # L_term <- delta$eHANPP / log( reg_2_data$eHANPP / reg_1_data$eHANPP )
  # 
  # effect_pop <- L_term * log(pop_IDA[2]/pop_IDA[1])
  # effect_GAS_per_POP <- L_term * log( sum(reg_2_data$GAS_per_POP) / sum(reg_1_data$GAS_per_POP) )
  # effect_GAS_sector_share <- L_term * log(reg_2_data$GAS_sector_share/reg_1_data$GAS_sector_share)
  # effect_RMC_per_GAS <- L_term * log(reg_2_data$RMC_per_GAS/reg_1_data$RMC_per_GAS)
  # effect_Land_per_RMC <- L_term * log(reg_2_data$Land_per_RMC/reg_1_data$Land_per_RMC)
  # effect_HANPP_per_Land <- L_term * log(reg_2_data$HANPP_per_Land/reg_1_data$HANPP_per_Land)
  # 
  # effect_total <- sum(effect_pop) + sum(effect_GAS_per_POP) + sum(effect_GAS_sector_share) + sum(effect_RMC_per_GAS) + sum(effect_Land_per_RMC) + sum(effect_HANPP_per_Land)
  # 
  # IDA_result <- data.frame("effect" = c("Population", "GAS_per_head","GAS_sector_share","RMC_per_GAS","Land_per_RMC","HANPP_per_Land"),
  #                          "value" = c(sum(effect_pop),sum(effect_GAS_per_POP),sum(effect_GAS_sector_share),sum(effect_RMC_per_GAS),sum(effect_Land_per_RMC),sum(effect_HANPP_per_Land) ))
  
  # #print( str_c( "The difference in eHANPP between ", region_agg[reg_2]," and ",region_agg[reg_1], " is ", sum(delta$eHANPP)) )
  # 
  # L_term <- delta$RMC / ( log( reg_2_data$RMC) - log(reg_1_data$RMC) ) 
  # # L_term <- sum(L_term)
  # # L_term <- sum(delta$RMC) / log( sum(reg_2_data$RMC) / sum(reg_1_data$RMC) ) 
  #  
  # # L_term <- (reg_2_data$eHANPP - reg_1_data$eHANPP) / log(reg_2_data$eHANPP / reg_1_data$eHANPP) 
  # sum(L_term) / 10^6
  # 
  # effect_pop <- L_term * log(pop_IDA[2]/pop_IDA[1])
  # effect_pop
  # 
  # effect_GAS_per_POP <- L_term * log(reg_2_data$GAS_per_POP/reg_1_data$GAS_per_POP)
  # sum(effect_GAS_per_POP)
  # 
  # effect_GAS_sector_share <- L_term * log(reg_2_data$GAS_sector_share/reg_1_data$GAS_sector_share)
  # effect_GAS_sector_share
  # #effect_GAS_sector_share <- sum(effect_GAS_sector_share)
  # 
  # effect_RMC_per_GAS <- L_term * log(reg_2_data$GAS_sector_share/reg_1_data$GAS_sector_share)
  # # effect_RMC_per_GAS <- sum(effect_RMC_per_GAS)
  # 
  # effect_total <- sum(effect_pop) + sum(effect_GAS_per_POP) + sum(effect_GAS_sector_share) + sum(effect_RMC_per_GAS) 
  # 
  # sum(delta$RMC)/10^6
  # sum(effect_total)/ 10^6

}