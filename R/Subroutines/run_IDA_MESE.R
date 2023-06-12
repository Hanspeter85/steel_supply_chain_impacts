

MEME_decomposition <- function(reg_1, reg_2)#df is a dataframe with 4 columns: sector, Year, environmental_pressure, output
{
  

  # Read population data for selected regions 1 & 2
  pop_IDA <- c( pop_agg$value[pop_agg$region == region_agg[reg_1]], pop_agg$value[pop_agg$region == region_agg[reg_2]])
  
  # Create empty data frames for region data sets
  reg_1_data <- reg_2_data <- data.frame("index" = 1:10,
                                         "region" = NA,
                                         "sector" = base$industry$Name[20:29],
                                         "POP" = NA,
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
  reg_1_data$POP <- pop_agg$value[pop_agg$region == region_agg[reg_1]]
  reg_1_data$GAS <- sum( Results$value[Results$destination_region_group == region_agg[reg_1] & Results$stressor == "Steel_GAS"] )
  reg_1_data$RMC <- sum( Results$value[Results$destination_region_group == region_agg[reg_1] & Results$stressor == "RMC"] )
  reg_1_data$eLand <- sum( Results$value[Results$destination_region_group == region_agg[reg_1] & Results$stressor == "eLand"] )
  reg_1_data$eHANPP <- sum( Results$value[Results$destination_region_group == region_agg[reg_1] & Results$stressor == "eHANPP"] )
  
  reg_2_data$region <- region_agg[reg_2]
  reg_2_data$POP <- pop_agg$value[pop_agg$region == region_agg[reg_2]]
  reg_2_data$GAS <- sum( Results$value[Results$destination_region_group == region_agg[reg_2] & Results$stressor == "Steel_GAS"] )
  reg_2_data$RMC <- sum( Results$value[Results$destination_region_group == region_agg[reg_2] & Results$stressor == "RMC"] )
  reg_2_data$eLand <- sum( Results$value[Results$destination_region_group == region_agg[reg_2] & Results$stressor == "eLand"] )
  reg_2_data$eHANPP <- sum( Results$value[Results$destination_region_group == region_agg[reg_2] & Results$stressor == "eHANPP"] )
  
  # Calculate shares and ratios
  reg_1_data$GAS_per_POP <- reg_1_data$GAS / reg_1_data$POP
  reg_1_data$GAS_sector_share <- reg_1_data$GAS / sum(reg_1_data$GAS)
  reg_1_data$RMC_per_GAS <- reg_1_data$RMC / reg_1_data$GAS
  reg_1_data$Land_per_RMC <- reg_1_data$eLand / reg_1_data$RMC
  reg_1_data$HANPP_per_Land <- reg_1_data$eHANPP / reg_1_data$eLand
  reg_1_data$HANPP_per_POP <- reg_1_data$eHANPP / reg_1_data$POP
  
  reg_2_data$GAS_per_POP <- reg_2_data$GAS / reg_2_data$POP
  reg_2_data$GAS_sector_share <- reg_2_data$GAS / sum(reg_2_data$GAS)
  reg_2_data$RMC_per_GAS <- reg_2_data$RMC / reg_2_data$GAS
  reg_2_data$Land_per_RMC <- reg_2_data$eLand / reg_2_data$RMC
  reg_2_data$HANPP_per_Land <- reg_2_data$eHANPP / reg_2_data$eLand
  reg_2_data$HANPP_per_POP <- reg_2_data$eHANPP / reg_2_data$POP
  
  # Calculate differences between regions
  delta <- reg_2_data[,3:14]
  delta[,2:12] <- reg_2_data[,4:14] - reg_1_data[,4:14]
  pop_delta <- pop_IDA[2] - pop_IDA[1]
  
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
  # 
  # 
  # 
  # toy_model_MEME <- left_join(toy_model1[1:8],
  #                             filter(toy_model1[1:8], Year==min(toy_model1$Year, na.rm=T))%>%
  #                               select(sector,
  #                                      environmental_pressure),
  #                             by = c("sector"),
  #                             suffix = c("", "_T0"))%>%
  #   mutate(environmental_pressure_increase_since_T0 = environmental_pressure - environmental_pressure_T0)%>%
  #   select(-environmental_pressure_T0)
  # 
  # 
  # toy_model_MEME <- left_join(toy_model_MEME,
  #                             filter(toy_model_MEME, Year==min(toy_model1$Year, na.rm=T))%>%
  #                               select(sector,
  #                                      output),
  #                             by = c("sector"),
  #                             suffix = c("", "_T0"))%>%
  #   mutate(output_increase_since_T0 = output - output_T0)
  # 
  # 
  # toy_model_MEME %<>%
  #   mutate(average_environmental_intensity = total_environmental_pressure / total_output)%>%
  #   mutate(difference_to_mean_intensity = environmental_intensity - average_environmental_intensity)
  # 
  # 
  # toy_model_MEME <- left_join(toy_model_MEME,
  #                             filter(toy_model_MEME, Year==min(toy_model1$Year, na.rm=T))%>%
  #                               select(sector,
  #                                      environmental_intensity),
  #                             by = c("sector"),
  #                             suffix = c("", "_T0"))%>%
  #   mutate(environmental_intensity_increase_since_T0 = environmental_intensity - environmental_intensity_T0)
  # 
  # 
  # 
  # toy_model_MEME <- left_join(toy_model_MEME,
  #                             filter(toy_model_MEME, Year==min(toy_model1$Year, na.rm=T))%>%
  #                               select(sector,
  #                                      average_environmental_intensity),
  #                             by = c("sector"),
  #                             suffix = c("", "_T0"))
  # 
  # toy_model_MEME <- toy_model_MEME%>%
  #   mutate(difference_to_mean_intensity_T0 = environmental_intensity_T0 - average_environmental_intensity_T0)
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # toy_model_MEME%<>%
  #   
  #   mutate(sector_mix_effect =
  #            
  #            1/2* (
  #              
  #              output_increase_since_T0 *
  #                difference_to_mean_intensity +
  #                
  #                output_increase_since_T0 *
  #                difference_to_mean_intensity_T0 )
  #          
  #   )%>%
  #   
  #   
  #   
  #   mutate(overall_activity_effect =
  #            
  #            
  #            1/2* (
  #              
  #              output_increase_since_T0 *
  #                average_environmental_intensity +
  #                
  #                output_increase_since_T0 *
  #                average_environmental_intensity_T0)
  #          
  #   )%>%
  #   
  #   
  #   
  #   mutate(intensity_effect =
  #            
  #            1/2* (
  #              
  #              environmental_intensity_increase_since_T0 *
  #                output +
  #                
  #                environmental_intensity_increase_since_T0 *
  #                output_T0 )
  #          
  #   )
  # 
  # 
  # 
  # toy_model_MEME %>% filter(!is.na(Year))
  # 
  # toy_model_MEME_decomposition <- toy_model_MEME%>%
  #   group_by(Year)%>%
  #   summarise(sector_mix_effect = sum(sector_mix_effect, na.rm=T),
  #             overall_activity_effect = sum(overall_activity_effect, na.rm = T),
  #             intensity_effect = sum(intensity_effect, na.rm = T),
  #             environmental_pressure = sum(environmental_pressure_increase_since_T0, na.rm = T))%>%
  #   mutate(sum_effects = sector_mix_effect+overall_activity_effect+intensity_effect)%>%
  #   mutate(ratio_environmental_pressure_to_sum_effects = environmental_pressure / sum_effects)
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # ### plot separate share of origin
  # 
  # 
  # library(tidyr)
  # toy_model_MEME_decomposition <- gather(toy_model_MEME_decomposition[1:5],
  #                                        2:5,
  #                                        key="primary_driver",
  #                                        value="contributions")
  # 
  # toy_model_MEME_decomposition$LHS_RHS <- "Primary_drivers"
  # toy_model_MEME_decomposition$LHS_RHS[toy_model_MEME_decomposition$primary_driver=="environmental_pressure"] <- "environmental_pressure"
  # toy_model_MEME_decomposition$T0 <- min(toy_model1$Year, na.rm=T)
  # 
  # toy_model_MEME_decomposition <- toy_model_MEME_decomposition%>%
  #   group_by(Year, primary_driver, LHS_RHS, T0)%>%
  #   summarise(contributions = sum(contributions, na.rm=T))
  # 
  # 
  # 
  # toy_model_MEME_decomposition$primary_driver <- factor(toy_model_MEME_decomposition$primary_driver,
  #                                                       levels = (c("environmental_pressure",
  #                                                                   "overall_activity_effect", "sector_mix_effect",
  #                                                                   "intensity_effect")))
  # levels(toy_model_MEME_decomposition$primary_driver)
  # #### plot decomposition
  # library(ggplot2)
  # theme_set(theme_classic())
  # library(RColorBrewer)
  # #display.brewer.all()
  # color_blind <- c('#e66101','#fdb863','#b2abd2','#5e3c99')[c(2,4,3)]
  # 
  # 
  # #setwd("C:/Users/nroux/Filr/Net Folders/DATAH73000/H73700/data/!projekt/2008_COUPLED/Nicolas/drivers environmental_pressure/decomposition/decomposition until 2011/plots/with trade mix/marshall edgeworth/with population/6 permutations")
  # plot_decomposition_MEME <- ggplot(toy_model_MEME_decomposition[toy_model_MEME_decomposition$LHS_RHS == "Primary_drivers",])+
  #   geom_col(aes(x = Year, y = contributions, fill = primary_driver))+
  #   scale_fill_manual(values = color_blind,
  #                     name = "",
  #                     labels = c("Overall activity",
  #                                "Sector mix",
  #                                "Environmental intensity"))+
  #   geom_line(data = toy_model_MEME_decomposition[toy_model_MEME_decomposition$LHS_RHS == "environmental_pressure",],
  #             aes(x = toy_model_MEME_decomposition$Year[toy_model_MEME_decomposition$LHS_RHS == "environmental_pressure"],
  #                 y = toy_model_MEME_decomposition$contributions[toy_model_MEME_decomposition$LHS_RHS == "environmental_pressure"],
  #                 colour="environmental pressure"),
  #             size=1.2)+
  #   scale_color_manual(name = NULL, values = c("environmental pressure" = "blue"))+
  #   scale_x_continuous(breaks = c(1,2))+
  #   ylab(paste("increase since", min(toy_model1$Year, na.rm=T), sep = " "))+
  #   ggtitle("MEME decomposition of environmental pressure")+
  #   theme_classic()
  # 
  # decomposition <- list(toy_model1, toy_model_MEME, toy_model_MEME_decomposition, plot_decomposition_MEME)
  # names(decomposition) <- c("input", "contributions", "results","plot")
  # return(decomposition)
}