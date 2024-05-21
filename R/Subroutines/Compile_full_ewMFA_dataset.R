
## Compile ew-MFA indicator of stocks and flows
# Traditional ew-MFA indicators (DE, DMC)
ewMFA <- cbind( base$region[,c(2,3,5)],Calc_ewMFA(IOT = IOT, Code = Code) )

ewMFA["Net-trade"] <- ewMFA$Import - ewMFA$Export

# Add Read RMC account
tmp <- Results %>% filter(stressor == "RMC") %>% select(destination_region, value) %>% 
  group_by(destination_region) %>% summarise(value = sum(value)) 

ewMFA <- left_join(ewMFA, tmp, by = c("Name" = "destination_region"))
colnames(ewMFA)[9] <- "RMC"

## Add steel consumption (GAS)
tmp <- Results %>% filter(stressor == "Steel_GAS") %>% select(destination_region, value) %>% 
  group_by(destination_region) %>% summarise(value = sum(value)) 
colnames(tmp) <- c("Name","GAS")
ewMFA <- left_join(ewMFA,tmp)

## Add population
ewMFA["Population"] <- pop


## Add steel stocks from MISO2
Stock_data <- read.csv(file = paste0(path$MISO2,"global_v1_enduse.csv"))
colnames(Stock_data)[6:202] <- 1820:2016
Stock_data <- Stock_data %>% filter(name == "S10_stock_enduse", material == "iron_steel") %>% select(region, "2014")

## Load region concordances to aggregate MISO2 data
Conco_EXIO2MISO <- read.csv( file = paste0(path$concordance,"Region_concordance_EXIOBASE_MISO.csv") )
Conco_EXIO2MISO <- Conco_EXIO2MISO %>% select(MISO_names, EXIOBASE_iso2)
EXIO_reg_list <- read.xlsx( xlsxFile = paste0(path$concordance,"RegionAggregator_49_to_32.xlsx") )[,1:4]

Stock_data <- left_join(Stock_data, Conco_EXIO2MISO, by = c("region" = "MISO_names"))
Stock_data <- Stock_data %>% select(EXIOBASE_iso2, "2014")
colnames(Stock_data) <- c("region", "value") 
Stock_data <- Stock_data %>% group_by(region) %>% summarise(value = sum(value))
Stock_data <- Stock_data[match(EXIO_reg_list$Abbrev,Stock_data$region),]

# MISO2 stock results are in kilo tons and need to be transformed into tons
Stock_data$value <- Stock_data$value * 1000

# Aggregate to 32 region PIOT classification
Stock_data <- colSums( Stock_data$value * Conco$EXIO_2_base )

ewMFA["Stocks"] <- Stock_data  

ewMFA <- Agg( as.matrix(ewMFA[,4:12]), aggkey = ewMFA$Region_new, dim = 1 )
ewMFA <- as.data.frame(ewMFA)

# write.xlsx(x = ewMFA, file = "./output/ewMFA_indicators_13_world_regions.xlsx", rowNames = TRUE)
