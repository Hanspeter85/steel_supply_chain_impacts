# Load functions:

fun <- list(paste0(path$repo,"/R/Subroutines/Load_IOCodes.R"),
            paste0(path$repo,"/R/Subroutines/Load_SUT.R"),
            paste0(path$repo,"/R/Subroutines/Build_IOT.R"),
            paste0(path$repo,"/R/Subroutines/Agg.R"),
            paste0(path$repo,"/R/Subroutines/Plot_HeadlineIndicators.R"),
#            paste0(path$repo,"/R/Subroutines/Check_Balance.R"),
#            paste0(path$repo,"/R/Subroutines/Check_domesticSUT.R"),
#            paste0(path$repo,"/R/Subroutines/Compile_RegionProfile.R"),
            paste0(path$repo,"/R/Subroutines/Calc_ewMFA.R"),
#            paste0(path$repo,"/R/Subroutines/Calc_EXIOfootprint.R"),
#            paste0(path$repo,"/R/Subroutines/MonteCarloSimulation.R"),
#            paste0(path$repo,"/R/Subroutines/Check_import.R"),
#            paste0(path$repo,"/R/Subroutines/Plot_Rocket.R"),
#            paste0(path$repo,"/R/Subroutines/Diagnostics.R"),
            paste0(path$repo,"/R/Subroutines/Calc_FP.R"),
#            paste0(path$repo,"/R/Subroutines/Analysis_Y.R"),
#            paste0(path$repo,"/R/Subroutines/Plot_Ratios.R"),
#            paste0(path$repo,"/R/Subroutines/Plot_BoundaryFlows.R"),
#            paste0(path$repo,"/R/Subroutines/Prepare_SankeyData.R"),
#            paste0(path$repo,"/R/Subroutines/mrSUT_heatmap.R"),
#            paste0(path$repo,"/R/Subroutines/Numbers2File.R"),
#            paste0(path$repo,"/R/Subroutines/Analysis_Global.R"),
            paste0(path$repo,"/R/Subroutines/Load_population_data.R"),
            paste0(path$repo,"/R/Subroutines/Build_Extension_Biodiversity_Carbon_Water_Score.R"), 
            paste0(path$repo,"/R/Subroutines/Build_Extension_Biomes.R"),
            paste0(path$repo,"/R/Subroutines/EPIP/EPIP_00_main.R") )



lapply(fun,source)

remove(fun)

base <<- list("region" = read.xlsx(paste0(path$input,"Settings/BaseRegionClassification.xlsx"),sheet = 1),
              "industry" = read.xlsx(paste0(path$input,"Settings/BaseSectorClassification.xlsx"),sheet = 1),
              "product" = read.xlsx(paste0(path$input,"Settings/BaseSectorClassification.xlsx"),sheet = 2),
              "input" = read.xlsx(paste0(path$input,"Settings/BaseSectorClassification.xlsx"),sheet = 4),
              "demand" = read.xlsx(paste0(path$input,"Settings/BaseSectorClassification.xlsx"),sheet = 3))

num <<- list("pro" = nrow(base$product),
             "ind" = nrow(base$industry),
             "reg" = nrow(base$region),
             "va" = nrow(base$input),
             "fd" = nrow(base$demand) )

path_rootclass <- paste0(path$input,"Settings/PIOLab_RootClassification.xlsx")

root <- list("region" = read.xlsx(path_rootclass,sheet = 1),
             "process" = read.xlsx(path_rootclass,sheet = 2)[,1:2],
             "flow" = read.xlsx(path_rootclass,sheet = 3),
             "demand" = read.xlsx(path_rootclass,sheet = 4),
             "input" = read.xlsx(path_rootclass,sheet = 5) )

Conco <<- list("EXIO_2_base" = as.matrix( read.xlsx(xlsxFile = paste0(path$concordance,"RegionAggregator_49_to_32.xlsx") )[,c(-1,-2,-3,-4)] ),
               "Root_2_base" = as.matrix( read.csv( "./input/Concordances/032_RegionAggregator.csv", header = FALSE ) ) )

Code <<- Load_IOCodes()                               # Read IO model codes

remove(path_rootclass)

