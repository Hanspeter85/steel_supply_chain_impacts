
Load_population_data <- function()
{
  pop <- read.xlsx(xlsxFile = paste0( path$input, "EXIOBASE/EXIOBASE population data.xlsx"), sheet = 3 )[1:49,c(-1,-2,-3,-4)]
  pop <- pop[,colnames(pop) == as.character(job$year)]    
  
  pop <- colSums( Conco$EXIO_2_base * pop )
  
  return(pop)
}