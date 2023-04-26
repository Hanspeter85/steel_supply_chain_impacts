
# Function to calculate footprint with original model:
Calc_FP <- function(stressor)
{
  L <- IOT$L
  y <- IOT$y
  e <- IOT$e
  x <- rowSums(L %*% y)  # Estimate new gross production
  intens <- e[, base$input$Code[base$input$Name == stressor] ]/x
  
  MP <- L * intens  # Material multipliers
  FP <- MP %*% y  # Material fooptrints
  
  FP <- Agg(FP, rep(Code$Y$index, each = 6),2)  # Aggregate columns in consuming regions
  FP <- Agg(FP, Code$Z$RegionCode[Code$Z$EntityCode == 1] , 1)  # Aggregate columns in consuming regions
  
  Result <- data.frame( "IM_RME" = colSums(FP) - diag(FP),
                        "EX_RME" = rowSums(FP) - diag(FP),
                        "RMC" = colSums(FP) )
  
  return(Result)  
}
