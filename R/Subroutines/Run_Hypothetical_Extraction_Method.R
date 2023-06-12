
# unique(base$region$Region_new)
# reg = "United States"

Run_Hypothetical_Extraction_Method <- function()
{
  S <- SUT$S
  U <- SUT$U
  v <- SUT$v
  y <- SUT$y
  w <- SUT$w
  
  # Code <- Load_IOCodes() # Load IO codes
  
  index <- list( "ind" = Code$Z_raw[Code$Z_raw$EntityCode == 1,],
                 "pro" = Code$Z_raw[Code$Z_raw$EntityCode == 2,]
  )
  
  index[["scrap"]] <- index$pro$SectorIndex[index$pro$SectorName == "Steel scrap"]
  index[["iron"]] <- index$pro$SectorIndex[index$pro$SectorName %in% c("Pig iron","Sponge iron")]
  
  D <- t( t(S) / colSums(S) )  # Commodity proportions i.e. market share matrix (ixp)
  # D[is.na(D)] <- 0
  
  C <- S/rowSums(S)  # Commodity composition of industry output (ixp)
  # C[is.na(C)] <- 0
  
  x_original <- rowSums(S)  # Gross industry output
  q_original <- colSums(S)  # Gross product output
  
  B <- t(t(U)/x_original)  # Product by industry input coefficients
  
  bal <- data.frame("inter_input_nec" = colSums(U),"inter_output" = rowSums(S))
  U_scrap <- colSums(U[index$scrap,])
  U_iron <- colSums(U[index$iron,])
  bal$inter_input_nec <- bal$inter_input_nec - ( U_scrap + U_iron )
  bal <- cbind(t(v),U_scrap,U_iron,bal,t(w))
  colnames(bal)[1:5] <- Code$V$Entity[1:5]
  colnames(bal)[7] <- "Reduced_iron"
  colnames(bal)[10:11] <- Code$V$Entity[6:7]
  bal["Total_IN"] <- rowSums(bal[,1:7])
  bal["Total_OUT"] <- rowSums(bal[,8:10])
  bal <- cbind(Code$Z[,c(1:3,6,7)],bal)
  write.xlsx(x = bal,"./output/Checking_balances_before_HEM.xlsx")
  
  # Create modiefied technology matrix
  Scrap_input <- colSums(B[index$scrap,])
  Iron_input <- B[index$iron,]
  Iron_input_normalized <- t(t(Iron_input)/colSums(Iron_input))
  Iron_input_add <- t(t(Iron_input_normalized) * Scrap_input) 
  
  B_reg <- B
  B_reg[index$scrap,] <- 0
  B_reg[index$iron,] <- B_reg[index$iron,] + Iron_input_add

  A <- D %*% B_reg  # ixi technology matrix (industry technology assumption)
  L <- solve(diag(nrow(A))-A)  # Derive inverse
  y <- D %*% y  # Change to industry classification
  x <- rowSums( L%*%y )   # Estimate new gross production (without wastes)
  Z <- A %*% diag(x)  # ixi transaction matrix
  
  # Create extension by merging primary inputs (v) with  wastes (in y)
  e_original <- matrix(c(t(v),t(w)),nrow = ncol(v),ncol = (nrow(w)+nrow(v)))
  
  e <- (e_original/x_original) * x
  
  Compare_extensions <- data.frame("stressor" = Code$V$Entity,
                                   "original" = colSums(e_original)/10^6,
                                   "HEM" = colSums(e)/10^6)
  
  
  print( paste( "Original gross production of industries:", round( sum(x)/10^6, digits = 0),"Mt" ) )
  print( paste( "Gross production of industries calculated with L:", round( sum(L%*%y)/10^6, digits = 0),"Mt" ) )

  print( paste( "Minimum value in L:", min(L) ) )
  
  model <- list("Z" = Z, "L" = L, "y" = y, "x" = x, "q" = q, "e" = e)
  
  remove(B,D,S,U,v,w)
  
  return(model)    
}