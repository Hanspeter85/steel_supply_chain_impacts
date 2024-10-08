
Plot_BoundaryFlows <- function()
{
  
  # Changing names of boundary flows for better readability
  name_input <- base$input$Name
  name_input[name_input == "End-of-Life Scrap"] <- "EoL-Scrap (O)"
  name_input[name_input == "Air"] <- "Air/Oxygen (E)"
  name_input[name_input == "Flux"] <- "Flux (O)"
  name_input[name_input == "Coke"] <- "Coke (O)"
  name_input[name_input == "Crude Ore"] <- "Crude Ore (E)"
  
  name_output <- c("steel (Y)","Solid/liquid residual (P)","Gaseous residual (P)")
  
  # Create data vectors
  input <- data.frame("Boundary" = "Boundary input",
                      "Type" = paste(c(rep("Input from SEM:",3),rep("Input from Nature:",2) ),name_input ) ,
                      "Value" = rowSums(SUT$v)/10^6,
                      stringsAsFactors = FALSE)
  
  input <- input[order(input$Value),]
  
  
  output <- data.frame("Boundary" = "Boundary output + final use",
                       "Type" = paste(c("Final use of",rep("Output to SEM:",2) ),name_output ),
                       "Value" =  c( sum(SUT$y), rowSums(SUT$w) ) / 10^6,
                       stringsAsFactors = FALSE)
  
  output <- output[c(3,2,1),]  # Reorder so that steel is at the bottom
  
  dat <- rbind( input, output )
  
  dat$Type <- factor(dat$Type, levels = c(input$Type,output$Type) )
  
  dat$Boundary <- factor(dat$Boundary, levels = c("Boundary output + final use","Boundary input") )
  
  v_colors <- c(viridis_pal()(5),inferno(3))  # Define 
  
  maximum <- round( (sum(dat$Value)/100 )/2 ) * 100  # find maximum for axis
  
  plot <- ggplot(data = dat) +
    geom_bar(stat = "identity",
             aes(x = Boundary, y = Value, fill = Type),
             position = "stack",
             colour="grey",
             width = 0.75) +
    scale_fill_manual(values = v_colors,name = "Legend:") +
    scale_y_continuous(expand=c(0,0),
                       breaks = seq(0,maximum,500),
                       position = "left") + 
    labs(x="", y = "Mega tonnes") +
    theme(legend.position = "right",
          axis.text.y = element_text(colour="black", size = 11) ) +
    coord_flip()
  
  plot
  
  # Save plot: 
  ggsave(path = path$output, 
         filename = paste0(job$year, "_BoundaryFlows.png"),
         width = 9, height = 4.5 )
  
  # Export data for SI of paper
  write.xlsx( dat, file = paste0( path$output,"/SI/",job$year,"_Data for total inputs and outputs.xlsx") )
  
  return(plot)
}
