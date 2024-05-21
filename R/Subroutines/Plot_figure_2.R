
### Figure 2.1 (ewMFA indicators in absolute terms)

# Select indicators to plot; transform to long format and change units to Giga tons
dat <- ewMFA %>% select(DE, DMC, RMC, GAS, Stocks) %>%  
  mutate("Region" = rownames(ewMFA)) %>% 
  remove_rownames() %>% 
  pivot_longer(cols = c(DE, DMC, RMC, GAS, Stocks), names_to = "Indicator", values_to = "Value") %>% 
  mutate(Value = Value/10^9)

# Sort regions is descending order of RMC
reg_sort <- dat %>% filter(Indicator == "RMC") %>% 
  arrange(desc(Value)) %>% 
  pull(Region)

dat$Region <- factor(dat$Region, levels = reg_sort )

# Sort indicators
dat$Indicator <- factor(dat$Indicator, levels = c("DE", "DMC", "RMC", "GAS", "Stocks"))


# scaling factor for secondary y-axis
scale_y <- 3

# Split into flows (left axis) and stocks (right axis)
dat_flow <- dat %>% filter(Indicator != "Stocks")
dat_stock <- dat %>% filter(Indicator == "Stocks") %>% 
  mutate(Value = Value/scale_y)


# Create plot
ggplot( data = dat_flow, aes(x = Region, y = Value, fill = Indicator) ) +
  geom_bar(stat = "identity", position = "dodge", colour="black") +
  geom_point(data = dat_stock, aes(x = Region, y = Value), shape = 21, color = "black", fill = "yellow", size = 4) +
  theme_minimal() +
  scale_fill_manual(labels = c("Domestic Extraction (DE; left axis)",
                               "Domestic Material Consumption (DMC; left axis)",
                               "Material Footprint (MF; left axis)",
                               "Gross Additions to Material Stocks (GAS; left axis)",
                               "Material Stocks (right axis)"),
                    values = c("#4472C4", "#ED7D31", "#A6A6A6", "#FFC000", "yellow")) +
  theme(legend.title = element_blank(),
        legend.position = c(0.7, 0.8),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous("Material flows in giga tons per year [Gt/y]",
                     sec.axis = sec_axis(~.*1, 
                                         name = "Material Stocks in giga tons [Gt]",
                                         breaks = c(0.5, 1, 1.5, 2),
                                         labels = c( 1.5, 3, 4.5, 6)),
                     limits = c(0,2.5),
                     expand = c(0,0)) +
  scale_x_discrete() +
  geom_vline(xintercept = seq(0.5, 14, by = 1), color="gray", size=.5, alpha=.5)


