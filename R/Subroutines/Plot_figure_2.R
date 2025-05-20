### This script creates the combined plot of figure 2 (ewMFA indicators and MF accounts)


# Set font/text size for figures
legend_text_size <- 16
axis_title_size <- 16
axis_text_size <- 12

# Get sorting of regions in descending order of RMC and DEC
# reg_sort_RMC <- ewMFA %>% mutate("Region" = rownames(ewMFA)) %>% select(Region, RMC) %>% 
#   remove_rownames() %>% arrange(desc(RMC)) %>% pull(Region)
# reg_sort_DE <- ewMFA %>% mutate("Region" = rownames(ewMFA)) %>% select(Region, DE) %>% 
#   remove_rownames() %>% arrange(-desc(DE)) %>% filter(Region != "Japan") %>% pull(Region)


### Figure 2.1 (ewMFA indicators in absolute terms)

# Select indicators to plot; transform to long format, change units to Giga tons and set factor levels for correct ordering
dat <- ewMFA %>% select(DE, DMC, RMC, GAS, Stocks) %>%  
  mutate("Region" = rownames(ewMFA)) %>% 
  remove_rownames() %>% 
  pivot_longer(cols = c(DE, DMC, RMC, GAS, Stocks), names_to = "Indicator", values_to = "Value") %>% 
  mutate(Value = Value/10^9) %>% 
  mutate(Region = factor(Region, levels = reg_sort_RMC)) %>% 
  mutate(Indicator = factor(Indicator, levels = c("DE", "DMC", "RMC", "GAS", "Stocks"))) %>% 
  filter(Indicator != "DMC")

data_SI[["2_1"]] <- dat

# scaling factor for secondary y-axis
scale_y <- 3

# Split into flows (left axis) and stocks (right axis)
dat_flow <- dat %>% filter(Indicator != "Stocks")
dat_stock <- dat %>% filter(Indicator == "Stocks") %>% 
  mutate(Value = Value/scale_y)

# Create plot
plot_1 <- ggplot() +
  geom_bar(data = dat_flow, aes(x = Region, y = Value, fill = Indicator) ,stat = "identity",position = "dodge",colour="black") +
  geom_point(data = dat_stock, aes(x = Region, y = Value, fill = Indicator), shape = 21, color = "black", size = 4) +
  theme_minimal() +
  scale_fill_manual(labels = c("Domestic Extraction (DE; left axis)",
                               "Iron Ore Material Footprint (IO-MF; left axis)",
                               "Steel Use (Steel-GAS; left axis)",
                               "Steel Stocks (right axis)"),
                    values = c("#4472C4", "#ED7D31", "#FFC000", "yellow")) +
  theme(legend.title = element_blank(),
        legend.position = c(0.7, 0.8),
        legend.background = element_rect(color = "lightgray",fill = "gray97", linewidth = 0.2),
        legend.text = element_text(size = legend_text_size),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(color = "grey", 
                                    fill = NA, 
                                    size = 1)) +
  scale_y_continuous("Material flows [Gt/year]",
                     sec.axis = sec_axis(~.*1, 
                                         name = "Steel Stocks [Gt]",
                                         breaks = c(0.5, 1, 1.5, 2, 2.5),
                                         labels = c( 1.5, 3, 4.5, 6, 7.5)),
                     limits = c(0,2.25),
                     expand = c(0,0)) +
  scale_x_discrete() +
  geom_vline(xintercept = seq(0.5, 14, by = 1), color="gray", size=.5, alpha=.5)

plot_1

remove(dat_flow, dat_stock)

### Figure 2.2 (MF by source region in relative terms)

# Aggregate across source and destination regions and set factor levels for sorting
dat <- Results_agg %>% filter(stressor == "RMC", source_region_group != "Japan") %>% ungroup() %>%  
  select(source_region_group, destination_region_group, value) %>% 
  group_by(source_region_group, destination_region_group) %>% 
  summarise(value = sum(value), .groups = 'drop') %>% 
  mutate(source_region_group = factor(source_region_group, levels = reg_sort_DE)) %>% 
  mutate(destination_region_group = factor(destination_region_group, levels = reg_sort_RMC))

# calculate import shares and set factor levels
IM_share <- Results_agg %>% filter(stressor == "RMC") %>% ungroup() %>% 
  select(destination_region_group, source, value) %>% group_by(destination_region_group, source) %>% 
  summarise(value = sum(value), .groups = 'drop') %>% 
  group_by(destination_region_group) %>%
  pivot_wider(names_from = source, values_from = value) %>% 
  mutate(value = Import/(Import+Domestic)) %>% 
  select(-Import, -Domestic) %>%  
  mutate( destination_region_group = factor(destination_region_group, levels = reg_sort_RMC) ) %>% 
  mutate(Indicator = "Import Share")

data_SI[["2_2_1"]] <- dat
data_SI[["2_2_2"]] <- IM_share

# Create and reorder color palette for 12 exraction regions (+ Import Shares)
reg_color_IM <- c( reg_color, "yellow" )

plot_2 <- ggplot() +
  geom_bar(data = dat, aes(x = destination_region_group, y = value, fill = source_region_group), stat = "identity", position = "fill") +
  geom_point(data = IM_share, aes(x = destination_region_group, y = value, fill = Indicator),  shape = 21, color = "black", size = 4) +
  theme_minimal() +
  scale_fill_manual(values = reg_color_IM) +
  theme(legend.title = element_blank(),
#        legend.position = c(0.7, 0.8),
        legend.position = "bottom",
        legend.background = element_rect(color = "lightgray",fill = "gray97", linewidth = 0.2),
        axis.title.x = element_blank(),
        legend.text = element_text(size = legend_text_size),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(color = "grey", 
                                    fill = NA, 
                                    size = 1)) +
  geom_vline(xintercept = seq(0.5, 14, by = 1), color="gray", size=.5, alpha=.5) +
  labs(y = "IO-MF by region of extraction [%]") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

plot_2

# remove(IM_share)


### Figure 2.3 (MF by end use)

dat <- Results_agg %>% ungroup() %>% 
  filter(stressor == "RMC") %>% 
  select(destination_region_group, end_use, value) %>% 
  group_by(destination_region_group, end_use) %>% 
  summarise(value = sum(value)) %>% 
  mutate(end_use, end_use = factor(end_use, levels = fct_rev(end_use))) %>% 
  mutate(destination_region_group = factor(destination_region_group, reg_sort_RMC) )

data_SI[["2_3"]] <- dat

plot_3 <- ggplot() +
  geom_bar(data = dat, aes(x = destination_region_group, y = value, fill = end_use), stat = "identity", position = "fill") +
  theme_minimal() +
  scale_fill_manual(values = enduse_color) +
  theme(legend.title = element_blank(),
        #        legend.position = c(0.7, 0.8),
        legend.position = "bottom",
        legend.background = element_rect(color = "lightgray",fill = "gray97", linewidth = 0.2),
        axis.title.x = element_blank(),
        legend.text = element_text(size = legend_text_size),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(color = "grey", 
                                    fill = NA, 
                                    size = 1)) +
  geom_vline(xintercept = seq(0.5, 14, by = 1), color="gray", size=.5, alpha=.5) +
  labs(y = "IO-MF by final product [%]") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

plot_3

# For better readability change axis labels of regions and color to black

reg_name_plot <- c("China", "Europe", "United\nStates", "Asia &\nPacific(nec)", "Japan",
                   "Middle\nEast", "Latin\nAmerica(nec)", "India", "Africa", "Russia", "Canada",
                   "Brazil", "Australia")

plot_1 <- plot_1 + scale_x_discrete(labels = reg_name_plot) + theme(axis.text = element_text(colour = "black"))
plot_2 <- plot_2 + scale_x_discrete(labels = reg_name_plot) + theme(axis.text = element_text(colour = "black"))
plot_3 <- plot_3 + scale_x_discrete(labels = reg_name_plot) + theme(axis.text = element_text(colour = "black"))

# Plot final product footprints to be included in SI
ggsave("./output/Fig_2_SI.png",  # File name for the saved plot
       plot = last_plot(),  # The last plot created in the session
       width = 13,  # Width of the plot in inches
       height = 6,  # Height of the plot in inches
       dpi = 1850,
       bg = "white")  # Resolution (dots per inch), adjust as needed


# Scale the plots and add "empty" rows to customize margins of plots
plot_grid(plot_1, 
          NULL, 
          plot_2, 
          align = "v", 
          nrow = 3, 
          rel_heights = c(1,0.05, 1.2),
          labels = c("A)", NA,"B)"))

ggsave("./output/Fig_2.pdf",  # File name for the saved plot
       plot = last_plot(),  # The last plot created in the session
       width = 13,  # Width of the plot in inches
       height = 13,  # Height of the plot in inches
       dpi = 1850,
       bg = "white")  # Resolution (dots per inch), adjust as needed
