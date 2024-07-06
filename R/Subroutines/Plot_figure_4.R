
## Scatter plot for pressure/impact intensities: 
## land per ton iron ore on y-axis and and HANPP per land on x-axis and maybe HANPP per ton as circle)
# Calculate intensities
{
  # Set font/text size for figures
  legend_text_size <- 16
  axis_title_size <- 15
  axis_text_size <- 14
  
  dat <- Results_agg %>% 
    select(stressor, destination_region_group, value) %>% 
    ungroup() %>% 
    group_by(stressor, destination_region_group) %>% 
    summarise(value = sum(value), .groups = 'drop') %>%
    rbind(POP[,c("stressor","destination_region_group","value")]) %>% 
    pivot_wider(names_from = stressor, values_from = value) %>% 
    mutate(m2_per_t = 10^6*eLand/RMC) %>% 
    mutate(gC_per_m2 = (10^6*eHANPP)/(10^6*eLand)) %>% 
    mutate(RMC_per_GAS = RMC/Steel_GAS) %>% 
    mutate(Steel_per_cap = Steel_GAS / POP) %>% 
    mutate(eHANPP_per_cap = 1000*eHANPP / POP) %>% 
    mutate(eLand_per_cap = 10^6*eLand / POP) %>% 
    mutate(RMC_per_cap = RMC / POP) %>% 
    arrange(desc(eHANPP_per_cap)) %>% 
    mutate(destination_region_group = factor(destination_region_group, levels = reg_sort_RMC))

    # calculate global averages
  tot <- dat %>% 
    select(RMC, Steel_GAS, eHANPP, eLand, POP) %>% 
    pivot_longer(names_to = "stressor", col = c("RMC", "Steel_GAS", "eHANPP", "eLand", "POP")) %>% 
    group_by(stressor) %>% 
    summarise(value = sum(value), .groups = 'drop')
  
  average_m2_per_t <- 10^6*tot$value[tot$stressor == "eLand"]/tot$value[tot$stressor == "RMC"]
  average_gC_per_m2 <- tot$value[tot$stressor == "eHANPP"]/tot$value[tot$stressor == "eLand"]
  average_GAS_per_cap <- 1000*tot$value[tot$stressor == "Steel_GAS"]/tot$value[tot$stressor == "POP"]
  average_RMC_per_GAS <- tot$value[tot$stressor == "RMC"]/tot$value[tot$stressor == "Steel_GAS"]
  average_RMC_per_GAS_withoutChina <- sum( dat$RMC[dat$destination_region_group != "China"] ) / sum( dat$Steel_GAS[dat$destination_region_group != "China"] )
  
  # Select symbols for point shapes of regions
  #reg_symbol <- c(21,21,21,21,22,22,22,22,25,25,25,25)
  reg_sort_RMC_Global <- c("Global Average", "Japan", reg_sort_DE)   
  # reg_symbol <- c(15,15,15,15,15,19,19,19,19,17,17,17,17)
  
  reg_symbol_Global <- c(18, 15, reg_symbol)
  reg_color_Global <- c("red","forestgreen", reg_color)
  
  # Create and rcolor palette for 13 RMC regions
  reg_color_RMC <- data.frame("color" = reg_color, "region" = reg_sort_DE) %>% 
    add_row(color = "forestgreen", region = "Japan")
  
  reg_color_RMC <- reg_color_RMC$color[match(reg_sort_RMC, reg_color_RMC$region)]
  
  dat_dot <- dat %>% select(destination_region_group, Steel_per_cap, RMC_per_GAS, m2_per_t, gC_per_m2) %>% 
    mutate(Steel_per_cap = Steel_per_cap*1000) %>% 
    add_row(destination_region_group = "Global Average", 
            Steel_per_cap = average_GAS_per_cap, 
            RMC_per_GAS = average_RMC_per_GAS, 
            m2_per_t = average_m2_per_t, 
            gC_per_m2 = average_gC_per_m2) %>% 
    mutate(destination_region_group = factor(destination_region_group, levels = reg_sort_RMC_Global))
  
}

plot_1 <- ggplot( data = dat_dot, aes(x = gC_per_m2, y = m2_per_t, group =  destination_region_group) ) +
  geom_point(size = 5, aes(shape = destination_region_group, color = destination_region_group ) ) +
  theme_minimal() +
  coord_fixed(ratio = (400/2)) +
  theme(panel.border = element_rect(color = "grey", 
                                    fill = NA, 
                                    size = 0.8),
        legend.title = element_text(size = legend_text_size, 
                                    face = "bold", 
                                    vjust = 1,
                                    hjust = 0.5),
        legend.background = element_rect(color = "lightgray",
                                         fill = "gray97", 
                                         linewidth = 0.2),
        legend.text = element_text(size = legend_text_size),
        axis.title = element_text(size = axis_title_size),
        axis.title.x = element_text(margin = margin(10,0,0,0), face = "bold"),
        axis.title.y = element_text(margin = margin(0,10,0,0), face = "bold"),
        axis.text = element_text(size = axis_text_size, color = "black"),
        panel.grid.minor = element_line(linetype="dashed",size=0.5),
        legend.position = "right",
        legend.position.inside = c(0.815, 0.6)) +
  scale_shape_manual(values = reg_symbol_Global) +
  scale_color_manual(values = reg_color_Global) +
  scale_x_continuous( limits = c(400, 800),
                      breaks = seq(400, 800, 50),
                      expand = c(0,0)) +
  scale_y_continuous( limits = c(1, 3),
                      breaks = seq(0, 3, 0.2),
                      expand = c(0,0),
                      position = "right") +
  # geom_hline(yintercept = average_m2_per_t, linetype = "dashed", color = "red") +
  # geom_vline(xintercept = average_gC_per_m2, linetype = "dashed", color = "red") +
  # geom_text(aes(y = (average_m2_per_t+0.05), label = str_c("Global average: ",round(average_m2_per_t, 2)), x = 450), 
  #           colour = "red",
  #           size = 4,
  #           fontface = 'italic') +
  # geom_text(aes(x = (average_gC_per_m2-10), label = str_c("Global average: ",round(average_gC_per_m2, 0)), y = 1.27), 
  #           colour = "red", 
  #           angle = 90, 
  #           size = 4,
  #           fontface = 'italic') +
  labs(x ="eHANPP-intensity of eLand [g/m²]",
       y = "eLand-intensity of Material Footprint [m²/t]")

plot_1


plot_2 <- ggplot( data = dat_dot, aes(x = Steel_per_cap, y = RMC_per_GAS, group =  destination_region_group) ) +
  geom_point(size = 5, aes(shape = destination_region_group, color = destination_region_group ) ) +
  theme_minimal() +
  coord_fixed(ratio = (720/3.4)) +
  theme(panel.border = element_rect(color = "grey", 
                                    fill = NA, 
                                    size = 0.8),
        legend.title = element_blank(),
        legend.background = element_rect(color = "lightgray",
                                         fill = "gray97", 
                                         linewidth = 0.2),
        legend.text = element_text(size = legend_text_size),
        axis.title = element_text(size = axis_title_size),
        axis.title.x = element_text(margin = margin(10,0,0,0), face = "bold"),
        axis.title.y = element_text(margin = margin(0,10,0,0), face = "bold"),
        axis.text = element_text(size = axis_text_size, color = "black"),
        panel.grid.minor = element_line(linetype="dashed",size=0.5),
        legend.position = "right",
        legend.position.inside = c(0.815, 0.6)) +
  scale_shape_manual(values = reg_symbol_Global) +
  scale_color_manual(values = reg_color_Global) +
  scale_x_continuous( limits = c(0, 720),
                      breaks = seq(0, 800, 100),
                      expand = c(0,0)) +
  scale_y_continuous( limits = c(0, 3.4),
                      breaks = seq(0, 4, 0.4),
                      expand = c(0,0)) +
  geom_hline(yintercept = average_RMC_per_GAS_withoutChina, linetype = "dashed", color = "red") +
  geom_text(aes(y = (average_RMC_per_GAS_withoutChina-0.08), 
                label = str_c("Global average (excluding China): ",round(average_RMC_per_GAS_withoutChina, 2)), 
                x = 530), 
            colour = "indianred1",
            size = 3.5,
            fontface = 'italic') +
  labs(x ="Steel Consumption per Capita [kg/head]",
       y = "Material Footprint per Steel Consumption [t/t]")

plot_2


## Plot per capita footprints

# Filter pe capita footprints and changes names for better visualization
dat_percap <- dat %>% 
  select(destination_region_group, Steel_per_cap, RMC_per_cap, eLand_per_cap, eHANPP_per_cap) %>% 
  pivot_longer(names_to = "stressor", cols = c("Steel_per_cap", 
                                               "RMC_per_cap", 
                                               "eLand_per_cap", 
                                               "eHANPP_per_cap")) %>% 
  mutate(stressor = factor(stressor, levels = c( "Steel_per_cap", 
                                                 "RMC_per_cap", 
                                                 "eLand_per_cap", 
                                                 "eHANPP_per_cap"))) %>% 
  mutate(destination_region_group = as.character(destination_region_group))


dat_percap$destination_region_group[dat_percap$destination_region_group == "United States"] <- "United\nStates"
dat_percap$destination_region_group[dat_percap$destination_region_group == "Asia and Pacific (nec)"] <- "Asia &\nPacific(nec)"
dat_percap$destination_region_group[dat_percap$destination_region_group == "Middle East"] <- "Middle\nEast"
dat_percap$destination_region_group[dat_percap$destination_region_group == "South America (nec)"] <- "South\nAmerica(nec)"

# Sort in descending eHANPP order
reg_sort_eHANPP <- dat_percap %>% 
  filter(stressor == "eHANPP_per_cap") %>% 
  arrange(desc(value)) %>%
  mutate(destination_region_group = as.character(destination_region_group)) %>% 
  pull(destination_region_group)

dat_percap <- dat_percap %>% 
  mutate(destination_region_group = factor(destination_region_group, levels = reg_sort_eHANPP))

# Create and color palette for 13 eHANPP regions
# reg_color_eHANPP <- data.frame("color" = reg_color, "region" = reg_sort_DE) %>% 
#   add_row(color = "forestgreen", region = "Japan")
# 
# reg_color_eHANPP <- reg_color_eHANPP$color[match(reg_sort_eHANPP, reg_color_eHANPP$region)]



plot_3 <- ggplot(dat_percap) +
  geom_bar(aes(x = stressor, y = value, fill = stressor),
           position = "stack",
           stat = "identity") +
  facet_grid(~destination_region_group) +
  theme_minimal() +
  theme(legend.position = "inside",
       legend.position.inside = c(0.8, 0.8),
        legend.title.position = "top",
        legend.title = element_text(face = "bold", size = legend_text_size+1),
        legend.background = element_rect(color = "lightgray",
                                         fill = "gray97", 
                                         linewidth = 0.2),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_text(size = legend_text_size),
        axis.text = element_text(size = axis_text_size, color = "black"),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_line(linetype = "dashed"),
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0.5),
        strip.text = element_text(size = 11, face = "bold")) +
  geom_vline(xintercept = seq(0.5, 14, by = 1), 
             color="gray", 
             size=.5, 
             alpha=.5) +
  scale_y_continuous(breaks = seq(0,2.1,0.25),
                     expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_fill_manual(values = c("royalblue", "tomato3", "yellow3", "forestgreen"),
                    name = "Per-Capita Consumption Indicators",
                    labels = c("Steel Consumption; GAS [t/cap/y]",
                               "Material Footprint; MF [t/cap/y]",
                               "Embodied Land; eLand [m²/cap/y]", 
                               "Embodied HANPP; eHANPP [kgC/cap/y]"))

plot_3



plot_1 <- plot_1 + theme(plot.margin=unit(c(0,0.75,0,0.15), 'cm'))

# Scale the plots and add "empty" rows to customize margins of plots
# plot_grid(plot_2, NULL, plot_1, ncol = 3, rel_widths = c(1,0.1,1))
plot_1_2 <- plot_grid(plot_2, plot_1 + theme(legend.position = "none"), ncol = 2, rel_widths = c(1.28,0.9))

plot_grid(plot_3, plot_1_2, nrow = 2)

ggsave("./output/Fig_4.png",
       plot = last_plot(),  
       width = 14,  
       height = 12,  
       dpi = 1850,
       bg = "white") 

