### Figure 3.1 (MF by biomes)

# Set font/text size for figures
legend_text_size <- 11
axis_title_size <- 16
axis_text_size <- 12


# Sort indicators for correct visualization and add name for tropics footprint
MF_tropics_label <- "MF in (Sub-)Tropics of Foreign Regions [Mt]"
Biome_label <- unique(Results_agg_biome$stressor_group)
indicator_sort <- c(Biome_label, MF_tropics_label)

# Aggregate across biomes and consuming regions
dat <- Results_agg_biome %>% ungroup() %>% select(stressor_group, destination_region_group, value) %>% 
  group_by(stressor_group, destination_region_group) %>% 
  summarise(value = sum(value),  .groups = 'drop') %>% 
  mutate(destination_region_group = factor(destination_region_group, levels = reg_sort_RMC)) %>% 
  rename(Indicator = stressor_group) %>% 
  mutate(Indicator = factor(Indicator, levels = indicator_sort))

# Calculate RMC from tropical/subtropical biomes in absolute terms and add label for figure
# IM_tropics <- Results_agg_biome %>% ungroup() %>%
#   filter(source == "Import", stressor_group == "Tropical/subtropical Forests & Grasslands") %>%
#   select(destination_region_group, value) %>%
#   group_by(destination_region_group) %>%
#   summarise(value = sum(value), .groups = 'drop') %>%
#   mutate(destination_region_group = factor(destination_region_group, levels = reg_sort_RMC)) %>%
#   mutate(Indicator = MF_tropics_label) %>%
#   mutate(Indicator = factor(Indicator, levels = indicator_sort)) %>%
#   mutate(value = value/10^6)

# Scaling factor for secondary axis (MF in tropics)
scale_y <- 1/150

# Create and rearrange color palette for biomes 
biome_color <- c( get_palette("nejm",6), "yellow")
biome_color <- biome_color[c(6,1,3,5,2,4,7)]

plot_1 <- ggplot() +
  geom_bar(data = dat, aes(x = destination_region_group, y = value, fill = Indicator), stat = "identity", position = "fill") +
#  geom_point(data = IM_tropics, aes(x = destination_region_group, y = value*scale_y, fill = Indicator),  shape = 21, color = "black", size = 4) +
  theme_minimal() +
  scale_fill_manual(values = biome_color) +
  theme(legend.title = element_blank(),
        #        legend.position = c(0.7, 0.8),
        legend.position = "bottom",
        legend.background = element_rect(color = "lightgray",fill = "gray97", linewidth = 0.2),
        axis.title.x = element_blank(),
        legend.text = element_text(size = legend_text_size),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(color = "grey", 
                                    fill = NA, 
                                    size = 1)) +
  geom_vline(xintercept = seq(0.5, 14, by = 1), color="gray", size=.5, alpha=.5) +
  scale_y_continuous("MF by biome of extraction [%]",
                     expand = c(0,0),
                     breaks = c(0.2, 0.4, 0.6, 0.8,1),
                     labels = scales::percent_format(accuracy = 1)) + 
  scale_x_discrete(labels = reg_name_plot)

plot_1

## RME in trade by Biomes

# Create Biome order following DE and add labels for plotting
tmp <-  c("Taiga &\nTundra", 
          "Deserts & Xeric\nShrublands",
          "Mediterranean\nForests &\n Woodlands",
          "Montane\nGrasslands\n& Shrublands", 
          "Temperate\nForests\n& Grasslands",
          "(Sub-)tropical\nForests &\nGrasslands")

tmp <- data.frame("stressor_group" = unique(Results_agg_biome$stressor_group),
                  "plot" = tmp)

# Biome_label <- Results_agg_biome %>% 
#   group_by(stressor_group) %>% 
#   summarise(value = sum(value)) %>% 
#   arrange(desc(value)) %>% 
#   left_join(tmp, by = "stressor_group") %>% 
#   ungroup()

# Create and rearrange color palette for biomes 
biome_color <- get_palette("nejm",6)
biome_color <- biome_color[c(6,1,3,5,2,4)]

# Calculate RMC from tropical/subtropical biomes in absolute terms and add label for figure
dat <- Results_agg_biome %>% ungroup() %>% 
  filter(source == "Import") %>% 
  select(destination_region_group, stressor_group, value) %>% 
  group_by(destination_region_group, stressor_group) %>% 
  summarise(value = sum(value), .groups = 'drop') %>% 
  mutate(destination_region_group = factor(destination_region_group, levels = reg_sort_RMC)) %>% 
  mutate(stressor_group = factor(stressor_group, levels = Biome_label) )



ggplot() +
  geom_col(data = dat, aes(x = destination_region_group, y = value/10^6, fill = stressor_group)) +
  theme_minimal() +
  scale_fill_manual(values = biome_color) +
  theme(legend.title = element_blank(),
        #        legend.position = c(0.7, 0.8),
        legend.position = "none",
        legend.background = element_rect(color = "lightgray",fill = "gray97", linewidth = 0.2),
        axis.title.x = element_blank(),
        legend.text = element_text(size = legend_text_size),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_line(linetype = "dashed"),
        panel.border = element_rect(color = "grey", 
                                    fill = NA, 
                                    size = 1)) +
  geom_vline(xintercept = seq(0.5, 14, by = 1), color="gray", size=.5, alpha=.5) +
  scale_y_continuous("RME in imports by biome of extraction [Mt/y]",
                     expand = c(0,0)) +
  scale_x_discrete(labels = reg_name_plot)

## All stressors in one plot in %
dat <- Results_agg %>% 
  group_by(stressor, destination_region_group, source) %>% 
  summarise(value = sum(value), .groups = 'drop') %>% 
  group_by(stressor) %>% 
  mutate(share_value = value/sum(value)) %>% 
  mutate(destination_region_group = factor(destination_region_group, levels = reg_sort_RMC)) %>% 
  mutate(stressor = factor(stressor, levels = c("Steel_GAS", "RMC", "eLand", "eHANPP"))) %>% 
  mutate(source = factor(source, levels = c("Import", "Domestic")))

ggplot(dat) +
  geom_bar(aes(x = stressor, y = share_value, fill = source),
           position = "stack",
           stat = "identity") +
  facet_grid(~destination_region_group)


# plot_4 <- plot_4 + scale_x_discrete(labels = reg_name_plot) + theme(axis.text = element_text(colour = "black"))

ggsave("./output/Fig_3.png",  # File name for the saved plot
       plot = last_plot(),  # The last plot created in the session
       width = 13,  # Width of the plot in inches
       height = 10,  # Height of the plot in inches
       dpi = 1850,
       bg = "white")  # Resolution (dots per inch), adjust as needed

