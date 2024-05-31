### This script creates the combined plot of figure 1 (mining extensions of PIOT)

## Change where output of plots should be directed to
# dev.new() # to new window
# dev.off() # to within main window

# Set font/text size for figures
legend_text_size <- 16
axis_title_size <- 16
axis_text_size <- 12

# Get sorting of regions in descending order of RMC and DEC
reg_sort_RMC <- ewMFA %>% mutate("Region" = rownames(ewMFA)) %>% select(Region, RMC) %>% 
  remove_rownames() %>% arrange(desc(RMC)) %>% pull(Region)
reg_sort_DE <- ewMFA %>% mutate("Region" = rownames(ewMFA)) %>% select(Region, DE) %>% 
  remove_rownames() %>% arrange(-desc(DE)) %>% filter(Region != "Japan") %>% pull(Region)

# Create color codes for MF and resort color palette for extraction in descending order
reg_color <- c( get_palette("jco",10), "#7b4173","#637939")
reg_color <- reg_color[c(3,6:11,5,2,12,1,4)]

reg_color <- data.frame( "region" = reg_sort_DE, "color" = reg_color ) %>% 
  mutate(region = factor(region, reg_sort_DE)) %>% 
  arrange(region) %>% 
  pull(color)

# Create Biome order following DE and add labels for plotting
tmp <-  c("Taiga & Tundra", 
          "Deserts &\nXeric Shrublands",
          "Mediterranean\nForests & Woodlands",
          "Montane Grasslands\n& Shrublands", 
          "Temperate Forests\n& Grasslands",
          "Tropical/subtropical\nForests & Grasslands")

tmp <- data.frame("stressor_group" = unique(Results_agg_biome$stressor_group),
                  "plot" = tmp)
                               
Biome_label <- Results_agg_biome %>% 
  group_by(stressor_group) %>% 
  summarise(value = sum(value)) %>% 
  arrange(desc(value)) %>% 
  left_join(tmp, by = "stressor_group") %>% 
  ungroup()


# Aggregate across source regions & biomes, set factor levels for sorting and 
# change units to Mega tons
dat <- Results_agg_biome %>% 
  filter(source_region_group != "Japan") %>% 
  ungroup() %>%  
  select(stressor_group, source_region_group, value) %>% 
  group_by(stressor_group, source_region_group) %>% 
  summarise(value = sum(value)/10^6, .groups = 'drop') %>% 
  mutate(source_region_group = factor(source_region_group, levels = reg_sort_DE)) %>% 
  mutate(stressor_group = factor(stressor_group, levels = Biome_label$stressor_group))


plot_1 <- ggplot() +
  geom_col(data = dat, aes(x = stressor_group, y = value, fill = source_region_group), 
           position = position_stack(reverse = FALSE)) +
  theme_minimal() +
  scale_fill_manual(values = reg_color, name = "Region of Extraction") +
  scale_y_continuous("Mega tons [Mt]") +
  scale_x_discrete(labels = Biome_label$plot,
                   name = "Biome of Iron Ore Extraction") +
  theme(legend.title.position = "top",
        legend.title.align = 0.5,
        legend.title = element_text(size = legend_text_size, face = "bold", vjust = 1),
        legend.position = "inside",
        legend.position.inside = c(0.8, 0.7),
        legend.background = element_rect(color = "lightgray",fill = "gray97", linewidth = 0.2),
        legend.text = element_text(size = legend_text_size),
        axis.title.x = element_text(vjust = -1,face = "bold"),
        axis.title.y = element_text(vjust = 1, face = "bold"),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(color = "grey", fill = NA, size = 1)) +
  geom_vline(xintercept = seq(0.5, 7, by = 1), color="gray", size=.5, alpha=.5)

plot_1    

  
  