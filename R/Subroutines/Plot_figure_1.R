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

## 1.1 Plot extraction by biomes and source regions  
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
        legend.title = element_text(size = legend_text_size, 
                                    face = "bold", 
                                    vjust = 1,
                                    hjust = 0.5),
        legend.position = "inside",
        legend.position.inside = c(0.8, 0.7),
        legend.background = element_rect(color = "lightgray",fill = "gray97", linewidth = 0.2),
        legend.text = element_text(size = legend_text_size),
        axis.title.x = element_text(vjust = -1,face = "bold"),
        axis.title.y = element_text(vjust = 1, face = "bold"),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(color = "grey", fill = NA, linewidth = 1)) +
  geom_vline(xintercept = seq(0.5, 7, by = 1), color="gray", linewidth=.5, alpha=.5)

plot_1

## Plot relative bar charts for extraction, land use and HANPP by regions
# Aggregate across RMC, eLand and eHANPP in terms of source regions
dat <- Results_agg %>% 
  filter(source_region_group != "Japan", stressor != "Steel_GAS") %>% 
  ungroup() %>%  
  select(stressor, source_region_group, value) %>% 
  group_by(stressor, source_region_group) %>% 
  summarise(value = sum(value), .groups = 'drop') %>% 
  mutate(source_region_group = factor(source_region_group, levels = reg_sort_DE)) %>% 
  mutate(stressor = factor(stressor, levels = c("RMC", "eLand", "eHANPP")))

# Create labels of stressors that show sum of global stressor
x_labels <- c("Extraction\n[3.2 Gt/y]", "Land use\n[5593 km2/y]", "HANPP\n[2.98 MtC/y]")


plot_2 <- ggplot() +
  geom_bar(data = dat, aes(x = stressor, y = value, fill = source_region_group), 
           stat = "identity", position = "fill") +
  theme_minimal() +
  scale_fill_manual(values = reg_color,
                    name = "Region of Extraction") +
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(size = legend_text_size, 
                                    face = "bold", 
                                    vjust = 1,
                                    hjust = 0.5),
        legend.background = element_rect(color = "lightgray",
                                         fill = "gray97", 
                                         linewidth = 0.2),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_text(size = legend_text_size),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  geom_vline(xintercept = seq(0.5, 14, by = 1), color="gray", size=.5, alpha=.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
                     expand = c(0,0)) +
  scale_x_discrete(labels = x_labels,
                   expand = c(0,0))

plot_2  

## Scatter plot for pressure/impact intensities: 
## land per ton iron ore on y-axis and and HANPP per land on x-axis and maybe HANPP per ton as circle)
# Calculate intensities
dat <- Results_agg %>% 
  filter(source_region_group != "Japan", stressor != "Steel_GAS") %>% 
  ungroup() %>%  
  select(stressor, source_region_group, value) %>% 
  group_by(stressor, source_region_group) %>% 
  summarise(value = sum(value), .groups = 'drop') %>% 
  mutate(source_region_group = factor(source_region_group, levels = reg_sort_DE)) %>% 
  pivot_wider(names_from = stressor, values_from = value) %>% 
  mutate(m2_per_t = 10^6*eLand/RMC) %>% 
  mutate(NPP_per_m2 = eHANPP/eLand) %>% 
  select(m2_per_t, NPP_per_m2, source_region_group) %>% 
  mutate(source_region_group = factor(source_region_group, levels = reg_sort_DE))

# calculate global averages
tot <- Results_agg %>% 
  filter(source_region_group != "Japan", stressor != "Steel_GAS") %>% 
  ungroup() %>%  
  select(stressor, value) %>% 
  group_by(stressor) %>% 
  summarise(value = sum(value), .groups = 'drop')

average_m2_per_t <- 10^6*tot$value[tot$stressor == "eLand"]/tot$value[tot$stressor == "RMC"]
average_NPP_per_m2 <- tot$value[tot$stressor == "eHANPP"]/tot$value[tot$stressor == "eLand"]

# Select symbols for point shapes of regions
#reg_symbol <- c(21,21,21,21,22,22,22,22,25,25,25,25)
reg_symbol <- c(15,15,15,15,19,19,19,19,17,17,17,17)

ggplot( data = dat, aes(x = NPP_per_m2, y = m2_per_t, group =  source_region_group) ) +
  geom_point(size = 5, aes(shape = source_region_group, color = source_region_group ) ) +
  theme_minimal() +
  coord_fixed(ratio = (1000/9)) +
  theme(panel.border = element_rect(color = "grey", 
                                    fill = NA, 
                                    size = 0.8),
        legend.title = element_text(size = legend_text_size, 
                                    face = "bold", 
                                    vjust = 1,
                                    hjust = 0.5),
        legend.background = element_rect(color = "lightgray",
                                         fill = "gray97", 
                                         linewidth = 0.2)) +
  scale_shape_manual(values = reg_symbol) +
  scale_color_manual(values = reg_color) +
  scale_x_continuous( limits = c(0, 1000),
                      expand = c(0,0)) +
  scale_y_continuous( limits = c(0, 9),
                      expand = c(0,0))

  






labs( x= paste0("",x_lab," [log10 tonnes]"),
        y = paste0(y_lab," [log10 tonnes]") ) +
  theme( panel.grid.minor = element_blank(), 
         legend.position = "top",
         text = element_text(size=14),
         legend.text=element_text(size=14),
         legend.title=element_blank(),
         legend.background = element_rect(fill="gray90", size=.5) ) +
  geom_abline(intercept = 0.15, slope = 1, linetype = 3, alpha = 0.5) + 
  geom_abline(intercept = -0.15, slope = 1, linetype = 3, alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, alpha = 0.5)
