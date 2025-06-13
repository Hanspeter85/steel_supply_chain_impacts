# This script plots the key results on the level of 32 regions, which is contained in the SI of the paper

reg_ranking_32 <- SI$IOMF_eLand_eHANPP_GAS %>% 
  filter(stressor == "IO-MF") %>% 
  group_by(destination_region) %>% 
  summarize(value = sum(value), .groups = 'drop') %>% 
  arrange(-desc(value)) %>% 
#  filter(value >= 1000) %>% 
  pull(destination_region)
  
  
dat_plot <- SI$IOMF_eLand_eHANPP_GAS %>% 
  group_by(source_region, stressor) %>% 
  summarize(value = sum(value), .groups = 'drop') %>% 
  filter(stressor %in% c("eHANPP-steel", "eLand-steel", "IO-MF")) %>% 
  mutate(stressor = case_when(stressor == "eHANPP-steel" ~ "HANPP",
                              stressor == "eLand-steel" ~ "LandUse",
                              stressor == "IO-MF" ~ "DE")) %>% 
  pivot_wider(names_from = stressor,
              values_from = value) %>% 
  mutate(HANPP = HANPP/1000,
         DE = DE/10^6) %>% 
  pivot_longer(cols = c("DE","LandUse","HANPP"),
               names_to = "stressor") %>% 
  mutate("account" = "PBA") %>% 
  rename("region" = "source_region") %>% 
  bind_rows(SI$IOMF_eLand_eHANPP_GAS %>% 
              group_by(destination_region, stressor) %>% 
              summarize(value = sum(value), .groups = 'drop') %>% 
              filter(stressor %in% c("eHANPP-steel", "eLand-steel", "IO-MF")) %>% 
              mutate(stressor = case_when(stressor == "eHANPP-steel" ~ "HANPP",
                                          stressor == "eLand-steel" ~ "LandUse",
                                          stressor == "IO-MF" ~ "DE")) %>% 
              pivot_wider(names_from = stressor,
                          values_from = value) %>% 
              mutate(HANPP = HANPP/1000,
                     DE = DE/10^6) %>% 
              pivot_longer(cols = c("DE","LandUse","HANPP"),
                           names_to = "stressor") %>% 
              mutate("account" = "CBA") %>% 
              rename("region" = "destination_region")) %>% 
  mutate(stressor = factor(stressor, levels = c("DE","LandUse","HANPP")),
         region = factor(region, levels = reg_ranking_32))

facet_label <- c("DE" = "Iron Ore Extraction\n[Mt/yr]",
                 "LandUse" = "Mining Land Use\n[km2]",
                 "HANPP" = "Mining HANPP\n[ktC/yr]")

ggplot() +
  geom_col(data = dat_plot,
           aes(y = region, x = value, fill = account),
           position = 'dodge') +
  facet_wrap(~stressor,
             ncol = 5,
             scales = "free_x",
             labeller = labeller(stressor = facet_label)) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
        axis.title = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold")) +
  scale_fill_manual(values = c("CBA" = "red","PBA" = "blue"),
                    labels = c("CBA" = "Consumption-based footprints","PBA" = "Production-based accounts"))

ggsave("./output/Fig_Extra_SI_32_regions.png",  # File name for the saved plot
       plot = last_plot(),  # The last plot created in the session
       width = 10,  # Width of the plot in inches
       height = 12,  # Height of the plot in inches
       dpi = 2050,
       bg = "white")  # Resolution (dots per inch), adjust as needed

dat_plot <- SI$`IO-MF-biome` %>% 
  filter(source_region %in% reg_ranking_32) %>% 
  group_by(source_region, stressor_group) %>% 
  summarize(value = sum(value), .groups = 'drop') %>% 
  filter(value > 1000) %>% 
  # mutate(stressor = case_when(stressor == "eHANPP-steel" ~ "HANPP",
  #                             stressor == "eLand-steel" ~ "LandUse",
  #                             stressor == "IO-MF" ~ "DE")) %>% 
  # pivot_wider(names_from = stressor,
  #             values_from = value) %>% 
  # mutate(HANPP = HANPP/1000,
  #        DE = DE/10^6) %>% 
  # pivot_longer(cols = c("DE","LandUse","HANPP"),
  #              names_to = "stressor") %>% 
  mutate("account" = "PBA") %>% 
  rename("region" = "source_region") %>% 
  bind_rows(SI$`IO-MF-biome` %>% 
              group_by(destination_region, stressor_group) %>% 
              summarize(value = sum(value), .groups = 'drop') %>% 
              mutate("account" = "CBA") %>% 
              rename("region" = "destination_region")) %>% 
  mutate(region = factor(region, levels = rev(reg_ranking_32)))

# Create and rearrange color palette for biomes 
biome_color <- c( get_palette("ucscgb",6) )
biome_color <- biome_color[c(1,3,5,6,2,4)]


ggplot(data = dat_plot, aes( y = account, x = value, fill = stressor_group)) +
  geom_bar(stat = "identity",
           position = 'fill') +
  facet_wrap(~region,
             ncol = 1,
             nrow = 32,
             switch = "y") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
        axis.title = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
                     expand = c(0,0),
                     position = "top") +
  scale_fill_manual(values = biome_color) +
  labs(title = "Consumption-based fooptrint (CBA; IO-MF-biome) and production-based account (PBA)\nof iron ore extraction disaggregated into six biomes for 32 regions")

ggsave("./output/Fig_Extra_SI_32_regions_biomes.png",  # File name for the saved plot
       plot = last_plot(),  # The last plot created in the session
       width = 10,  # Width of the plot in inches
       height = 12,  # Height of the plot in inches
       dpi = 2050,
       bg = "white")  # Resolution (dots per inch), adjust as needed

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

# Create and color palette and name ordering to include Japan because of steel production
reg_color_Fig1 <- data.frame("color" = c("forestgreen", reg_color), "region" = c("Japan",reg_sort_DE))
reg_color_Fig1 <- data.frame("color" = reg_color, "region" = reg_sort_DE)

biome_sort <- SI$`IO-MF-biome` %>% 
  group_by(stressor) %>% 
  summarize(value = sum(value), .groups = 'drop') %>% 
  arrange(-desc(value)) %>% 
  pull(stressor)

dat_plot <- SI$`IO-MF-biome` %>% 
  group_by(source_region_group, stressor) %>% 
  summarize(value = sum(value), .groups = 'drop') %>% 
  filter(value > 1000) %>% 
  mutate(value = value/10^6) %>% 
  mutate(source_region_group = factor(source_region_group, levels = reg_color_Fig1$region),
         stressor = factor(stressor, levels = biome_sort))

ggplot() +
  geom_col_pattern(data = dat_plot,
                   aes(x = stressor,
                       y = value,
                       fill = source_region_group,
                       pattern = source_region_group),
                   pattern_fill = "grey30",
                   pattern_spacing = 0.03) +
  coord_flip() +
  theme_minimal() +
  scale_pattern_manual(values=c(rep("none", 9),"stripe", "crosshatch", "circle")) +
  scale_fill_manual(values = reg_color_Fig1$color) +
  scale_y_continuous("Extraction [Mt/year]",
                     expand = c(0,0),
                     breaks = seq(0,1500,200),
                     position = "right") +
  scale_x_discrete(name = "Sub-biome of Iron Ore Mining",
                   expand = c(0,0)) +
  theme(legend.title.position = "top",
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.7, 0.3),
        legend.background = element_rect(color = "lightgray",fill = "gray97", linewidth = 0.2),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(vjust = -1,face = "bold"),
        axis.title.y = element_text(vjust = 1, face = "bold"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_line(linetype = "dashed"), 
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0.5))


ggsave("./output/Fig_Extra_SI_15_biomes_by_13_regions.png",  # File name for the saved plot
       plot = last_plot(),  # The last plot created in the session
       width = 8,  # Width of the plot in inches
       height = 7,  # Height of the plot in inches
       dpi = 2050,
       bg = "white")  # Resolution (dots per inch), adjust as needed
