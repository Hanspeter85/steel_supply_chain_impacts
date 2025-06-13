### Figure 3.1 (MF by biomes)

# Set font/text size for figures
legend_text_size <- 11
axis_title_size <- 16
axis_text_size <- 10


# Sort indicators for correct visualization and add name for tropics footprint
MF_tropics_label <- "MF in (Sub-)Tropics of Foreign Regions [Mt]"
Biome_label <- unique(Results_agg_biome$stressor_group)
indicator_sort <- c(Biome_label, MF_tropics_label)

# Aggregate across biomes and consuming regions
dat <- Results_agg_biome %>% ungroup() %>% select(stressor_group, destination_region_group, value) %>% 
  group_by(stressor_group, destination_region_group) %>% 
  summarise(value = sum(value),  .groups = 'drop') %>% 
  mutate("Indicator" = "IO-MF") %>% 
  bind_rows(Results_agg_biome %>% ungroup() %>% select(stressor_group, source_region_group, value) %>% 
              group_by(stressor_group, source_region_group) %>% 
              summarise(value = sum(value),  .groups = 'drop') %>% 
              mutate("Indicator" = "DE") %>% 
              filter(source_region_group != "Japan") %>% 
              rename("destination_region_group" = "source_region_group")
            ) %>% 
  mutate(destination_region_group = factor(destination_region_group, levels = reg_sort_RMC),
         stressor_group = factor(stressor_group, levels = indicator_sort),
         Indicator = factor(Indicator, levels = c("DE","IO-MF"))) 
  


data_SI[["3_1"]] <- dat

# Create and rearrange color palette for biomes 
biome_color <- c( get_palette("ucscgb",6), "yellow")
biome_color <- biome_color[c(6,1,3,5,2,4,7)]

# Make look-up data frame with labels for a better plot
look_up_reg_name <- setNames(reg_name_plot, dat$destination_region_group %>% levels() %>% as.character())

plot_1 <- ggplot() +
  geom_bar(data = dat,
           aes(x = Indicator,
               y = value,
               fill = stressor_group),
           stat = "identity",
           position = "fill") +
  facet_wrap(~destination_region_group,
             strip.position = "top",
             nrow = 1,
             ncol = 13,
             labeller = labeller(destination_region_group = look_up_reg_name)
             ) +
  theme_minimal() +
  scale_fill_manual(values = biome_color) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        legend.background = element_rect(color = "lightgray",fill = "gray97", linewidth = 0.2),
        legend.text = element_text(size = legend_text_size),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size, colour = "black"),
        axis.text.x = element_text(size = axis_text_size-2),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(color = "grey", 
                                    fill = NA, 
                                    size = 1),
        strip.placement = "outside",
        strip.text = element_text(size = 8, face = "bold")) +
  geom_vline(xintercept = seq(0.5, 14, by = 1), color="gray", size=.5, alpha=.5) +
  scale_y_continuous("Biome share\n",
                     expand = c(0,0),
                     breaks = c(0.2, 0.4, 0.6, 0.8,1),
                     labels = scales::percent_format(accuracy = 1))


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
biome_color <- get_palette("ucscgb",6)
biome_color <- biome_color[c(6,1,3,5,2,4)]

# Calculate RMC from tropical/subtropical biomes in absolute terms and add label for figure
dat <- Results_agg_biome %>% ungroup() %>% 
  filter(source == "Import") %>% 
  select(destination_region_group, stressor_group, value) %>% 
  group_by(destination_region_group, stressor_group) %>% 
  summarise(value = sum(value), .groups = 'drop') %>%
  rename("region_group" = "destination_region_group") %>% 
  mutate("Indicator" = "IM") %>% 
  bind_rows(Results_agg_biome %>% ungroup() %>% 
              filter(source == "Import") %>% 
              select(source_region_group, stressor_group, value) %>% 
              group_by(source_region_group, stressor_group) %>% 
              summarise(value = sum(value), .groups = 'drop') %>% 
              rename("region_group" = "source_region_group") %>% 
              mutate("Indicator" = "EX")
            ) %>% 
  mutate(region_group = factor(region_group, levels = reg_sort_RMC),
         stressor_group = factor(stressor_group, levels = Biome_label),
         Indicator = factor(Indicator, levels = c("IM","EX")))


data_SI[["3_2"]] <- dat

plot_2 <- ggplot() +
  geom_col(data = dat,
           aes(x = Indicator,
               y = value/10^6,
               fill = stressor_group)) +
  facet_wrap(~region_group,
             strip.position = "top",
             nrow = 1,
             ncol = 13,
             labeller = labeller(region_group = look_up_reg_name)) +
  theme_minimal() +
  scale_fill_manual(values = biome_color) +
  theme(legend.title = element_blank(),
        legend.position.inside = c(0.5, 0.7),
        legend.position = "inside",
        legend.background = element_rect(color = "lightgray",fill = "gray97", linewidth = 0.2),
        legend.text = element_text(size = legend_text_size),
        axis.title = element_text(size = axis_title_size),
        axis.text = element_text(size = axis_text_size, colour = "black"),
        axis.text.x = element_text(size = axis_text_size),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", vjust = 2, colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_line(linetype = "dashed"),
        panel.border = element_rect(color = "grey", 
                                    fill = NA, 
                                    size = 1),
        strip.placement = "outside",
        strip.text = element_text(size = 8, face = "bold")) +
  geom_vline(xintercept = seq(0.5, 14, by = 1), color="gray", size=.5, alpha=.5) +
  scale_y_continuous("IO-MF-biome of imports (IM)\nand exports (EX) in Mt/year",
                     expand = c(0,0),
                     breaks = seq(0, 700, by = 100))

plot_2

## All stressors in one plot in %

# Clean population data and add it to plotting data
POP <- data.frame("stressor" = "POP",
                  "destination_region_group" = rownames(ewMFA),
                  "source" = "Population",
                  "value" = ewMFA$Population)

dat <- Results_agg %>% 
  group_by(stressor, destination_region_group) %>% 
  summarise(value = sum(value), .groups = 'drop') %>% 
  mutate(stressor = as.character(stressor)) %>%
  rbind(POP %>% 
          select(-source)) %>% 
  group_by(stressor) %>% 
  mutate(share_value = value/sum(value)) %>% 
  mutate(destination_region_group = factor(destination_region_group, levels = reg_sort_RMC)) %>% 
  mutate(stressor = factor(stressor, levels = c("POP", "Steel_GAS", "RMC", "eLand", "eHANPP"))) %>% 
  mutate(destination_region_group = as.character(destination_region_group))

data_SI[["3_3"]] <- dat

dat$destination_region_group[dat$destination_region_group == "United States"] <- "United\nStates"
dat$destination_region_group[dat$destination_region_group == "Asia and Pacific (nec)"] <- "Asia &\nPacific(nec)"
dat$destination_region_group[dat$destination_region_group == "Middle East"] <- "Middle\nEast"
dat$destination_region_group[dat$destination_region_group == "Latin America (nec)"] <- "Latin\nAmerica(nec)"

dat <- dat %>% mutate(destination_region_group = factor(destination_region_group, levels = reg_name_plot))


x_labels <- c("POP", "Steel-GAS", "IO-MF", "eLand-steel", "eHANPP-steel")

plot_3 <- ggplot(dat) +
  geom_bar(aes(x = stressor,
               y = share_value,
               fill = stressor),
           position = "stack",
           stat = "identity",
           color = "black") +
  facet_grid(~destination_region_group) +
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.5, 0.8),
        legend.title.position = "left",
        legend.title = element_blank(),
        legend.background = element_rect(color = "lightgray",
                                         fill = "gray97", 
                                         linewidth = 0.2),
        axis.title.y = element_text(size = axis_title_size, vjust = 2,face = "bold"),
        axis.title.x = element_blank(),
        legend.text = element_text(size = legend_text_size),
        axis.text = element_text(size = axis_text_size, color = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10, hjust=1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_line(linetype = "dashed"),
        panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 0.5),
        strip.text = element_text(size = 8, face = "bold"),
        panel.spacing = unit(1, "lines")) +
  geom_vline(xintercept = seq(0.5, 14, by = 1), 
             color="gray", 
             size=.5, 
             alpha=.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0,0.6,0.1),
                     expand = c(0,0),
                     name = "Global Share\n") +
  scale_x_discrete(labels = x_labels,
                   expand = c(0,0),
                   name = "Consumption Indicators") +
  scale_fill_manual(values = c("black", "grey", "orange","yellow3", "forestgreen"),
                    labels = c("Population", "Steel-GAS", "IO-MF","eLand-steel","eHANPP-steel"))

plot_3

# Scale the plots and add "empty" rows to customize margins of plots
plot_grid(plotlist = list(plot_1, NULL, plot_2,NULL,plot_3),
          axis = "l",
          nrow = 5,
          labels = c("A)",NA, "B)",NA, "C)"), 
          rel_heights = c(1,0.02, 1, 0.02 ,1.2))

# plot_4 <- plot_4 + scale_x_discrete(labels = reg_name_plot) + theme(axis.text = element_text(colour = "black"))

ggsave("./output/Fig_3.pdf",
       plot = last_plot(),  
       width = 13,  
       height = 16,  
       dpi = 1850,
       bg = "white") 
