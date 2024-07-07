## Plotting IDA results for India, China and Europe

# Transform to relative scale
# China
tmp <- dat_percap %>% 
  filter(stressor == "eHANPP_per_cap",
         destination_region_group == "China") %>%
  mutate(value = value * 1000) %>% 
  pull(value)

result_IDA_China[,2:13] <- result_IDA_China[,2:13] / tmp

# Europe
tmp <- dat_percap %>% 
  filter(stressor == "eHANPP_per_cap",
         destination_region_group == "Europe") %>%
  mutate(value = value * 1000) %>% 
  pull(value)

result_IDA_Europe[,2:13] <- result_IDA_Europe[,2:13] / tmp

# India
tmp <- dat_percap %>% 
  filter(stressor == "eHANPP_per_cap",
         destination_region_group == "India") %>%
  mutate(value = value * 1000) %>% 
  pull(value)

result_IDA_India[,2:13] <- result_IDA_India[,2:13] / tmp

# Transform data for plotting
dat_China <- result_IDA_China %>% 
  pivot_longer(names_to = "other_region", 
               col = all_of(colnames(result_IDA_China)[-1])) %>% 
  mutate("destination_region_group" = "China")

dat_Europe <- result_IDA_Europe %>% 
  pivot_longer(names_to = "other_region", 
               col = all_of(colnames(result_IDA_Europe)[-1])) %>% 
  mutate("destination_region_group" = "Europe")

dat_India <- result_IDA_India %>% 
  pivot_longer(names_to = "other_region", 
               col = all_of(colnames(result_IDA_India)[-1])) %>% 
  mutate("destination_region_group" = "India")

dat <- dat_China %>% 
  bind_rows(dat_Europe, dat_India)

dat$other_region[dat$other_region == "United States"] <- "United\nStates"
dat$other_region[dat$other_region == "Asia and Pacific (nec)"] <- "Asia &\nPacific\n(nec)"
dat$other_region[dat$other_region == "Middle East"] <- "Middle\nEast"
dat$other_region[dat$other_region == "South America (nec)"] <- "South\nAmerica\n(nec)"

dat <- dat %>% 
  mutate(other_region = factor(other_region, levels = reg_sort_eHANPP[-9])) %>% 
  mutate(effect = factor(effect, levels = c("GAS_per_head", "RMC_per_GAS", "Land_per_RMC", "HANPP_per_Land", "TOTAL_delta")))

dat_tot <- dat %>% filter(effect == "TOTAL_delta")

dat <- dat %>% filter(effect != "TOTAL_delta")

# dat <- Results_agg_biome %>% ungroup() %>% 
#   filter(source == "Import") %>% 
#   select(destination_region_group, stressor_group, value) %>% 
#   group_by(destination_region_group, stressor_group) %>% 
#   summarise(value = sum(value), .groups = 'drop') %>% 
#   mutate(destination_region_group = factor(destination_region_group, levels = reg_sort_RMC)) %>% 
#   mutate(stressor_group = factor(stressor_group, levels = Biome_label) )

# set_region <- "China"
plot_IDA <- function(set_region)
{
  plot <- ggplot() +
    geom_col(data = dat %>% filter(destination_region_group == set_region), 
             aes(x = other_region, y = value, fill = effect)) +
    geom_point(data = dat_tot %>% filter(destination_region_group == set_region), 
               aes(x = other_region, y = value, fill = effect),  
               shape = 21, 
               color = "black", 
               size = 4) +
    theme_minimal() +
    theme(legend.title = element_text(face = "bold", size = legend_text_size+1),
          legend.position = "top",
          legend.box="horizontal",
          legend.background = element_rect(color = "lightgray",fill = "gray97", linewidth = 0.2),
          legend.text = element_text(size = legend_text_size),
          legend.box.spacing = unit(1, "cm"),
          axis.title = element_text(size = axis_title_size),
          axis.text = element_text(size = axis_text_size, colour = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(face = "bold", vjust = 2, colour = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_line(linetype = "dashed"),
          panel.border = element_rect(color = "grey", 
                                      fill = NA, 
                                      size = 1)) +
    guides(fill = guide_legend(theme = theme(legend.title.position="top"), title.hjust = 0.5)) +
    scale_fill_manual(values = c("royalblue", "tomato3", "yellow3", "forestgreen", "yellow"),
                      name = "Cross-Country Decomposition Effects:",
                      labels = c("Steel-GAS per Capita",
                                 "MF per Steel-GAS",
                                 "eLand per MF", 
                                 "eHANPP per eLand",
                                 "Delta-TOTAL")) +
    geom_vline(xintercept = seq(0.5, 14, by = 1), color="gray", size=.5, alpha=.5) +
    scale_y_continuous(str_c("% of Per-Capita eHANPP of ", set_region),
                       breaks = seq(-25,25,0.2),
                       expand = c(0, 0),
                       labels = scales::percent_format(accuracy = 1))
  
  return(plot)
}

plot_Europe <- plot_IDA("Europe") + theme(legend.position = "none")
plot_China <- plot_IDA("China") + theme(legend.position = "top")
plot_India <- plot_IDA("India") + theme(legend.position = "none") + scale_y_continuous(str_c("% of Per-Capita eHANPP of India"),
                                                                                       breaks = seq(-25,25,2),
                                                                                       expand = c(0, 0),
                                                                                       labels = scales::percent_format(accuracy = 1))

plot_grid(plot_China, NULL, plot_Europe, NULL, plot_India, axis = "l", nrow = 5, rel_heights = c(1.2,0.02, 1, 0.02 ,1))

# plot_4 <- plot_4 + scale_x_discrete(labels = reg_name_plot) + theme(axis.text = element_text(colour = "black"))

ggsave("./output/Fig_5.png",
       plot = last_plot(),  
       width = 13,  
       height = 16,  
       dpi = 1850,
       bg = "white") 
