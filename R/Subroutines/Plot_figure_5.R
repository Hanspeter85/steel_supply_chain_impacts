## Plotting IDA results for India, China and Europe

# Transform to relative scale
# China
tmp <- dat_percap %>% 
  filter(stressor == "eHANPP_per_cap",
         destination_region_group == "China") %>%
  mutate(value = value * 1000) %>% 
  pull(value)

result_IDA_China[,2:14] <- result_IDA_China[,2:14] / tmp

# Europe
tmp <- dat_percap %>% 
  filter(stressor == "eHANPP_per_cap",
         destination_region_group == "Europe") %>%
  mutate(value = value * 1000) %>% 
  pull(value)

result_IDA_Europe[,2:14] <- result_IDA_Europe[,2:14] / tmp

# Global average
tmp <- IDA_data %>% 
  filter(destination_region_group == "Global",
         stressor %in% c("eHANPP","POP"))

tmp <- 1e6* tmp$value[tmp$stressor == "eHANPP"] / tmp$value[tmp$stressor == "POP"]

result_IDA_Global[,2:14] <- result_IDA_Global[,2:14] / tmp

# Transform data for plotting
dat_China <- result_IDA_China %>% 
  pivot_longer(names_to = "other_region", 
               col = all_of(colnames(result_IDA_China)[-1])) %>% 
  mutate("destination_region_group" = "China")

dat_Europe <- result_IDA_Europe %>% 
  pivot_longer(names_to = "other_region", 
               col = all_of(colnames(result_IDA_Europe)[-1])) %>% 
  mutate("destination_region_group" = "Europe")

dat_Global <- result_IDA_Global %>% 
  pivot_longer(names_to = "other_region", 
               col = all_of(colnames(result_IDA_Global)[-1])) %>% 
  mutate("destination_region_group" = "Global")

dat <- dat_China %>% 
  bind_rows(dat_Europe, dat_Global)

data_SI[["5"]] <- dat

dat$other_region[dat$other_region == "United States"] <- "United\nStates"
dat$other_region[dat$other_region == "Asia and Pacific (nec)"] <- "Asia &\nPacific\n(nec)"
dat$other_region[dat$other_region == "Middle East"] <- "Middle\nEast"
dat$other_region[dat$other_region == "Latin America (nec)"] <- "Latin\nAmerica\n(nec)"
dat$other_region[dat$other_region == "Global"] <- "Global\nAverage"

dat$destination_region_group[dat$destination_region_group == "Global"] <- "Global Average"

dat <- dat %>% 
  mutate(other_region = factor(other_region, levels = reg_sort_eHANPP)) %>% 
  mutate(effect = factor(effect, levels = c("GAS_per_head", "RMC_per_GAS", "Land_per_RMC", "HANPP_per_Land", "TOTAL_delta")))

dat_tot <- dat %>% filter(effect == "TOTAL_delta")

dat <- dat %>% filter(effect != "TOTAL_delta")

legend_text_size <- 14

# set_region <- "Global"
plot_IDA <- function(set_region)
{
  plot <- ggplot() +
    geom_col(data = dat %>% filter(destination_region_group == set_region), 
             aes(x = other_region, y = value, fill = effect),
             color = "black") +
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
    scale_fill_manual(values = c("grey", "orange", "yellow3", "forestgreen", "yellow"),
                      name = "Cross-regional Index Decomposition Analysis (IDA) Factors:",
                      labels = c("Consumption level\n(per-cap Steel-GAS)",
                                 "Production structure\n(IO-MF/Steel-GAS)",
                                 "Land requirements\n(eLand-steel/IO-MF)", 
                                 "Land use intensity\n(eHANPP-steel/eLand-steel)",
                                 "Delta-TOTAL")) +
    geom_vline(xintercept = seq(0.5, 14, by = 1), color="gray", size=.5, alpha=.5) +
    scale_y_continuous(str_c(set_region," per-cap eHANPP-steel"),
                       breaks = seq(-25,25,0.2),
                       expand = c(0.02, 0.02),
                       labels = scales::percent_format(accuracy = 1))
  
  return(plot)
}

plot_China <- plot_IDA("China") + theme(legend.position = "top")

ggsave("./output/Fig_5.pdf",
       plot = last_plot(),  
       width = 13,  
       height = 7,  
       dpi = 1850,
       bg = "white") 


plot_Europe <- plot_IDA("Europe") + theme(legend.position = "top")
plot_Global <- plot_IDA("Global Average") + theme(legend.position = "none")

plot_grid(plotlist = list(plot_Europe, NULL, plot_Global), 
          axis = "l", 
          nrow = 3, 
          rel_heights = c(1,0.02, 1),
          labels = c("A)",NA,"B)"))

# plot_4 <- plot_4 + scale_x_discrete(labels = reg_name_plot) + theme(axis.text = element_text(colour = "black"))

ggsave("./output/Fig_5_SI.png",
       plot = last_plot(),  
       width = 13,  
       height = 10,  
       dpi = 1850,
       bg = "white") 
