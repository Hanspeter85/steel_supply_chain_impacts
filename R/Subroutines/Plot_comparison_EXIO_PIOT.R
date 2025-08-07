Compare_EXIO <- list()

Compare_EXIO[["FP"]] <- SI$IOMF_eLand_eHANPP_GAS %>% 
  filter(stressor != "Steel-GAS") %>% 
  group_by(source_region, destination_region, source_region_group,
           destination_region_group, stressor, unit) %>% 
  summarize(GPIOT = sum(value), .groups = 'drop') %>% 
  left_join(Result_EXIO$FP %>% 
              select(source_region, destination_region, stressor, value) %>% 
              mutate(stressor = case_when(stressor == "RMC" ~ "IO-MF",
                                          stressor == "eLand" ~ "eLand-steel",
                                          stressor == "eHANPP" ~"eHANPP-steel")),
            by = c("source_region","destination_region","stressor")) %>% 
  rename("EXIOBASE" = "value")


reg_ranking_32 <- SI$IOMF_eLand_eHANPP_GAS %>% 
  filter(stressor == "IO-MF") %>% 
  group_by(destination_region) %>% 
  summarize(value = sum(value), .groups = 'drop') %>% 
  arrange(-desc(value)) %>% 
  #  filter(value >= 1000) %>% 
  pull(destination_region)

plot_dat <- Compare_EXIO$FP %>% 
  group_by(destination_region, stressor) %>% 
  summarize(GPIOT = sum(GPIOT),
            EXIOBASE = sum(EXIOBASE),
            .groups = 'drop') %>% 
  mutate(stressor = case_when(stressor == "eHANPP-steel" ~ "HANPP",
                              stressor == "eLand-steel" ~ "LandUse",
                              stressor == "IO-MF" ~ "DE")) %>% 
  pivot_longer(cols = c("GPIOT","EXIOBASE"),
               names_to = "Model") %>% 
  pivot_wider(names_from = stressor,
              values_from = value) %>% 
  mutate(HANPP = HANPP/1000,
         DE = DE/10^6) %>% 
  pivot_longer(cols = c("DE","LandUse","HANPP"),
               names_to = "stressor") %>% 
  rename("region" = "destination_region") %>% 
  mutate(stressor = factor(stressor, levels = c("DE","LandUse","HANPP")),
         region = factor(region, levels = reg_ranking_32))

facet_label <- c("DE" = "Iron-Ore Material Footprint (IO-MF)\n[Mt/yr]",
                 "LandUse" = "Embodied Land (eLand-steel)\n[km2]",
                 "HANPP" = "Embodied HANPP (eHANPP-steel)\n[ktC/yr]")

ggplot() +
  geom_col(data = plot_dat,
           aes(y = region, x = value, fill = Model),
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
        legend.text = element_text(size = 12, face = "bold"),
        plot.margin = margin(1, 1, 1, 0, "cm")) +
  scale_fill_manual(values = c("EXIOBASE" = "green","GPIOT" = "blue"))

ggsave("./output/Fig_Extra_SI_EXIO_vs_PIOT.png",  # File name for the saved plot
       plot = last_plot(),  # The last plot created in the session
       width = 10,  # Width of the plot in inches
       height = 12,  # Height of the plot in inches
       dpi = 2050,
       bg = "white")  # Resolution (dots per inch), adjust as needed


Compare_EXIO[["biome"]] <- SI$`IO-MF-biome` %>% 
  group_by(source_region, destination_region, source_region_group,
           destination_region_group, stressor_group, unit) %>% 
  summarize(GPIOT = sum(value), .groups = 'drop') %>% 
  left_join(Result_EXIO$biome %>% 
              group_by(source_region, destination_region, stressor_group) %>% 
              summarize(EXIOBASE = sum(value), .groups = 'drop'),
            by = c("source_region","destination_region","stressor_group")) %>% 
  filter(GPIOT != 0)


plot_dat <- Compare_EXIO$biome %>% 
  group_by(destination_region, stressor_group) %>% 
  summarize(GPIOT = sum(GPIOT)/10^6,
            EXIOBASE = sum(EXIOBASE)/10^6,
            .groups = 'drop') %>% 
  pivot_longer(cols = c("GPIOT","EXIOBASE"),
               names_to = "Model") %>% 
  rename("region" = "destination_region") %>% 
  mutate(
#    stressor = factor(stressor, levels = c("DE","LandUse","HANPP")),
         region = factor(region, levels = rev(reg_ranking_32)))


# Create and rearrange color palette for biomes 
biome_color <- c( get_palette("ucscgb",6) )
biome_color <- biome_color[c(1,3,5,6,2,4)]


ggplot(data = plot_dat, aes( y = Model, x = value, fill = stressor_group)) +
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
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),,
        plot.margin = margin(1, 1, 1, 0, "cm")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
                     expand = c(0,0),
                     position = "top") +
  scale_fill_manual(values = biome_color) +
  labs(title = "Iron-Ore Material Footprint disaggregated into six biomes (IO-MF-biome) for 32 regions\nfrom the iron-steel GPIOT and the EXIOBASE MRIO and for 2014")

ggsave("./output/Fig_Extra_SI_EXIOBASE_GPIOT_biomes.png",  # File name for the saved plot
       plot = last_plot(),  # The last plot created in the session
       width = 10,  # Width of the plot in inches
       height = 13,  # Height of the plot in inches
       dpi = 2050,
       bg = "white")  # Resolution (dots per inch), adjust as needed


write.xlsx(Compare_EXIO, "./output/Footprint_commparison_GPIOT_vs_EXIOBASE.xlsx")

tmp <- Result_EXIO$biome %>% 
  select(stressor, stressor_group) %>% 
  unique() %>% 
  arrange(desc(stressor_group)) %>% 
  rename("Sub-biome" = "stressor", "Biome-group" = "stressor_group")

write.xlsx(tmp, "./output/Biome_groupings.xlsx")
