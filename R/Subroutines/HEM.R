## Create supply use tables from raw data and compile IO model
SUT <- Load_SUT(type = "Results")                    
IOT <- Build_IOT(SUT = SUT,mode = "ixi")                          
## Build extensions
source( paste0(path$subroutines,"/Build_Extension.R") )


Z <- IOT$Z
x <- IOT$x
y <- rowSums(IOT$y)


# Basic technology matrix
A <- t(t(Z)/x)
A[A < 10^-6] <- 0

# Identity matrix
I <- diag(1, nrow(A),nrow(A))

# Inverse
L <- solve(I - A)

x <- rowSums(L%*%y)
sum(L)

# Define sectors to extract
extract_sec <- 4:6
base$industry$Name

# Get filter vector
filt <- Code$Z %>% 
  mutate(v = case_when(SectorCode %in% extract_sec ~ 0,
                       .default = 1)) %>% 
  pull(v)

# The remaining economy (= star)
A_star <- A * filt
Y_star <- y * filt 
L_star <- solve(I - A_star)

# The "extracted" economy
A_0 <- A - A_star

term_1 <- c( A_0 %*% L_star %*% Y_star )
FP <- ( L %*% diag( term_1 ) ) * (IOT$e[,4]/x)

colSums(IOT$e)[4]/10^6
sum(FP)/10^6

final <- Agg(FP, aggkey = Code$Z$RegionCode, dim = 1)
final <- Agg(final, aggkey = Code$Z$RegionCode, dim = 2)

# Aggregate to destination region in 13 region classification
tmp <- data.frame("from_region" = rep(1:32, 32),
                  "to_region" = rep(1:32, each = 32),
                  "value" = c(final)) %>% 
  left_join(base$region[,c("Code", "Region_new")],
            by = c("from_region" = "Code")) %>% 
  left_join(base$region[,c("Code", "Region_new")],
            by = c("to_region" = "Code")) %>% 
  rename("from_region_name" = "Region_new.x",
         "to_region_name" = "Region_new.y") %>% 
  group_by(to_region_name) %>% 
  summarise(value = sum(value)/10^9) %>% 
  mutate(Indicator = "MF-PBA") %>% 
  rename("Region" = "to_region_name",
         "Value" = "value") %>% 
  select(Region, Indicator, Value)

# Load ewMFA data from paper SI
SI <- read.xlsx("./output/Figure_data_for_SI.xlsx", sheet = "Figure_2_A") %>% 
  mutate(Indicator = case_when(Indicator == "RMC" ~ "MF-CBA",
                               .default = Indicator))

df <- SI %>% 
  bind_rows(tmp)

write.xlsx(df, file = "./output/Data_for_HEM_session.xlsx")

# Set font/text size for figures
legend_text_size <- 16
axis_title_size <- 16
axis_text_size <- 12

plot_data  <- df %>% 
  filter(Indicator %in% c("MF-CBA","MF-PBA", "DE")) %>% 
  mutate(Region = case_when(Region == "Asia and Pacific (nec)" ~ "Asia &\nPacific(nec)",
                            Region == "South America (nec)" ~ "South\nAmerica(nec)",
                            Region == "United States" ~ "United\nStates",
                            Region == "Middle East" ~ "Middle\nEast",
                            .default = Region)) %>% 
  mutate(Region = factor(Region, levels = c("China",
                                            "Europe",
                                            "United\nStates",
                                            "Asia &\nPacific(nec)",
                                            "Japan",
                                            "Middle\nEast",
                                            "South\nAmerica(nec)",
                                            "India",
                                            "Africa",
                                            "Russia",
                                            "Canada",
                                            "Brazil",
                                            "Australia")),
         Indicator = factor(Indicator, levels = c("DE", "MF-PBA","MF-CBA")))

# Create plot
plot_1 <- ggplot() +
  geom_bar(data = plot_data, aes(x = Region, y = Value, fill = Indicator) ,stat = "identity",position = "dodge",colour="black") +
  theme_minimal() +
  scale_fill_manual(labels = c("Domestic Iron Ore Extraction (DE)",
                               "Material Footprint of Steel Production (PB-MF)",
                               "Material Footprint of Steel Consumption (CB-MF)"),
                    values = c("yellow","#ED7D31", "#4472C4")) +
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
  scale_y_continuous("Iron Ore [Gt/year]",
                     limits = c(0,2.25),
                     expand = c(0,0)) +
  scale_x_discrete() +
  geom_vline(xintercept = seq(0.5, 14, by = 1), color="gray", size=.5, alpha=.5)

plot_1

ggsave("./output/HEM_session_PB_CB_DE_indicators.png",
       plot = plot_1,
       width = 13,
       height = 6,
       dpi = 1850,
       bg = "white")


