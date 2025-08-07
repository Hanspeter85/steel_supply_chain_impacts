# Units: MF = tons; eHANPP = tC; Land = km2


df <- SI$IOMF_eLand_eHANPP_GAS %>% 
  filter(stressor %in% c("eLand-steel","eHANPP-steel")) %>% 
  group_by(source_region, destination_region_group, stressor) %>% 
  summarize(BAU = sum(value), .groups = 'drop') 

PBA <- df %>% 
  group_by(source_region, stressor) %>% 
  summarize(BAU = sum(BAU), .groups = 'drop') %>%
  pivot_wider(names_from = stressor,
              values_from = BAU) %>% 
  rename("eHANPP" = 'eHANPP-steel',
         "eLand" = 'eLand-steel') %>% 
  mutate(intensity = eHANPP/eLand,
         mean = 1,
         SD = case_when(source_region == "Canada" ~ 0.20,
                        source_region == "RoW Asia and Pacific" ~ 0.20,
                        .default = .20)) 



  
Origi <- df %>% 
  filter(stressor == "eLand-steel") %>% 
  select(source_region, destination_region_group, BAU) %>% 
  pivot_wider(names_from = destination_region_group,
              values_from = BAU) %>% 
  select(-source_region) %>% 
  as.matrix()

## 1. Exponential function

# Function to sample from the custom distribution
# sample_custom_exponential <- function(n, lambda) {
#   # Generate uniform random numbers
#   u <- runif(n)
#   
#   # Apply the inverse CDF to generate samples
#   samples <- 1 + log(1 - u * (1 - exp(-lambda * 0.5))) / -lambda
#   
#   return(samples)
# }
# 
# # Parameters
# lambda <- 10  # Steepness of the exponential decay
# n <- 1000000  # Number of samples
# 
# # Generate samples
# samples <- 2 - sample_custom_exponential(n, lambda)
# 
# # Sort the samples for cumulative probability calculation
# sorted_samples <- sort(samples)
# 
# # Calculate cumulative probabilities
# cumulative_probabilities <- seq(1, length(sorted_samples)) / length(sorted_samples)
# 
# jpeg("./output/Fig_CDF_monte_carlo_simulation.jpg", width = 350, height = "350")
# 
# # Plot the cumulative density function
# plot(sorted_samples, cumulative_probabilities, type = "l", col = "blue", lwd = 2,
#      main = "Cumulative Probablity Density Function (CDF)\nfor the Monte Carlo Simulation of the host-metal share in iron ore mining land use",
#      xlab = "Value", ylab = "Cumulative Probability")
# 
# # Add grid lines for better visualization
# grid(ny = 10)
# 
# ggsave("./output/Fig_CDF_monte_carlo_simulation.png",  # File name for the saved plot
#        plot = plot_CDF,  # The last plot created in the session
#        width = 10,  # Width of the plot in inches
#        height = 8,  # Height of the plot in inches
#        dpi = 1000,
#        bg = "white")  # Resolution (dots per inch), adjust as needed


## 1. Left-skewed normal distribution

LeftSkewedNormal <- function()
{
  # Generate samples from a skew-normal distribution
  samples <- rsn(n = n, xi = mean, omega = sd, alpha = skewness)
  
  return(samples)
}

# mean <- 1
# sd <- 0.2
# n <- 10000000
# skewness <- 0  # Negative value for left skewness
# 
# # Generate samples
# samples <- rsn(n = n, xi = mean, omega = sd, alpha = skewness)
# 
# samples <- rsn(n = nrow(PBA), xi = PBA$mean, omega = PBA$SD, alpha = skewness)
# 
# # Plot the histogram
# hist(samples, breaks = 1000, probability = TRUE, main = "Left-Skewed Normal Distribution",
#      xlab = "Value", col = "lightblue")
# 
# # Add a vertical line for the mean
# abline(v = mean(samples), col = "red", lwd = 2, lty = 2)
# 
# # Add a density curve
# lines(density(samples), col = "darkblue", lwd = 2)





## 2. LogNormal distribution

# Create function
LogNormal <- function(m, s)
{
  location <- log(m^2 / sqrt(s^2 + m^2))
  shape <- sqrt(log(1 + (s^2 / m^2)))
  right_skewed <- rlnorm(n=length(m), location, shape)
  left_skewed <- 2-right_skewed
  left_skewed[left_skewed < 0 ] <- 1
  
  return(left_skewed)
}


i_max <- 100000

tmp <- matrix(NA, nrow = nrow(PBA),ncol = i_max)  

MP <- list("LogNormal" = tmp,
           "Exp" = tmp)

FP <- list("Land" = list(),
           "HANPP" = list())

tmp <- matrix(NA, nrow = ncol(Origi),ncol = i_max)

FP$Land[["LogNormal"]] <- tmp
FP$HANPP[["LogNormal"]] <- tmp
FP$Land[["Normal"]] <- tmp
FP$HANPP[["Normal"]] <- tmp

FP$Land$LogNormal %>% dim()


i <- 1
for(i in 1:i_max)
{
  print(i)
  
  ## 1. LogNormal calculation
  tmp <- LogNormal(PBA$mean, PBA$SD)
  MP$LogNormal[,i] <- tmp
  
  # Land Footprints
  FP$Land$LogNormal[,i] <- colSums( Origi * tmp )
  
  # HANPP Footprints
  FP$HANPP$LogNormal[,i] <- colSums( Origi * tmp * PBA$intensity)
  
  ## 2. Normal distribution
  tmp <- sapply(PBA$SD, function(sd) {
    rsn(n = 1, xi = 1, omega = sd, alpha = 0)
  })
  
  MP$LogNormal[,i] <- tmp
  
  # Land Footprints
  FP$Land$Normal[,i] <- colSums( Origi * tmp )
  
  # HANPP Footprints
  FP$HANPP$Normal[,i] <- colSums( Origi * tmp * PBA$intensity)
}


FP[["LogNormal"]] <- bind_rows(as.data.frame(FP$Land$LogNormal) %>%
    mutate(destination_region_group = colnames(Origi), Indicator = "eLand-steel"),
  as.data.frame(FP$HANPP$LogNormal) %>%
    mutate(destination_region_group = colnames(Origi), Indicator = "eHANPP-steel")) %>%
  pivot_longer(cols = -c(destination_region_group, Indicator), names_to = "Simulation", values_to = "Value")

FP[["Normal"]] <- bind_rows(as.data.frame(FP$Land$Normal) %>%
                                 mutate(destination_region_group = colnames(Origi), Indicator = "eLand-steel"),
                               as.data.frame(FP$HANPP$Normal) %>%
                                 mutate(destination_region_group = colnames(Origi), Indicator = "eHANPP-steel")) %>%
  pivot_longer(cols = -c(destination_region_group, Indicator), names_to = "Simulation", values_to = "Value")


summary <- list()
mode_list <- c("LogNormal","Normal")

mode <- "LogNormal"
for(mode in mode_list)
{
  summary[[mode]] <- as.data.frame(FP[[mode]]) %>%
  group_by(destination_region_group, Indicator) %>%
  summarise(
    Mean = mean(Value),
    Min = min(Value),
    Max = max(Value),
    Q1 = quantile(Value, 0.25),  # 25th percentile
    Q3 = quantile(Value, 0.75)   # 75th percentile
  ) %>% 
  left_join(df %>% 
              group_by(destination_region_group, stressor) %>% 
              summarize(Actual_mean = sum(BAU)),
            by = c("destination_region_group" = "destination_region_group",
                    "Indicator" = "stressor")) %>% 
  mutate(diff_rel = 100*(Actual_mean-Mean)/Actual_mean,
         Mean_rel = 1,
         Min_rel = Min/Mean,
         Max_rel = Max/Mean,
         Q1_rel = Q1/Mean,
         Q3_rel = Q3/Mean)
}




for(mode in mode_list)
{
  sort_reg <- summary[[mode]] %>% 
    filter(Indicator == "eLand-steel") %>% 
    mutate(Q_range = Q3_rel - Q1_rel) %>% 
    arrange(desc(Q_range)) %>% 
    pull(destination_region_group)
  
  summary[[mode]] <- summary[[mode]] %>% 
    mutate(destination_region_group = factor(destination_region_group, levels = sort_reg))  
}



ggplot(summary$LogNormal, aes(x = Indicator, y = Mean_rel)) +
  facet_wrap(~destination_region_group,
             ncol = 13) +
  geom_point(size = 3,
             color = "blue") +  # Mean as points
  geom_errorbar(aes(ymin = Min_rel, ymax = Max_rel), width = 0.2, color = "red") +  # Min-Max range
  geom_errorbar(aes(ymin = Q1_rel, ymax = Q3_rel), width = 0.4, color = "green") +  # Interquartile range
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(
    title = "Monte Carlo Simulation of production-based land intensities of iron ore mining for eLand-steel and eHANPP-steel indicators",
    x = "Country",
    y = "Value",
    caption = "Blue: Mean, Red: Min-Max, Green: Interquartile Range") +
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
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1.5),
                     labels = scales::percent_format(accuracy = 1),
                     breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5))



ggplot(summary$Normal, aes(x = Mean_rel, y = Indicator)) +
  facet_wrap(~destination_region_group,
             nrow = 13) +
  geom_point(size = 3,
             color = "blue") +  # Mean as points
  geom_errorbar(aes(xmin = Min_rel, xmax = Max_rel), width = 0.2, color = "red") +  # Min-Max range
  geom_errorbar(aes(xmin = Q1_rel, xmax = Q3_rel), width = 0.4, color = "green") +  # Interquartile range
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(
    title = "Monte Carlo Simulation of land intensities of iron ore mining for the eLand-steel and eHANPP-steel indicators of 13 regions\n(100,000 iterations were performed, assuming a normal distribution with a standard deviation of 20%)",
    y = "Country",
    x = "Value",
    caption = "Blue: Mean, Red: Min-Max, Green: Interquartile Range") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
        axis.title = element_blank(),
        axis.text = element_text(colour = "black", size = 12),
#        axis.text.x = element_text(angle = 45, vjust = .5),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.placement = "outside",
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0.2,1.8),
                     labels = scales::percent_format(accuracy = 1),
                     breaks = c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5, 1.6, 1.7, 1.8))

ggsave("./output/Fig_MonteCarloSimulation.tiff",  # File name for the saved plot
       plot = last_plot(),  # The last plot created in the session
       width = 15,  # Width of the plot in inches
       height = 10,  # Height of the plot in inches
       dpi = 1000,
       bg = "white")  # Resolution (dots per inch), adjust as needed


tmp <- summary$Normal %>% 
  group_by(Indicator) %>% 
  mutate(Mean_total = sum(Mean)) %>% 
  ungroup() %>% 
  select(-Actual_mean, -diff_rel, -Mean_rel, -Min_rel, -Max_rel, -Q1_rel, -Q3_rel) %>% 
  mutate(Mean_rel = 100*Mean/Mean_total,
         Min_rel = 100*Min/Mean_total,
         Max_rel = 100*Max/Mean_total,
         Q1_rel = 100*Q1/Mean_total,
         Q3_rel = 100*Q3/Mean_total)


ranking <- tmp %>%
  filter(Indicator == "eHANPP-steel") %>% 
  arrange(-desc(Mean_rel)) %>% 
  pull(destination_region_group)

tmp_log10 <- tmp %>% 
  mutate(destination_region_group = factor(destination_region_group, levels = ranking))

ggplot(tmp_log10, aes(x = Mean_rel, y = destination_region_group)) +
  facet_wrap(~Indicator,
#             nrow = 13,
             ncol = 2) +
  geom_point(size = 3,
             color = "blue") +  # Mean as points
  geom_errorbar(aes(xmin = Min_rel, xmax = Max_rel), width = 0.2, color = "red") +  # Min-Max range
  geom_errorbar(aes(xmin = Q1_rel, xmax = Q3_rel), width = 0.4, color = "green") +  # Interquartile range
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(
    title = "Monte Carlo Simulation of land intensities of iron ore mining for the eLand-steel and eHANPP-steel indicators of 13 regions\n(100,000 iterations were performed, assuming a normal distribution with a standard deviation of 20%)",
    y = "Country",
    x = "Percentage of global pressure/impact (log10 scale)",
    caption = "Blue: Mean, Red: Min-Max, Green: Interquartile Range") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
        axis.title.y = element_blank(),
        axis.text = element_text(colour = "black", size = 12),
        #        axis.text.x = element_text(angle = 45, vjust = .5),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.placement = "outside",
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
  scale_x_log10()

  # scale_x_continuous(
  #   expand = c(0,0),
  #                    limits = c(0.2,1.8),
  #                    labels = scales::percent_format(accuracy = 1),
  #                   breaks = c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5, 1.6, 1.7, 1.8)
  #                    ) 


ggsave("./output/Fig_MonteCarloSimulation_log10.tiff",  # File name for the saved plot
       plot = last_plot(),  # The last plot created in the session
       width = 12,  # Width of the plot in inches
       height = 10,  # Height of the plot in inches
       dpi = 1000,
       bg = "white")  # Resolution (dots per inch), adjust as needed














library(ggridges)

CBA <- df %>% 
  filter(stressor == "eLand-steel") %>% 
  group_by(destination_region_group) %>% 
  summarize(BAU = sum(BAU), .groups = 'drop') 

plot_dat <- FP$Exp %>% 
  filter(Indicator == "eLand-steel") %>% 
  left_join(CBA,
            by = c("destination_region_group")) %>% 
  mutate("Baseline" = 100 * Value/BAU)

ggplot(plot_dat,
       aes(x = Baseline, y = destination_region_group, fill = destination_region_group)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
