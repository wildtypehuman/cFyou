library(dplyr)
library(ggplot2)
library(scales)

# Take datLNone_summary and make datLBDQ_summary by replacing Treatment from None to BDQ and Day 28 mean_CFU
# and se_CFU replace by 0 or mabye 1 for plating on a log scale 
datLBDQ_summary <- datLNone_summary %>%
  mutate(
    Treatment = "BDQ", # Change Treatment to BDQ
    mean_CFU = ifelse(Time_Point == "Day 28", 1, mean_CFU), # Replace Day 28 mean_CFU with 1
    se_CFU = ifelse(Time_Point == "Day 28", 0, se_CFU)      # Replace Day 28 se_CFU with 0
  )

# Update the factor levels for Time_Point
datLBDQ_summary <- datLBDQ_summary %>%
  mutate(
    Time_Point = factor(Time_Point, levels = c("Input", "Day 1", "Day 3", "Day 28"))
  )




# Define a custom color palette
color_palette <- c("None" = "#F8766D", "BDQ" = "#7CAE00") # Adjust colors if needed

# Create the plot
ggplot(datLBDQ_summary, aes(
  x = Time_Point, 
  y = mean_CFU, 
  color = Treatment, 
  group = Treatment
)) +
  geom_point(size = 3) +                    # Keep original style for points
  geom_line(data = subset(datLBDQ_summary, Time_Point %in% c("Day 1", "Day 28")), size = 1) + # Connect Day 1 and Day 28
  geom_errorbar(aes(
    ymin = mean_CFU - se_CFU, 
    ymax = mean_CFU + se_CFU
  ), width = 0.2, size = 0.8) +            # Keep error bars same style
  scale_y_log10(                           # Logarithmic scale for y-axis
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_color_manual(values = color_palette) + # Apply custom color palette
  theme_minimal(base_size = 14) +             # Minimal theme with adjusted font sizes
  labs(
    title = "Mean Lung CFU Counts",
    x = "Time Point",
    y = "Mean CFU (log scale)",
    color = "Treatment"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )