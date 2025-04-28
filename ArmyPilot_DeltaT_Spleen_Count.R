# Spleens all together Counts
library(dplyr)
library(ggplot2)
library(scales)    

datS3_28 <- bind_rows(datS3, datS28)

# Prepare the data (keeping your existing data preparation)
datS3_28 <- datS3_28 %>%
  filter(Group != "1") %>%
  mutate(
    Time_Point = factor(Time_Point, levels = c("Day 3", "Day 28")),  
    Time_Point_staggered = as.numeric(as.factor(Time_Point)) + 
      as.numeric(as.factor(Group)) * 0.1  
  )

# Create summary statistics
combined_data_summary <- datS3_28 %>%
  group_by(Time_Point_staggered, Group, Time_Point) %>%
  summarise(
    mean_CFU = mean(CFU_per_mL, na.rm = TRUE),
    sd_CFU = sd(CFU_per_mL, na.rm = TRUE),
    .groups = "drop"
  )

# Mapping for x-axis labels
time_point_mapping <- combined_data_summary %>%
  select(Time_Point_staggered, Time_Point) %>%
  distinct()

# Create enhanced plot
ggplot(combined_data_summary, 
       aes(x = Time_Point_staggered, y = mean_CFU, color = Group, group = Group)) +
  # Add connecting lines with controlled opacity
  geom_line(size = 1) +
  # Add error bars with clean aesthetics
  geom_errorbar(
    aes(ymin = mean_CFU - sd_CFU, ymax = mean_CFU + sd_CFU),
    width = 0.1,
    size = 0.5
  ) +
  # Add points with larger size
  geom_point(size = 3) +
  # Set y-axis to log scale with prettier breaks
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  # Keep original x-axis ordering
  scale_x_continuous(
    breaks = time_point_mapping$Time_Point_staggered,
    labels = time_point_mapping$Time_Point
  ) +
  # Add labels
  labs(
    title = "Changes in CFU Over Time by Group",
    x = "Time Point",
    y = expression(paste("CFU Counts (", log[10], " scale)")),
    color = "Group"
  ) +
  # Apply customized theme_minimal
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Changed to 45 degrees
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"),
    legend.position = "top",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )