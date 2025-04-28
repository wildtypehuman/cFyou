library(dplyr)
library(ggplot2)
library(scales)    

# Combine the datasets
datS3_28 <- bind_rows(datS3, datS28)

# Prepare the data (with properly closed parentheses)
datS3_28 <- datS3_28 %>%
  filter(Group != "1") %>%
  mutate(
    Time_Point = factor(Time_Point, levels = c("Day 3", "Day 28"))
  ) %>%
  mutate(
    Treatment = case_when(
      Group == 2 ~ "None",                     # Group = 2 -> Treatment = None
      Group %in% c(3, 4, 5) ~ "BDQ",          # Group = 3, 4, or 5 -> Treatment = BDQ
      TRUE ~ NA_character_                     # Default to NA if no condition matches
    )
  )

# Filter datS3_28 into datNone to have a Treatmetn = None, make a box plot with Time_Point on the x axis, CFU_per_mL
# on the y axis color by Treatment
# Filter datS3_28 to create datNone
datBDQ <- datS3_28 %>%
  filter(Treatment == "BDQ") # Keep only rows where Treatment is "None"

# Add Treatment = "BDQ" to datI, 
# Make sure datI is freshly sorted
datI <- datI %>%
  mutate(Treatment = "BDQ")

# Bind datNone and modified datI
datBDQ <- bind_rows(datBDQ, datI)


# Summarize datNone for mean and standard error
datBDQ_summary <- datBDQ %>%
  group_by(Time_Point, Treatment) %>%
  summarize(
    mean_CFU = mean(CFU_per_mL, na.rm = TRUE),
    se_CFU = sd(CFU_per_mL, na.rm = TRUE) / sqrt(n())
  )

# Add a blank time point for "Day 1"
datBDQ_summary <- datBDQ_summary %>%
  bind_rows(
    tibble(
      Time_Point = factor("Day 1", levels = c("Input","Day 1", "Day 3", "Day 28")),
      Treatment = "BDQ",
      mean_CFU = NA_real_,
      se_CFU = NA_real_
    )
  )
# Update the factor levels for Time_Point
datBDQ_summary <- datBDQ_summary %>%
  mutate(
    Time_Point = factor(Time_Point, levels = c("Input", "Day 1", "Day 3", "Day 28"))
  )

# Define custom color
color_palette <- c("BDQ" = "#7CAE00")

# Create the plot
ggplot(datBDQ_summary, aes(
  x = Time_Point, 
  y = mean_CFU, 
  color = Treatment, 
  group = Treatment
)) +
  geom_point(size = 3) +                    # Plot points
  geom_line(data = subset(datBDQ_summary, Time_Point %in% c("Day 3", "Day 28")), size = 1) + # Connect Day 3 and Day 28
  geom_errorbar(aes(
    ymin = mean_CFU - se_CFU, 
    ymax = mean_CFU + se_CFU
  ), width = 0.2, size = 0.8) +            # Add error bars
  scale_y_log10(                           # Logarithmic scale for y-axis
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  scale_color_manual(values = color_palette) + # Apply custom color
  theme_minimal() +
  labs(
    title = "Mean Spleen CFU Counts",
    x = "Time Point",
    y = "Mean CFU (log scale)",
    color = "Treatment"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

