# Lung CFU trend for No treatment

library(dplyr)
library(ggplot2)
library(scales)  

# Add column Treatment to datI and datL1, all Treatment values = None
# Filter a datL28 from datC, where Time_Point = D28 and Organ = Lung
# Add column Treatmetn to datL28   
# Filter datL28None from datL28, keep values Treatment = None



# 1. Add column `Treatment` to datI and datL1, all Treatment values set to "None"
datI$Treatment <- "None"
datL1$Treatment <- "None"

# 2. Filter datL28 from datC where Time_Point = "D28" and Organ = "Lung"
datL28 <- datC[datC$Time_Point == "Day 28" & datC$Organ == "Lung", ]

# 3. Add column `Treatment` to datL28 with conditional assignment
datL28 <- datL28 %>%
  mutate(
    Treatment = case_when(
      Group == 2 ~ "None",                  # Group = 2 -> Treatment = "None"
      Group %in% c(3, 4, 5) ~ "BDQ",       # Group = 3, 4, or 5 -> Treatment = "BDQ"
      TRUE ~ NA_character_                 # Default to NA if no condition matches
    )
  )
datL28None <- datL28 %>%
  filter(Count > 0, Treatment == "None") %>%
  drop_na()


# Make a column CFU_per_mL in datL28None 


datL28None <- datL28None %>%
  mutate(
    Count = as.numeric(Count), 
    CFU_per_mL = Count * 50 * case_when(
      Dilution == "N0" ~ 1,
      Dilution == "N4" ~ 10000,
      TRUE ~ 1 # Default multiplier is 1 if Dilution is not "N6" or "N7"
    )
  )
# Combine the datasets
datLNone <- bind_rows(datI, datL1, datL28None)



# Summarize datNone for mean and standard error
datLNone_summary <- datLNone %>%
  group_by(Time_Point, Treatment) %>%
  summarize(
    mean_CFU = mean(CFU_per_mL, na.rm = TRUE),
    se_CFU = sd(CFU_per_mL, na.rm = TRUE) / sqrt(n())
  )

# Add a blank time point for "Day 3"
datLNone_summary <- datLNone_summary %>%
  bind_rows(
    tibble(
      Time_Point = factor("Day 3", levels = c("Input","Day 1", "Day 3", "Day 28")),
      Treatment = "None",
      mean_CFU = NA_real_,
      se_CFU = NA_real_
    )
  )
# Update the factor levels for Time_Point
datLNone_summary <- datLNone_summary %>%
  mutate(
    Time_Point = factor(Time_Point, levels = c("Input", "Day 1", "Day 3", "Day 28"))
  )



  
  # Define a custom color palette
  color_palette <- c("None" = "#F8766D", "BDQ" = "#7CAE00") # Adjust colors if needed
  
  # Create the plot
  ggplot(datLNone_summary, aes(
    x = Time_Point, 
    y = mean_CFU, 
    color = Treatment, 
    group = Treatment
  )) +
    geom_point(size = 3) +                    # Keep original style for points
    geom_line(data = subset(datLNone_summary, Time_Point %in% c("Day 1", "Day 28")), size = 1) + # Connect Day 1 and Day 28
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
































