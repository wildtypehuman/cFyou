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
datNone <- datS3_28 %>%
  filter(Treatment == "None") # Keep only rows where Treatment is "None"

# Add Treatment = "None" to datI
datI <- datI %>%
  mutate(Treatment = "None")

# Bind datNone and modified datI
datNone <- bind_rows(datNone, datI)



# Summarize datNone for mean and standard error
datNone_summary <- datNone %>%
  group_by(Time_Point, Treatment) %>%
  summarize(
    mean_CFU = mean(CFU_per_mL, na.rm = TRUE),
    se_CFU = sd(CFU_per_mL, na.rm = TRUE) / sqrt(n())
  )

# Add a blank time point for "Day 1"
datNone_summary <- datNone_summary %>%
  bind_rows(
    tibble(
      Time_Point = factor("Day 1", levels = c("Day 1", "Day 3", "Day 28")),
      Treatment = "None",
      mean_CFU = NA_real_,
      se_CFU = NA_real_
    )
  )




# Create the dot plot with error bars
ggplot(datNone_summary, aes(
  x = Time_Point, 
  y = mean_CFU, 
  color = Treatment, 
  group = Treatment
)) +
  geom_point(size = 3) +                     # Add points
  geom_line(size = 1) +                     # Add connecting line
  geom_errorbar(aes(
    ymin = mean_CFU - se_CFU, 
    ymax = mean_CFU + se_CFU
  ), width = 0.2) +                         # Add error bars
  scale_y_log10(                            # Logarithmic scale for y-axis
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  theme_minimal() +
  labs(
    title = "Mean Spleen CFU Counts",
    x = "Time Point",
    y = "Mean CFU (log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )