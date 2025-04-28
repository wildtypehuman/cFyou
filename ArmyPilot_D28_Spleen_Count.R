library(ggplot2)
library(tidyr)
library(dplyr)


datC <- read.csv("/Users/wildtype.human/bioinformatics/cFu/Army_Prelim_CFU_Counts.csv")

datS28 <- datC %>%
  filter(Time_Point == "Day 28", Organ == "Spleen")

datS28 <- datS28 %>%
  mutate(
    Count = as.numeric(Count), 
    CFU_per_mL = Count * 20 * case_when(
      Dilution == "N0" ~ 1,
      Dilution == "N4" ~ 10000,
      TRUE ~ 1 # Default multiplier is 1 if Dilution is not "N6" or "N7"
    )
  )

# Box plot 


datS28 <- datS28 %>%
  mutate(
    Number = factor(Number, levels = 1:10),  # Ensure Number is a factor
    Group = as.factor(Group)                # Ensure Group is a factor
  )

ggplot(datS28, aes(
  x = Group,   
  y = CFU_per_mL, 
  color = Group    # Use Group as a discrete variable
)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "Spleen D 28 CFU Counts",
    x = "Group #",
    y = "CFU (log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )