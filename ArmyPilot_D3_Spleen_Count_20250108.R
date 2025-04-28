library(ggplot2)
library(tidyr)
library(dplyr)


datC <- read.csv("/Users/wildtype.human/bioinformatics/cFu/Army_Prelim_CFU_Counts.csv")

datS3 <- datC %>%
  filter(Count > 1, Time_Point == "Day 3", Organ == "Spleen")

datS3 <- datS3 %>%
  mutate(
    Count = as.numeric(Count), 
    CFU_per_mL = Count * 50 * case_when(
      Dilution == "N2" ~ 100,
      Dilution == "N3" ~ 1000,
      Dilution == "N4" ~ 10000,
      TRUE ~ 1 # Default multiplier is 1 if Dilution is not "N6" or "N7"
    )
  )

# Box plot 


datS3 <- datS3 %>%
  mutate(
    Number = factor(Number, levels = 1:10),  # Ensure Number is a factor
    Group = as.factor(Group)                # Ensure Group is a factor
  )

ggplot(datS3, aes(
  x = Number,   
  y = CFU_per_mL, 
  fill = Group    # Use Group as a discrete variable
)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "Spleen D3 CFU Counts",
    x = "Spleen #",
    y = "CFU (log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )