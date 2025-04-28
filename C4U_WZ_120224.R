library(tidyr)
library(ggplot2)
library(dplyr)

datWZ <- read.csv("/Users/wildtype.human/bioinformatics/cFu/WZ_12_2_2025_01_28.csv")

# 500 µL sample 


datWZ <- datWZ %>%
  filter(Task == "Unknown" & !is.na(Sample)) %>%
  mutate(CFU_TaqMan = replace_na((Quantity / 0.00478) / 0.002, 0))



ggplot(datWZ, aes(x = Sample, y = CFU_TaqMan)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "WZ CFU from TaqMqn assuming 0.00000478ng per cell (Log Scale)",
    x = "Sample",
    y = "CFU_TaqMan (per mL, log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )