#Plot ZH TaqMan results, data from Oct. 15
require(ggplot2)
require(tidyverse)

datZH <- read_csv("/Users/wildtype.human/bioinformatics/cFu/cFu_ZH_20241015.csv")

# Filter datZH to contain only values of Task = Unknown
# Add a new column to the datZH called CFU_TaqMan, obtain CFU_TaqMan values by dividing Quantity by 0.00478, 
# then to account for 150 µL in 500µL dilution multiply by (500/150), then to get to CFU_TaqMan/ 1mL multiply by 0.002
# the the final Quantity value should be in CFU_TaqMan/mL


datZH <- datZH %>%
  filter(Task == "Unknown" & !is.na(Sample)) %>%
  mutate(CFU_TaqMan = (Quantity / 0.00478) * (500 / 150) / 0.002)

# Make a boxplot of datZH with Sample on the x-axis and CFU_TaqMan on the y-axis 

ggplot(datZH, aes(x = Sample, y = CFU_TaqMan)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "ZH CFU assuming 0.00000478ng per cell",
    x = "Sample",
    y = "CFU_TaqMan (per mL)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(datZH, aes(x = factor(Sample, levels = paste0("ZH_Oct15_", 1:22)), y = CFU_TaqMan)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "ZH CFU from TaqMqn assuming 0.00000478ng per cell (Log Scale)",
    x = "Sample",
    y = "CFU_TaqMan (per mL, log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

