library(dplyr)
library(ggplot2)
library(scales)


dat_WZ_121124 <- read.csv("/Users/wildtype.human/bioinformatics/cFu/cFu_WZ121124.csv")

dat_WZ_121124 <- dat_WZ_121124 %>%
  filter(Task == "Unknown" & !is.na(Sample)) %>%
  mutate(
    Quantity = ifelse(is.na(Quantity), 0, as.numeric(Quantity)),
    CFU_TaqMan = (Quantity / 0.00478) / 0.002
  )

ordered_samples <- c(
  "mLN_S1", "Spleen_S1",
  "mLN_S2", "Spleen_S2",
  "mLN_S3", "Spleen_S3",
  "mLN_S4", "Spleen_S4",  
  "mLN_S5", "Spleen_S5",
  "mLN_B1", "Spleen_B1",
  "mLN_B2", "Spleen_B2",
  "mLN_B3", "Spleen_B3",
  "mLN_B4", "Spleen_B4"   
)

# Plot
ggplot(dat_WZ_121124, aes(x = factor(Sample, levels = ordered_samples), y = CFU_TaqMan)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "WZ CFU from TaqMan assuming 0.00000478ng per cell (Log Scale)",
    x = "Sample",
    y = "CFU_TaqMan (per mL, log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )