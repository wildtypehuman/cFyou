library(dplyr)
library(ggplot2)
library(scales)

dat_AM_102424 <- read.csv("/Users/wildtype.human/bioinformatics/cFu/cFu_AM_102424.csv")

# 20 uL of sample 500 uL of PBS
dat_AM_102424 <- dat_AM_102424 %>%
  filter(Task == "Unknown" & !is.na(Sample)) %>%
  mutate(
    Quantity = ifelse(is.na(Quantity), 0, as.numeric(Quantity)),
    CFU_TaqMan = (Quantity / 0.00478) / 0.002/(20/520))

# Define desired sample order (10925_1–5 then 21425_1–16)
ordered_samples <- c(
  paste0("AM_102424_", 1:14)
 
)

# Plot
ggplot(dat_AM_102424, aes(x = factor(Sample, levels = ordered_samples), y = CFU_TaqMan)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "AM_102424 CFU from TaqMan (Log Scale)",
    x = "Sample",
    y = "CFU_TaqMan (per mL, log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )