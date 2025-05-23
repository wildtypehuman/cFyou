library(dplyr)
library(ggplot2)
library(scales)

dat_AM_91224 <- read.csv("/Users/wildtype.human/bioinformatics/cFu/cFu_AM_91224.csv")

# 20 uL of sample 500 uL of PBS
dat_AM_91224 <- dat_AM_91224 %>%
  filter(Task == "Unknown" & !is.na(Sample)) %>%
  mutate(
    Quantity = ifelse(is.na(Quantity), 0, as.numeric(Quantity)),
    CFU_TaqMan = (Quantity / 0.00478) / 0.002/(20/520))

# Add metadata and rename

cFu_AM_20240912_Taq <- dat_AM_91224 %>%
  mutate(Date_Collected = "20240912", Collected_By = "AM")

#Export as CSV
write.csv(
  cFu_AM_20240912_Taq,
  file = "cFu_AM_20240912_Taq.csv",
  row.names = FALSE
)

# # Define the desired order of Sample levels
# ordered_samples <- c(
#   paste0("AM_91224_L_", 1:12),
#   paste0("AM_91224_S_", 1:12)
# )
# 
# # Plot
# ggplot(dat_AM_91224, aes(x = factor(Sample, levels = ordered_samples), y = CFU_TaqMan)) +
#   geom_boxplot() +
#   scale_y_log10() +
#   theme_minimal() +
#   labs(
#     title = "AM_91224 CFU from TaqMan assuming 0.00000478ng per cell (Log Scale)",
#     x = "Sample",
#     y = "CFU_TaqMan (per mL, log scale)"
#   ) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)
  )