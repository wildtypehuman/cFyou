library(dplyr)
library(ggplot2)
library(scales)

dat_AM_21425 <- read.csv("/Users/wildtype.human/bioinformatics/cFu/cFu_AM_21425_10925.csv")

# Filter and calculate CFU_TaqMan
dat_AM_21425 <- dat_AM_21425 %>%
  filter(
    Task == "Unknown",
    !is.na(Sample),
    grepl("21425", Sample) # Keep only samples containing "21425"
  ) %>%
  mutate(
    Quantity = ifelse(is.na(Quantity), 0, as.numeric(Quantity)),
    CFU_TaqMan = (Quantity / 0.00478) / 0.002 / (20 / 520)
  )

#Add metadata and rename

cFu_AM_20241219_Taq <- dat_AM_21425 %>%
  mutate(Date_Collected = "20241219", Collected_By = "AM")

# Export as CSV
write.csv(
  cFu_AM_20241219_Taq,
  file = "cFu_AM_20241219_Taq.csv",
  row.names = FALSE
)

# 
# # Define desired sample order (10925_1–5 then 21425_1–16)
# ordered_samples <- c(
#   paste0("AM_10925_", 1:5),
#   paste0("AM_21425_", 1:16)
# )
# 
# # Plot
# ggplot(dat_AM_21425, aes(x = factor(Sample, levels = ordered_samples), y = CFU_TaqMan)) +
#   geom_boxplot() +
#   scale_y_log10() +
#   theme_minimal() +
#   labs(
#     title = "AM_10925 & AM_21425 CFU from TaqMan (Log Scale)",
#     x = "Sample",
#     y = "CFU_TaqMan (per mL, log scale)"
#   ) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )