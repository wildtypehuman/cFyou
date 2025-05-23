library(dplyr)
library(ggplot2)
library(scales)

dat_AM_10925 <- read.csv("/Users/wildtype.human/bioinformatics/cFu/cFu_AM_21425_10925.csv")

# Filter and calculate CFU_TaqMan
dat_AM_10925 <- dat_AM_10925 %>%
  filter(
    Task == "Unknown",
    !is.na(Sample),
    grepl("10925", Sample) # Keep only samples containing "21425"
  ) %>%
  mutate(
    Quantity = ifelse(is.na(Quantity), 0, as.numeric(Quantity)),
    CFU_TaqMan = (Quantity / 0.00478) / 0.002 / (20 / 520)
  )

#Add metadata

cFu_AM_20250109_Taq <- dat_AM_10925 %>%
  mutate(Date_Collected = "20250109", Collected_By = "AM")

#Export final TaqMan data

write.csv(cFu_AM_20250109_Taq, file = "cFu_AM_20250109_Taq.csv", row.names = FALSE)

# Export as CSV
# write.csv(
#   dat_AM_10925,
#   file = "dat_AM_10925.csv",
#   row.names = FALSE
# )