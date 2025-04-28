library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(stringr)

dat_ZH_Count <- read.csv("/Users/wildtype.human/bioinformatics/cFu/cFu_ZH_20241015Counts.csv")

#Pivot Longer
dat_ZH_Count <- dat_ZH_Count %>%
  pivot_longer(
    cols = -Sample,
    names_to = "Dilution",
    values_to = "Count"
  )

#Zach plates 50uL per 12 wellplate, so transform per mL


dat_ZH_Count <- dat_ZH_Count %>%
  mutate(
    CFU_Count = case_when(
      str_starts(Dilution, "N3") ~ Count * 1000 * 20,
      str_starts(Dilution, "N4") ~ Count * 10000 * 20,
      str_starts(Dilution, "N5") ~ Count * 100000 * 20,
      TRUE ~ NA_real_
    )
  )

combined_df <- left_join(dat_ZH_Count, datZH, by = "Sample")

cFu_ZH_20241015_TaqAndCount <- combined_df %>%
  select(-Well, -Omit, -Target, -Dye, -Task)

cFu_ZH_20241015_TaqAndCount <- cFu_ZH_20241015_TaqAndCount %>%
  mutate(Date_Collected = "20241015", Collected_By = "ZH")

# Export as CSV
write.csv(
  cFu_ZH_20241015_TaqAndCount,
  file = "cFu_ZH_20241015_TaqAndCount.csv",
  row.names = FALSE
)

