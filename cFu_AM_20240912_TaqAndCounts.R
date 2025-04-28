library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(stringr)

dat_AM_Count <- read.csv("/Users/wildtype.human/bioinformatics/cFu/AM_081524_Counts.csv")

#Pivot Longer
dat_AM_Count <- dat_AM_Count %>%
  pivot_longer(
    cols = -Sample,
    names_to = "Dilution",
    values_to = "Count"
  )

#Alex plated 50 microliters of 1 to 200 dilution of the spleen and 50 microliters of 1 to 20,000 dilution of the lung.

dat_AM_Count <- dat_AM_Count %>%
  mutate(
    CFU_Count = case_when(
      str_starts(Dilution, "N_2") ~ Count * 200 * 20,
      str_starts(Dilution, "N_4") ~ Count * 20000 * 20,
      TRUE ~ NA_real_
    )
  )


combined_df <- left_join(dat_AM_Count, dat_AM_91224, by = "Sample")

combined_df <- combined_df %>%
  select(-Well, -Omit, -Target, -Dye, -Task)


cFu_AM_20240912_TaqAndCounts <- combined_df %>%
  mutate(Date_Collected = "20240912", Collected_By = "AM")

# Export as CSV
write.csv(
  cFu_AM_20240912_TaqAndCounts,
  file = "cFu_AM_20240912_TaqAndCounts.csv",
  row.names = FALSE
)






