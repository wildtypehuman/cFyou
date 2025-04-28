library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(stringr)

df_AM_Count <- read.csv("/Users/wildtype.human/bioinformatics/cFu/AM_112224_Count.csv")


df_AM_Count <- df_AM_Count %>%
  pivot_longer(
    cols = -Sample,
    names_to = "Dilution",
    values_to = "Count"
  )

df_AM_Count <- df_AM_Count %>%
  mutate(CFU_Count = case_when(
    str_starts(Dilution, "N1") ~ Count * 10 * 20,
    str_starts(Dilution, "N2") ~ Count * 100 * 20,
    str_starts(Dilution, "N3") ~ Count * 1000 * 20,
    TRUE ~ NA_real_
  ))

combined_df <- left_join(dat_AM_21425, df_AM_Count, by = "Sample")

combined_df <- combined_df %>%
  select(-Well, -Omit, -Target, -Dye, -Task)

cFu_AM_20241219_TaqAndCount <- combined_df %>%
  mutate(Date_Collected = "20241219", Collected_By = "AM")

# Export as CSV
write.csv(
  cFu_AM_20241219_TaqAndCount,
  file = "cFu_AM_20241219_TaqAndCount.csv",
  row.names = FALSE
)

